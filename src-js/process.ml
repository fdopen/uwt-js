(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Lwt_process
 * Copyright (C) 2009 Jérémie Dimino
 * Copyright (C) 2015 Andreas Hauptmann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

open Internal
open Itypes
open Lwt.Infix

type command = string * string array

let shell =
    if Js_sys.win32 then
      fun cmd -> ("cmd.exe", [|"cmd.exe"; "/c"; cmd|])
    else
      fun cmd -> ("/bin/sh", [|"/bin/sh"; "-c"; cmd|])

type redirection =
    [ `Keep
    | `Dev_null
    | `Close
    | `FD_copy of file_descr
    | `FD_move of file_descr
    | `File_copy of file
    | `File_move of file
    | `Pipe_copy of Pipe.t
    | `Pipe_move of Pipe.t
    | `Stream_copy of Stream.t
    | `Stream_move of Stream.t
    ]

type iredirection =
  [ redirection | `Pipe of Pipe.t ]

let get_fd fd = function
| `Keep ->   Some (Subproc.Inherit_file fd)
| `Pipe x -> Some (Subproc.Create_pipe x)
| `Dev_null (* libuv always redirects stdin, stdout, stderr to /dev/null *)
| `Close   -> None
| `Pipe_move p
| `Pipe_copy p -> Some (Subproc.Inherit_pipe p)
| `Stream_copy s
| `Stream_move s -> Some (Subproc.Inherit_stream s)
| `File_copy s
| `File_move s -> Some (Subproc.Inherit_file s)
| `FD_copy fd'
| `FD_move fd' -> Some (Subproc.Inherit_file fd')


type res = {
  exit_status: int;
  term_signal: int;
}

type proc = {
  t : Subproc.t;
  sleeper : res Lwt.t
}

let spawn
    ?uid
    ?gid
    ?env
    ?cwd
    ?(stdin:iredirection=`Keep)
    ?(stdout:iredirection=`Keep)
    ?(stderr:iredirection=`Keep)
    ((prog, args):command)
  =
  let pstdin  = get_fd Internal.stdin stdin
  and pstdout = get_fd Internal.stdout stdout
  and pstderr = get_fd Internal.stderr stderr
  and sleeper,waker = Lwt.task () in
  let exit_cb t ~exit_status ~term_signal =
    Subproc.close_noerr t;
    Lwt.wakeup waker {exit_status; term_signal}
  in
  let env = match env with
  | None -> None
  | Some x -> Some(Array.to_list x)
  in
  let prog =
    if prog = "" && Array.length args > 0 then
      args.(0)
    else
      prog in
  let t =
    Subproc.spawn_exn
      ?uid
      ?gid
      ?cwd
      ?env
      ?stdin:pstdin
      ?stdout:pstdout
      ?stderr:pstderr
      ~exit_cb
      prog
      (Array.to_list args)
  in
  let close = function
  | `FD_move fd -> Lwt.async (fun () -> Fs.Async.close (Obj.magic fd))
  | `Pipe_move p -> Pipe.close_noerr p
  | `Stream_move s -> Stream.close_noerr s
  | `File_move s -> let _ : unit Lwt.t = Fs.Async.close s in ()
  | _ -> () in
  close stdin;
  close stdout;
  close stderr;
  {
    t;
    sleeper
  }

type state =
  | Running
  | Exited of Unix.process_status


let status x = (* WSTOPPED not provided libuv *)
  if x.term_signal <> 0 then
    Unix.WSIGNALED(x.term_signal)
  else
    Unix.WEXITED(x.exit_status)


class virtual common timeout proc channels =
  let wait = proc.sleeper in
object(self)
  val mutable closed = false
  method pid = Subproc.pid_exn proc.t

  method state =
    match Lwt.poll wait with
      | None -> Running
      | Some x -> Exited (status x)

  method kill signum =
    if Lwt.state wait = Lwt.Sleep then
      Subproc.process_kill_exn proc.t signum

  method terminate =
    if Lwt.state wait = Lwt.Sleep then
      Subproc.process_kill_exn proc.t Sys.sigkill

  method close =
    if closed then self#status
    else (
      closed <- true;
      Lwt.protected (Lwt.join (List.map Io.close channels))
      >>= fun () -> self#status
    )
  method status = Lwt.protected wait >|= status

  initializer
    (* Ensure channels are closed when no longer used. *)
    (* TODO: What? List.iter (Gc.finalise ignore_close) channels; *)
    (* Handle timeout. *)
    match timeout with
      | None ->
          ()
      | Some dt ->
          ignore (
            (* Ignore errors since they can be obtained by
               self#close. *)
            Lwt.try_bind
              (fun () ->
                 Lwt.choose [(Timer.sleep_seconds dt >>= fun () ->
                              Lwt.return_false);
                             (wait >>= fun _ -> Lwt.return_true)])
              (function
                 | true ->
                     Lwt.return_unit
                 | false ->
                     self#terminate;
                     self#close >>= fun _ -> Lwt.return_unit)
              (fun _exn ->
                 (* The exception is dropped because it can be
                    obtained with self#close. *)
                 Lwt.return_unit)
          )
end

external cast_chan : 'a Io.channel -> unit Io.channel = "%identity"

class process_none ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd =
  let stdin = (stdin : redirection option :> iredirection option)
  and stdout = (stdout : redirection option :> iredirection option)
  and stderr = (stderr : redirection option :> iredirection option) in
  let proc = spawn ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd in
object
  inherit common timeout proc []
end

class process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  let stdin = (stdin : redirection option :> iredirection option)
  and stdout = Pipe.init ()
  and stderr = (stderr : redirection option :> iredirection option) in
  let proc = spawn ?uid ?gid ?env ?cwd ?stdin ~stdout:(`Pipe stdout) ?stderr cmd in
  let stdout =
    Io.of_stream
      ~mode:Io.input
      (Pipe.to_stream stdout)
  in
object
  inherit common timeout proc [cast_chan stdout]
  method stdout = stdout
end

class process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd =
  let stdin = Pipe.init ()
  and stdout = (stdout : redirection option :> iredirection option)
  and stderr = (stderr : redirection option :> iredirection option) in
  let proc = spawn ?cwd ?uid ?gid ?env ~stdin:(`Pipe stdin) ?stdout ?stderr cmd in
  let stdin =
    Io.of_stream
      ~mode:Io.output
      (Pipe.to_stream stdin)
  in
object
  inherit common timeout proc [cast_chan stdin]
  method stdin = stdin
end

class process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd =
  let stdin = Pipe.init ()
  and stdout = Pipe.init ()
  and stderr = (stderr : redirection option :> iredirection option) in
  let proc =
    spawn ?uid ?gid ?env ?cwd ~stdin:(`Pipe stdin) ~stdout:(`Pipe stdout) ?stderr cmd
  in
  let stdin =
    Io.of_stream
      ~mode:Io.output
      (Pipe.to_stream stdin)
  and stdout =
    Io.of_stream
      ~mode:Io.input
      (Pipe.to_stream stdout)
  in
object
  inherit common timeout proc [cast_chan stdin; cast_chan stdout]
  method stdin = stdin
  method stdout = stdout
end

class process_full ?timeout ?uid ?gid ?env ?cwd cmd =
  let stdin = Pipe.init ()
  and stdout = Pipe.init ()
  and stderr = Pipe.init () in
  let proc =
    spawn ?uid ?gid ?env ?cwd
      ~stdin:(`Pipe stdin)
      ~stdout:(`Pipe stdout)
      ~stderr:(`Pipe stderr)
      cmd
  in
  let stdin =
    Io.of_stream
      ~mode:Io.output
      (Pipe.to_stream stdin)
  and stdout =
    Io.of_stream
      ~mode:Io.input
      (Pipe.to_stream stdout)
  and stderr =
    Io.of_stream
      ~mode:Io.input
      (Pipe.to_stream stderr)
  in
object
  inherit common timeout proc [cast_chan stdin; cast_chan stdout; cast_chan stderr]
  method stdin = stdin
  method stdout = stdout
  method stderr = stderr
end

let open_process_none ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd =
  new process_none ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd
let open_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  new process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd
let open_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd =
  new process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd
let open_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd =
  new process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd
let open_process_full ?timeout ?uid ?gid ?env ?cwd cmd =
  new process_full ?timeout ?uid ?gid ?env ?cwd cmd


let make_with backend ?timeout ?uid ?gid ?env ?cwd cmd f =
  let process = backend ?timeout ?uid ?gid ?env ?cwd cmd in
  Lwt.finalize
    (fun () -> f process)
    (fun () ->
      process#close >>= fun _ ->
      Lwt.return_unit)

let with_process_none ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd f =
  make_with (open_process_none ?stdin ?stdout ?stderr) ?timeout ?uid ?gid ?env ?cwd cmd f
let with_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd f =
  make_with (open_process_in ?stdin ?stderr) ?timeout ?uid ?gid ?env ?cwd cmd f
let with_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd f =
  make_with (open_process_out ?stdout ?stderr) ?timeout ?uid ?gid ?env ?cwd cmd f
let with_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd f =
  make_with (open_process ?stderr) ?timeout ?uid ?gid ?env ?cwd cmd f
let with_process_full ?timeout ?uid ?gid ?env ?cwd cmd f =
  make_with open_process_full ?timeout ?uid ?gid ?env ?cwd cmd f

let exec ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd =
  (open_process_none ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd)#close


let read_opt read ic =
  Lwt.catch
    (fun () -> read ic >|= fun x -> Some x)
    (function
    | End_of_file -> Lwt.return_none
    | Unix.Unix_error(Unix.EPIPE, _, _) when Js_sys.win32 -> Lwt.return_none
    | Unix.Unix_error(x, _, _)  when Int_result.unix_error_to_error x = EOF ->
      Lwt.return_none
    | exn -> Lwt.fail exn)

let recv_chars pr =
  let ic = pr#stdout in
  (* TODO: What? Gc.finalise ignore_close ic; *)
  Lwt_stream.from (fun _ ->
      read_opt Io.read_char ic >>= fun x ->
      if x = None then begin
        Io.close ic >>= fun () ->
        Lwt.return x
      end else
        Lwt.return x)

let recv_lines pr =
  let ic = pr#stdout in
  (* TODO: What? Gc.finalise ignore_close ic; *)
  Lwt_stream.from (fun _ ->
                     read_opt Io.read_line ic >>= fun x ->
                     if x = None then begin
                       Io.close ic >>= fun () ->
                       Lwt.return x
                     end else
                       Lwt.return x)

let recv pr =
  let ic = pr#stdout in
  Lwt.finalize
    (fun () -> Io.read ic)
    (fun () -> Io.close ic)

let recv_line pr =
  let ic = pr#stdout in
  Lwt.finalize
    (fun () -> Io.read_line ic)
    (fun () -> Io.close ic)

let send f pr data =
  let oc = pr#stdin in
  Lwt.finalize
    (fun () -> f oc data)
    (fun () -> Io.close oc)

(* Receiving *)

let pread ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  recv (open_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd)

let pread_chars ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  recv_chars (open_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd)

let pread_line ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  recv_line (open_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd)

let pread_lines ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  recv_lines (open_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd)

(* Sending *)

let pwrite ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd text =
  send Io.write (open_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd) text

let pwrite_chars ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd chars =
  send Io.write_chars (open_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd) chars

let pwrite_line ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd line =
  send Io.write_line (open_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd) line

let pwrite_lines ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd lines =
  send Io.write_lines (open_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd) lines

(* Mapping *)

type 'a map_state =
  | Init
  | Save of 'a option Lwt.t
  | Done

(* Monitor the thread [sender] in the stream [st] so write errors are
   reported. *)
let monitor sender st =
  let sender = sender >|= fun () -> None in
  let state = ref Init in
  Lwt_stream.from
    (fun () ->
       match !state with
         | Init ->
             let getter = Lwt.apply Lwt_stream.get st in
             let result _ =
               match Lwt.state sender with
                 | Lwt.Sleep ->
                     (* The sender is still sleeping, behave as the
                        getter. *)
                     getter
                 | Lwt.Return _ ->
                     (* The sender terminated successfully, we are
                        done monitoring it. *)
                     state := Done;
                     getter
                 | Lwt.Fail _ ->
                     (* The sender failed, behave as the sender for
                        this element and save current getter. *)
                     state := Save getter;
                     sender
             in
             Lwt.try_bind (fun () -> Lwt.choose [sender; getter]) result result
         | Save t ->
             state := Done;
             t
         | Done ->
             Lwt_stream.get st)

let pmap ?timeout ?uid ?gid ?env ?cwd ?stderr cmd text =
  let pr = open_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd in
  (* Start the sender and getter at the same time. *)
  let sender = send Io.write pr text in
  let getter = recv pr in
  Lwt.catch
    (fun () ->
      (* Wait for both to terminate, returning the result of the
         getter. *)
      sender >>= fun () -> getter)
    (function
    | Lwt.Canceled as exn ->
        (* Cancel the getter if the sender was canceled. *)
        Lwt.cancel getter;
        Lwt.fail exn
    | exn -> Lwt.fail exn)

let pmap_chars ?timeout ?uid ?gid ?env ?cwd ?stderr cmd chars =
  let pr = open_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd in
  let sender = send Io.write_chars pr chars in
  monitor sender (recv_chars pr)

let pmap_line ?timeout ?uid ?gid ?env ?cwd ?stderr cmd line =
  let pr = open_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd in
  (* Start the sender and getter at the same time. *)
  let sender = send Io.write_line pr line in
  let getter = recv_line pr in
  Lwt.catch
    (fun () ->
      (* Wait for both to terminate, returning the result of the
         getter. *)
      sender >>= fun () -> getter)
    (function
    | Lwt.Canceled as exn ->
        (* Cancel the getter if the sender was canceled. *)
        Lwt.cancel getter;
        Lwt.fail exn
    | exn -> Lwt.fail exn)

let pmap_lines ?timeout ?uid ?gid ?env ?cwd ?stderr cmd lines =
  let pr = open_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd in
  let sender = send Io.write_lines pr lines in
  monitor sender (recv_lines pr)
