(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Lwt_io
 * Copyright (C) 2009 Jérémie Dimino
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


open! Internal
open Itypes
open Lwt.Infix

module Uwt_bytes = Cbytes
module JU = Js.Unsafe

exception Channel_closed of string

(* Minimum size for buffers: *)
let min_buffer_size = 16

let check_buffer_size fun_name buffer_size =
  if buffer_size < min_buffer_size then
    Printf.ksprintf invalid_arg "Uwt_io.%s: too small buffer size" fun_name
  else if buffer_size > Sys.max_string_length then
    Printf.ksprintf invalid_arg "Uwt_io.%s: too big buffer size" fun_name
  else
    ()

let check_buffer fun_name buffer =
  check_buffer_size fun_name (Uwt_bytes.length buffer)

let default_buffer_size = ref 4096

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type input
type output

type 'a mode =
  | Input : input mode
  | Output : output mode

let input : input mode = Input
let output : output mode = Output

(* A channel state *)
type 'mode state =
  | Busy_primitive
      (* A primitive is running on the channel *)

  | Busy_atomic of 'mode channel
      (* An atomic operations is being performed on the channel. The
         argument is the temporary atomic wrapper. *)

  | Waiting_for_busy
      (* A queued operation has not yet started. *)

  | Idle
      (* The channel is unused *)

  | Closed
      (* The channel has been closed *)

  | Invalid
      (* The channel is a temporary channel created for an atomic
         operation which has terminated. *)

(* A wrapper, which ensures that io operations are atomic: *)
and 'mode channel = {
  mutable state : 'mode state;

  channel : 'mode _channel;
  (* The real channel *)

  mutable queued : unit Lwt.u Lwt_sequence.t;
  (* Queued operations *)
}

and 'mode _channel = {
  mutable buffer : Uwt_bytes.t;
  mutable length : int;

  mutable ptr : int;
  (* Current position *)

  mutable max : int;
  (* Position of the end of data int the buffer. It is equal to
     [length] for output channels. *)

  abort_waiter : int Lwt.t;
  (* Thread which is wakeup with an exception when the channel is
     closed. *)
  abort_wakener : int Lwt.u;

  mutable auto_flushing : bool;
  (* Wether the auto-flusher is currently running or not *)

  main : 'mode channel;
  (* The main wrapper *)

  close : unit Lwt.t Lazy.t;
  (* Close function *)

  mode : 'mode mode;
  (* The channel mode *)

  mutable offset : int64;
  (* Number of bytes really read/written *)

  typ : typ;
  (* Type of the channel. *)
}

and typ =
  | Type_normal of (Uwt_bytes.t -> int -> int -> int Lwt.t)
      (* The channel has been created with [make]. The first argument
         is the refill/flush function and the second is the seek
         function. *)
  | Type_bytes
      (* The channel has been created with [of_bytes]. *)

type input_channel = input channel
type output_channel = output channel

type direct_access = {
  da_buffer : Uwt_bytes.t;
  mutable da_ptr : int;
  mutable da_max : int;
  da_perform : unit -> int Lwt.t;
}

let mode wrapper = wrapper.channel.mode

(* +-----------------------------------------------------------------+
   | Creations, closing, locking, ...                                |
   +-----------------------------------------------------------------+ *)

let position : type mode. mode channel -> int64 = fun wrapper ->
  let ch = wrapper.channel in
  match ch.mode with
    | Input ->
      Int64.sub ch.offset (Int64.of_int (ch.max - ch.ptr))
    | Output ->
        Int64.add ch.offset (Int64.of_int ch.ptr)

let name : type mode. mode _channel -> string = fun ch ->
  match ch.mode with
    | Input -> "input"
    | Output -> "output"

let closed_channel ch = Channel_closed(name ch)
let invalid_channel ch = Failure(Printf.sprintf "temporary atomic channel %s no more valid" (name ch))


let is_busy ch =
  match ch.state with
    | Invalid ->
        raise (invalid_channel ch.channel)
    | Idle | Closed ->
        false
    | Busy_primitive | Busy_atomic _ | Waiting_for_busy ->
        true

(* Flush/refill the buffer. No race condition could happen because
   this function is always called atomically: *)
let perform_io : type mode. mode _channel -> int Lwt.t = fun ch -> match ch.main.state with
  | Busy_primitive | Busy_atomic _ -> begin
      match ch.typ with
        | Type_normal(perform_io) ->
            let ptr, len = match ch.mode with
              | Input ->
                  (* Size of data in the buffer *)
                  let size = ch.max - ch.ptr in
                  (* If there are still data in the buffer, keep them: *)
                  if size > 0 then Uwt_bytes.unsafe_blit ch.buffer ch.ptr ch.buffer 0 size;
                  (* Update positions: *)
                  ch.ptr <- 0;
                  ch.max <- size;
                  (size, ch.length - size)
              | Output ->
                  (0, ch.ptr) in
            Lwt.pick [ch.abort_waiter;
(*
if HAVE_WINDOWS <> 0
                        Lwt.catch
                          (fun () -> perform_io ch.buffer ptr len)
                          (function
                          | Unix.Unix_error(Unix.EPIPE, _, _) ->
                            Internal.lwt_return_zero
                          | exn -> Lwt.fail exn)
else
*)
                        perform_io ch.buffer ptr len
                     ] >>= fun n ->
            (* Never trust user functions... *)
            if n < 0 || n > len then
              Lwt.fail (Failure (Printf.sprintf "Uwt_io.perform_io: invalid result of the [%s] function"
                                   (match ch.mode with Input -> "read" | Output -> "write")))
            else begin
              (* Update the global offset: *)
              ch.offset <- Int64.add ch.offset (Int64.of_int n);
              (* Update buffer positions: *)
              begin match ch.mode with
                | Input ->
                    ch.max <- ch.max + n
                | Output ->
                    (* Shift remaining data: *)
                    let len = len - n in
                    Uwt_bytes.unsafe_blit ch.buffer n ch.buffer 0 len;
                    ch.ptr <- len
              end;
              Lwt.return n
            end

        | Type_bytes -> begin
            match ch.mode with
              | Input ->
                  Internal.lwt_return_zero
              | Output ->
                  Lwt.fail (Failure "cannot flush a channel created with Lwt_io.of_string")
          end
    end

  | Closed ->
      Lwt.fail (closed_channel ch)

  | Invalid ->
      Lwt.fail (invalid_channel ch)

  | Idle | Waiting_for_busy ->
      assert false

let refill = perform_io
let flush_partial = perform_io

let rec flush_total oc =
  if oc.ptr > 0 then
    flush_partial oc >>= fun _ ->
    flush_total oc
  else
    Lwt.return_unit

let safe_flush_total oc =
  Lwt.catch
    (fun () -> flush_total oc)
    (fun _  -> Lwt.return_unit)

let deepest_wrapper ch =
  let rec loop wrapper =
    match wrapper.state with
      | Busy_atomic wrapper ->
          loop wrapper
      | _ ->
          wrapper
  in
  loop ch.main

let auto_flush oc =
  (* Lwt.pause () >>= fun () -> *)
  Main.yield () >>= fun () ->
  let wrapper = deepest_wrapper oc in
  match wrapper.state with
    | Busy_primitive | Waiting_for_busy ->
        (* The channel is used, cancel auto flushing. It will be
           restarted when the channel Lwt.returns to the [Idle] state: *)
        oc.auto_flushing <- false;
        Lwt.return_unit

    | Busy_atomic _ ->
        (* Cannot happen since we took the deepest wrapper: *)
        assert false

    | Idle ->
        oc.auto_flushing <- false;
        wrapper.state <- Busy_primitive;
        safe_flush_total oc >>= fun () ->
        if wrapper.state = Busy_primitive then
          wrapper.state <- Idle;
        if not (Lwt_sequence.is_empty wrapper.queued) then
          Lwt.wakeup_later (Lwt_sequence.take_l wrapper.queued) ();
        Lwt.return_unit

    | Closed | Invalid ->
        Lwt.return_unit

(* A ``locked'' channel is a channel in the state [Busy_primitive] or
   [Busy_atomic] *)

let unlock : type m. m channel -> unit = fun wrapper -> match wrapper.state with
  | Busy_primitive | Busy_atomic _ ->
      if Lwt_sequence.is_empty wrapper.queued then
        wrapper.state <- Idle
      else begin
        wrapper.state <- Waiting_for_busy;
        Lwt.wakeup_later (Lwt_sequence.take_l wrapper.queued) ()
      end;
      (* Launches the auto-flusher: *)
      let ch = wrapper.channel in
      if (* Launch the auto-flusher only if the channel is not busy: *)
        (wrapper.state = Idle &&
            (* Launch the auto-flusher only for output channel: *)
            (match ch.mode with Input -> false | Output -> true) &&
            (* Do not launch two auto-flusher: *)
            not ch.auto_flushing &&
            (* Do not launch the auto-flusher if operations are queued: *)
            Lwt_sequence.is_empty wrapper.queued) then begin
        ch.auto_flushing <- true;
        ignore (auto_flush ch)
      end

  | Closed | Invalid ->
      (* Do not change channel state if the channel has been closed *)
      if not (Lwt_sequence.is_empty wrapper.queued) then
        Lwt.wakeup_later (Lwt_sequence.take_l wrapper.queued) ()

  | Idle | Waiting_for_busy ->
      (* We must never unlock an unlocked channel *)
      assert false

(* Wrap primitives into atomic io operations: *)
let primitive f wrapper = match wrapper.state with
  | Idle ->
      wrapper.state <- Busy_primitive;
      Lwt.finalize
        (fun () -> f wrapper.channel)
        (fun () ->
          unlock wrapper;
          Lwt.return_unit)

  | Busy_primitive | Busy_atomic _ | Waiting_for_busy ->
      Lwt.add_task_r wrapper.queued >>= fun () ->
      begin match wrapper.state with
        | Closed ->
            (* The channel has been closed while we were waiting *)
            unlock wrapper;
            Lwt.fail (closed_channel wrapper.channel)

        | Idle | Waiting_for_busy ->
            wrapper.state <- Busy_primitive;
            Lwt.finalize
              (fun () -> f wrapper.channel)
              (fun () ->
                unlock wrapper;
                Lwt.return_unit)

        | Invalid ->
            Lwt.fail (invalid_channel wrapper.channel)

        | Busy_primitive | Busy_atomic _ ->
            assert false
      end

  | Closed ->
      Lwt.fail (closed_channel wrapper.channel)

  | Invalid ->
      Lwt.fail (invalid_channel wrapper.channel)

(* Wrap a sequence of io operations into an atomic operation: *)
let atomic f wrapper = match wrapper.state with
  | Idle ->
      let tmp_wrapper = { state = Idle;
                          channel = wrapper.channel;
                          queued = Lwt_sequence.create () } in
      wrapper.state <- Busy_atomic tmp_wrapper;
      Lwt.finalize
        (fun () -> f tmp_wrapper)
        (fun () ->
          (* The temporary wrapper is no more valid: *)
          tmp_wrapper.state <- Invalid;
          unlock wrapper;
          Lwt.return_unit)

  | Busy_primitive | Busy_atomic _ | Waiting_for_busy ->
      Lwt.add_task_r wrapper.queued >>= fun () ->
      begin match wrapper.state with
        | Closed ->
            (* The channel has been closed while we were waiting *)
            unlock wrapper;
            Lwt.fail (closed_channel wrapper.channel)

        | Idle | Waiting_for_busy ->
            let tmp_wrapper = { state = Idle;
                                channel = wrapper.channel;
                                queued = Lwt_sequence.create () } in
            wrapper.state <- Busy_atomic tmp_wrapper;
            Lwt.finalize
              (fun () -> f tmp_wrapper)
              (fun () ->
                tmp_wrapper.state <- Invalid;
                unlock wrapper;
                Lwt.return_unit)

        | Invalid ->
            Lwt.fail (invalid_channel wrapper.channel)

        | Busy_primitive | Busy_atomic _ ->
            assert false
      end

  | Closed ->
      Lwt.fail (closed_channel wrapper.channel)

  | Invalid ->
      Lwt.fail (invalid_channel wrapper.channel)

let rec abort wrapper = match wrapper.state with
  | Busy_atomic tmp_wrapper ->
      (* Close the depest opened wrapper: *)
      abort tmp_wrapper
  | Closed ->
      (* Double close, just returns the same thing as before *)
      Lazy.force wrapper.channel.close
  | Invalid ->
      Lwt.fail (invalid_channel wrapper.channel)
  | Idle | Busy_primitive | Waiting_for_busy ->
      wrapper.state <- Closed;
      (* Abort any current real reading/writing operation on the
         channel: *)
      Lwt.wakeup_exn wrapper.channel.abort_wakener (closed_channel wrapper.channel);
      Lazy.force wrapper.channel.close

let close : type mode. mode channel -> unit Lwt.t = fun wrapper ->
  let channel = wrapper.channel in
  if channel.main != wrapper then
    Lwt.fail (Failure "Lwt_io.close: cannot close a channel obtained via Lwt_io.atomic")
  else
    match channel.mode with
      | Input ->
          (* Just close it now: *)
          abort wrapper
      | Output ->
          Lwt.catch
            (fun () ->
              (* Performs all pending actions, flush the buffer, then close it: *)
              primitive (fun channel ->
                safe_flush_total channel >>= fun () -> abort wrapper) wrapper)
            (fun _ ->
              abort wrapper)

let make :
  type m.
    ?buffer : Uwt_bytes.t ->
    ?close : (unit -> unit Lwt.t) ->
    mode : m mode ->
    (Uwt_bytes.t -> int -> int -> int Lwt.t) ->
    m channel = fun ?buffer ?(close=Lwt.return) ~mode perform_io ->
  let (buffer, size) =
    match buffer with
      | Some buffer ->
        check_buffer "Uwt_io.make" buffer;
        (buffer, Uwt_bytes.length buffer)
      | None ->
        let size = !default_buffer_size in
        (Uwt_bytes.create size, size)
  in
  let abort_waiter, abort_wakener = Lwt.wait () in
  let rec ch = {
    buffer = buffer;
    length = size;
    ptr = 0;
    max = (match mode with
             | Input -> 0
             | Output -> size);
    close = lazy(Lwt.catch close Lwt.fail);
    abort_waiter = abort_waiter;
    abort_wakener = abort_wakener;
    main = wrapper;
    auto_flushing = false;
    mode = mode;
    offset = 0L;
    typ = Type_normal(perform_io);
  } and wrapper = {
    state = Idle;
    channel = ch;
    queued = Lwt_sequence.create ();
  } in
  wrapper

let of_bytes ~mode bytes =
  let length = Uwt_bytes.length bytes in
  let abort_waiter, abort_wakener = Lwt.wait () in
  let rec ch = {
    buffer = bytes;
    length = length;
    ptr = 0;
    max = length;
    close = lazy(Lwt.return_unit);
    abort_waiter = abort_waiter;
    abort_wakener = abort_wakener;
    main = wrapper;
    (* Auto flush is set to [true] to prevent writing functions from
       trying to launch the auto-fllushed. *)
    auto_flushing = true;
    mode = mode;
    offset = 0L;
    typ = Type_bytes;
  } and wrapper = {
    state = Idle;
    channel = ch;
    queued = Lwt_sequence.create ();
  } in
  wrapper


let of_file : type m. ?buffer : Uwt_bytes.t -> ?close : (unit -> unit Lwt.t) -> mode : m mode -> file -> m channel = fun ?buffer ?close ~mode fd ->
  let perform_io buf pos len = match mode with
    | Input -> Fs.Async.read_ba fd ~buf ~pos ~len
    | Output -> Fs.Async.write_ba fd ~buf ~pos ~len
  in
  make
    ?buffer
    ~close:(match close with
              | Some f -> f
              | None -> (fun () -> Fs.Async.close fd))
    ~mode
    perform_io

let of_stream : type m. ?buffer : Uwt_bytes.t -> ?close : (unit -> unit Lwt.t) -> mode : m mode -> Stream.t -> m channel = fun ?buffer ?close ~mode fd ->
  let perform_io buf pos len = match mode with
    | Input -> Stream.read_ba fd ~buf ~pos ~len
    | Output -> Stream.write_ba fd ~buf ~pos ~len >>= fun () -> Lwt.return len
  in
  make
    ?buffer
    ~close:(match close with
              | Some f -> f
              | None -> fun () -> Stream.close_noerr fd; Lwt.return_unit)
    ~mode
    perform_io

let of_pipe ?buffer ?close ~mode t =
  let t = Pipe.to_stream t in
  of_stream ?buffer ?close ~mode t

let of_tcp ?buffer ?close ~mode t =
  let t = Tcp.to_stream t in
  of_stream ?buffer ?close ~mode t

let buffered : type m. m channel -> int = fun ch ->
  match ch.channel.mode with
    | Input -> ch.channel.max - ch.channel.ptr
    | Output -> ch.channel.ptr

let buffer_size ch = ch.channel.length

let resize_buffer : type m. m channel -> int -> unit Lwt.t = fun wrapper len ->
  if len < min_buffer_size then invalid_arg "Uwt_io.resize_buffer: buffer size too small";
  match wrapper.channel.typ with
    | Type_bytes ->
        Lwt.fail (Failure "Lwt_io.resize_buffer: cannot resize the buffer of a channel created with Lwt_io.of_string")
    | Type_normal _ ->
        let f : type m. m _channel -> unit Lwt.t = fun ch ->
          match ch.mode with
            | Input ->
                let unread_count = ch.max - ch.ptr in
                (* Fail if we want to decrease the buffer size and there is
                   too much unread data in the buffer: *)
                if len < unread_count then
                  Lwt.fail (Failure "Uwt_io.resize_buffer: cannot decrease buffer size, too much unread data")
                else begin
                  let buffer = Uwt_bytes.create len in
                  Uwt_bytes.unsafe_blit ch.buffer ch.ptr buffer 0 unread_count;
                  ch.buffer <- buffer;
                  ch.length <- len;
                  ch.ptr <- 0;
                  ch.max <- unread_count;
                  Lwt.return_unit
                end
            | Output ->
                (* If we decrease the buffer size, flush the buffer until
                   the number of buffered bytes fits into the new buffer: *)
                let rec loop () =
                  if ch.ptr > len then
                    flush_partial ch >>= fun _ ->
                    loop ()
                  else
                    Lwt.return_unit
                in
                loop () >>= fun () ->
                let buffer = Uwt_bytes.create len in
                Uwt_bytes.unsafe_blit ch.buffer 0 buffer 0 ch.ptr;
                ch.buffer <- buffer;
                ch.length <- len;
                ch.max <- len;
                Lwt.return_unit
        in
        primitive f wrapper

(* +-----------------------------------------------------------------+
   | Byte-order                                                      |
   +-----------------------------------------------------------------+ *)
let bbinding = JU.global##.process##binding (Js.string "buffer")

module ByteOrder =
struct
  module type S = sig

    val pos16_0 : int
    val pos16_1 : int
    val pos32_0 : int
    val pos32_1 : int
    val pos32_2 : int
    val pos32_3 : int
    val pos64_0 : int
    val pos64_1 : int
    val pos64_2 : int
    val pos64_3 : int
    val pos64_4 : int
    val pos64_5 : int
    val pos64_6 : int
    val pos64_7 : int

    val read_float32: Uwt_bytes.t -> int -> float
    val read_float64: Uwt_bytes.t -> int -> float

    val write_float32: Uwt_bytes.t -> int -> float -> unit
    val write_float64: Uwt_bytes.t -> int -> float -> unit
  end

  module LE =
  struct

    let pos16_0 = 0
    let pos16_1 = 1
    let pos32_0 = 0
    let pos32_1 = 1
    let pos32_2 = 2
    let pos32_3 = 3
    let pos64_0 = 0
    let pos64_1 = 1
    let pos64_2 = 2
    let pos64_3 = 3
    let pos64_4 = 4
    let pos64_5 = 5
    let pos64_6 = 6
    let pos64_7 = 7

    let read_float32 (b:Uwt_bytes.t) (n:int) : float =
      let buf = Obj.magic b in
      let f = bbinding##readFloatLE buf n in
      f

    let read_float64 (b:Uwt_bytes.t) (n:int) : float =
      let buf = Obj.magic b in
      let f = bbinding##readDoubleLE buf n in
      f

    let write_float32 (b:Uwt_bytes.t) (n:int) (v:float) : unit =
      let buf = Obj.magic b in
      let _f = bbinding##writeFloatLE buf v n in
      ()

    let write_float64 (b:Uwt_bytes.t) (n:int) (v:float) : unit =
      let buf = Obj.magic b in
      let _f = bbinding##writeDoubleLE buf v n in
      ()
  end

  module BE =
  struct
    let pos16_0 = 1
    let pos16_1 = 0
    let pos32_0 = 3
    let pos32_1 = 2
    let pos32_2 = 1
    let pos32_3 = 0
    let pos64_0 = 7
    let pos64_1 = 6
    let pos64_2 = 5
    let pos64_3 = 4
    let pos64_4 = 3
    let pos64_5 = 2
    let pos64_6 = 1
    let pos64_7 = 0


    let read_float32 (b:Uwt_bytes.t) (n:int) : float =
      let buf = Obj.magic b in
      let f = bbinding##readFloatBE buf n Js._true in
      f

    let read_float64 (b:Uwt_bytes.t) (n:int) : float =
      let buf = Obj.magic b in
      let f = bbinding##readDoubleBE buf n Js._true in
      f

    let write_float32 (b:Uwt_bytes.t) (n:int) (v:float) : unit =
      let buf = Obj.magic b in
      let _f = bbinding##writeFloatBE buf v n Js._true in
      ()

    let write_float64 (b:Uwt_bytes.t) (n:int) (v:float) : unit =
      let buf = Obj.magic b in
      let _f = bbinding##writeDoubleBE buf v n Js._true in
      ()
  end
end

module Primitives =
struct

  (* This module contains all primitives operations. The operates
     without protection regarding locking, they are wrapped after into
     safe operations. *)

  (* +---------------------------------------------------------------+
     | Reading                                                       |
     +---------------------------------------------------------------+ *)

  let rec read_char ic =
    let ptr = ic.ptr in
    if ptr = ic.max then
      refill ic >>= function
        | 0 -> Lwt.fail End_of_file
        | _ -> read_char ic
    else begin
      ic.ptr <- ptr + 1;
      Lwt.return (Uwt_bytes.unsafe_get ic.buffer ptr)
    end

  let read_char_opt ic =
    Lwt.catch
      (fun () -> read_char ic >|= fun ch -> Some ch)
      (function
      | End_of_file -> Lwt.return_none
      | exn -> Lwt.fail exn)

  let rev_concat len l =
    let buf = Bytes.create len in
    ignore (
      List.fold_left
        (fun ofs str ->
           let len = Bytes.length str in
           let ofs = ofs - len in
           Bytes.unsafe_blit str 0 buf ofs len;
           ofs)
        len l );
    buf

  let read_line ic =
    let ret l a =
      rev_concat l a |> Bytes.unsafe_to_string |> Lwt.return
    and help orig =
      let rec iter = function
      | [] -> orig
      | x::tl ->
        let lenm1 = Bytes.length x - 1 in
        if lenm1 < 0 then
          iter tl
        else if Bytes.get x lenm1 = '\r' then
          let s = Bytes.sub x 0 lenm1 in
          s::tl
        else
          orig
      in
      iter orig
    in
    let rec iter whole_len accu ic =
      let orig_len = ic.max - ic.ptr
      and ptr = ic.ptr in
      let nl_pos =
        Uwt_bytes.unsafe_memchr
          ~pos:ptr
          ~len:orig_len
          ic.buffer
          '\n' in
      if nl_pos = ptr then
        let () = ic.ptr <- succ ptr in
        let accu' = help accu in
        if accu' == accu then
          ret whole_len accu
        else
          ret (pred whole_len) accu'
      else if nl_pos > 0 then
        let () = ic.ptr <- succ nl_pos in
        let len = nl_pos - ptr in
        let len =
          if Uwt_bytes.unsafe_get ic.buffer (pred nl_pos) = '\r' then
            len - 1
          else
            len
        in
        let b = Bytes.create len in
        Uwt_bytes.unsafe_blit_to_bytes ic.buffer ptr b 0 len;
        if accu = [] then
          Lwt.return (Bytes.unsafe_to_string b)
        else
          ret (len + whole_len) (b::accu)
      else
        let () = ic.ptr <- ic.max in
        let accu =
          if orig_len = 0 then
            accu
          else
            let b = Bytes.create orig_len in
            Uwt_bytes.unsafe_blit_to_bytes ic.buffer ptr b 0 orig_len;
            b::accu
        in
        refill ic >>= function
        | 0 ->
          if accu = [] then
            Lwt.fail End_of_file
          else
            ret (whole_len + orig_len) accu
        | _ -> iter (whole_len + orig_len) accu ic
    in
    iter 0 [] ic

  let read_line_opt ic =
    Lwt.catch
      (fun () -> read_line ic >|= fun ch -> Some ch)
      (function
      | End_of_file -> Lwt.return_none
      | exn -> Lwt.fail exn)

  let unsafe_read_into ic buf ofs len =
    let avail = ic.max - ic.ptr in
    if avail > 0 then begin
      let len = min len avail in
      Uwt_bytes.unsafe_blit_to_bytes ic.buffer ic.ptr buf ofs len;
      ic.ptr <- ic.ptr + len;
      Lwt.return len
    end else begin
      refill ic >>= fun n ->
        let len = min len n in
        Uwt_bytes.unsafe_blit_to_bytes ic.buffer 0 buf ofs len;
        ic.ptr <- len;
        ic.max <- n;
        Lwt.return len
    end

  let read_into ic buf ofs len =
    if ofs < 0 || len < 0 || ofs + len > Bytes.length buf then
      Lwt.fail (Invalid_argument "Uwt_io.read_into")
    else begin
      if len = 0 then
        Internal.lwt_return_zero
      else
        unsafe_read_into ic buf ofs len
    end

  let rec unsafe_read_into_exactly ic buf ofs len =
    unsafe_read_into ic buf ofs len >>= function
      | 0 ->
          Lwt.fail End_of_file
      | n ->
          let len = len - n in
          if len = 0 then
            Lwt.return_unit
          else
            unsafe_read_into_exactly ic buf (ofs + n) len

  let read_into_exactly ic buf ofs len =
    if ofs < 0 || len < 0 || ofs + len > Bytes.length buf then
      Lwt.fail (Invalid_argument "Uwt_io.read_into_exactly")
    else begin
      if len = 0 then
        Lwt.return_unit
      else
        unsafe_read_into_exactly ic buf ofs len
    end

  let rec read_all ic total_len acc =
    let len = ic.max - ic.ptr in
    let buf = Bytes.create len in
    Uwt_bytes.unsafe_blit_to_bytes ic.buffer ic.ptr buf 0 len;
    ic.ptr <- ic.max;
    refill ic >>= function
      | 0 ->
          Lwt.return (rev_concat (len + total_len) (buf :: acc))
      | _n ->
          read_all ic (len + total_len) (buf :: acc)

  let read count ic =
    match count with
      | None ->
          read_all ic 0 [] >|= Bytes.unsafe_to_string
      | Some len ->
          let buf = Bytes.create len in
          unsafe_read_into ic buf 0 len >>= fun real_len ->
          if real_len < len then
            Lwt.return Bytes.(sub buf 0 real_len |> unsafe_to_string)
          else
            Lwt.return (Bytes.unsafe_to_string buf)

  let read_value ic =
    let header = Bytes.create 20 in
    unsafe_read_into_exactly ic header 0 20 >>= fun () ->
    let bsize = Marshal.data_size header 0 in
    let buffer = Bytes.create (20 + bsize) in
    Bytes.unsafe_blit header 0 buffer 0 20;
    unsafe_read_into_exactly ic buffer 20 bsize >>= fun () ->
    Lwt.return (Marshal.from_bytes buffer 0)

  (* +---------------------------------------------------------------+
     | Writing                                                       |
     +---------------------------------------------------------------+ *)

  let flush = flush_total

  let rec write_char oc ch =
    let ptr = oc.ptr in
    if ptr < oc.length then begin
      oc.ptr <- ptr + 1;
      Uwt_bytes.unsafe_set oc.buffer ptr ch;
      Lwt.return_unit
    end else
      flush_partial oc >>= fun _ ->
      write_char oc ch

  let rec unsafe_write_from oc str ofs len =
    let avail = oc.length - oc.ptr in
    if avail >= len then begin
      Uwt_bytes.unsafe_blit_from_bytes str ofs oc.buffer oc.ptr len;
      oc.ptr <- oc.ptr + len;
      Internal.lwt_return_zero
    end else begin
      Uwt_bytes.unsafe_blit_from_bytes str ofs oc.buffer oc.ptr avail;
      oc.ptr <- oc.length;
      flush_partial oc >>= fun _ ->
      let len = len - avail in
      if oc.ptr = 0 then begin
        if len = 0 then
          Internal.lwt_return_zero
        else
          (* Everything has been written, try to write more: *)
          unsafe_write_from oc str (ofs + avail) len
      end else
        (* Not everything has been written, just what is
           remaining: *)
        Lwt.return len
    end

  let write_from oc buf ofs len =
    if ofs < 0 || len < 0 || ofs + len > Bytes.length buf then
      Lwt.fail (Invalid_argument "Lwt_io.write_from")
    else begin
      if len = 0 then
        Internal.lwt_return_zero
      else
        unsafe_write_from oc buf ofs len >>= fun remaining -> Lwt.return (len - remaining)
    end

  let write_from_string oc buf ofs len =
    let buf = Bytes.unsafe_of_string buf in
    write_from oc buf ofs len

  let rec unsafe_write_from_exactly oc buf ofs len =
    unsafe_write_from oc buf ofs len >>= function
      | 0 ->
          Lwt.return_unit
      | n ->
          unsafe_write_from_exactly oc buf (ofs + len - n) n

  let write_from_exactly oc buf ofs len =
    if ofs < 0 || len < 0 || ofs + len > Bytes.length buf then
      Lwt.fail (Invalid_argument "Uwt_io.write_from_exactly")
    else begin
      if len = 0 then
        Lwt.return_unit
      else
        unsafe_write_from_exactly oc buf ofs len
    end

  let write_from_string_exactly oc buf ofs len =
    let buf = Bytes.unsafe_of_string buf in
    write_from_exactly oc buf ofs len

  let write oc str =
    let buf = Bytes.unsafe_of_string str in
    unsafe_write_from_exactly oc buf 0 (Bytes.length buf)

  let write_line oc str =
    let buf = Bytes.unsafe_of_string str in
    unsafe_write_from_exactly oc buf 0 (Bytes.length buf) >>= fun () ->
    write_char oc '\n'

  let write_value oc ?(flags=[]) x =
    write oc (Marshal.to_string x flags)

  (* +---------------------------------------------------------------+
     | Low-level access                                              |
     +---------------------------------------------------------------+ *)

  let rec read_block_unsafe ic size f =
    if ic.max - ic.ptr < size then
      refill ic >>= function
        | 0 ->
            Lwt.fail End_of_file
        | _ ->
            read_block_unsafe ic size f
    else begin
      let ptr = ic.ptr in
      ic.ptr <- ptr + size;
      f ic.buffer ptr
    end

  let rec write_block_unsafe oc size f =
    if oc.max - oc.ptr < size then
      flush_partial oc >>= fun _ ->
      write_block_unsafe oc size f
    else begin
      let ptr = oc.ptr in
      oc.ptr <- ptr + size;
      f oc.buffer ptr
    end

  let block : type m. m _channel -> int -> (Uwt_bytes.t -> int -> 'a Lwt.t) -> 'a Lwt.t = fun ch size f ->
    if size < 0 || size > min_buffer_size then
      Lwt.fail (Invalid_argument "Uwt_io.block")
    else
      if ch.max - ch.ptr >= size then begin
        let ptr = ch.ptr in
        ch.ptr <- ptr + size;
        f ch.buffer ptr
      end else
        match ch.mode with
          | Input ->
              read_block_unsafe ch size f
          | Output ->
              write_block_unsafe ch size f

  let perform token da ch =
    if !token then begin
      if da.da_max <> ch.max || da.da_ptr < ch.ptr || da.da_ptr > ch.max then
        Lwt.fail (Invalid_argument "Uwt_io.direct_access.da_perform")
      else begin
        ch.ptr <- da.da_ptr;
        perform_io ch >>= fun count ->
        da.da_ptr <- ch.ptr;
        da.da_max <- ch.max;
        Lwt.return count
      end
    end else
      Lwt.fail (Failure "Uwt_io.perform: this function can not be called outside Lwt_io.direct_access")


  let direct_access ch f =
    let token = ref true in
    let rec da = {
      da_ptr = ch.ptr;
      da_max = ch.max;
      da_buffer = ch.buffer;
      da_perform = (fun _ -> perform token da ch);
    } in
    f da >>= fun x ->
    token := false;
    if da.da_max <> ch.max || da.da_ptr < ch.ptr || da.da_ptr > ch.max then
      Lwt.fail (Failure "Lwt_io.direct_access: invalid result of [f]")
    else begin
      ch.ptr <- da.da_ptr;
      Lwt.return x
    end

  module MakeNumberIO(ByteOrder : ByteOrder.S) =
  struct
    open ByteOrder

    let get buffer ptr = Char.code (Uwt_bytes.unsafe_get buffer ptr)

    let read_int ic =
      read_block_unsafe ic 4
        (fun buffer ptr ->
           let v0 = get buffer (ptr + pos32_0)
           and v1 = get buffer (ptr + pos32_1)
           and v2 = get buffer (ptr + pos32_2)
           and v3 = get buffer (ptr + pos32_3) in
           let v = v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24) in
           Lwt.return v)

    let read_int16 ic =
      read_block_unsafe ic 2
        (fun buffer ptr ->
           let v0 = get buffer (ptr + pos16_0)
           and v1 = get buffer (ptr + pos16_1) in
           let v = v0 lor (v1 lsl 8) in
           if v1 land 0x80 = 0 then
             Lwt.return v
           else
             Lwt.return (v - (1 lsl 16)))

    let read_int32 x = read_int x >>= fun x -> Lwt.return (Int32.of_int x)

    let read_int64 ic =
      read_block_unsafe ic 8
        (fun buffer ptr ->
           let v0 = get buffer (ptr + pos64_0)
           and v1 = get buffer (ptr + pos64_1)
           and v2 = get buffer (ptr + pos64_2)
           and v3 = get buffer (ptr + pos64_3)
           and v4 = get buffer (ptr + pos64_4)
           and v5 = get buffer (ptr + pos64_5)
           and v6 = get buffer (ptr + pos64_6)
           and v7 = get buffer (ptr + pos64_7) in
           Lwt.return (Int64.logor
                         (Int64.logor
                            (Int64.logor
                               (Int64.of_int v0)
                               (Int64.shift_left (Int64.of_int v1) 8))
                            (Int64.logor
                               (Int64.shift_left (Int64.of_int v2) 16)
                               (Int64.shift_left (Int64.of_int v3) 24)))
                         (Int64.logor
                            (Int64.logor
                               (Int64.shift_left (Int64.of_int v4) 32)
                               (Int64.shift_left (Int64.of_int v5) 40))
                            (Int64.logor
                               (Int64.shift_left (Int64.of_int v6) 48)
                               (Int64.shift_left (Int64.of_int v7) 56)))))

    let read_float32 ic =
      read_block_unsafe ic 4
        (fun buffer ptr ->
           Lwt.return (read_float32 buffer ptr))
    let read_float64 ic =
      read_block_unsafe ic 8
        (fun buffer ptr ->
           Lwt.return (read_float64 buffer ptr))

    (* +-------------------------------------------------------------+
       | Writing numbers                                             |
       +-------------------------------------------------------------+ *)

    let set buffer ptr x = Uwt_bytes.unsafe_set buffer ptr (Char.unsafe_chr x)

    let write_int oc v =
      write_block_unsafe oc 4
        (fun buffer ptr ->
           set buffer (ptr + pos32_0) v;
           set buffer (ptr + pos32_1) (v lsr 8);
           set buffer (ptr + pos32_2) (v lsr 16);
           set buffer (ptr + pos32_3) (v asr 24);
           Lwt.return_unit)

    let write_int16 oc v =
      write_block_unsafe oc 2
        (fun buffer ptr ->
           set buffer (ptr + pos16_0) v;
           set buffer (ptr + pos16_1) (v lsr 8);
           Lwt.return_unit)

    let write_int32 oc v = write_int oc (Int32.to_int v)

    let write_int64 oc v =
      write_block_unsafe oc 8
        (fun buffer ptr ->
           set buffer (ptr + pos64_0) (Int64.to_int v);
           set buffer (ptr + pos64_1) (Int64.to_int (Int64.shift_right v 8));
           set buffer (ptr + pos64_2) (Int64.to_int (Int64.shift_right v 16));
           set buffer (ptr + pos64_3) (Int64.to_int (Int64.shift_right v 24));
           set buffer (ptr + pos64_4) (Int64.to_int (Int64.shift_right v 32));
           set buffer (ptr + pos64_5) (Int64.to_int (Int64.shift_right v 40));
           set buffer (ptr + pos64_6) (Int64.to_int (Int64.shift_right v 48));
           set buffer (ptr + pos64_7) (Int64.to_int (Int64.shift_right v 56));
           Lwt.return_unit)

    let write_float32 oc v =
      write_block_unsafe oc 4
        (fun buf pos ->
           write_float32 buf pos v; Lwt.return_unit)
    let write_float64 oc v =
      write_block_unsafe oc 8
        (fun buf pos ->
           write_float64 buf pos v; Lwt.return_unit)
  end

end

(* +-----------------------------------------------------------------+
   | Primitive operations                                            |
   +-----------------------------------------------------------------+ *)

let read_char wrapper =
  let channel = wrapper.channel in
  let ptr = channel.ptr in
  (* Speed-up in case a character is available in the buffer. It
     increases performances by 10x. *)
  if wrapper.state = Idle && ptr < channel.max then begin
    channel.ptr <- ptr + 1;
    Lwt.return (Uwt_bytes.unsafe_get channel.buffer ptr)
  end else
    primitive Primitives.read_char wrapper

let read_char_opt wrapper =
  let channel = wrapper.channel in
  let ptr = channel.ptr in
  if wrapper.state = Idle && ptr < channel.max then begin
    channel.ptr <- ptr + 1;
    Lwt.return (Some(Uwt_bytes.unsafe_get channel.buffer ptr))
  end else
    primitive Primitives.read_char_opt wrapper

let read_line ic = primitive Primitives.read_line ic
let read_line_opt ic = primitive Primitives.read_line_opt ic
let read ?count ic = primitive (fun ic -> Primitives.read count ic) ic
let read_into ic str ofs len = primitive (fun ic -> Primitives.read_into ic str ofs len) ic
let read_into_exactly ic str ofs len = primitive (fun ic -> Primitives.read_into_exactly ic str ofs len) ic
let read_value ic = primitive Primitives.read_value ic

let flush oc = primitive Primitives.flush oc

let write_char wrapper x =
  let channel = wrapper.channel in
  let ptr = channel.ptr in
  if wrapper.state = Idle && ptr < channel.max then begin
    channel.ptr <- ptr + 1;
    Uwt_bytes.unsafe_set channel.buffer ptr x;
    (* Fast launching of the auto flusher: *)
    if not channel.auto_flushing then begin
      channel.auto_flushing <- true;
      ignore (auto_flush channel);
      Lwt.return_unit
    end else
      Lwt.return_unit
  end else
    primitive (fun oc -> Primitives.write_char oc x) wrapper

let write oc str = primitive (fun oc -> Primitives.write oc str) oc
let write_line oc x = primitive (fun oc -> Primitives.write_line oc x) oc
let write_from oc str ofs len = primitive (fun oc -> Primitives.write_from oc str ofs len) oc
let write_from_string oc str ofs len = primitive (fun oc -> Primitives.write_from_string oc str ofs len) oc
let write_from_exactly oc str ofs len = primitive (fun oc -> Primitives.write_from_exactly oc str ofs len) oc
let write_from_string_exactly oc str ofs len = primitive (fun oc -> Primitives.write_from_string_exactly oc str ofs len) oc
let write_value oc ?flags x = primitive (fun oc -> Primitives.write_value oc ?flags x) oc

let block ch size f = primitive (fun ch -> Primitives.block ch size f) ch
let direct_access ch f = primitive (fun ch -> Primitives.direct_access ch f) ch

module type NumberIO = sig
  val read_int : input_channel -> int Lwt.t
  val read_int16 : input_channel -> int Lwt.t
  val read_int32 : input_channel -> int32 Lwt.t
  val read_int64 : input_channel -> int64 Lwt.t
  val read_float32 : input_channel -> float Lwt.t
  val read_float64 : input_channel -> float Lwt.t
  val write_int : output_channel -> int -> unit Lwt.t
  val write_int16 : output_channel -> int -> unit Lwt.t
  val write_int32 : output_channel -> int32 -> unit Lwt.t
  val write_int64 : output_channel -> int64 -> unit Lwt.t
  val write_float32 : output_channel -> float -> unit Lwt.t
  val write_float64 : output_channel -> float -> unit Lwt.t
end

module MakeNumberIO(ByteOrder : ByteOrder.S) =
struct
  module Primitives = Primitives.MakeNumberIO(ByteOrder)

  let read_int ic = primitive Primitives.read_int ic
  let read_int16 ic = primitive Primitives.read_int16 ic
  let read_int32 ic = primitive Primitives.read_int32 ic
  let read_int64 ic = primitive Primitives.read_int64 ic
  let read_float32 ic = primitive Primitives.read_float32 ic
  let read_float64 ic = primitive Primitives.read_float64 ic

  let write_int oc x = primitive (fun oc -> Primitives.write_int oc x) oc
  let write_int16 oc x = primitive (fun oc -> Primitives.write_int16 oc x) oc
  let write_int32 oc x = primitive (fun oc -> Primitives.write_int32 oc x) oc
  let write_int64 oc x = primitive (fun oc -> Primitives.write_int64 oc x) oc
  let write_float32 oc x = primitive (fun oc -> Primitives.write_float32 oc x) oc
  let write_float64 oc x = primitive (fun oc -> Primitives.write_float64 oc x) oc
end

module LE = MakeNumberIO(ByteOrder.LE)
module BE = MakeNumberIO(ByteOrder.BE)

type byte_order = Little_endian | Big_endian
let system_byte_order =
  match Js_sys.big_endian with
  | true -> Big_endian
  | false -> Little_endian

include (val (match system_byte_order with
                | Little_endian -> (module LE : NumberIO)
                | Big_endian -> (module BE : NumberIO)) : NumberIO)

(* +-----------------------------------------------------------------+
   | Other                                                           |
   +-----------------------------------------------------------------+ *)

let read_chars ic = Lwt_stream.from (fun _ -> read_char_opt ic)
let write_chars oc chars = Lwt_stream.iter_s (fun char -> write_char oc char) chars
let read_lines ic = Lwt_stream.from (fun _ -> read_line_opt ic)
let write_lines oc lines = Lwt_stream.iter_s (fun line -> write_line oc line) lines

let zero =
  make
    ~mode:input
    ~buffer:(Uwt_bytes.create min_buffer_size)
    (fun str ofs len -> Uwt_bytes.fill str ofs len '\x00'; Lwt.return len)

let null =
  make
    ~mode:output
    ~buffer:(Uwt_bytes.create min_buffer_size)
    (fun _str _ofs len -> Lwt.return len)

(* Do not close standard ios on close, otherwise uncaught exceptions
   will not be printed *)
let stdin = of_file ~mode:input stdin
let stdout = of_file ~mode:output stdout
let stderr = of_file ~mode:output stderr

let fprint oc txt = write oc txt
let fprintl oc txt = write_line oc txt
let fprintf oc fmt = Printf.ksprintf (fun txt -> write oc txt) fmt
let fprintlf oc fmt = Printf.ksprintf (fun txt -> write_line oc txt) fmt

let print txt = write stdout txt
let printl txt = write_line stdout txt
let printf fmt = Printf.ksprintf print fmt
let printlf fmt = Printf.ksprintf printl fmt

let eprint txt = write stderr txt
let eprintl txt = write_line stderr txt
let eprintf fmt = Printf.ksprintf eprint fmt
let eprintlf fmt = Printf.ksprintf eprintl fmt

type file_name = string

let open_file : type m. ?buffer : Uwt_bytes.t -> ?flags : Fs_types.uv_open_flag list -> ?perm : Unix.file_perm -> mode : m mode -> file_name -> m channel Lwt.t = fun ?buffer ?flags ?perm ~mode filename ->
  let flags = match flags, mode with
    | Some l, _ ->
        l
    | None, Input ->
        [Fs_types.O_RDONLY; Fs_types.O_NONBLOCK]
    | None, Output ->
        [Fs_types.O_WRONLY; Fs_types.O_CREAT; Fs_types.O_TRUNC; Fs_types.O_NONBLOCK]
  and perm = match perm, mode with
    | Some p, _ ->
        p
    | None, Input ->
        0
    | None, Output ->
        0o666
  in
  Fs.Async.openfile ~mode:flags ~perm filename >>= fun fd ->
  Lwt.return (of_file ?buffer ~mode fd)

let with_file ?buffer ?flags ?perm ~mode filename f =
  open_file ?buffer ?flags ?perm ~mode filename >>= fun ic ->
  Lwt.finalize
    (fun () -> f ic)
    (fun () -> close ic)

let close_socket s =
  if Stream.((*is_writable s = false ||*) write_queue_size s <= 0) then (
    Stream.close_noerr s; Lwt.return_unit )
  else
    Lwt.finalize ( fun () ->
        Lwt.catch (fun () -> Stream.shutdown s) (function
          (* This may happen if the server closed the connection before us *)
          | Unix.Unix_error(Unix.ENOTCONN,_,_) -> Lwt.return_unit
          | x -> Lwt.fail x )
    ) ( fun () -> Stream.close_noerr s; Lwt.return_unit )

let stream_connect_common ?in_buffer ?out_buffer stream =
  let io_in buf pos len =
    Stream.read_ba stream ~buf ~pos ~len
  and io_out buf pos len =
    Stream.write_ba stream ~buf ~pos ~len >>= fun () -> Lwt.return len
  in
  let close = lazy (close_socket stream) in
  let a = make ?buffer:in_buffer
      ~close:(fun _ -> Lazy.force close)
      ~mode:input io_in
  and b = make ?buffer:out_buffer
      ~close:(fun _ -> Lazy.force close)
      ~mode:output io_out
  in
  Lwt.return (a,b)

let open_tcp_connection ?in_buffer ?out_buffer inet port  =
  let t = Tcp.init () in
  let stream = Tcp.to_stream t in
  Lwt.catch (fun () ->
      Tcp.connect t inet port >>= fun () ->
      stream_connect_common ?in_buffer ?out_buffer stream
    ) ( fun exn -> Stream.close_noerr stream; Lwt.fail exn)

let open_pipe_connection ?in_buffer ?out_buffer ~path =
  let t = Pipe.init () in
  let stream = Pipe.to_stream t in
  Lwt.catch (fun () ->
      Pipe.connect t ~path >>= fun () ->
      stream_connect_common ?in_buffer ?out_buffer stream
    ) ( fun exn -> Stream.close_noerr stream; Lwt.fail exn)

let open_connection ?in_buffer ?out_buffer addr =
  match addr with
  | Unix.ADDR_UNIX path -> open_pipe_connection ?in_buffer ?out_buffer ~path
  | Unix.ADDR_INET(inet,port) -> open_tcp_connection ?in_buffer ?out_buffer inet port

let close_once c = if c.state = Closed then Lwt.return_unit else close c

let with_connection ?in_buffer ?out_buffer sockaddr f =
  open_connection ?in_buffer ?out_buffer sockaddr >>= fun ((ic, oc) as chs) ->
  (* If the user already tried to close the socket and got an exception, we
     don't want to raise that exception again during implicit close. *)
  Lwt.finalize (fun () -> f chs) (fun () -> close_once ic <&> close_once oc)

type server = {
  shutdown : unit Lwt.t Lazy.t;
}

let sb_close s c =
  if s || c.state = Closed then Lwt.return_unit else
    Lwt.catch
      (fun () -> close c)
      (fun x -> !Lwt.async_exception_hook x; Lwt.return_unit)

let dummy_address_tcp = Unix.ADDR_INET(Unix.inet_addr_of_string "192.0.2.0",0)
let dummy_address_pipe = Unix.ADDR_UNIX("(unknown)")

let establish_server_intern
    ?(buffer_size = !default_buffer_size)
    ?(backlog=5)
    ?(no_close=false)
    addr
    ft =
  let with_address,f = match ft with
  | `With_address f -> true,f
  | `No_address f -> false, (fun _addr x -> f x) in
  let f_cb address s_client =
    let closed = ref false in
    let close () =
      if !closed then Lwt.return_unit else
      let () = closed := true in
      close_socket s_client
    in
    let buffer = Uwt_bytes.create buffer_size in
    let ic = of_stream ~close ~buffer ~mode:input s_client in
    let buffer = Uwt_bytes.create buffer_size in
    let oc = of_stream ~close ~buffer ~mode:output s_client in
    if no_close then
      Lwt.async (fun () -> f addr (ic,oc))
    else
      Lwt.async (fun () ->
        let ic_closed = ref false
        and oc_closed = ref false in
        Lwt.catch (fun () ->
            f address (ic,oc) >>= fun () ->
            ic_closed := true;
            close_once ic >>= fun () ->
            oc_closed := true;
            close_once oc)
          (fun x ->
             sb_close !ic_closed ic >>= fun () ->
             sb_close !oc_closed oc >>= fun () ->
             Lwt.fail x ))
  and f_es server er =
    if Int_result.is_error er then
      let () = Stream.close_noerr server in
      Lwt.fail (Int_result.to_exn ~name:"listen" er)
    else
      let shutdown () =
        Stream.close_noerr server;
        Lwt.return_unit
      in
      Lwt.return { shutdown = Lazy.from_fun shutdown }
  in
  match addr with
  | Unix.ADDR_UNIX path ->
    let server = Pipe.init () in
    let s_server = Pipe.to_stream server in
    Pipe.bind_exn server ~path;
    let cb = function
    | Error _ -> Pipe.close_noerr server
    | Ok client ->
      (* peername only supported for tcp ... *)
      Pipe.to_stream client |> f_cb dummy_address_pipe
    in
    let er = Pipe.listen server ~max:backlog ~cb in
    f_es s_server er
  | Unix.ADDR_INET(a,b) ->
    let server = Tcp.init () in
    let s_server = Tcp.to_stream server in
    Tcp.bind_exn server a b;
    let cb = function
    | Error _ -> Tcp.close_noerr server
    | Ok client ->
      let s_client = Tcp.to_stream client in
      let address = match with_address with
      | false -> dummy_address_tcp
      | true ->
        (* ugly, but otherwise the API would be different
           from Lwt_io :-( *)
        match Tcp.getpeername client with
        | Error _ -> dummy_address_tcp (* see comment above *)
        | Ok (x,b) -> Unix.ADDR_INET(x,b) in
      f_cb address s_client in
    let er = Tcp.listen server ~max:backlog ~cb in
    f_es s_server er

let establish_server ?buffer_size ?backlog ?no_close addr f =
  establish_server_intern ?no_close ?buffer_size ?backlog addr (`No_address f)

let establish_server_with_client_address ?buffer_size ?backlog ?no_close addr f =
  establish_server_intern ?no_close ?buffer_size ?backlog addr (`With_address f)

let shutdown_server server = Lazy.force server.shutdown

let ignore_close ch =
  ignore (close ch)

let make_stream f lazy_ic =
  let lazy_ic =
    lazy(Lazy.force lazy_ic >>= fun ic ->
         Gc.finalise ignore_close ic;
         Lwt.return ic)
  in
  Lwt_stream.from (fun _ ->
                     Lazy.force lazy_ic >>= fun ic ->
                     f ic >>= fun x ->
                     if x = None then
                       close ic >>= fun () ->
                       Lwt.return x
                     else
                       Lwt.return x)

let lines_of_file filename =
  make_stream read_line_opt (lazy(open_file ~mode:input filename))

let lines_to_file filename lines =
  with_file ~mode:output filename (fun oc -> write_lines oc lines)

let chars_of_file filename =
  make_stream read_char_opt (lazy(open_file ~mode:input filename))

let chars_to_file filename chars =
  with_file ~mode:output filename (fun oc -> write_chars oc chars)

let hexdump_stream oc stream = write_lines oc (Lwt_stream.hexdump stream)
let hexdump oc buf = hexdump_stream oc (Lwt_stream.of_string buf)

let set_default_buffer_size size =
  check_buffer_size "set_default_buffer_size" size;
  default_buffer_size := size

let default_buffer_size _ = !default_buffer_size
