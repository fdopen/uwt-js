open Internal
open Itypes

type c_handle_type =
  | Create_stream
  | Create_nonstream

type unknown (* todo: type? *)
type t = {
  id: int;
  typ: handle_type;
  mutable initialized: bool;
  mutable handle: unknown Js.t;
  mutable cstate: close_state;
}
and handle_type =
  | Stream of stream_info
  | Nonstream
and stream_info = (* pull read emulation *)
  {
    mutable state: read_state;
    bufs: Cbytes.t Queue.t;
    ha: t;
    mutable hread: int;
    mutable listening: bool;
  }
and read_state =
  | No_read       (* Read not started *)
  | Read_broken
  | Read_finished (* eof received *)
  | Waiting of (int * Cbytes.t) Lwt.u (* read waiting *)
  | Read_not_stopped (* Message, for the read-thread, that was woken up,
                        that readStop was not yet called. *)
  | Iterative of (Bytes.t uv_result -> unit)
and close_state =
  | Open
  | Closed
  | Corrupted of int (* close was not called, but marked as broken.
                        Currently used by read *)

let close_internal w t =
  match t.cstate with
  | Closed -> Int_result.ebadf
  | Corrupted _ | Open ->
    t.cstate <- Closed;
    (* ensure reader get the canceled messages!*)
    (match t.typ with
    | Nonstream -> ()
    | Stream i ->
      match i.state with
      | Read_finished | Iterative _ | Read_broken | No_read | Read_not_stopped -> ()
      | Waiting w ->
        i.state <- No_read;
        Lwt.wakeup w (Int_result.ecanceled,Obj.magic Js.null));
    let h = JU.coerce t.handle in
    match w with
    | None ->
      let _ = h##close in
      0
    | Some w ->
      let f () : unit = Lwt.wakeup w () in
      let _ = h##close (JU.callback f) in
      0

let cnt = ref min_int
let create (h:'a Js.t) (ht:c_handle_type) : t =
  let cstate = Open
  and handle = JU.coerce h
  and id = !cnt in
  if !cnt = max_int then
    cnt := min_int
  else
    incr cnt;
  match ht with
  | Create_nonstream -> { id; cstate; handle ; typ = Nonstream;
                          initialized = false }
  | Create_stream ->
    let rec t = { id; cstate; handle ; typ = Stream i ; initialized = false }
    and i = {
      state = No_read;
      bufs = Queue.create ();
      ha = t;
      hread = 0;
      listening = false; }  in
    t

external to_handle : t -> t = "%identity"

let close_wait t =
  let s,w = Lwt.wait () in
  let x = close_internal (Some w) t in
  if x = 0 then s
  else Int_result.fail ~name:"close" x

let close t = close_internal None t
let close_noerr t = close_internal None t |> ignore

let void t m =
  match t.cstate with
  | Closed -> ()
  | Corrupted _ | Open ->
    let _ = JU.meth_call (JU.coerce t.handle) m [| |] in
    ()

let ref' t = void t "ref"
let unref t = void t "unref"

let has_ref t =
  match t.cstate with
  | Closed -> false
  | Corrupted _ | Open -> (JU.coerce t.handle)##hasRef == Js._true
