open Internal
open Itypes
open Lwt.Infix

include (Stream: (module type of Stream))
include (Handle_fileno: (module type of Handle_fileno) with type t := t)

let pipe_wrap = JU.global##.process##binding ~$"pipe_wrap"
let pipe = pipe_wrap##.Pipe
let pipe_connect = pipe_wrap##.PipeConnectWrap

let init () : t = create (new%js pipe) Create_stream
let with_pipe f =
  let t = init () in
  Lwt.finalize (fun () -> f t)
    (fun () -> close_noerr t; Lwt.return_unit)

let bind t ~(path:string) =
  if path = "" then Int_result.einval else
  let jpath = Js.string path in
  if jpath##indexOf ~$"\000" <> -1 then Int_result.echarset else
  match t.cstate with
  | Closed -> Int_result.ebadf
  | Corrupted x -> x
  | Open ->
    let er = (JU.coerce t.handle)##bind jpath in
    if er = 0 then
      t.initialized <- true;
    er

let bind_exn t ~path = bind t ~path |> Int_result.to_exnu "bind"

let connect t ~path =
  let name = "connect" in
  match t.cstate with
  | Closed -> Int_result.ufail ~name ~param:path Unix.EBADF
  | Corrupted x -> Int_result.fail ~name ~param:path x
  | Open ->
    let jpath = Js.string path in
    if Int_result.cnull jpath then Int_result.enull ~name ~param:path else
    let s,w = Lwt.wait () in
    let oncomplete (e:int) : unit =
      if e <> 0 then Int_result.wakeup_exn ~name ~param:path w e else
      let () = t.initialized <- true in
      Lwt.wakeup w () in
    let req = new%js pipe_connect in
    req##.oncomplete := JU.callback oncomplete;
    let er = (JU.coerce t.handle)##connect req jpath oncomplete in
    if er = 0 then s
    else Int_result.fail ~name ~param:path er

let with_connect ~path f =
  let t = init () in
  Lwt.finalize (fun () -> connect t ~path >>= fun () -> f t)
    (fun () -> close_noerr t; Lwt.return_unit)

let openpipe (n:file_descr) =
  let t = init () in
  try
    let _er = (JU.coerce t.handle)##_open n in
    t.initialized <- true;
    Ok t
  with
  | x -> Error (node_exn_to_error x)
let openpipe_exn t = openpipe t |> to_exn "uv_open_pipe"

let with_open n f =
  match openpipe n with
  | Error x -> Int_result.ufail ~name:"uv_open_pipe" (Int_result.error_to_unix_error x)
  | Ok t ->
    Lwt.finalize (fun () -> f t)
      (fun () -> close_noerr t; Lwt.return_unit)
