open Internal
open Itypes
open Handle
type t = Handle.t

let fileno t : file_descr uv_result =
  match t.cstate with
  | Closed -> Error EBADF
  | Open | Corrupted _ ->
    let x = (JU.coerce t.handle)##.fd in
    if x >= 0 then
      Ok x
    else if t.initialized && Internal.win32 then
      Error ENOSYS
    else
      Error EBADF

let fileno_exn s = fileno s |> to_exn "uv_fileno"
