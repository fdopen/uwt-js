open Internal
open Itypes

type x
type t = x Js.t option ref

let fs_poll_wrap = Jg.fs##._StatWatcher

type report = {
  prev : Fs_types.stats;
  curr : Fs_types.stats;
}

type cb = t -> report uv_result -> unit

let nothing _ = ()

let start ?(persistent=true) fln time ~(cb:cb) : t uv_result =
  if time < 1_000 then  Error EINVAL else
  match string_to_nodefln fln with
  | Empty_string -> Error EINVAL
  | String_with_null -> Error ECHARSET
  | Nodefln jfln ->
    let pw = new%js fs_poll_wrap in
    let t = ref (Some (JU.coerce pw)) in
    let onchange status =
      let v =
        if status <> 0 then
          Error (Int_result.to_error status)
        else
          Ok {
            prev = Stat.get false;
            curr = Stat.get true } in
      try
        cb t v
      with
      | x -> !Lwt.async_exception_hook x in
    pw##.onchange := JU.callback onchange;
    pw##.onstop := JU.callback nothing;
    let persistent = if persistent then Js._true else Js._false in
    let _ = pw##start jfln persistent time in (* returns void *)
    Ok t

let start_exn ?persistent fln time ~cb =
  start ?persistent fln time ~cb |> to_exn "uv_fs_poll_start"

let close t =
  match !t with
  | None -> ()
  | Some x ->
    t := None;
    let x = JU.coerce x in
    let _ = x##stop in (* no close method, the garbage collector will free it *)
    x##.onchange := JU.callback nothing
