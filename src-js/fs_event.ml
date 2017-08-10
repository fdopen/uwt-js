open Internal
open Itypes
let fs_event_wrap = (Jg.process##binding ~$"fs_event_wrap")##._FSEvent

type x
type t = x Js.t option ref

type event =
  | Rename
  | Change

type cb = t -> (string * event) uv_result -> unit

let start ?(recursive=false) ?(persistent=true) fln ~(cb:cb) : t uv_result =
  match string_to_nodefln fln with
  | Empty_string -> Error EINVAL
  | String_with_null -> Error ECHARSET
  | Nodefln jfln ->
    let ev = new%js fs_event_wrap in
    let t = ref (Some (JU.coerce ev)) in
    let onchange status event_type filename =
      let v =
        if status < 0 then
          let _ = ev##close in
          t := None;
          Error (Int_result.to_error status)
        else
        let event =
          if event_type == ~$"rename" then
            Rename
          else
            Change
        and fln = nodefln_to_string filename in
        Ok (fln,event)
      in
      try
        cb t v
      with
      | x -> !Lwt.async_exception_hook x
    in
    ev##.onchange := JU.callback onchange;
    let recursive = if recursive then Js._true else Js._false
    and persistent = if persistent then Js._true else Js._false in
    let er = ev##start jfln recursive persistent nodefln_encoding in
    if er = 0 then Ok t else
    let _ = ev##close in
    Error (Int_result.to_error er)

let start_exn ?recursive ?persistent fln ~cb =
  start ?recursive ?persistent ~cb fln |> to_exn "uv_fs_event_start"

let close t =
  match !t with
  | None -> ()
  | Some x ->
    t := None;
    let _ = (JU.coerce x)##close in
    ()
