open Internal

let win32 = win32

let unix = not win32
let cygwin = false (* cygwin not supported by nodejs *)
let os_type = if win32 then "Win32" else "Unix"

let big_endian = Jg.os##endianness == ~$"BE"

let exit_function = ref None

let my_exit : (unit, unit -> unit) Js.meth_callback =
  JU.callback @@ fun () ->
  match !exit_function with
  | None -> ()
  | Some f -> f ()

let at_exit (f:(unit -> unit)) =
  match !exit_function with
  | Some g -> exit_function := Some (fun () -> f(); g())
  | None ->
    exit_function := Some f;
    Jg.process##on ~$"exit" my_exit

let hexn e = raise (Sys_error (Js.to_string e##.message))

let pr a f = try f a with Js.Error e -> hexn e

let getcwd () = pr () @@ fun () ->
  let x = Jg.process##cwd in
  Js.to_string x

let chdir x = pr () @@ fun () ->
  let _ = Jg.process##chdir (Js.string x) in
  ()

let fs = Jg.require ~$"fs"

let remove x = pr x @@ fun x ->
  let _ = fs##unlinkSync (string_to_nodefln_unsafe x) in
  ()

let readdir x = pr x @@ fun x ->
  let opts = JU.obj [| "encoding", JU.inject nodefln_encoding |] in
  let jar = fs##readdirSync (string_to_nodefln_unsafe x) opts in
  nodefln_array_to_string_array jar

let file_exists f =
  try
    let _ = fs##statSync (string_to_nodefln_unsafe f) in
    true
  with
  | Js.Error _ -> false

let is_directory f = pr f @@ fun f ->
  let x = fs##statSync (string_to_nodefln_unsafe f) in
  x##isDirectory == Js._true

let rename a b = pr () @@ fun () ->
  let _ =
    fs##renameSync
      (string_to_nodefln_unsafe a)
      (string_to_nodefln_unsafe b)
  in ()

let child_process = Jg.require ~$"child_process"
let command x =
  try
    let opts = JU.obj [| "stdio", JU.inject ~$"inherit" |] in
    let _ = child_process##execSync (Js.string x) opts in
    0
  with
  | Js.Error x ->
    let t = Obj.magic x in
    if Js.typeof t##.status == ~$"number" then
      t##.status
    else
      hexn x
