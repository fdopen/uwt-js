open Internal
open Itypes
include Handle

type io =
  | Inherit_file of file
  | Create_pipe of Pipe.t
  | Inherit_pipe of Pipe.t
  | Inherit_stream of Stream.t

type exit_cb = t -> exit_status:int -> term_signal:int -> unit

let process_wrap = (Jg.process##binding ~$"process_wrap")##._Process

let spawn_exn
    ?stdin ?stdout ?stderr ?(uid:int option) ?(gid:int option)
    ?(verbatim_arguments=false) ?(detach=false)
    ?env ?cwd ?exit_cb file args =
  let name = "uv_spawn" in
  let e_einval = Unix.Unix_error(Unix.EINVAL,name,file)
  and e_charset = Unix.Unix_error(Unix.EUNKNOWNERR Int_result.echarset,
                                  name,file)
  and options = new%js Jg.obj
  and stdio = new%js Js.array_length 3
  and npipes = Array.make 3 None in
  let f std n =
    let o = new%js Jg.obj in
    JU.set stdio n o;
    match std with
    | None -> o##._type := ~$"ignore"
    | Some x ->
      match x with
      | Inherit_file file ->
        if file < 0 then raise e_einval;
        o##._type := ~$"fd";
        o##.fd := file;
      | Inherit_pipe p ->
        if not p.initialized then raise e_einval;
        o##._type := ~$"wrap";
        o##.handle := p.handle;
      | Inherit_stream s ->
        if not s.initialized then raise e_einval;
        o##._type := ~$"wrap";
        o##.handle := s.handle;
      | Create_pipe p ->
        match p.cstate with
        | Closed | Corrupted _ -> raise e_einval
        | Open ->
          if (JU.coerce p.handle)##.fd >= 0 || p.initialized then
            raise e_einval;
          o##._type := ~$"pipe";
          o##.handle := p.handle;
          Array.unsafe_set npipes  n (Some p);
  in
  f stdin 0;
  f stdout 1;
  f stderr 2;
  options##.stdio := stdio;
  (match uid with
  | None ->  options##.uid := Js.undefined;
  | Some x -> options##.uid := x);
  (match gid with
  | None ->  options##.gid := Js.undefined;
  | Some x -> options##.gid := x);
  let wva = if verbatim_arguments then Js._true else Js._false in
  options##.windowsVerbatimArguments := wva;
  options##.detach := if detach then Js._true else Js._false;
  let l_to_jsar l =
    list_map_to_js_array l @@ fun s ->
    let s = Js.string s in
    if Int_result.cnull s then raise e_charset;
    s in
  options##.envPairs := (match env with
  | None -> Jg.process##.env;
  | Some l -> l_to_jsar l);
  (match cwd with
  | None -> options##.cwd  := Js.undefined;
  | Some x ->
    let x = Js.string x in
    if Int_result.cnull x then raise e_charset;
    options##.cwd := x);
  if file = "" then
    raise e_einval;
  let jfile = Js.string file in
  if Int_result.cnull jfile then raise e_charset;
  options##.file := jfile;
  options##.args := l_to_jsar args;

  let process = new%js process_wrap in
  let handle = create process Create_nonstream in
  (match exit_cb with
  | None ->
    let onexit _ _ : unit = close_noerr handle in
    process##.onexit := JU.callback onexit;
  | Some exit_cb ->
    let onexit (exit_status:int) term_signal : unit =
      let term_signal = Signal.jsstring_to_caml_signal term_signal in
      exit_cb handle ~exit_status ~term_signal;
    in
    process##.onexit := JU.callback onexit);
  let er = process##spawn options in
  for i = 0 to 2 do
    match Array.unsafe_get npipes i with
    | None -> ()
    | Some p ->
      if er = 0 then
        p.initialized <- true
      else
      let old' = JU.coerce p.handle in
      if old'##.fd >= 0 then
        let _ = old'##close in
        p.handle <- new%js Pipe.pipe
  done;
  if er = 0 then handle else
  let () = close_noerr handle in
  Int_result.raise_exn ~name ~param:file er

let spawn ?stdin ?stdout ?stderr ?uid ?gid ?verbatim_arguments
    ?detach ?env ?cwd ?exit_cb exe args =
  try
    Ok (spawn_exn ?stdin ?stdout ?stderr ?uid ?gid ?verbatim_arguments
          ?detach ?env ?cwd ?exit_cb exe args)
  with
  | Unix.Unix_error(x,_,_) -> Error (Int_result.unix_error_to_error x)

let process_kill t (signum:int) =
  match t.cstate with
  | Closed | Corrupted _ -> Int_result.einval
  | Open ->
    let signum = Signal.caml_signal_to_node_signal signum in
    if signum = 0 then Int_result.enosys
    else (JU.coerce t.handle)##kill signum

let process_kill_exn t s = process_kill t s |> Int_result.to_exnu "kill"

let pid (t:t) : Int_result.int =
  match t.cstate with
  | Closed | Corrupted _ -> Int_result.einval
  | Open -> (JU.coerce t.handle)##.pid

let pid_exn t = pid t |> Int_result.to_exni "uv_pid"

let kill ~(pid:int) ~(signum:int) =
  let signum = Signal.caml_signal_to_node_signal signum in
  if signum = 0 then Int_result.enosys
  else Jg.process##__kill pid signum
let kill_exn ~pid ~signum = kill ~pid ~signum |> Int_result.to_exnu "kill"
