open Internal
open Itypes

let signals = Jg.constants##.os##.signals
let sigwinch : int = 0 lor (JU.get signals "SIGWINCH")

let jsstring_to_caml_signal = Jg.obj##create Js.null
let caml_signal_to_node_signal : 'a Js.t = JU.new_obj Jg.map [||]
let () =
  JU.set jsstring_to_caml_signal ~$"SIGABRT" Sys.sigabrt;
  JU.set jsstring_to_caml_signal ~$"SIGALRM" Sys.sigalrm;
  JU.set jsstring_to_caml_signal ~$"SIGFPE" Sys.sigfpe;
  JU.set jsstring_to_caml_signal ~$"SIGHUP" Sys.sighup;
  JU.set jsstring_to_caml_signal ~$"SIGILL" Sys.sigill;
  JU.set jsstring_to_caml_signal ~$"SIGINT" Sys.sigint;
  JU.set jsstring_to_caml_signal ~$"SIGKILL" Sys.sigkill;
  JU.set jsstring_to_caml_signal ~$"SIGPIPE" Sys.sigpipe;
  JU.set jsstring_to_caml_signal ~$"SIGQUIT" Sys.sigquit;
  JU.set jsstring_to_caml_signal ~$"SIGSEGV" Sys.sigsegv;
  JU.set jsstring_to_caml_signal ~$"SIGTERM" Sys.sigterm;
  JU.set jsstring_to_caml_signal ~$"SIGUSR1" Sys.sigusr1;
  JU.set jsstring_to_caml_signal ~$"SIGUSR2" Sys.sigusr2;
  JU.set jsstring_to_caml_signal ~$"SIGCHLD" Sys.sigchld;
  JU.set jsstring_to_caml_signal ~$"SIGCONT" Sys.sigcont;
  JU.set jsstring_to_caml_signal ~$"SIGSTOP" Sys.sigstop;
  JU.set jsstring_to_caml_signal ~$"SIGTSTP" Sys.sigtstp;
  JU.set jsstring_to_caml_signal ~$"SIGTTIN" Sys.sigttin;
  JU.set jsstring_to_caml_signal ~$"SIGTTOU" Sys.sigttou;
  JU.set jsstring_to_caml_signal ~$"SIGVTALRM" Sys.sigvtalrm;
  JU.set jsstring_to_caml_signal ~$"SIGPROF" Sys.sigprof;
  JU.set jsstring_to_caml_signal ~$"SIGBUS" Sys.sigbus;
  JU.set jsstring_to_caml_signal ~$"SIGPOLL" Sys.sigpoll;
  JU.set jsstring_to_caml_signal ~$"SIGSYS" Sys.sigsys;
  JU.set jsstring_to_caml_signal ~$"SIGTRAP" Sys.sigtrap;
  JU.set jsstring_to_caml_signal ~$"SIGURG" Sys.sigurg;
  JU.set jsstring_to_caml_signal ~$"SIGXCPU" Sys.sigxcpu;
  JU.set jsstring_to_caml_signal ~$"SIGXFSZ" Sys.sigxfsz;
  JU.set jsstring_to_caml_signal ~$"SIGWINCH" sigwinch;
  let _ = caml_signal_to_node_signal##set Sys.sigabrt (0 lor (JU.get signals ~$"SIGABRT")) in
  let _ = caml_signal_to_node_signal##set Sys.sigalrm (0 lor (JU.get signals ~$"SIGALRM")) in
  let _ = caml_signal_to_node_signal##set Sys.sigfpe (0 lor (JU.get signals ~$"SIGFPE")) in
  let _ = caml_signal_to_node_signal##set Sys.sighup (0 lor (JU.get signals ~$"SIGHUP")) in
  let _ = caml_signal_to_node_signal##set Sys.sigill (0 lor (JU.get signals ~$"SIGILL")) in
  let _ = caml_signal_to_node_signal##set Sys.sigint (0 lor (JU.get signals ~$"SIGINT")) in
  let _ = caml_signal_to_node_signal##set Sys.sigkill (0 lor (JU.get signals ~$"SIGKILL")) in
  let _ = caml_signal_to_node_signal##set Sys.sigpipe (0 lor (JU.get signals ~$"SIGPIPE")) in
  let _ = caml_signal_to_node_signal##set Sys.sigquit (0 lor (JU.get signals ~$"SIGQUIT")) in
  let _ = caml_signal_to_node_signal##set Sys.sigsegv (0 lor (JU.get signals ~$"SIGSEGV")) in
  let _ = caml_signal_to_node_signal##set Sys.sigterm (0 lor (JU.get signals ~$"SIGTERM")) in
  let _ = caml_signal_to_node_signal##set Sys.sigusr1 (0 lor (JU.get signals ~$"SIGUSR1")) in
  let _ = caml_signal_to_node_signal##set Sys.sigusr2 (0 lor (JU.get signals ~$"SIGUSR2")) in
  let _ = caml_signal_to_node_signal##set Sys.sigchld (0 lor (JU.get signals ~$"SIGCHLD")) in
  let _ = caml_signal_to_node_signal##set Sys.sigcont (0 lor (JU.get signals ~$"SIGCONT")) in
  let _ = caml_signal_to_node_signal##set Sys.sigstop (0 lor (JU.get signals ~$"SIGSTOP")) in
  let _ = caml_signal_to_node_signal##set Sys.sigtstp (0 lor (JU.get signals ~$"SIGTSTP")) in
  let _ = caml_signal_to_node_signal##set Sys.sigttin (0 lor (JU.get signals ~$"SIGTTIN")) in
  let _ = caml_signal_to_node_signal##set Sys.sigttou (0 lor (JU.get signals ~$"SIGTTOU")) in
  let _ = caml_signal_to_node_signal##set Sys.sigvtalrm (0 lor (JU.get signals ~$"SIGVTALRM")) in
  let _ = caml_signal_to_node_signal##set Sys.sigprof (0 lor (JU.get signals ~$"SIGPROF")) in
  let _ = caml_signal_to_node_signal##set Sys.sigbus (0 lor (JU.get signals ~$"SIGBUS")) in
  let _ = caml_signal_to_node_signal##set Sys.sigpoll (0 lor (JU.get signals ~$"SIGPOLL")) in
  let _ = caml_signal_to_node_signal##set Sys.sigsys (0 lor (JU.get signals ~$"SIGSYS")) in
  let _ = caml_signal_to_node_signal##set Sys.sigtrap (0 lor (JU.get signals ~$"SIGTRAP")) in
  let _ = caml_signal_to_node_signal##set Sys.sigurg (0 lor (JU.get signals ~$"SIGURG")) in
  let _ = caml_signal_to_node_signal##set Sys.sigxcpu (0 lor (JU.get signals ~$"SIGXCPU")) in
  let _ = caml_signal_to_node_signal##set Sys.sigxfsz (0 lor (JU.get signals ~$"SIGXFSZ")) in
  let _ = caml_signal_to_node_signal##set sigwinch sigwinch in
  ()

let caml_signal_to_node_signal (x:int) =
  let i = caml_signal_to_node_signal##get x in
  if i == Js.undefined then 0
  else Obj.magic i

let jsstring_to_caml_signal (x:Js.js_string Js.t) =
  if x == ~$"" then 0 else
  let x = JU.get jsstring_to_caml_signal x in
  if x == Js.undefined then 0
  else Obj.magic x

include Handle

let signal_wrap = (Jg.process##binding ~$"signal_wrap")##._Signal
let start signum ~(cb:t -> int -> unit) : t uv_result =
  let js_signum = caml_signal_to_node_signal signum in
  if js_signum = 0 then Error ENOSYS else
  let s = new%js signal_wrap in
  let handle = create s Create_nonstream in
  let callback _ : unit = cb handle signum in
  s##.onsignal := JU.callback callback ;
  let er = s##start js_signum in
  if er = 0 then Ok handle else
  let _ = s##close in
  Error (Int_result.to_error er)

let start_exn i ~cb = start i ~cb |> to_exn "uv_signal_start"
