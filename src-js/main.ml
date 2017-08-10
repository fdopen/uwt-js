open Internal
let yield () =
  let sleeper,waker = Lwt.wait () in
  let cb () = Lwt.wakeup waker () in
  (* See: https://nodejs.org/en/docs/guides/event-loop-timers-and-nexttick/
     Immediate callbacks are called after the poll phase. Like the yielded
     threads inside Uwt.Main. Difference: close callbacks! *)
  let _id = JU.global##setImmediate (JU.callback cb) in
  sleeper

(* Nodejs doesn't provide the means to emulate
   enter_iter_hooks and leave_iter_hooks.
   And the Lwt interface, doesn't allow us to support Lwt.pause.
   paused thread will pause forever! *)
