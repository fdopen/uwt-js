open Internal

let sleep_float s =
  if s < 0. || s > 2_147_483_647. then
    Lwt.fail_invalid_arg "Timer.sleep"
  else
  let sleeper,waker = Lwt.task () in
  let cb () : unit = Lwt.wakeup waker () in
  let id = JU.global##setTimeout (JU.callback cb) s in
  Lwt.on_cancel sleeper (fun () -> JU.global##clearTimeout id);
  sleeper

let sleep s =
  sleep_float (float_of_int s)

let sleep_seconds s =
  sleep_float (s *. 1_000.)
