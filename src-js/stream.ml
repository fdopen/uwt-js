open Internal
open Itypes
include Handle
external to_stream : t -> t = "%identity"

let stream_wrap = JU.global##.process##binding ~$"stream_wrap"
let write_wrap = stream_wrap##.WriteWrap
let shutdown_wrap = stream_wrap##.ShutdownWrap

let bufs_to_bytes s new_buf =
  let b1 = Queue.take s.bufs in
  let b1_len = Cbytes.length b1 - s.hread in
  let new_buf_len = Cbytes.length new_buf in
  let res_len = b1_len + new_buf_len in
  let res_len = Queue.fold (fun a e -> a + Cbytes.length e) res_len s.bufs in
  let res_bytes = Bytes.create res_len in
  Cbytes.blit_to_bytes b1 s.hread res_bytes 0 b1_len;
  let f acc elem =
    let len = Cbytes.length elem in
    Cbytes.blit_to_bytes elem 0 res_bytes acc len;
    acc + len
  in
  let pos = Queue.fold f b1_len s.bufs in
  Cbytes.blit_to_bytes new_buf 0 res_bytes pos new_buf_len;
  s.hread <- 0;
  Queue.clear s.bufs;
  res_bytes

let add_onread s =
  let h = JU.coerce s.ha.handle
  and t = s.ha in
  let onread (nread:int) (buf:Cbytes.t) : unit =
    if nread = 0 then () else
    match s.state with
    | Iterative cb ->
      if nread < 0 then
        if t.cstate = Closed then () else
        let er = Int_result.to_error nread in
        cb (Error er)
      else
      let obuf =
        if Queue.is_empty s.bufs then
          let obuf = Bytes.create nread in
          Cbytes.blit_to_bytes buf 0 obuf 0 nread;
          obuf
        else
          bufs_to_bytes s buf
      in
      (try cb (Ok obuf)
      with exn -> !Lwt.async_exception_hook exn);
    | Read_finished | Read_broken | Read_not_stopped | No_read ->
      if nread > 0 then
        let er = h##readStop in (* readStop can only fail for TTY handles
                                   under Windows ...*)
        Queue.add buf s.bufs;
        if er < 0 then (
          if Queue.length s.bufs > 5 then
            (* We can't continue fill the internal buffer forever ...*)
            close_noerr t;
          match t.cstate with
          | Closed | Corrupted _ -> ()
          | Open -> t.cstate <- Corrupted er);
    | Waiting w ->
      if nread >= 0 then
        s.state <- Read_not_stopped
      else
        (match t.cstate with
        | Closed -> s.state <- No_read
        | Corrupted _ -> let _er = h##readStop in ()
        | Open ->
          let er = h##readStop in
          if er < 0 then
            t.cstate <- Corrupted er;
          s.state <- Read_broken);
      Lwt.wakeup w (nread,buf);
      if nread > 0 then
        match s.state with
        | Read_finished | Iterative _ | Read_broken | No_read | Waiting _ -> ()
        | Read_not_stopped ->
          let er = h##readStop in
          if er >= 0 then
            s.state <- No_read
          else
          match t.cstate with
          | Closed | Corrupted _ -> ()
          | Open -> t.cstate <- Corrupted er
  in
  h##.onread := JU.callback onread

let shutdown t =
  let name = "write" in
  match t.cstate with
  | Closed -> Int_result.ufail ~name Unix.EBADF
  | Corrupted _ | Open ->
    let h = JU.coerce t.handle
    and req = new%js shutdown_wrap in
    req##.handle := h;
    let s,w = Lwt.wait () in
    let oncomplete status : unit =
      if status < 0 then
        Int_result.wakeup_exn ~name w status
      else
        Lwt.wakeup w () in
    req##.oncomplete := JU.callback oncomplete;
    let er = h##shutdown req in
    if er = 0 then s
    else Int_result.fail ~name er

let write ?(pos=0) ?len t ~buf ~dim =
  let name = "write" in
  if t.initialized = false then Int_result.ufail ~name Unix.EBADF else
  match t.cstate with
  | Corrupted x -> Int_result.fail ~name x
  | Closed -> Int_result.ufail ~name Unix.EBADF
  | Open ->
    let len = match len with
    | None -> dim - pos
    | Some x -> x in
    if pos < 0 || len < 0 || pos > dim - len then
      Lwt.fail (Invalid_argument "Uwt.Stream.write")
    else
    let s,w = Lwt.wait () in
    let oncomplete status : unit =
      if status = 0 then Lwt.wakeup w ()
      else Int_result.wakeup_exn ~name w status
    in
    let h = JU.coerce t.handle
    and req = new%js write_wrap in
    req##.handle := h;
    req##.async := Js._false;
    req##.oncomplete := JU.callback oncomplete;
    let data = match buf with
    | Wba buf -> Cbytes.proxy buf pos len
    | Wbytes b -> Cbytes.of_bytes_maybe_proxy b ~pos ~len
    | Wstring s -> Cbytes.of_string_maybe_proxy s ~pos ~len in
    let er : int = h##writeBuffer req data in
    if er <> 0 then
      Int_result.fail ~name er
    else if req##.async != Js._false && h##.writeQueueSize != 0 then
      let () = req##.__b := data in (* gc *)
      s
    else
      Lwt.return_unit

let write_string ?pos ?len t ~buf =
  let dim = String.length buf in
  write ?pos ?len t ~buf:(Wstring buf) ~dim

let write_ba ?pos ?len t ~buf =
  let dim = Cbytes.length buf in
  write ?pos ?len t ~buf:(Wba buf) ~dim

let write ?pos ?len t ~buf =
  let dim = Bytes.length buf in
  write ?pos ?len t ~buf:(Wbytes buf) ~dim

let rec use_rbufs t ~buf ~pos ~len ~read =
  if len = 0 || Queue.is_empty t.bufs then Lwt.return read else
  let b = Queue.peek t.bufs in
  let b_len = Cbytes.length b in
  let b_pos = t.hread in
  let to_read = min len (b_len - b_pos) in
  (match buf with
  | Rbytes buf -> Cbytes.blit_to_bytes b b_pos buf pos to_read;
  | Rba buf -> Cbytes.blit b b_pos buf pos to_read);
  let read = read + to_read in
  let len = len - to_read in
  let pos = pos + to_read in
  if b_pos + to_read < b_len then
    let () = t.hread <- t.hread + to_read in
    Lwt.return read
  else
    let _ = Queue.take t.bufs in
    t.hread <- 0;
    use_rbufs t ~buf ~pos ~len ~read

let rec read ~pos ~len s ~buf =
  let name = "read" in
  match s.state with
  | Read_finished ->
    if Queue.is_empty s.bufs then lwt_return_zero
    else use_rbufs s ~buf ~pos ~len ~read:0
  | Iterative _  | Waiting _ -> Int_result.ufail ~name:"read" Unix.EBUSY
  | Read_broken ->
    (match s.ha.cstate with
    | Open | Closed -> Int_result.ufail ~name Unix.EBADF
    | Corrupted x -> Int_result.fail ~name x);
  | Read_not_stopped | No_read ->
    if Queue.is_empty s.bufs = false then
      use_rbufs s ~buf ~pos ~len ~read:0
    else
    let h = JU.coerce s.ha.handle in
    let er =
      if s.state = Read_not_stopped then
        0
      else
      let er = h##readStart in
      if er <> 0 then er else (
        if Jg.equals h##.onread Js.null then
          add_onread s;
        0
      )
    in
    if er <> 0 then Int_result.fail ~name er else
    let sl,w = Lwt.task () in
    s.state <- Waiting w ;
    let f () =
      let er = h##readStop in
      if er = 0 then s.state <- No_read
      else match s.ha.cstate with
      | Corrupted _ | Closed -> ()
      | Open -> s.ha.cstate <- Corrupted er in
    Lwt.on_cancel sl f;
    let%lwt er,xbuf = sl in
    if er = 0 then
      if len = 0 then
        lwt_return_zero
      else
        read ~pos ~len s ~buf
    else if er > 0 then
      if er <= len && Queue.is_empty s.bufs then (
        (match buf with
        | Rbytes buf -> Cbytes.blit_to_bytes xbuf 0 buf pos er;
        | Rba buf -> Cbytes.blit xbuf 0 buf pos er);
        Lwt.return er
      )
      else
      let () = Queue.add xbuf s.bufs in
      use_rbufs s ~buf ~pos ~len ~read:0
    else if er = Int_result.eof then
      let () = s.state <- Read_finished in
      if Queue.is_empty s.bufs then
        lwt_return_zero
      else
        use_rbufs s ~buf ~pos ~len ~read:0
    else
      Int_result.fail ~name:"read" er

let read ?(pos=0) ?len t ~buf ~dim =
  let len = match len with
  | None -> dim - pos
  | Some x -> x in
  if pos < 0 || len < 0 || pos > dim - len then
    Lwt.fail (Invalid_argument "Uwt.Stream.read")
  else
  let name = "read" in
  if t.initialized = false then Int_result.ufail ~name Unix.EBADF else
  match t.typ with
  | Nonstream -> Int_result.ufail ~name Unix.EBADF
  | Stream s ->
    match t.cstate with
    | Open -> read ~pos ~len s ~buf
    | Corrupted _ | Closed ->
      (* Allow to read buffers after close *)
      if Queue.is_empty s.bufs = false then
        use_rbufs s ~buf ~pos ~len ~read:0
      else match t.cstate with
      | Corrupted x -> Int_result.fail ~name  x
      | Open | Closed -> Int_result.ufail ~name Unix.EBADF

let read_ba ?pos ?len t ~buf =
  let dim = Cbytes.length buf in
  read ?pos ?len t ~buf:(Rba buf) ~dim

let read ?pos ?len t ~buf =
  let dim = Bytes.length buf in
  read ?pos ?len t ~buf:(Rbytes buf) ~dim

let read_start t ~cb : int =
  if t.initialized = false then Int_result.ebadf else
  match t.cstate with
  | Corrupted x -> x
  | Closed -> Int_result.ebadf
  | Open ->
    match t.typ with
    | Nonstream -> Int_result.ebadf
    | Stream i ->
      match i.state with
      | Read_finished -> Int_result.eof
      | Iterative _ | Waiting _ -> Int_result.ebusy
      | Read_broken -> Int_result.ebadf
      | Read_not_stopped ->
        i.state <- Iterative cb;
        0
      | No_read ->
        let h = JU.coerce t.handle in
        let er = h##readStart in
        if er <> 0 then er else
        let () = i.state <- Iterative cb in
        if Jg.equals h##.onread Js.null then
          add_onread i;
        0

let read_start_exn a ~cb = read_start a ~cb |> Int_result.to_exnu "read"

let read_stop t =
  match t.cstate with
  | Closed -> Int_result.ebadf
  | Corrupted _  | Open ->
    match t.typ with
    | Nonstream -> Int_result.ebadf
    | Stream i ->
      let h = JU.coerce t.handle in
      match i.state with
      | Read_not_stopped | Waiting _ ->
        Int_result.ebusy (* don't allow to interfer with other read threads *)
      | No_read -> Int_result.einval
      | Iterative _ ->
        let er = h##readStop in
        if er = 0 then
          i.state <- No_read;
        er
      | Read_finished  | Read_broken -> h##readStop

let read_stop_exn a = read_stop a |> Int_result.to_exnu "uv_read_stop"

let listen t ~max ~cb =
  if max < 1 then Int_result.einval else
  if t.initialized = false then Int_result.ebadf else
  match t.cstate with
  | Closed -> Int_result.ebadf
  | Corrupted x -> x
  | Open ->
    let h = JU.coerce t.handle in
    if Obj.magic h##.onconnection then Int_result.ebusy else
    let onconnection er client : unit =
      let v =
        if er <> 0 then
          let e = Int_result.to_error er in
          Error e
        else
        let t = create client Create_stream in
        t.initialized <- true;
        Ok t in
      try
        cb v
      with
      | x -> !Lwt.async_exception_hook x in
    let () = h##.onconnection := onconnection in
    h##listen max

let listen_exn t ~max ~cb = listen t ~max ~cb |> Int_result.to_exnu "listen"

let write_queue_size t =
  match t.cstate with
  | Closed -> 0
  | Corrupted  _ | Open -> 0 lor (JU.coerce t.handle)##.writeQueueSize
