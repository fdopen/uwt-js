open Internal
open Itypes
open Lwt.Infix

include (Stream: (module type of Stream))
include (Handle_fileno: (module type of Handle_fileno) with type t := t)

let tcp_wrap = JU.global##.process##binding ~$"tcp_wrap"
let tcp = tcp_wrap##.TCP
let tcp_connect = tcp_wrap##.TCPConnectWrap

let init () : t = create (new%js tcp) Create_stream

let with_tcp f =
  let t = init () in
  Lwt.finalize (fun () -> f t)
    (fun () -> close_noerr t; Lwt.return_unit)

let bind t net (port:int) =
  if port < 0 || port >= 65536 then Int_result.einval else
  match t.cstate with
  | Closed -> Int_result.ebadf
  | Corrupted x -> x
  | Open ->
    let net = of_inet_addr net
    and h = JU.coerce t.handle in
    let er = match net##.l with
    | 4 -> h##bind net##.i port
    | _ -> h##bind6 net##.i port in
    if er = 0 then
      t.initialized <- true;
    er

let bind_exn t n p = bind t n p |> Int_result.to_exnu "bind"

let connect t net port =
  let name = "connect" in
  if port < 0 || port >= 65536 then Int_result.ufail ~name Unix.EINVAL else
  match t.cstate with
  | Closed -> Int_result.ufail ~name Unix.EBADF
  | Corrupted x -> Int_result.fail ~name x
  | Open ->
    let s,w = Lwt.wait () in
    let oncomplete (e:int) : unit =
      if e <> 0 then Int_result.wakeup_exn ~name w e else
      let () = t.initialized <- true in
      Lwt.wakeup w ()
    in
    let req = new%js tcp_connect in
    req##.oncomplete := JU.callback oncomplete;
    let net = of_inet_addr net
    and h = JU.coerce t.handle in
    let er = match net##.l with
    | 4 -> h##connect req net##.i port
    | _ -> h##connect6 req net##.i port in
    if er = 0 then s
    else Int_result.fail ~name er

let with_connect net port f =
  let t = init () in
  Lwt.finalize (fun () -> connect t net port >>= fun ()-> f t)
    (fun () -> close_noerr t; Lwt.return_unit)

let sock_peer sock t =
  let h = JU.coerce t.handle
  and ret = new%js Jg.obj in
  let er : int =
    if sock then h##getsockname ret
    else h##getpeername ret in
  if er <> 0 then Error (Int_result.to_error er) else
  let port : int = ret##.port
  and addr : Js.js_string Js.t = ret##.address
  and familiy : Js.js_string Js.t = ret##.family in
  if familiy == ~$"IPv4" then Ok (create_inet_addr Ip4 addr,port)
  else if familiy == ~$"IPv6" then Ok (create_inet_addr Ip6 addr,port)
  else Error UNKNOWN

let getpeername t = sock_peer false t
let getpeername_exn t = getpeername t |> to_exn "getpeername"

let getsockname t = sock_peer true t
let getsockname_exn t = getsockname t |> to_exn "getsockname"

let common allow_uninit t f =
  match t.cstate with
  | Closed -> Int_result.ebadf
  | Corrupted x -> x
  | Open ->
    let h = JU.coerce t.handle in
    if allow_uninit || (h##.fd >= 0 || t.initialized) then f h
    else Int_result.ebadf

let nodelay t b =
  common true t @@ fun x -> x##setNoDelay (if b then Js._true else Js._false)
let nodelay_exn t b = nodelay t b |> Int_result.to_exnu "uv_tcp_nodelay"

let enable_keepalive t n =
  if n < 0 then
    Int_result.einval
  else
    common false t @@ fun x -> x##setKeepAlive Js._true n
let enable_keepalive_exn t n =
  enable_keepalive t n |> Int_result.to_exnu "uv_tcp_keepalive"

let disable_keepalive t =
  common true t @@ fun x -> x##setKeepAlive Js._false 0
let disable_keepalive_exn t =
  disable_keepalive t |> Int_result.to_exnu "uv_tcp_keepalive"

let simultaneous_accepts =
  if win32 then
    fun t b ->
      common true t @@ fun x ->
      x##setSimultaneousAccepts (if b then Js._true else Js._false)
  else fun _ _ -> 0

let simultaneous_accepts_exn t b =
  simultaneous_accepts t b |> Int_result.to_exnu "uv_tcp_simultaneous_accepts"
