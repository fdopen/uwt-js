open Internal
open Itypes

let cares_wrap = Jg.process##binding ~$"cares_wrap"
let addrinfo_req = cares_wrap##.GetAddrInfoReqWrap
let getnameinfo_req = cares_wrap##.GetNameInfoReqWrap
let ip_length (str:(Js.js_string Js.t)) : int = cares_wrap##isIP str

type family =
  | UNSPEC
  | INET
  | INET6

let getaddrinfo ?(family=UNSPEC) host =
  let name = "getaddrinfo"
  and jhost = Js.string host in
  if Int_result.cnull jhost then Int_result.enull ~param:host ~name else
  let jfamily = match family with
  | UNSPEC -> 0
  | INET -> 4
  | INET6 -> 6 in
  let s,w = Lwt.wait () in
  let oncomplete er res : unit =
    if Obj.magic er then
      let er = Int_result.to_error er in
      Lwt.wakeup w (Error er)
    else
    let length = res##.length lor 0 in
    let l = ref [] in
    for i = length - 1 downto 0 do
      let x = JU.get res i in
      let t = match jfamily with
      | 4 -> Ip4
      | 6 -> Ip6
      | _ ->
        match ip_length x with
        | 4 -> Ip4
        | _ -> Ip6 in (* everything other than AF_INET[6]? ignored by node*)
      let addr = create_inet_addr t x in
      l := addr::!l
    done;
    Lwt.wakeup w (Ok !l)
  in
  let req = new%js addrinfo_req in
  req##.oncomplete := JU.callback oncomplete;
  let er = cares_wrap##getaddrinfo req jhost jfamily 0 in
  if er = 0 then s
  else Int_result.fail ~name ~param:host er

let getnameinfo addr port =
  let name = "getnameinfo"
  and jaddr = (of_inet_addr addr)##.i in
  if port < 0 || port >= 65536 then Int_result.ufail ~name Unix.EINVAL else
  let s,w = Lwt.wait () in
  let oncomplete er hostname service : unit =
    if Obj.magic er then
      let er = Int_result.to_error er in
      Lwt.wakeup w @@ Error er
    else
      Lwt.wakeup w @@ Ok {
        Unix.ni_hostname = Js.to_string hostname;
        Unix.ni_service = Js.to_string service } in
  let req = new%js getnameinfo_req in
  req##.oncomplete := oncomplete;
  let er = cares_wrap##getnameinfo req jaddr port in
  if er = 0 then s
  else Int_result.fail ~name er
