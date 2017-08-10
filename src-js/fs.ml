open Internal
open Itypes
open Fs_types

module type Fs_help = sig
  type 'a t
  val is_lwt: bool
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val return : 'a -> 'a t
  val return_unit : unit t
  val fail : exn -> 'a t
  val efail : ?param:string -> name:string -> error -> 'a t

  val call1 : string -> 'a -> 'b t
  val call2 : string -> 'a -> 'b -> 'c t
  val call3 : string -> 'a -> 'b -> 'c -> 'd t
  val call5 : string -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f t
end

module Lwt_help = struct
  type 'a t = 'a Lwt.t
  let is_lwt = true
  let (>>=) = Lwt.bind
  let (>|=) = Lwt.(>|=)
  let fail = Lwt.fail
  let return = Lwt.return
  let return_unit = Lwt.return_unit
  let efail ?(param="") ~name e =
    let e = Int_result.error_to_unix_error e in
    Lwt.fail (Unix.Unix_error(e,name,param))

  let fs_req_wrap = Jg.fs##.FSReqWrap
  let sleeper_req () =
    let s,w = Lwt.wait () in
    let oncomplete er res : unit =
      if Obj.magic er then
        Lwt.wakeup_exn w (Int_result.node_exn_to_unix_exn er)
      else
        Lwt.wakeup w res in
    let req = new%js fs_req_wrap in
    req##.oncomplete := JU.callback oncomplete;
    s,req

  let call1 meth param1 =
    let s,req = sleeper_req () in
    let _ = JU.(meth_call Jg.fs meth [|inject param1 ; inject req|]) in
    s

  let call2 meth param1 param2 =
    let s,req = sleeper_req () in
    let _ = JU.(meth_call Jg.fs meth
                  [|inject param1 ;inject param2 ; inject req|]) in
    s

  let call3 meth param1 param2 param3 =
    let s,req = sleeper_req () in
    let _ =
      JU.(meth_call Jg.fs meth
            [|inject param1 ;inject param2 ;inject param3 ; inject req|]) in
    s

  let call5 meth param1 param2 param3 param4 param5 =
    let s,req = sleeper_req () in
    let _ =
      JU.(meth_call Jg.fs meth
            [|inject param1 ;inject param2 ;inject param3 ;
            inject param4 ; inject param5 ; inject req|]) in
    s
end

module Sync_help = struct
  type 'a t = 'a Itypes.uv_result
  let is_lwt = false
  let (>>=) v f =
    match v with
    | Error _ as e -> e
    | Ok v -> f v
  let (>|=) v f =
    match v with
    | Error _ as e -> e
    | Ok v -> Ok (f v)
  let fail exn = raise exn
  let efail ?param:_ ~name:_ e = Error e
  let return s = Ok s
  let return_unit = Ok ()

  let call meth ar =
    try
      Ok JU.(meth_call Jg.fs meth ar)
    with
    | Js.Error t -> Error (Int_result.node_js_error_to_error t)

  let i = JU.inject
  let call1 meth param1 = call meth [|i param1|]
  let call2 meth param1 param2 = call meth [|i param1 ;i param2|]
  let call3 meth param1 param2 param3 =
    call meth [|i param1 ;i param2 ;i param3|]
  let call5 meth param1 param2 param3 param4 param5 =
    call meth [|i param1 ;i param2 ;i param3 ;i param4;i param5|]
end

let oconv = function
| O_RDONLY -> 0 lor (JU.get Jg.fs_const "O_RDONLY")
| O_WRONLY -> 0 lor (JU.get Jg.fs_const "O_WRONLY")
| O_RDWR -> 0 lor (JU.get Jg.fs_const "O_RDWR")
| O_NONBLOCK -> 0 lor (JU.get Jg.fs_const "O_NONBLOCK")
| O_CREAT -> 0 lor (JU.get Jg.fs_const "O_CREAT")
| O_EXCL -> 0 lor (JU.get Jg.fs_const "O_EXCL")
| O_TRUNC -> 0 lor (JU.get Jg.fs_const "O_TRUNC")
| O_APPEND -> 0 lor (JU.get Jg.fs_const "O_APPEND")
| O_NOCTTY -> 0 lor (JU.get Jg.fs_const "O_NOCTTY")
| O_DSYNC -> 0 lor (JU.get Jg.fs_const "O_DSYNC")
| O_SYNC -> 0 lor (JU.get Jg.fs_const "O_SYNC")
| O_RSYNC -> 0 lor (JU.get Jg.fs_const "O_RSYNC")
| O_TEMPORARY -> 0 lor (JU.get Jg.fs_const "O_TEMPORARY")
| O_SHORT_LIVED -> 0 lor (JU.get Jg.fs_const "O_SHORT_LIVED")
| O_SEQUENTIAL -> 0 lor (JU.get Jg.fs_const "O_SEQUENTIAL")
| O_RANDOM -> 0 lor (JU.get Jg.fs_const "O_RANDOM")

let aconv = function
| Read -> 0 lor (JU.get Jg.fs_const "R_OK")
| Write -> 0 lor (JU.get Jg.fs_const "W_OK")
| Exec -> 0 lor (JU.get Jg.fs_const "X_OK")
| Exists -> 0 lor (JU.get Jg.fs_const "F_OK")

module Impl (H : Fs_help) = struct
  open H

  include Fs_types

  let tr ~name fln f =
    match string_to_nodefln fln with
    | Empty_string -> efail ~param:fln ~name EINVAL
    | String_with_null -> efail ~param:fln ~name ECHARSET
    | Nodefln s -> f s

  let tr2 ~name fln1 fln2 f =
    match string_to_nodefln fln1 with
    | Empty_string -> efail ~param:fln1 ~name EINVAL
    | String_with_null -> efail ~param:fln1 ~name ECHARSET
    | Nodefln fln1 ->
      match string_to_nodefln fln2 with
      | Empty_string -> efail ~param:fln2 ~name EINVAL
      | String_with_null -> efail ~param:fln2 ~name ECHARSET
      | Nodefln fln2 -> f fln1 fln2

  let openfile ?(perm=0o644) ~mode fln : file t =
    let name = "open" in
    tr ~name fln @@ fun fln ->
    let mode = List.fold_left (fun ac el -> ac lor oconv el) 0 mode in
    call3 name fln mode perm

  let xstat name fln =
    tr ~name fln @@ fun fln ->
    call1 name fln >|= fun _ ->
    Stat.get true
  let lstat fln = xstat "lstat" fln
  let stat fln = xstat "stat" fln

  let fstat (fd:file) =
    call1 "fstat" fd >|= fun _ ->
    Stat.get true

  let iread ?(pos=0) ?len ~fd_offset ~dim buf (file:file) =
    let len = match len with
    | None -> dim - pos
    | Some x -> x in
    if pos < 0 || len < 0 || pos > dim - len then
      fail (Invalid_argument "Uwt.Fs.read")
    else
    let fd_offset = Obj.magic fd_offset (* null or number *)
    and buf = match buf with
    | Rba buf -> buf
    | Rbytes buf ->
      Cbytes.convert_bytes_to_array buf;
      Cbytes.of_bytes_maybe_proxy ~pos:0 ~len:dim buf in
    call5 "read" file buf pos len fd_offset >>= fun n ->
    (* prevent garbage collection of buf.
       Hopefully it won't be optimized away. *)
    if n > 9_000_000 then (
      let c = Cbytes.get buf 0 in
      Cbytes.set buf 0 c
    );
    return n

  let read_ba ?pos ?len t ~buf =
    let dim = Cbytes.length buf in
    iread ~fd_offset:Js.null ?pos ?len ~dim (Rba buf) t

  let read ?pos ?len t ~buf =
    let dim = Bytes.length buf in
    iread ~fd_offset:Js.null ?pos ?len ~dim (Rbytes buf) t

  let pread_ba ?pos ?len t ~fd_offset ~buf =
    if Int64.compare fd_offset Int64.zero < 0 then efail ~name:"pread" EINVAL else
    let dim = Cbytes.length buf in
    iread ~fd_offset:(Int64.to_float fd_offset) ?pos ?len ~dim (Rba buf) t

  let pread ?pos ?len t ~fd_offset ~buf =
    if Int64.compare fd_offset Int64.zero < 0 then efail ~name:"pread" EINVAL else
    let dim = Bytes.length buf in
    iread ~fd_offset:(Int64.to_float fd_offset) ?pos ?len ~dim (Rbytes buf) t

  let iwrite ~fd_offset ?(pos=0) ?len ~dim (file:file) buf =
    let len = match len with
    | None -> dim - pos
    | Some x -> x in
    if pos < 0 || len < 0 || pos > dim - len then
      fail (Invalid_argument "Uwt.Fs.write")
    else
    let fd_offset = Obj.magic fd_offset in (* null or number *)
    (match buf with
    | Wbytes buf ->
      let buf = Cbytes.of_bytes_maybe_proxy ~pos ~len buf in
      call5 "writeBuffer" file buf 0 len fd_offset
    | Wstring buf ->
      let buf = Cbytes.of_string_maybe_proxy ~pos ~len buf in
      call5 "writeBuffer" file buf 0 len fd_offset
    | Wba buf ->
      call5 "writeBuffer" file buf pos len fd_offset) >>= fun n ->
    if n > 900_000 then ( (* prevent GC *)
      match buf with
      | Wbytes _ | Wstring _ -> ()
      | Wba buf ->
        let c = Cbytes.get buf 0 in
        Cbytes.set buf 0 c
    );
    return n

  let write_ba ?pos ?len t ~(buf:buf) =
    let dim = Cbytes.length buf in
    iwrite ~fd_offset:Js.null ~dim ?pos ?len t (Wba buf)

  let write_string ?pos ?len t ~buf =
    let dim = String.length buf in
    iwrite ~fd_offset:Js.null ~dim ?pos ?len t (Wstring buf)

  let write ?pos ?len t ~buf =
    let dim = Bytes.length buf in
    iwrite ~fd_offset:Js.null ~dim ?pos ?len t (Wbytes buf)

  let iwrite ~fd_offset ?pos ?len ~dim t buf =
    if Int64.compare fd_offset Int64.zero < 0 then efail ~name:"pwrite" EINVAL
    else iwrite ~fd_offset:(Int64.to_float fd_offset) ~dim ?pos ?len t buf

  let pwrite_ba ?pos ?len t ~fd_offset ~(buf:buf) =
    let dim = Cbytes.length buf in
    iwrite ~fd_offset ~dim ?pos ?len t (Wba buf)

  let pwrite_string ?pos ?len t ~fd_offset ~buf =
    let dim = String.length buf in
    iwrite ~fd_offset ~dim ?pos ?len t (Wstring buf)

  let pwrite ?pos ?len t ~fd_offset ~buf =
    let dim = Bytes.length buf in
    iwrite ~fd_offset ~dim ?pos ?len t (Wbytes buf)

  let to_unit _ = return_unit

  let close (fd:file) = call1 "close" fd >>= to_unit
  let fsync (fd:file) = call1 "fsync" fd >>= to_unit
  let fdatasync (fd:file) = call1 "fdatasync" fd >>= to_unit

  let ftruncate (fd:file) ~len =
    let flen = Int64.to_float len in
    let len' = Int64.of_float flen in
    if len <> len' then efail ~name:"ftruncate" ERANGE
    else call2 "ftruncate" fd flen >>= to_unit

  let string_unit ~name ~fln =
    tr ~name fln @@ fun fln ->
    call1 name fln >>= to_unit

  let unlink fln = string_unit ~name:"unlink" ~fln
  let rmdir fln = string_unit ~name:"rmdir" ~fln

  let mkdir ?(perm=0o777) fln =
    let name = "mkdir" in
    tr ~name fln @@ fun fln ->
    call2 name fln perm >>= to_unit

  let mkdtemp pat =
    let name = "mkdtemp" in
    tr ~name pat @@ fun pat ->
    call2 name pat nodefln_encoding >|= fun fln ->
    nodefln_to_string fln

  let scandir fln =
    tr ~name:"scandir" fln @@ fun fln ->
    call2 "readdir" fln nodefln_encoding >|= fun jar ->
    nodefln_array_to_string_array jar

  let utime fln ~(access:float) ~(modif:float) =
    tr ~name:"utime" fln @@ fun fln ->
    call3 "utimes" fln access modif >>= to_unit

  let futime (fd:file) ~(access:float) ~(modif:float) =
    call3 "futimes" fd access modif >>= to_unit

  let rename ~src ~dst =
    let name = "rename" in
    tr2 ~name:"rename" src dst @@ fun src dst ->
    call2 name src dst >>= to_unit

  let link ~target ~link_name =
    let name = "link" in
    tr2 ~name target link_name @@ fun target link_name ->
    call2 name target link_name >>= to_unit

  let symlink ?(mode=S_Default) ~src ~dst () =
    let mode = match mode with
    | S_Default -> ~$"file"
    | S_Dir -> ~$"dir"
    | S_Junction -> ~$"junction" in
    let name ="symlink" in
    tr2 ~name src dst @@ fun src dst ->
    call3 name src dst mode >>= to_unit

  let access fln mode =
    let name = "access" in
    let mode = List.fold_left (fun ac el -> ac lor aconv el) 0 mode in
    tr ~name fln @@ fun fln ->
    call2 name fln mode >>= to_unit

  let chmod fln ~(perm:int) =
    let name = "chmod" in
    tr ~name fln @@ fun fln ->
    call2 name fln perm >>= to_unit

  let fchmod (fd:file) ~(perm:int) =
    call2 "fchmod" fd perm >>= to_unit

  let chown fln ~(uid:int) ~(gid:int) =
    let name = "chown" in
    tr ~name fln @@ fun fln ->
    call3 name fln uid gid >>= to_unit

  let fchown (fd:file) ~(uid:int) ~(gid:int) =
    call3 "fchown" fd uid gid >>= to_unit

  let readlink fln =
    let name = "readlink" in
    tr ~name fln @@ fun fln ->
    call2 name fln nodefln_encoding >|= fun fln ->
    nodefln_to_string fln
end

module Async = Impl(Lwt_help)
module Sync = Impl(Sync_help)
