include Uwt_base
open Lwt.Infix

let file_of_file_descr = Conv.file_of_file_descr
type file_descr = Unix.file_descr

module Cbytes = struct
  include Uwt_bytes
  external to_bigarray : t -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t = "%identity"
  external from_bigarray : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> t = "%identity"
end

module Timer = struct
  let sleep = Uwt.Timer.sleep
  let sleep_seconds = Uwt.Unix.sleep
end

module Sys = struct
  include Sys
  let at_exit = Pervasives.at_exit
end

module Handle = Uwt.Handle
module Handle_fileno = Uwt.Handle_fileno

module type Fs_funcs = sig
  include module type of Fs_types
  with type uv_open_flag = Fs_types.uv_open_flag
  with type file_kind = Fs_types.file_kind
  with type symlink_mode = Fs_types.symlink_mode
  with type access_permission = Fs_types.access_permission
  with type stats = Fs_types.stats
  type 'a t
  val openfile : ?perm:int -> mode:uv_open_flag list -> string -> file t
  val read : ?pos:int -> ?len:int -> file -> buf:bytes -> int t
  val read_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int t
  val pread : ?pos:int -> ?len:int -> file -> fd_offset:int64 -> buf:bytes ->
    int t
  val pread_ba : ?pos:int -> ?len:int -> file -> fd_offset:int64 -> buf:buf ->
    int t
  val write : ?pos:int -> ?len:int -> file -> buf:bytes -> int t
  val write_string : ?pos:int -> ?len:int -> file -> buf:string -> int t
  val write_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int t
  val pwrite : ?pos:int -> ?len:int -> file -> fd_offset:int64 ->
    buf:bytes -> int t
  val pwrite_string : ?pos:int -> ?len:int -> file -> fd_offset:int64 ->
    buf:string -> int t
  val pwrite_ba : ?pos:int -> ?len:int -> file -> fd_offset:int64 ->
    buf:buf -> int t
  (*val writev : file -> Iovec_write.t list -> int t
    val pwritev : file -> Iovec_write.t list -> int64 -> int t*)
  val close : file -> unit t
  val unlink : string -> unit t
  val mkdir : ?perm:int -> string -> unit t
  val rmdir : string -> unit t
  val fsync : file -> unit t
  val fdatasync : file -> unit t
  val ftruncate: file -> len:int64 -> unit t
  val stat : string -> stats t
  val lstat : string -> stats t
  val fstat : file -> stats t
  val rename : src:string -> dst:string -> unit t
  val link : target:string -> link_name:string -> unit t
  val symlink :
    ?mode:symlink_mode -> src:string -> dst:string -> unit -> unit t
  val mkdtemp : string -> string t
  val utime : string -> access:float -> modif:float -> unit t
  val futime : file -> access:float -> modif:float -> unit t
  val readlink : string -> string t
  val access : string -> access_permission list -> unit t
  val chmod : string -> perm:int -> unit t
  val fchmod : file -> perm:int -> unit t
  val chown : string -> uid:int -> gid:int -> unit t
  val fchown : file -> uid:int -> gid:int -> unit t
  val scandir : string -> string array t
end


let scandirh a = Array.map (fun (_,b) -> b ) a

module Fs = struct
  include Uwt.Fs

  let scandir a = scandir a >|= scandirh
end

module Fs_sync = struct
  include Uv_fs_sync

  let scandir a =
    match scandir a with
    | (Error _ ) as x -> x
    | Ok x -> Ok (scandirh x)
end

module Stream = Uwt.Stream

let to_exnu name (n: Int_result.unit) =
  if Int_result.is_error n then
    Int_result.raise_exn ~name n
  else
    ()

let to_exn n = function
| Ok x -> x
| Error x -> raise (Unix.Unix_error(to_unix_error x,n,""))

module Pipe = struct
  include Uwt.Pipe
  let init () = Uwt.Pipe.init ()
  let with_pipe f = Uwt.Pipe.with_pipe ~ipc:false f
  let openpipe fd = Uwt.Pipe.openpipe ~ipc:false fd
  let openpipe_exn fd = Uwt.Pipe.openpipe_exn ~ipc:false fd
  let with_open fd f = Uwt.Pipe.with_open ~ipc:false fd f

  let listen t ~max ~cb =
    let cb server status =
      if Int_result.is_error status then
        cb (Error (Int_result.to_error status))
      else
        cb (Uwt.Pipe.accept server)
    in
    listen t ~max ~cb

  let listen_exn t ~max ~cb = listen t ~max ~cb |> to_exnu "listen"
end


module Tcp = struct
  include Uwt.Tcp

  let bind t inet port =
    bind t ~addr:(Unix.ADDR_INET(inet,port)) ()

  let bind_exn t inet port = bind t inet port |> to_exnu "bind"

  let connect t inet port =
    connect t ~addr:(Unix.ADDR_INET(inet,port))

  let with_connect inet port f =
    with_connect ~addr:(Unix.ADDR_INET(inet,port)) f

  let listen t ~max ~cb =
    let cb server status =
      if Int_result.is_error status then
        cb (Error (Int_result.to_error status))
      else
        cb (Uwt.Tcp.accept server)
    in
    listen t ~max ~cb

  let listen_exn t ~max ~cb =
    listen t ~max ~cb |> to_exnu "listen"


  let tr = function
  | (Error _) as x ->x
  | Ok (Unix.ADDR_UNIX _) -> Ok (Unix.inet_addr_loopback,0)
  | Ok (Unix.ADDR_INET(a,b)) -> Ok (a,b)

  let getpeername t = getpeername t |> tr
  let getpeername_exn t = getpeername t |> to_exn "getpeername"
  let getsockname t = getsockname t |> tr
  let getsockname_exn t = getsockname t |> to_exn "getsockname"
end

module Subproc = struct
  include Uwt.Process

  type io =
    | Inherit_file of file
    | Create_pipe of Pipe.t
    | Inherit_pipe of Pipe.t
    | Inherit_stream of Stream.t

  let tr = function
  | None -> None
  | Some (x:io) ->
    match x with
    | Inherit_file x -> Some (Uwt.Process.Inherit_file x)
    | Create_pipe t -> Some (Uwt.Process.Create_pipe t)
    | Inherit_pipe t -> Some (Uwt.Process.Inherit_pipe t)
    | Inherit_stream t -> Some (Uwt.Process.Inherit_stream t)

  let spawn ?stdin ?stdout ?stderr ?uid ?gid ?verbatim_arguments
      ?detach ?env ?cwd ?exit_cb exe args =
    Uwt.Process.spawn ?stdin:(tr stdin) ?stdout:(tr stdout) ?stderr:(tr stderr)
      ?uid ?gid ?verbatim_arguments ?detach ?env ?cwd ?exit_cb exe args

  let spawn_exn ?stdin ?stdout ?stderr ?uid ?gid ?verbatim_arguments
      ?detach ?env ?cwd ?exit_cb exe args =
    Uwt.Process.spawn_exn ?stdin:(tr stdin) ?stdout:(tr stdout) ?stderr:(tr stderr)
      ?uid ?gid ?verbatim_arguments ?detach ?env ?cwd ?exit_cb exe args
end

module Dns = struct
  type family =
    | UNSPEC
    | INET
    | INET6

  let getaddrinfo ?(family=UNSPEC) host =
    let service = "" in
    let options = match family with
    | UNSPEC -> []
    | INET -> [Unix.AI_FAMILY Unix.PF_INET]
    | INET6 -> [Unix.AI_FAMILY Unix.PF_INET6] in
    Uwt.Dns.getaddrinfo ~host ~service options >>= function
    | (Error _) as x -> Lwt.return x
    | Ok l ->
      let f ac el =
        match el.Unix.ai_addr with
        | Unix.ADDR_INET(i,_) -> i :: ac
        | Unix.ADDR_UNIX _ -> ac
      in
      let l' = List.fold_left f [] l |> List.rev in
      Lwt.return (Ok l')

  let getnameinfo i p =
    Uwt.Dns.getnameinfo (Unix.ADDR_INET(i,p)) [Unix.NI_NAMEREQD]
end

module Signal = Uwt.Signal

module Fs_event = struct
  type t = Uwt.Fs_event.t

  type event =
    | Rename
    | Change

  type cb = t -> (string * event) uv_result -> unit

  let start ?(recursive=false) ?(persistent=true) fln ~(cb:cb) : t uv_result =
    let cb t = function
    | (Error _) as x ->
      Uwt.Fs_event.close_noerr t;
      cb t x
    | Ok (fln,l) ->
      let event =
        if List.mem Uwt.Fs_event.Rename l then
          Rename
        else
          Change
      in
      cb t (Ok (fln,event)) in
    let flags = if recursive then [Uwt.Fs_event.Recursive] else [] in
    match Uwt.Fs_event.start fln flags ~cb with
    | (Error _ ) as x -> x
    | (Ok t) as x ->
      if persistent = false then
        Uwt.Fs_event.unref t;
      x

  let start_exn ?recursive ?persistent fln ~cb =
    start ?recursive ?persistent ~cb fln |> to_exn "uv_fs_event_start"

  let close = Uwt.Fs_event.close_noerr
end

module Fs_poll = struct
  type t = Uwt.Fs_poll.t
  type report = Uwt.Fs_poll.report = {
    prev : Fs_types.stats;
    curr : Fs_types.stats;
  }

  type cb = t -> report uv_result -> unit

  let start ?(persistent=true) fln time ~cb =
    if time < 1_000 then  Error EINVAL else
    match Uwt.Fs_poll.start fln time ~cb with
    | (Error _) as x -> x
    | (Ok t) as x ->
      if persistent = false then
        Uwt.Fs_poll.unref t;
      x

  let start_exn ?persistent fln time ~cb =
  start ?persistent fln time ~cb |> to_exn "uv_fs_poll_start"

  let close = Uwt.Fs_poll.close_noerr
end

module Main = Uwt.Main
module Filename = Filename

module Io = struct
  include Uwt_io
  let make ?buffer ?close ~mode x = make ?buffer ?seek:None ?close ~mode x
end

module Process = Uwt_process
