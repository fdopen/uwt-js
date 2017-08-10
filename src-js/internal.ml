module JU = Js.Unsafe

external (~$) : string -> Js.js_string Js.t = "caml_js_from_string"

module Jg = struct
  let gl = JU.global

  let process = Cbytes.process
  let constants = process##binding ~$"constants"
  let uv = process##binding ~$"uv"
  let obj = gl##._Object
  let map = gl##._Map
  external equals : 'a -> 'b -> bool = "caml_js_equals"

  let require (s: Js.js_string Js.t) =
    JU.fun_call (JU.js_expr "require")  [|JU.inject s|]

  let os = require ~$"os"
  let fs = process##binding ~$"fs"
  let fs_const = constants##.fs

  let node_version_major = JU.global##parseInt process##.versions##.node 10
end

module Itypes = struct

  type error =
    | E2BIG
    | EACCES
    | EADDRINUSE
    | EADDRNOTAVAIL
    | EAFNOSUPPORT
    | EAGAIN
    | EAI_ADDRFAMILY
    | EAI_AGAIN
    | EAI_BADFLAGS
    | EAI_BADHINTS
    | EAI_CANCELED
    | EAI_FAIL
    | EAI_FAMILY
    | EAI_MEMORY
    | EAI_NODATA
    | EAI_NONAME
    | EAI_OVERFLOW
    | EAI_PROTOCOL
    | EAI_SERVICE
    | EAI_SOCKTYPE
    | EALREADY
    | EBADF
    | EBUSY
    | ECANCELED
    | ECHARSET
    | ECONNABORTED
    | ECONNREFUSED
    | ECONNRESET
    | EDESTADDRREQ
    | EEXIST
    | EFAULT
    | EFBIG
    | EHOSTUNREACH
    | EINTR
    | EINVAL
    | EIO
    | EISCONN
    | EISDIR
    | ELOOP
    | EMFILE
    | EMSGSIZE
    | ENAMETOOLONG
    | ENETDOWN
    | ENETUNREACH
    | ENFILE
    | ENOBUFS
    | ENODEV
    | ENOENT
    | ENOMEM
    | ENONET
    | ENOPROTOOPT
    | ENOSPC
    | ENOSYS
    | ENOTCONN
    | ENOTDIR
    | ENOTEMPTY
    | ENOTSOCK
    | ENOTSUP
    | EPERM
    | EPIPE
    | EPROTO
    | EPROTONOSUPPORT
    | EPROTOTYPE
    | ERANGE
    | EROFS
    | ESHUTDOWN
    | ESPIPE
    | ESRCH
    | ETIMEDOUT
    | ETXTBSY
    | EXDEV
    | UNKNOWN
    | EOF
    | ENXIO
    | EMLINK
    | UWT_EFATAL

  type 'a uv_result = ('a , error) result

  type ip_addr_type =
     | Ip4
     | Ip6

  type real_inet_addr =
    < l : int Js.readonly_prop;
      i : Js.js_string Js.t Js.readonly_prop; >

  type buf = Cbytes.t

  type file_descr = int
  type file = int

  module Fs_types = struct
    type file_kind =
      | S_REG
      | S_DIR
      | S_CHR
      | S_BLK
      | S_LNK
      | S_FIFO
      | S_SOCK
      | S_UNKNOWN

    type stats = {
      st_dev: int;
      st_kind: file_kind;
      st_perm: int;
      st_nlink: int;
      st_uid: int;
      st_gid: int;
      st_rdev: int;
      st_ino: int;
      st_size: int64;
      st_blksize: int;
      st_blocks: int;
      st_flags: int;
      st_gen: int;
      st_atime: int64;
      st_atime_nsec: int;
      st_mtime: int64;
      st_mtime_nsec: int;
      st_ctime: int64;
      st_ctime_nsec: int;
      st_birthtime: int64;
      st_birthtime_nsec: int;
    }

    type uv_open_flag =
      | O_RDONLY
      | O_WRONLY
      | O_RDWR
      | O_NONBLOCK
      | O_CREAT
      | O_EXCL
      | O_TRUNC
      | O_APPEND
      | O_NOCTTY
      | O_DSYNC
      | O_SYNC
      | O_RSYNC
      | O_TEMPORARY
      | O_SHORT_LIVED
      | O_SEQUENTIAL
      | O_RANDOM

    type symlink_mode =
      | S_Default
      | S_Dir
      | S_Junction

    type access_permission =
      | Read
      | Write
      | Exec
      | Exists
  end

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

  type read_b =
    | Rbytes of Bytes.t
    | Rba of buf

  type wbuf_t =
    | Wstring of string
    | Wbytes of bytes
    | Wba of buf
end

open Itypes

let stdin  : file_descr = Jg.process##.stdin##.fd
let stdout : file_descr = Jg.process##.stdout##.fd
let stderr : file_descr = Jg.process##.stderr##.fd

module Int_result = struct
  type 'a t = int

  type nonrec int = int t
  type nonrec unit = unit t

  let error_table = Jg.obj##create Js.null
  let () =
    JU.set error_table ~$"E2BIG" E2BIG;
    JU.set error_table ~$"EACCES" EACCES;
    JU.set error_table ~$"EADDRINUSE" EADDRINUSE;
    JU.set error_table ~$"EADDRNOTAVAIL" EADDRNOTAVAIL;
    JU.set error_table ~$"EAFNOSUPPORT" EAFNOSUPPORT;
    JU.set error_table ~$"EAGAIN" EAGAIN;
    JU.set error_table ~$"EAI_ADDRFAMILY" EAI_ADDRFAMILY;
    JU.set error_table ~$"EAI_AGAIN" EAI_AGAIN;
    JU.set error_table ~$"EAI_BADFLAGS" EAI_BADFLAGS;
    JU.set error_table ~$"EAI_BADHINTS" EAI_BADHINTS;
    JU.set error_table ~$"EAI_CANCELED" EAI_CANCELED;
    JU.set error_table ~$"EAI_FAIL" EAI_FAIL;
    JU.set error_table ~$"EAI_FAMILY" EAI_FAMILY;
    JU.set error_table ~$"EAI_MEMORY" EAI_MEMORY;
    JU.set error_table ~$"EAI_NODATA" EAI_NODATA;
    JU.set error_table ~$"EAI_NONAME" EAI_NONAME;
    JU.set error_table ~$"EAI_OVERFLOW" EAI_OVERFLOW;
    JU.set error_table ~$"EAI_PROTOCOL" EAI_PROTOCOL;
    JU.set error_table ~$"EAI_SERVICE" EAI_SERVICE;
    JU.set error_table ~$"EAI_SOCKTYPE" EAI_SOCKTYPE;
    JU.set error_table ~$"EALREADY" EALREADY;
    JU.set error_table ~$"EBADF" EBADF;
    JU.set error_table ~$"EBUSY" EBUSY;
    JU.set error_table ~$"ECANCELED" ECANCELED;
    JU.set error_table ~$"ECHARSET" ECHARSET;
    JU.set error_table ~$"ECONNABORTED" ECONNABORTED;
    JU.set error_table ~$"ECONNREFUSED" ECONNREFUSED;
    JU.set error_table ~$"ECONNRESET" ECONNRESET;
    JU.set error_table ~$"EDESTADDRREQ" EDESTADDRREQ;
    JU.set error_table ~$"EEXIST" EEXIST;
    JU.set error_table ~$"EFAULT" EFAULT;
    JU.set error_table ~$"EFBIG" EFBIG;
    JU.set error_table ~$"EHOSTUNREACH" EHOSTUNREACH;
    JU.set error_table ~$"EINTR" EINTR;
    JU.set error_table ~$"EINVAL" EINVAL;
    JU.set error_table ~$"EIO" EIO;
    JU.set error_table ~$"EISCONN" EISCONN;
    JU.set error_table ~$"EISDIR" EISDIR;
    JU.set error_table ~$"ELOOP" ELOOP;
    JU.set error_table ~$"EMFILE" EMFILE;
    JU.set error_table ~$"EMSGSIZE" EMSGSIZE;
    JU.set error_table ~$"ENAMETOOLONG" ENAMETOOLONG;
    JU.set error_table ~$"ENETDOWN" ENETDOWN;
    JU.set error_table ~$"ENETUNREACH" ENETUNREACH;
    JU.set error_table ~$"ENFILE" ENFILE;
    JU.set error_table ~$"ENOBUFS" ENOBUFS;
    JU.set error_table ~$"ENODEV" ENODEV;
    JU.set error_table ~$"ENOENT" ENOENT;
    JU.set error_table ~$"ENOMEM" ENOMEM;
    JU.set error_table ~$"ENONET" ENONET;
    JU.set error_table ~$"ENOPROTOOPT" ENOPROTOOPT;
    JU.set error_table ~$"ENOSPC" ENOSPC;
    JU.set error_table ~$"ENOSYS" ENOSYS;
    JU.set error_table ~$"ENOTCONN" ENOTCONN;
    JU.set error_table ~$"ENOTDIR" ENOTDIR;
    JU.set error_table ~$"ENOTEMPTY" ENOTEMPTY;
    JU.set error_table ~$"ENOTSOCK" ENOTSOCK;
    JU.set error_table ~$"ENOTSUP" ENOTSUP;
    JU.set error_table ~$"EPERM" EPERM;
    JU.set error_table ~$"EPIPE" EPIPE;
    JU.set error_table ~$"EPROTO" EPROTO;
    JU.set error_table ~$"EPROTONOSUPPORT" EPROTONOSUPPORT;
    JU.set error_table ~$"EPROTOTYPE" EPROTOTYPE;
    JU.set error_table ~$"ERANGE" ERANGE;
    JU.set error_table ~$"EROFS" EROFS;
    JU.set error_table ~$"ESHUTDOWN" ESHUTDOWN;
    JU.set error_table ~$"ESPIPE" ESPIPE;
    JU.set error_table ~$"ESRCH" ESRCH;
    JU.set error_table ~$"ETIMEDOUT" ETIMEDOUT;
    JU.set error_table ~$"ETXTBSY" ETXTBSY;
    JU.set error_table ~$"EXDEV" EXDEV;
    JU.set error_table ~$"EOF" EOF;
    JU.set error_table ~$"ENXIO" ENXIO;
    JU.set error_table ~$"EMLINK" EMLINK

  let error_of_jsstring (x: Js.js_string Js.t) : Itypes.error =
    if x == ~$"" then UNKNOWN else
    let x = JU.get error_table x in
    if x == Js.undefined then UNKNOWN
    else Obj.magic x

  let hh =
    let i = ref (-32_768) in
    fun () ->
      let x = !i in
      decr i;
      x

  let map_nint_to_error : 'a Js.t = Jg.obj##create Js.null
  let map_nint_to_uerror : 'a Js.t = Jg.obj##create Js.null
  let map_error_to_nint : 'a Js.t = Jg.obj##create Js.null

  let h (s:Js.js_string Js.t) (e:Itypes.error) (u:Unix.error) =
    let x : int Js.Optdef.t = JU.get Jg.uv s in
    let i : int = Js.Optdef.get x hh in
    let ni = i * (-1) in
    JU.set map_nint_to_error ni e;
    JU.set map_nint_to_uerror ni u;
    JU.set map_error_to_nint e i;
    i

  let h2 (s:Js.js_string Js.t) (e:Itypes.error) =
    let x : int Js.Optdef.t = JU.get Jg.uv s in
    let i : int = Js.Optdef.get x hh in
    JU.set map_nint_to_error (i*(-1)) e;
    JU.set map_error_to_nint e i;
    i

  let e2big = h ~$"UV_E2BIG" E2BIG Unix.E2BIG
  let eacces = h ~$"UV_EACCES" EACCES Unix.EACCES
  let eaddrinuse = h ~$"UV_EADDRINUSE" EADDRINUSE Unix.EADDRINUSE
  let eaddrnotavail = h ~$"UV_EADDRNOTAVAIL" EADDRNOTAVAIL Unix.EADDRNOTAVAIL
  let eafnosupport = h ~$"UV_EAFNOSUPPORT" EAFNOSUPPORT Unix.EAFNOSUPPORT
  let eagain = h ~$"UV_EAGAIN" EAGAIN Unix.EAGAIN
  let eai_addrfamily = h2 ~$"UV_EAI_ADDRFAMILY" EAI_ADDRFAMILY
  let eai_again = h2 ~$"UV_EAI_AGAIN" EAI_AGAIN
  let eai_badflags = h2 ~$"UV_EAI_BADFLAGS" EAI_BADFLAGS
  let eai_badhints = h2 ~$"UV_EAI_BADHINTS" EAI_BADHINTS
  let eai_canceled = h2 ~$"UV_EAI_CANCELED" EAI_CANCELED
  let eai_fail = h2 ~$"UV_EAI_FAIL" EAI_FAIL
  let eai_family = h2 ~$"UV_EAI_FAMILY" EAI_FAMILY
  let eai_memory = h2 ~$"UV_EAI_MEMORY" EAI_MEMORY
  let eai_nodata = h2 ~$"UV_EAI_NODATA" EAI_NODATA
  let eai_noname = h2 ~$"UV_EAI_NONAME" EAI_NONAME
  let eai_overflow = h2 ~$"UV_EAI_OVERFLOW" EAI_OVERFLOW
  let eai_protocol = h2 ~$"UV_EAI_PROTOCOL" EAI_PROTOCOL
  let eai_service = h2 ~$"UV_EAI_SERVICE" EAI_SERVICE
  let eai_socktype = h2 ~$"UV_EAI_SOCKTYPE" EAI_SOCKTYPE
  let ealready = h ~$"UV_EALREADY" EALREADY Unix.EALREADY
  let ebadf = h ~$"UV_EBADF" EBADF Unix.EBADF
  let ebusy = h ~$"UV_EBUSY" EBUSY Unix.EBUSY
  let ecanceled = h2 ~$"UV_ECANCELED" ECANCELED
  let echarset = h2 ~$"UV_ECHARSET" ECHARSET
  let econnaborted = h ~$"UV_ECONNABORTED" ECONNABORTED Unix.ECONNABORTED
  let econnrefused = h ~$"UV_ECONNREFUSED" ECONNREFUSED Unix.ECONNREFUSED
  let econnreset = h ~$"UV_ECONNRESET" ECONNRESET Unix.ECONNRESET
  let edestaddrreq = h ~$"UV_EDESTADDRREQ" EDESTADDRREQ Unix.EDESTADDRREQ
  let eexist = h ~$"UV_EEXIST" EEXIST Unix.EEXIST
  let efault = h ~$"UV_EFAULT" EFAULT Unix.EFAULT
  let efbig = h ~$"UV_EFBIG" EFBIG Unix.EFBIG
  let ehostunreach = h ~$"UV_EHOSTUNREACH" EHOSTUNREACH Unix.EHOSTUNREACH
  let eintr = h ~$"UV_EINTR" EINTR Unix.EINTR
  let einval = h ~$"UV_EINVAL" EINVAL Unix.EINVAL
  let eio = h ~$"UV_EIO" EIO Unix.EIO
  let eisconn = h ~$"UV_EISCONN" EISCONN Unix.EISCONN
  let eisdir = h ~$"UV_EISDIR" EISDIR Unix.EISDIR
  let eloop = h ~$"UV_ELOOP" ELOOP Unix.ELOOP
  let emfile = h ~$"UV_EMFILE" EMFILE Unix.EMFILE
  let emsgsize = h ~$"UV_EMSGSIZE" EMSGSIZE Unix.EMSGSIZE
  let enametoolong = h ~$"UV_ENAMETOOLONG" ENAMETOOLONG Unix.ENAMETOOLONG
  let enetdown = h ~$"UV_ENETDOWN" ENETDOWN Unix.ENETDOWN
  let enetunreach = h ~$"UV_ENETUNREACH" ENETUNREACH Unix.ENETUNREACH
  let enfile = h ~$"UV_ENFILE" ENFILE Unix.ENFILE
  let enobufs = h ~$"UV_ENOBUFS" ENOBUFS Unix.ENOBUFS
  let enodev = h ~$"UV_ENODEV" ENODEV Unix.ENODEV
  let enoent = h ~$"UV_ENOENT" ENOENT Unix.ENOENT
  let enomem = h ~$"UV_ENOMEM" ENOMEM Unix.ENOMEM
  let enonet = h2 ~$"UV_ENONET" ENONET
  let enoprotoopt = h ~$"UV_ENOPROTOOPT" ENOPROTOOPT Unix.ENOPROTOOPT
  let enospc = h ~$"UV_ENOSPC" ENOSPC Unix.ENOSPC
  let enosys = h ~$"UV_ENOSYS" ENOSYS Unix.ENOSYS
  let enotconn = h ~$"UV_ENOTCONN" ENOTCONN Unix.ENOTCONN
  let enotdir = h ~$"UV_ENOTDIR" ENOTDIR Unix.ENOTDIR
  let enotempty = h ~$"UV_ENOTEMPTY" ENOTEMPTY Unix.ENOTEMPTY
  let enotsock = h ~$"UV_ENOTSOCK" ENOTSOCK Unix.ENOTSOCK
  let enotsup = h ~$"UV_ENOTSUP" ENOTSUP Unix.EOPNOTSUPP
  let eperm = h ~$"UV_EPERM" EPERM Unix.EPERM
  let epipe = h ~$"UV_EPIPE" EPIPE Unix.EPIPE
  let eproto = h2 ~$"UV_EPROTO" EPROTO
  let eprotonosupport = h ~$"UV_EPROTONOSUPPORT" EPROTONOSUPPORT Unix.EPROTONOSUPPORT
  let eprototype = h ~$"UV_EPROTOTYPE" EPROTOTYPE Unix.EPROTOTYPE
  let erange = h ~$"UV_ERANGE" ERANGE Unix.ERANGE
  let erofs = h ~$"UV_EROFS" EROFS Unix.EROFS
  let eshutdown = h ~$"UV_ESHUTDOWN" ESHUTDOWN Unix.ESHUTDOWN
  let espipe = h ~$"UV_ESPIPE" ESPIPE Unix.ESPIPE
  let esrch = h ~$"UV_ESRCH" ESRCH Unix.ESRCH
  let etimedout = h ~$"UV_ETIMEDOUT" ETIMEDOUT Unix.ETIMEDOUT
  let etxtbsy = h2 ~$"UV_ETXTBSY" ETXTBSY
  let exdev = h ~$"UV_EXDEV" EXDEV Unix.EXDEV
  let unknown = h2 ~$"UV_UNKNOWN" UNKNOWN
  let eof = h2 ~$"UV_EOF" EOF
  let enxio = h ~$"UV_ENXIO" ENXIO Unix.ENXIO
  let emlink = h ~$"UV_EMLINK" EMLINK Unix.EMLINK
  let uwt_efatal = hh ()

  let error_to_unix_error = function
  | E2BIG -> Unix.E2BIG
  | EACCES -> Unix.EACCES
  | EADDRINUSE -> Unix.EADDRINUSE
  | EADDRNOTAVAIL -> Unix.EADDRNOTAVAIL
  | EAFNOSUPPORT -> Unix.EAFNOSUPPORT
  | EAGAIN -> Unix.EAGAIN
  | EALREADY -> Unix.EALREADY
  | EBADF -> Unix.EBADF
  | EBUSY -> Unix.EBUSY
  | ECONNABORTED -> Unix.ECONNABORTED
  | ECONNREFUSED -> Unix.ECONNREFUSED
  | ECONNRESET -> Unix.ECONNRESET
  | EDESTADDRREQ -> Unix.EDESTADDRREQ
  | EEXIST -> Unix.EEXIST
  | EFAULT -> Unix.EFAULT
  | EFBIG -> Unix.EFBIG
  | EHOSTUNREACH -> Unix.EHOSTUNREACH
  | EINTR -> Unix.EINTR
  | EINVAL -> Unix.EINVAL
  | EIO -> Unix.EIO
  | EISCONN -> Unix.EISCONN
  | EISDIR -> Unix.EISDIR
  | ELOOP -> Unix.ELOOP
  | EMFILE -> Unix.EMFILE
  | EMLINK -> Unix.EMLINK
  | EMSGSIZE -> Unix.EMSGSIZE
  | ENAMETOOLONG -> Unix.ENAMETOOLONG
  | ENETDOWN -> Unix.ENETDOWN
  | ENETUNREACH -> Unix.ENETUNREACH
  | ENFILE -> Unix.ENFILE
  | ENOBUFS -> Unix.ENOBUFS
  | ENODEV -> Unix.ENODEV
  | ENOENT -> Unix.ENOENT
  | ENOMEM -> Unix.ENOMEM
  | ENOPROTOOPT -> Unix.ENOPROTOOPT
  | ENOSPC -> Unix.ENOSPC
  | ENOSYS -> Unix.ENOSYS
  | ENOTCONN -> Unix.ENOTCONN
  | ENOTDIR -> Unix.ENOTDIR
  | ENOTEMPTY -> Unix.ENOTEMPTY
  | ENOTSOCK -> Unix.ENOTSOCK
  | ENXIO -> Unix.ENXIO
  | EPERM -> Unix.EPERM
  | EPIPE -> Unix.EPIPE
  | EPROTONOSUPPORT -> Unix.EPROTONOSUPPORT
  | EPROTOTYPE -> Unix.EPROTOTYPE
  | ERANGE -> Unix.ERANGE
  | EROFS -> Unix.EROFS
  | ESHUTDOWN -> Unix.ESHUTDOWN
  | ESPIPE -> Unix.ESPIPE
  | ESRCH -> Unix.ESRCH
  | ETIMEDOUT -> Unix.ETIMEDOUT
  | EXDEV -> Unix.EXDEV
  | ENOTSUP -> Unix.EOPNOTSUPP
  | UWT_EFATAL -> Unix.EUNKNOWNERR uwt_efatal
  | EAI_ADDRFAMILY -> Unix.EUNKNOWNERR eai_addrfamily
  | EAI_AGAIN -> Unix.EUNKNOWNERR eai_again
  | EAI_BADFLAGS -> Unix.EUNKNOWNERR eai_badflags
  | EAI_BADHINTS -> Unix.EUNKNOWNERR eai_badhints
  | EAI_CANCELED -> Unix.EUNKNOWNERR eai_canceled
  | EAI_FAIL -> Unix.EUNKNOWNERR eai_fail
  | EAI_FAMILY -> Unix.EUNKNOWNERR eai_family
  | EAI_MEMORY -> Unix.EUNKNOWNERR eai_memory
  | EAI_NODATA -> Unix.EUNKNOWNERR eai_nodata
  | EAI_NONAME -> Unix.EUNKNOWNERR eai_noname
  | EAI_OVERFLOW -> Unix.EUNKNOWNERR eai_overflow
  | EAI_PROTOCOL -> Unix.EUNKNOWNERR eai_protocol
  | EAI_SERVICE -> Unix.EUNKNOWNERR eai_service
  | EAI_SOCKTYPE -> Unix.EUNKNOWNERR eai_socktype
  | ECANCELED -> Unix.EUNKNOWNERR ecanceled
  | ECHARSET -> Unix.EUNKNOWNERR echarset
  | ENONET -> Unix.EUNKNOWNERR enonet
  | EPROTO -> Unix.EUNKNOWNERR eproto
  | ETXTBSY -> Unix.EUNKNOWNERR etxtbsy
  | UNKNOWN -> Unix.EUNKNOWNERR unknown
  | EOF -> Unix.EUNKNOWNERR eof

  let of_error (e:Itypes.error) =
    let i = JU.get map_error_to_nint e in
    if i == Js.undefined then uwt_efatal
    else Obj.magic i

  let to_error (i:int) =
    if i >= 0 then UNKNOWN else
    let e = JU.get map_nint_to_error (i * (-1)) in
    if e == Js.undefined then
      if i = uwt_efatal then
        UWT_EFATAL
      else
        UNKNOWN
    else Obj.magic e

  let to_unix_error (i:int) =
    if i >= 0 then Unix.EUNKNOWNERR i else
    let e = JU.get map_nint_to_uerror (i * (-1)) in
    if e == Js.undefined then Unix.EUNKNOWNERR i
    else Obj.magic e

  let unix_error_to_error = function
  | Unix.E2BIG -> E2BIG
  | Unix.EACCES -> EACCES
  | Unix.EADDRINUSE -> EADDRINUSE
  | Unix.EADDRNOTAVAIL -> EADDRNOTAVAIL
  | Unix.EAFNOSUPPORT -> EAFNOSUPPORT
  | Unix.EAGAIN -> EAGAIN
  | Unix.EALREADY -> EALREADY
  | Unix.EBADF -> EBADF
  | Unix.EBUSY -> EBUSY
  | Unix.ECONNABORTED -> ECONNABORTED
  | Unix.ECONNREFUSED -> ECONNREFUSED
  | Unix.ECONNRESET -> ECONNRESET
  | Unix.EDESTADDRREQ -> EDESTADDRREQ
  | Unix.EEXIST -> EEXIST
  | Unix.EFAULT -> EFAULT
  | Unix.EFBIG -> EFBIG
  | Unix.EHOSTUNREACH -> EHOSTUNREACH
  | Unix.EINTR -> EINTR
  | Unix.EINVAL -> EINVAL
  | Unix.EIO -> EIO
  | Unix.EISCONN -> EISCONN
  | Unix.EISDIR -> EISDIR
  | Unix.ELOOP -> ELOOP
  | Unix.EMFILE -> EMFILE
  | Unix.EMLINK -> EMLINK
  | Unix.EMSGSIZE -> EMSGSIZE
  | Unix.ENAMETOOLONG -> ENAMETOOLONG
  | Unix.ENETDOWN -> ENETDOWN
  | Unix.ENETUNREACH -> ENETUNREACH
  | Unix.ENFILE -> ENFILE
  | Unix.ENOBUFS -> ENOBUFS
  | Unix.ENODEV -> ENODEV
  | Unix.ENOENT -> ENOENT
  | Unix.ENOMEM -> ENOMEM
  | Unix.ENOPROTOOPT -> ENOPROTOOPT
  | Unix.ENOSPC -> ENOSPC
  | Unix.ENOSYS -> ENOSYS
  | Unix.ENOTCONN -> ENOTCONN
  | Unix.ENOTDIR -> ENOTDIR
  | Unix.ENOTEMPTY -> ENOTEMPTY
  | Unix.ENOTSOCK -> ENOTSOCK
  | Unix.ENXIO -> ENXIO
  | Unix.EPERM -> EPERM
  | Unix.EPIPE -> EPIPE
  | Unix.EPROTONOSUPPORT -> EPROTONOSUPPORT
  | Unix.EPROTOTYPE -> EPROTOTYPE
  | Unix.ERANGE -> ERANGE
  | Unix.EROFS -> EROFS
  | Unix.ESHUTDOWN -> ESHUTDOWN
  | Unix.ESPIPE -> ESPIPE
  | Unix.ESRCH -> ESRCH
  | Unix.ETIMEDOUT -> ETIMEDOUT
  | Unix.EXDEV -> EXDEV
  | Unix.EOPNOTSUPP -> ENOTSUP
  | Unix.ECHILD
  | Unix.EDEADLK
  | Unix.EDOM
  | Unix.ENOEXEC
  | Unix.ENOLCK
  | Unix.ENOTTY
  | Unix.EWOULDBLOCK
  | Unix.EINPROGRESS
  | Unix.ESOCKTNOSUPPORT
  | Unix.EPFNOSUPPORT
  | Unix.ENETRESET
  | Unix.ETOOMANYREFS
  | Unix.EHOSTDOWN
  | Unix.EOVERFLOW -> UNKNOWN
  | Unix.EUNKNOWNERR i -> to_error i

  let err_name n =
    if n = uwt_efatal then "UWT_EFATAL"
    else if n >= 0 then "SUCCESS"
    else Jg.uv##errname n |> Js.to_string

  let to_exn ?(name="") ?(param="") (x: int) =
    if x >= 0 then Invalid_argument "Uwt.Int_result.to_exn"
    else Unix.Unix_error(to_unix_error x,name,param)

  let raise_exn ?name ?param x = raise (to_exn ?name ?param x)

  let fail ?name ?param (x: int) = Lwt.fail (to_exn ?name ?param x)

  let wakeup_exn ?name ?param w (x: int) =
    let x = if x >= 0 then uwt_efatal else x in
    Lwt.wakeup_exn w (to_exn ?name ?param x)

  let is_error x = x < 0
  let is_ok x = x >= 0

  let node_js_error_to_error (h_orig:Js.error Js.t) : error =
    let h = JU.coerce h_orig in
    let e =
      if Js.typeof h##.code == ~$"string" then
        error_of_jsstring h##.code
      else
        UNKNOWN in
    if e <> UNKNOWN then e else
    let e =
      if Js.typeof h##.errno == ~$"string" then
        error_of_jsstring h##.errno
      else
        UNKNOWN in
    if e <> UNKNOWN then e
    else if Js.typeof h##.errno == ~$"number" then to_error h##.errno
    else UNKNOWN

  let node_exn_to_unix_exn (h_orig:Js.error Js.t) : exn =
    let h = JU.coerce h_orig in
    let x = Js.typeof h##.errno in
    if (x != ~$"string" && x != ~$"number") ||
       Js.typeof h##.code != ~$"string" then
      Js.Error h_orig
    else
    let code = node_js_error_to_error h_orig |> error_to_unix_error
    and syscall =
      if Js.typeof h##.syscall == ~$"string" then
        Js.to_string h##.syscall
      else
        ""
    and path =
      if Js.typeof h##.path == ~$"string" && h##.path != ~$"" then
        Js.to_string h##.path
      else if Js.typeof h##.address == ~$"string" && h##.address != ~$"" then
        Js.to_string h##.address
      else
        "" in
    Unix.Unix_error(code,syscall,path)

  let ufail ?(name="") ?(param="") c = Lwt.fail (Unix.Unix_error(c,name,param))

  let to_exni name i =
    if i < 0 then raise_exn ~name i
    else i

  let to_exnu name i =
    if i < 0 then raise_exn ~name i
    else ()

  let cnull (s: Js.js_string Js.t) = s##indexOf ~$"\000" <> -1 || s##.length = 0

  let enull ~param ~name =
    if param = "" then Lwt.fail (Unix.Unix_error(Unix.EINVAL,name,param))
    else Lwt.fail (Unix.Unix_error(Unix.EUNKNOWNERR echarset,name,param))
end

let win32 = Jg.process##.platform == ~$"win32"

let to_exn n = function
| Ok x -> x
| Error x -> raise (Unix.Unix_error(Int_result.error_to_unix_error x,n,""))

let node_exn_to_error = function
| Js.Error er -> Int_result.node_js_error_to_error er
| _ -> UNKNOWN

external of_inet_addr : Unix.inet_addr -> real_inet_addr Js.t = "%identity"
let create_inet_addr x (i:Js.js_string Js.t) : Unix.inet_addr =
  let l = match x with Ip4 -> 4 | Ip6 -> 16 in
  JU.obj [| ("l", JU.inject l) ; ("i", JU.inject i) |]

(* - Linux/BSDs: We must pass buffers, not strings to functions, that deal
   with the filesystem. Otherwise we can't deal with filenames
   that contain invalid unicode sequences.

   - Windows stores files as uint16 arrays, but nodejs/libuv
   convert them from utf8 to utf16 - therefore, we can't deal
   with all files.

   - OS X: not sure what to do, it depends on the file system.

   Functions outside the Fs hierarchy don't support buffers yet.
*)

type nodefln
let nodefln_encoding = if win32 then ~$"utf8" else ~$"buffer"

type string_res =
  | Empty_string
  | String_with_null
  | Nodefln of nodefln

let string_to_nodefln_unsafe =
  if win32 then fun s : nodefln -> Obj.magic (Js.string s)
  else fun s -> Obj.magic (Cbytes.of_string s)

let string_to_nodefln =
  if win32 then
    fun s ->
      let s = Js.string s in
      if s##.length = 0 then
        Empty_string
      else if s##indexOf ~$"\000" <> -1 then
        String_with_null
      else
        Nodefln (Obj.magic s)
  else
  fun s_orig ->
    let s = Cbytes.of_string s_orig in
    if Cbytes.length s = 0 then
      Empty_string
    else if Cbytes.(unsafe_memchr s ~pos:0 ~len:(length s) '\000' <> -1) then
      String_with_null
    else
      Nodefln (Obj.magic s)

let nodefln_to_string =
  if win32 then fun (s:nodefln) : string -> Js.to_string (Obj.magic s)
  else fun s -> Cbytes.to_string (Obj.magic s)

let list_map_to_js_array l f =
  let len = List.length l in
  if len = 0 then new%js Js.array_empty else
  let ar = new%js Js.array_length len in
  List.iteri (fun i s -> Js.Unsafe.set ar i (f s)) l;
  ar

let nodefln_array_to_string_array jar =
  let length = jar##.length in
  if length = 0 then [| |] else
  let first = JU.get jar 0 |> nodefln_to_string in
  let ar = Array.make length first in
  for i = 1 to length - 1 do
    let cur = JU.get jar i |> nodefln_to_string in
    Array.unsafe_set ar i cur
  done;
  ar

let lwt_return_zero = Lwt.return 0

module Stat = struct
  open Fs_types

  let get cur =
    let off = if cur then 0 else 14 in
    let a = Jg.fs##getStatValues in
    let atime_ms = JU.get a (off + 10)
    and mtime_ms = JU.get a (off + 11)
    and ctime_ms = JU.get a (off + 12)
    and birthime_ms = JU.get a (off + 13)
    and mode = JU.get a (off + 1) in
    let fk = JU.get Jg.fs_const "S_IFMT" land mode in
    let fk =
      if fk =  JU.get Jg.fs_const "S_IFREG" then
        S_REG
      else if fk = JU.get Jg.fs_const "S_IFDIR" then
        S_DIR
      else if fk = JU.get Jg.fs_const "S_IFCHR" then
        S_CHR
      else if fk = JU.get Jg.fs_const "S_IFBLK" then
        S_BLK
      else if fk = JU.get Jg.fs_const "S_IFLNK" then
        S_LNK
      else if fk = JU.get Jg.fs_const "S_IFIFO" then
        S_FIFO
      else if fk = JU.get Jg.fs_const "S_IFSOCK" then
        S_SOCK
      else
        S_UNKNOWN in
    let sec_float x = Int64.of_float (x /. 1000.) in
    (* mod_float is not %, and something like it doesn't seem to exist ....
       TODO: more precise conversion!  *)
    let nsec_float : ( float -> int ) =
      JU.js_expr {|function(x){ return (0|((x % 1000.) * 1000000.));}|} in
    {
      st_dev = JU.get a (off + 0);
      st_perm = mode land 0o7777;
      st_nlink = JU.get a (off + 2);
      st_uid = JU.get a (off + 3);
      st_gid = JU.get a (off + 4);
      st_rdev = JU.get a (off + 5);
      st_blksize = JU.get a (off + 6);
      st_ino = JU.get a (off + 7);
      st_size = JU.get a (off + 8) |> Int64.of_float;
      st_blocks = JU.get a (off + 9);

      st_atime = sec_float atime_ms;
      st_mtime = sec_float mtime_ms;
      st_ctime = sec_float ctime_ms;

      st_atime_nsec = nsec_float atime_ms;
      st_mtime_nsec = nsec_float mtime_ms;
      st_ctime_nsec = nsec_float ctime_ms;

      st_birthtime = sec_float birthime_ms;
      st_birthtime_nsec = nsec_float birthime_ms;

      st_flags = 0;
      st_gen = 0;
      st_kind = fk;
    }
end

let () =
  if Jg.node_version_major < 8 then (
    prerr_endline "Node version too low. 8.0 or later required";
    exit 2);
  (* important code, to enforce the linkage of our unix_string_of_inet_addr
     stub code. This function must be linked in, so it can replace
     unix_inet_addr_of_string *)
  let raise_convert exn = raise (Int_result.node_exn_to_unix_exn exn) in
  let f = JU.callback raise_convert in
  let _ : string = Unix.string_of_inet_addr (Obj.magic f) in
  ()
