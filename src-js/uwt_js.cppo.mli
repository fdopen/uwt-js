#ifdef NATIVE
#define XX(x) x
#else
#define XX(x)
#endif

type error = XX(Uwt.error =)
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

val err_name: error -> string
(** error name for the given error code *)

val to_unix_error: error -> Unix.error
(** map error to [Unix.error] , [Unix.EUNKNOWNERR] is
    used, if the mapping is not possible *)

val of_unix_error : Unix.error -> error
(** get back the original Uwt.error from an exception
    raised by a Uwt function *)

module Int_result : sig

#ifdef NATIVE
  type 'a t = 'a Uwt.Int_result.t
#else
  type 'a t = private int
#endif

  type nonrec int = int t
  type nonrec unit = unit t

  val e2big : int
  val eacces : int
  val eaddrinuse : int
  val eaddrnotavail : int
  val eafnosupport : int
  val eagain : int
  val eai_addrfamily : int
  val eai_again : int
  val eai_badflags : int
  val eai_badhints : int
  val eai_canceled : int
  val eai_fail : int
  val eai_family : int
  val eai_memory : int
  val eai_nodata : int
  val eai_noname : int
  val eai_overflow : int
  val eai_protocol : int
  val eai_service : int
  val eai_socktype : int
  val ealready : int
  val ebadf : int
  val ebusy : int
  val ecanceled : int
  val echarset : int
  val econnaborted : int
  val econnrefused : int
  val econnreset : int
  val edestaddrreq : int
  val eexist : int
  val efault : int
  val efbig : int
  val ehostunreach : int
  val eintr : int
  val einval : int
  val eio : int
  val eisconn : int
  val eisdir : int
  val eloop : int
  val emfile : int
  val emsgsize : int
  val enametoolong : int
  val enetdown : int
  val enetunreach : int
  val enfile : int
  val enobufs : int
  val enodev : int
  val enoent : int
  val enomem : int
  val enonet : int
  val enoprotoopt : int
  val enospc : int
  val enosys : int
  val enotconn : int
  val enotdir : int
  val enotempty : int
  val enotsock : int
  val enotsup : int
  val eperm : int
  val epipe : int
  val eproto : int
  val eprotonosupport : int
  val eprototype : int
  val erange : int
  val erofs : int
  val eshutdown : int
  val espipe : int
  val esrch : int
  val etimedout : int
  val etxtbsy : int
  val exdev : int
  val unknown : int
  val eof : int
  val enxio : int
  val emlink : int
  val uwt_efatal : int

  val to_error : 'a t -> error
  val to_exn : ?name:string -> ?param:string -> 'a t -> exn
  val is_error : 'a t -> bool
  val is_ok : 'a t -> bool
end

type file_descr XX(= Unix.file_descr)
(** file_descr is the equivalent of {!Unix.file_descr}. {!Unix.file_descr} can't
    be used, because its representation differs from platform to platform *)

type file XX(= Uwt.file)
(** Abstract type for a file descriptor.
    Unlike {!file_descr} it is not intended to wrap a SOCKET. *)

val stdin : file
val stdout : file
val stderr : file

val file_of_file_descr : file_descr -> file option

module Cbytes : sig
  (** Cbytes.t are memory buffers outside the normal OCaml or Javascript heap.
      If compiled to native or byte code, [t] is a normal bigarray.
      If compiled to javascript, type t is an alias for node's Buffer.

      Cbytes.t is not an alias for bigarrays, because the memory represantation
      of Buffer objects and Uint8Arrays changed between different node versions.
      Its kept abstract in order to be prepared for changes in the future :D

      Conversion from an to bigarrays is a noop under OCaml, but there is slight
      overhead under Javascript (but your data is still not duplicated).
      If possible use Cbytes operations like {!get} or {!set} instead of
      Bigarray accessors. *)

  type t
  val create : int -> t
  val length : t -> int
#ifdef NATIVE
  val unsafe_get : t -> int -> char
  val unsafe_set : t -> int -> char -> unit
#else
  external unsafe_get : t -> int -> char = "caml_js_get"
  external unsafe_set : t -> int -> char -> unit = "caml_js_set"
#endif
  val get : t -> int -> char
  val set : t -> int -> char -> unit
  val unsafe_fill : t -> int -> int -> char -> unit
  val fill : t -> int -> int -> char -> unit
  val unsafe_blit_from_bytes : bytes -> int -> t -> int -> int -> unit
  val blit_from_bytes : bytes -> int -> t -> int -> int -> unit
  val unsafe_blit_to_bytes : t -> int -> Bytes.t -> int -> int -> unit
  val blit_to_bytes : t -> int -> Bytes.t -> int -> int -> unit
  val unsafe_blit : t -> int -> t -> int -> int -> unit
  val blit : t -> int -> t -> int -> int -> unit
  val proxy : t -> int -> int -> t
  val extract : t -> int -> int -> t
  val copy : t -> t
  val to_string : t -> string
  val to_bytes : t -> bytes
  val of_string : string -> t
  val of_bytes : bytes -> t

#ifdef NATIVE
  external to_bigarray : t -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t = "%identity"
  external from_bigarray : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> t = "%identity"
#else
  val to_bigarray : t -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  val from_bigarray : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> t
#endif
end

type buf = Cbytes.t

module Timer : sig
  val sleep : int -> unit Lwt.t
  (** [sleep d] is a thread that remains suspended for [d] milliseconds
      and then terminates. *)

  val sleep_seconds : float -> unit Lwt.t
  (** Javascript: you can still sleep for at most INT32_MAX milliseconds.
      When compiled to native code, the limit depends on Sys.word_size *)
end

module Sys : sig
  val win32 : bool
  val unix : bool
  val cygwin : bool
  val os_type : string
  val big_endian : bool

  val file_exists : string -> bool
  val is_directory : string -> bool
  val remove : string -> unit
  val rename : string -> string -> unit
  val chdir : string -> unit
  val getcwd : unit -> string
  val readdir : string -> string array
  val command : string -> int

  val at_exit : (unit -> unit) -> unit
end

module Handle : sig
  type t

  val close_wait : t -> unit Lwt.t
  val close : t -> Int_result.unit
  val close_noerr : t -> unit
  val ref' : t -> unit
  val unref : t -> unit
  val has_ref : t -> bool
end

module Handle_fileno : sig
  type t
  val fileno : t -> file_descr uv_result
  (** Note: fileno will not work, if you compile it to Javascript
      and then try to run it on Windows ... *)

  val fileno_exn : t -> file_descr
end

module Fs_types : sig
  type file_kind = XX(Uwt.Fs_types.file_kind=)
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK
    | S_UNKNOWN

  type stats = XX(Uwt.Fs_types.stats=){
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

  type uv_open_flag = XX(Uwt.Fs_types.uv_open_flag=)
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

  type symlink_mode = XX(Uwt.Fs_types.symlink_mode=)
    | S_Default
    | S_Dir
    | S_Junction

  type access_permission = XX(Uwt.Fs_types.access_permission=)
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
  val symlink : ?mode:symlink_mode -> src:string -> dst:string -> unit -> unit t
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

module Fs : sig
  include Fs_funcs with type 'a t := 'a Lwt.t
end

module Fs_sync : sig
  include Fs_funcs with type 'a t := 'a uv_result
end

module Stream : sig
  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  val shutdown : t -> unit Lwt.t

  val write_ba :
    ?pos:int -> ?len:int -> t -> buf:buf -> unit Lwt.t

  val write_string :
    ?pos:int -> ?len:int -> t -> buf:string -> unit Lwt.t

  val write :
    ?pos:int -> ?len:int -> t -> buf:bytes -> unit Lwt.t

  val read_ba :
    ?pos:int -> ?len:int -> t -> buf:buf -> int Lwt.t

  val read :
    ?pos:int -> ?len:int -> t -> buf:Bytes.t -> int Lwt.t

  val read_start :
    t -> cb:(Bytes.t uv_result -> unit) -> Int_result.unit
  val read_start_exn :
    t -> cb:(Bytes.t uv_result -> unit) -> unit

  val read_stop : t -> Int_result.unit
  val read_stop_exn : t -> unit

  val write_queue_size : t -> int
end


module Pipe : sig
  type t XX(= Uwt.Pipe.t)
  include module type of Stream with type t := t
  include module type of Handle_fileno with type t := t

  val to_stream: t -> Stream.t

  val init : unit -> t
  val with_pipe : (t -> 'a Lwt.t) -> 'a Lwt.t

  val bind : t -> path:string -> Int_result.unit
  val bind_exn : t -> path:string -> unit
  val connect : t -> path:string -> unit Lwt.t
  val with_connect : path:string -> (t -> 'a Lwt.t) -> 'a Lwt.t

  val openpipe : file_descr -> t uv_result
  val openpipe_exn : file_descr -> t
  val with_open : file_descr -> (t -> 'a Lwt.t) -> 'a Lwt.t

  val listen : t -> max:int -> cb:(t uv_result -> unit) -> Int_result.unit
  val listen_exn : t -> max:int -> cb:(t uv_result -> unit) -> unit
end


module Tcp : sig
  type t XX(= Uwt.Tcp.t)
  include module type of Stream with type t := t
  include module type of Handle_fileno with type t := t

  val to_stream: t -> Stream.t

  val init : unit -> t
  val with_tcp : (t -> 'a Lwt.t) -> 'a Lwt.t

  val bind : t -> Unix.inet_addr -> int -> Int_result.unit
  val bind_exn : t -> Unix.inet_addr -> int -> unit

  val connect : t -> Unix.inet_addr -> int -> unit Lwt.t
  val with_connect : Unix.inet_addr -> int -> (t -> 'a Lwt.t) -> 'a Lwt.t

  val listen : t -> max:int -> cb:(t uv_result -> unit) -> Int_result.unit
  val listen_exn : t -> max:int -> cb:(t uv_result -> unit) -> unit

  val getpeername : t -> (Unix.inet_addr * int) uv_result
  val getpeername_exn : t -> Unix.inet_addr * int

  val getsockname : t -> (Unix.inet_addr * int) uv_result
  val getsockname_exn : t -> Unix.inet_addr * int

  val nodelay : t -> bool -> Int_result.unit
  val nodelay_exn : t -> bool -> unit

  val enable_keepalive : t -> int -> Int_result.unit
  val enable_keepalive_exn : t -> int -> unit

  val disable_keepalive : t -> Int_result.unit
  val disable_keepalive_exn : t -> unit

  val simultaneous_accepts : t -> bool -> Int_result.unit
  val simultaneous_accepts_exn : t -> bool -> unit
end

module Subproc : sig
  type t XX(=Uwt.Process.t)
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  type io =
    | Inherit_file of file
    | Create_pipe of Pipe.t
    | Inherit_pipe of Pipe.t
    | Inherit_stream of Stream.t
  type exit_cb = t -> exit_status:int -> term_signal:int -> unit

  val spawn :
    ?stdin:io ->
    ?stdout:io ->
    ?stderr:io ->
    ?uid:int ->
    ?gid:int ->
    ?verbatim_arguments:bool ->
    ?detach:bool ->
    ?env:string list ->
    ?cwd:string ->
    ?exit_cb:exit_cb ->
    string ->
    string list ->
    t uv_result


  val spawn_exn :
    ?stdin:io ->
    ?stdout:io ->
    ?stderr:io ->
    ?uid:int ->
    ?gid:int ->
    ?verbatim_arguments:bool ->
    ?detach:bool ->
    ?env:string list ->
    ?cwd:string ->
    ?exit_cb:exit_cb ->
    string ->
    string list-> t

  val process_kill : t -> int -> Int_result.unit
  val process_kill_exn : t -> int -> unit

  val pid : t -> Int_result.int
  val pid_exn : t -> int

  val kill : pid:int -> signum:int -> Int_result.unit
  val kill_exn : pid:int -> signum:int -> unit
end

module Dns : sig
  type family =
    | UNSPEC
    | INET
    | INET6

  val getaddrinfo : ?family:family -> string -> Unix.inet_addr list uv_result Lwt.t
  val getnameinfo : Unix.inet_addr -> int -> Unix.name_info uv_result Lwt.t
end

module Signal : sig
  type t XX(=Uwt.Signal.t)
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  val sigwinch: int

  val start : int -> cb:(t -> int -> unit) -> t uv_result
  val start_exn : int -> cb:(t -> int -> unit) -> t
end

module Fs_event : sig
  type t XX(=Uwt.Fs_event.t)

  type event =
    | Rename
    | Change

  type cb = t -> (string * event) uv_result -> unit

  (**
     If persistent is true, the underlying handle will be referenced.
     The event loop will continue to run, until there are only unreferenced
     handles.

     In case your callback receives an error, your handle will also be closed
     and your callback we never be called again ...

     @param recursive default false
     @param persistent default true *)
  val start : ?recursive:bool -> ?persistent:bool -> string -> cb:cb -> t uv_result
  val start_exn : ?recursive:bool -> ?persistent:bool -> string -> cb:cb -> t
  val close : t -> unit
end

module Fs_poll : sig
  type t XX(=Uwt.Fs_poll.t)

  type report = XX(Uwt.Fs_poll.report=) {
    prev : Fs_types.stats;
    curr : Fs_types.stats;
  }
  type cb = t -> report uv_result -> unit

  (**
     If persistent is true, the underlying handle will be referenced.
     The event loop will continue to run, until there are only unreferenced
     handles.

     @param recursive default false
     @param persistent default true *)
  val start : ?persistent:bool -> string -> int -> cb:cb -> t uv_result
  val start_exn : ?persistent:bool -> string -> int -> cb:cb -> t
  val close : t -> unit
end

module Main : sig
  val yield : unit -> unit Lwt.t
end

module Filename : sig
  (** A copy of useful functions from OCaml stdlibs,
      but the Os detection is delayed to runtime.
      Your OS at compile time might differ from your runtime os,
      so the [Filename.quote], [Filename.dirname] of OCaml's stdlib
      get it wrong. *)

  val current_dir_name : string
  val parent_dir_name : string
  val dir_sep : string
  val is_relative : string -> bool
  val is_implicit : string -> bool
  val check_suffix : string -> string -> bool
  val quote : string -> string
  val basename : string -> string
  val dirname : string -> string
  val concat : string -> string -> string
  val chop_suffix : string -> string -> string
  val extension : string -> string
  val chop_extension : string -> string
  val remove_extension : string -> string
  val set_temp_dir_name : string -> unit
  val get_temp_dir_name : unit -> string
end

module Io : sig
  exception Channel_closed of string
  (** Exception raised when a channel is closed. The parameter is a
      description of the channel. *)

  (** {2 Types} *)

  type 'mode channel XX(='mode Uwt_io.channel)
  (** Type of buffered byte channels *)

  type input XX(=Uwt_io.input)
  (** Input mode *)

  type output XX(=Uwt_io.output)
  (** Output mode *)

  (** Channel mode *)
  type 'a mode = XX('a Uwt_io.mode =)
    | Input : input mode
    | Output : output mode

  val input : input mode
  (** [input] input mode representation *)

  val output : output mode
  (** [output] output mode representation *)

  type input_channel = input channel
  (** Type of input channels *)

  type output_channel = output channel
  (** Type of output channels *)

  val mode : 'a channel -> 'a mode
  (** [mode ch] returns the mode of a channel *)

  (** {2 Well-known instances} *)

  val stdin : input_channel
  (** The standard input, it reads data from {!Uwt.stdin} *)

  val stdout : output_channel
  (** The standard output, it writes data to {!Uwt.stdout} *)

  val stderr : output_channel
  (** The standard output for error messages, it writes data to
      {!Uwt.stderr} *)

  val zero : input_channel
  (** Inputs which returns always ['\x00'] *)

  val null : output_channel
  (** Output which drops everything *)

  (** {2 Channels creation/manipulation} *)

  val make :
    ?buffer : Cbytes.t ->
    ?close : (unit -> unit Lwt.t) ->
    mode : 'mode mode ->
    (Cbytes.t -> int -> int -> int Lwt.t) -> 'mode channel
  (** [make ?buffer ?close ~mode perform_io] is the
      main function for creating new channels.

      @param buffer user-supplied buffer. When this argument is
      present, its value will be used as the buffer for created
      channel. Size of buffer must conform to limitations described
      in {!set_default_buffer_size}.
      When this argument is not present, a new internal buffer of default
      size will be allocated for this channel.
      Warning: do not use the same buffer for simultaneous work with
      more than one channel.
      There are other functions in this module that take [buffer]
      argument, their semantics agrees with the described above.

      @param close close function of the channel. It defaults to
      [Lwt.return]

      @param mode either {!input} or {!output}

      @param perform_io is the read or write function. It is called
      when more input is needed or when the buffer need to be
      flushed. *)


  val of_bytes : mode : 'mode mode -> Cbytes.t -> 'mode channel
  (** Create a channel from a byte array. Reading/writing is done
      directly on the provided array. *)


  val of_file :
    ?buffer : Cbytes.t ->
    ?close:(unit -> unit Lwt.t) -> mode:'m mode -> file -> 'm channel
  (** [of_file ?buffer ?close ~mode fd] creates a channel from a
      file descriptor.

      @param close defaults to closing the file descriptor. *)

  val of_stream :
    ?buffer : Cbytes.t ->
    ?close:(unit -> unit Lwt.t) -> mode:'m mode -> Stream.t -> 'm channel

  val of_pipe :
    ?buffer : Cbytes.t ->
    ?close:(unit -> unit Lwt.t) -> mode:'m mode -> Pipe.t -> 'm channel

  val of_tcp :
    ?buffer : Cbytes.t ->
    ?close:(unit -> unit Lwt.t) -> mode:'m mode -> Tcp.t -> 'm channel

  val close : 'a channel -> unit Lwt.t
  (** [close ch] closes the given channel. If [ch] is an output
      channel, it performs all pending actions, flushes it and closes
      it. If [ch] is an input channel, it just closes it immediately.

      [close] returns the result of the close function of the
      channel. Multiple calls to [close] will return exactly the same
      value.

      Note: you cannot use [close] on channels obtained with
      {!atomic}. *)

  val abort : 'a channel -> unit Lwt.t
  (** [abort ch] abort current operations and close the channel
      immediately. *)

  val atomic : ('a channel -> 'b Lwt.t) -> ('a channel -> 'b Lwt.t)
  (** [atomic f] transforms a sequence of io operations into one
      single atomic io operation.

      Note:
      - the channel passed to [f] is invalid after [f] terminates
      - [atomic] can be called inside another [atomic] *)

  val buffered : 'a channel -> int
  (** [buffered oc] returns the number of bytes in the buffer *)

  val flush : output_channel -> unit Lwt.t
  (** [flush oc] performs all pending writes on [oc] *)

  val buffer_size : 'a channel -> int
  (** Returns the size of the internal buffer. *)

  val resize_buffer : 'a channel -> int -> unit Lwt.t
  (** Resize the internal buffer to the given size *)

  val is_busy : 'a channel -> bool
  (** [is_busy channel] returns whether the given channel is currently
      busy. A channel is busy when there is at least one job using it
      that has not yet terminated. *)

  (** {2 Random access} *)

  val position : 'a channel -> int64
  (** [position ch] Returns the current position in the channel. *)

  (** {2 Reading} *)

  (** Note: except for functions dealing with streams ({!read_chars} and
      {!read_lines}) all functions are {b atomic}. *)

  val read_char : input_channel -> char Lwt.t
  (** [read_char ic] reads the next character of [ic].

      @raise End_of_file if the end of the file is reached *)

  val read_char_opt : input_channel -> char option Lwt.t
  (** Same as {!read_byte} but does not raise [End_of_file] on end of
      input *)

  val read_chars : input_channel -> char Lwt_stream.t
  (** [read_chars ic] returns a stream holding all characters of
      [ic] *)

  val read_line : input_channel -> string Lwt.t
  (** [read_line ic] reads one complete line from [ic] and returns it
      without the end of line. End of line is either ["\n"] or
      ["\r\n"].

      If the end of line is reached before reading any character,
      [End_of_file] is raised. If it is reached before reading an end
      of line but characters have already been read, they are
      returned. *)

  val read_line_opt : input_channel -> string option Lwt.t
  (** Same as {!read_line} but do not raise [End_of_file] on end of
      input. *)

  val read_lines : input_channel -> string Lwt_stream.t
  (** [read_lines ic] returns a stream holding all lines of [ic] *)

  val read : ?count : int -> input_channel -> string Lwt.t
  (** [read ?count ic] reads at most [count] characters from [ic]. It
      returns [""] if the end of input is reached. If [count] is not
      specified, it reads all bytes until the end of input. *)

  val read_into : input_channel -> Bytes.t -> int -> int -> int Lwt.t
  (** [read_into ic buffer offset length] reads up to [length] bytes,
      stores them in [buffer] at offset [offset], and returns the
      number of bytes read.

      Note: [read_into] does not raise [End_of_file], it returns a
      length of [0] instead. *)

  val read_into_exactly : input_channel -> Bytes.t -> int -> int -> unit Lwt.t
  (** [read_into_exactly ic buffer offset length] reads exactly
      [length] bytes and stores them in [buffer] at offset [offset].

      @raise End_of_file on end of input *)

  val read_value : input_channel -> 'a Lwt.t
  (** [read_value ic] reads a marshaled value from [ic] *)

  (** {2 Writing} *)

  (** Note: as for reading functions, all functions except
      {!write_chars} and {!write_lines} are {b atomic}.

      For example if you use {!write_line} in two different threads, the
      two operations will be serialized, and lines cannot be mixed.
  *)

  val write_char : output_channel -> char -> unit Lwt.t
  (** [write_char oc char] writes [char] on [oc] *)

  val write_chars : output_channel -> char Lwt_stream.t -> unit Lwt.t
  (** [write_chars oc chars] writes all characters of [chars] on
      [oc] *)

  val write : output_channel -> string -> unit Lwt.t
  (** [write oc str] writes all characters of [str] on [oc] *)

  val write_line : output_channel -> string -> unit Lwt.t
  (** [write_line oc str] writes [str] on [oc] followed by a
      new-line. *)

  val write_lines : output_channel -> string Lwt_stream.t -> unit Lwt.t
  (** [write_lines oc lines] writes all lines of [lines] to [oc] *)

  val write_from : output_channel -> Bytes.t -> int -> int -> int Lwt.t
  (** [write_from oc buffer offset length] writes up to [length] bytes
      to [oc], from [buffer] at offset [offset] and returns the number
      of bytes actually written *)

  val write_from_string : output_channel -> string -> int -> int -> int Lwt.t
  (** See {!write}. *)

  val write_from_exactly : output_channel -> Bytes.t -> int -> int -> unit Lwt.t
  (** [write_from_exactly oc buffer offset length] writes all [length]
      bytes from [buffer] at offset [offset] to [oc] *)

  val write_from_string_exactly : output_channel -> string -> int -> int -> unit Lwt.t
  (** See {!write_from_exactly}. *)

  val write_value : output_channel -> ?flags : Marshal.extern_flags list -> 'a -> unit Lwt.t
  (** [write_value oc ?flags x] marshals the value [x] to [oc] *)

  (** {2 Printing} *)

  (** These functions are basically helpers. Also you may prefer
      using the name {!printl} rather than {!write_line} because it is
      shorter.

      The general name of a printing function is [<prefix>print<suffixes>],

      where [<prefix>] is one of:
      - ['f'], which means that the function takes as argument a channel
      - nothing, which means that the function prints on {!stdout}
      - ['e'], which means that the function prints on {!stderr}

      and [<suffixes>] is a combination of:
      - ['l'] which means that a new-line character is printed after the message
      - ['f'] which means that the function takes as argument a {b format} instead
      of a string
  *)

  val fprint : output_channel -> string -> unit Lwt.t
  val fprintl : output_channel -> string -> unit Lwt.t
  val fprintf : output_channel -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  val fprintlf : output_channel -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  val print : string -> unit Lwt.t
  val printl : string -> unit Lwt.t
  val printf : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val printlf : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val eprint : string -> unit Lwt.t
  val eprintl : string -> unit Lwt.t
  val eprintf : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val eprintlf : ('a, unit, string, unit Lwt.t) format4 -> 'a

  (** {2 Utilities} *)

  val hexdump_stream : output_channel -> char Lwt_stream.t -> unit Lwt.t
  (** [hexdump_stream oc byte_stream] produces the same output as the
      command [hexdump -C]. *)

  val hexdump : output_channel -> string -> unit Lwt.t
  (** [hexdump oc str = hexdump_stream oc (Lwt_stream.of_string str)] *)

  (** {2 File utilities} *)

  type file_name = string
  (** Type of file names *)

  val open_file :
    ?buffer : Cbytes.t ->
    ?flags : Fs.uv_open_flag list ->
    ?perm : Unix.file_perm ->
    mode : 'a mode ->
    file_name -> 'a channel Lwt.t
  (** [open_file ?buffer ?flags ?perm ~mode filename] opens the
      file with name [filename] and returns a channel for
      reading/writing it.

      @raise Unix.Unix_error on error.
  *)

  val with_file :
    ?buffer : Cbytes.t ->
    ?flags : Fs.uv_open_flag list ->
    ?perm : Unix.file_perm ->
    mode : 'a mode ->
    file_name -> ('a channel -> 'b Lwt.t) -> 'b Lwt.t
  (** [with_file ?buffer ?flags ?perm ~mode filename f] opens a
      file and passes the channel to [f]. It is ensured that the
      channel is closed when [f ch] terminates (even if it fails). *)


  val open_connection :
    ?in_buffer:Cbytes.t ->
    ?out_buffer:Cbytes.t ->
    Unix.sockaddr -> (input channel * output channel) Lwt.t
  (** [open_connection ?in_buffer ?out_buffer sockaddr] opens
      a connection to the given address and returns two channels for
      using it.

      The connection is completly closed when you close both channels.

      @raise Unix.Unix_error on error.  *)

  val with_connection :
    ?in_buffer:Cbytes.t ->
    ?out_buffer:Cbytes.t ->
    Unix.sockaddr ->
    (input channel * output channel -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_connection ?fd ?in_buffer ?out_buffer inet_addr port f]
      opens a connection to the given address and passes the channels to
      [f] *)

  type server XX(=Uwt_io.server)
  (** Type of a server *)

  val establish_server_with_client_address :
    ?buffer_size : int ->
    ?backlog : int ->
    ?no_close : bool ->
    Unix.sockaddr ->
    (Unix.sockaddr -> input_channel * output_channel -> unit Lwt.t) ->
    server Lwt.t
  (** [establish_server_with_client_address listen_address f] creates a server
      which listens for incoming connections on [listen_address]. When a client
      makes a new connection, it is passed to [f]: more precisely, the server
      calls

      {[
        f client_address (in_channel, out_channel)
      ]}

      where [client_address] is the address (peer name) of the new client, and
      [in_channel] and [out_channel] are two channels wrapping the socket for
      communicating with that client.

      The server does not block waiting for [f] to complete: it concurrently tries
      to accept more client connections while [f] is handling the client.

      When the promise returned by [f] completes (i.e., [f] is done handling the
      client), [establish_server_with_client_address] automatically closes
      [in_channel] and [out_channel]. This is a default behavior that is useful
      for simple cases, but for a robust application you should explicitly close
      these channels yourself, and handle any exceptions. If the channels are
      still open when [f] completes, and their automatic closing raises an
      exception, [establish_server_with_client_address] treats it as an unhandled
      exception reaching the top level of the application: it passes that
      exception to {!Lwt.async_exception_hook}, the default behavior of which is
      to print the exception and {e terminate your process}.

      Automatic closing can be completely disabled by passing [~no_close:true].

      Similarly, if [f] raises an exception (or the promise it returns fails with
      an exception), [establish_server_with_client_address] can do nothing with
      that exception, except pass it to {!Lwt.async_exception_hook}.

      [~backlog] is the argument passed to {!Lwt_unix.listen}.

      The returned promise (a [server Lwt.t]) resolves when the server has just
      started listening on [listen_address]: right after the internal call to
      [listen], and right before the first internal call to [accept]. *)


  val establish_server :
    ?buffer_size : int ->
    ?backlog : int ->
    ?no_close : bool ->
    Unix.sockaddr ->
    (input_channel * output_channel -> unit Lwt.t) -> server Lwt.t
  (** Like [establish_server_with_client_address], but does not pass the client
      address to the callback [f]. *)


  val shutdown_server : server -> unit Lwt.t (** Shutdown the given server *)


  val lines_of_file : file_name -> string Lwt_stream.t
  (** [lines_of_file name] returns a stream of all lines of the file
      with name [name]. The file is automatically closed when all
      lines have been read. *)

  val lines_to_file : file_name -> string Lwt_stream.t -> unit Lwt.t
  (** [lines_to_file name lines] writes all lines of [lines] to
      file with name [name]. *)

  val chars_of_file : file_name -> char Lwt_stream.t
  (** [chars_of_file name] returns a stream of all characters of the
      file with name [name]. As for {!lines_of_file} the file is
      closed when all characters have been read. *)

  val chars_to_file : file_name -> char Lwt_stream.t -> unit Lwt.t
  (** [chars_to_file name chars] writes all characters of [chars] to
      [name] *)

  (** {2 Input/output of integers} *)

  (** Common interface for reading/writing integers in binary *)
  module type NumberIO = sig

    (** {3 Reading} *)

    val read_int : input_channel -> int Lwt.t
    (** Reads a 32-bits integer as an ocaml int *)

    val read_int16 : input_channel -> int Lwt.t
    val read_int32 : input_channel -> int32 Lwt.t
    val read_int64 : input_channel -> int64 Lwt.t

    val read_float32 : input_channel -> float Lwt.t
    (** Reads an IEEE single precision floating point value *)

    val read_float64 : input_channel -> float Lwt.t
    (** Reads an IEEE double precision floating point value *)

    (** {3 Writing} *)

    val write_int : output_channel -> int -> unit Lwt.t
    (** Writes an ocaml int as a 32-bits integer *)

    val write_int16 : output_channel -> int -> unit Lwt.t
    val write_int32 : output_channel -> int32 -> unit Lwt.t
    val write_int64 : output_channel -> int64 -> unit Lwt.t

    val write_float32 : output_channel -> float -> unit Lwt.t
    (** Writes an IEEE single precision floating point value *)

    val write_float64 : output_channel -> float -> unit Lwt.t
    (** Writes an IEEE double precision floating point value *)
  end

  module LE : NumberIO
  (** Reading/writing of numbers in little-endian *)

  module BE : NumberIO
  (** Reading/writing of numbers in big-endian *)

  include NumberIO
  (** Reading/writing of numbers in the system endianness. *)

  type byte_order = XX(Uwt_io.byte_order=) Little_endian | Big_endian
  (** Type of byte order *)

  val system_byte_order : byte_order

  (** {2 Low-level access to the internal buffer} *)

  val block : 'a channel  -> int -> (Cbytes.t -> int -> 'b Lwt.t) -> 'b Lwt.t
  (** [block ch size f] pass to [f] the internal buffer and an
      offset. The buffer contains [size] chars at [offset]. [f] may
      read or write these chars.  [size] must satisfy [0 <= size <=
      16] *)

  (** Information for directly accessing the internal buffer of a
      channel *)
  type direct_access = {
    da_buffer : Cbytes.t;
    (** The internal buffer *)
    mutable da_ptr : int;
    (** The pointer to:
        - the beginning of free space for output channels
        - the beginning of data for input channels *)
    mutable da_max : int;
    (** The maximum offset *)
    da_perform : unit -> int Lwt.t;
    (** - for input channels:
          refills the buffer and returns how many bytes have been read
        - for output channels:
          flush partially the buffer and returns how many bytes have been written *)
  }

  val direct_access : 'a channel -> (direct_access -> 'b Lwt.t) -> 'b Lwt.t
  (** [direct_access ch f] passes to [f] a {!direct_access}
      structure. [f] must use it and update [da_ptr] to reflect how
      many bytes have been read/written. *)

  (** {2 Misc} *)

  val default_buffer_size : unit -> int
  (** Return the default size for buffers. Channels that are created
      without a specific buffer use new buffer of this size. *)

  val set_default_buffer_size : int -> unit
  (** Change the default buffer size.

      @raise Invalid_argument if the given size is smaller than [16]
      or greater than [Sys.max_string_length] *)
end

module Process : sig
  (** Process management *)

  (** This module allows you to spawn processes and communicate with them. *)

  type command = string * string array
  (** A command. The first field is the name of the executable and
      the second is the list of arguments. For example:

      {[
        ("ls", [|"ls"; "-l"|])
      ]}

      Notes:

      - if the name is the empty string, then the first argument
        will be used (for backward compatibility with [Lwt_process]).

      - It is not possible to ``inline'' an argument,
        i.e. split it into multiple arguments with "\000".
        (like under [Lwt_process])
  *)

  val shell : string -> command
  (** A command executed with the shell. (with ["/bin/sh -c <cmd>"] on
      Unix and ["cmd.exe /c <cmd>"] on Windows). *)

  (** All the following functions take an optional argument
      [timeout]. If specified, after expiration, the process will be
      sent a [Unix.sigkill] signal and channels will be closed. *)

  (** {2 High-level functions} *)

  (** {3 Redirections} *)

  (** A file descriptor redirection. It describes how standard file
      descriptors are redirected in the child process. *)
  type redirection =
    [ `Keep  (** The file descriptor is left unchanged *)
    | `Dev_null  (** Connect the file descriptor to [/dev/null] *)
    | `Close  (** It's now the same as `Dev_null *)
    | `FD_copy of file_descr
    (** The file descriptor is replaced by the given
        one *)
    | `FD_move of file_descr
    (** The file descriptor is replaced by the given one, which is
        then closed. *)
    | `File_copy of file
    | `File_move of file
    | `Pipe_copy of Pipe.t
    | `Pipe_move of Pipe.t
    | `Stream_copy of Stream.t
    | `Stream_move of Stream.t ]


  (** Note: all optional redirection arguments default to [`Keep] *)

  (** {3 Executing} *)

  val exec :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdin : redirection ->
    ?stdout : redirection ->
    ?stderr : redirection ->
    command -> Unix.process_status Lwt.t
  (** Executes the given command and returns its exit status.
      [Unix.WSTOPPED] is not supported by libuv at the moment.
      It will return either [Unix.WSIGNALED] or [Unix.WEXITED]
  *)

  (** {3 Receiving} *)

  val pread :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdin : redirection ->
    ?stderr : redirection ->
    command -> string Lwt.t
  val pread_chars :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdin : redirection ->
    ?stderr : redirection ->
    command -> char Lwt_stream.t
  val pread_line :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdin : redirection ->
    ?stderr : redirection ->
    command -> string Lwt.t
  val pread_lines :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdin : redirection ->
    ?stderr : redirection ->
    command -> string Lwt_stream.t

  (** {3 Sending} *)

  val pwrite :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdout : redirection ->
    ?stderr : redirection ->
    command -> string -> unit Lwt.t
  val pwrite_chars :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdout : redirection ->
    ?stderr : redirection ->
    command -> char Lwt_stream.t -> unit Lwt.t
  val pwrite_line :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdout : redirection ->
    ?stderr : redirection ->
    command -> string -> unit Lwt.t
  val pwrite_lines :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdout : redirection ->
    ?stderr : redirection ->
    command -> string Lwt_stream.t -> unit Lwt.t

  (** {3 Mapping} *)

  val pmap :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stderr : redirection ->
    command -> string -> string Lwt.t
  val pmap_chars :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stderr : redirection ->
    command -> char Lwt_stream.t -> char Lwt_stream.t
  val pmap_line :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stderr : redirection ->
    command -> string -> string Lwt.t
  val pmap_lines :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stderr : redirection ->
    command -> string Lwt_stream.t -> string Lwt_stream.t

  (** {2 Spawning processes} *)

  (** State of a sub-process *)
  type state = XX(Uwt_process.state=)
    | Running
    (** The process is still running *)
    | Exited of Unix.process_status
    (** The process has exited *)

  class process_none :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdin : redirection ->
    ?stdout : redirection ->
    ?stderr : redirection ->
    command ->
    object
      method pid : int
      (** Pid of the sub-process *)

      method state : state
      (** Return the state of the process *)

      method kill : int -> unit
      (** [kill signum] sends [signum] to the process if it is still
          running. *)

      method terminate : unit
      (** Terminates the process. It is equivalent to [kill Sys.sigkill]
          on Unix but also works on Windows (unlike {!kill}). *)

      method status : Unix.process_status Lwt.t
      (** Threads which wait for the sub-process to exit then returns its
          exit status *)

      method close : Unix.process_status Lwt.t
      (** Closes the process and returns its exit status. This closes all
          channels used to communicate with the process *)
    end

  val open_process_none :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdin : redirection ->
    ?stdout : redirection ->
    ?stderr : redirection ->
    command -> process_none
  val with_process_none :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdin : redirection ->
    ?stdout : redirection ->
    ?stderr : redirection ->
    command -> (process_none -> 'a Lwt.t) -> 'a Lwt.t

  class process_in :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdin : redirection ->
    ?stderr : redirection ->
    command ->
    object
      inherit process_none

      method stdout : Io.input_channel
      (** The standard output of the process *)
    end

  val open_process_in :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdin : redirection ->
    ?stderr : redirection ->
    command -> process_in
  val with_process_in :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdin : redirection ->
    ?stderr : redirection ->
    command -> (process_in -> 'a Lwt.t) -> 'a Lwt.t

  class process_out :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdout : redirection ->
    ?stderr : redirection ->
    command ->
    object
      inherit process_none

      method stdin : Io.output_channel
      (** The standard input of the process *)
    end

  val open_process_out :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdout : redirection ->
    ?stderr : redirection ->
    command -> process_out
  val with_process_out :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stdout : redirection ->
    ?stderr : redirection ->
    command -> (process_out -> 'a Lwt.t) -> 'a Lwt.t

  class process :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stderr : redirection ->
    command ->
    object
      inherit process_none

      method stdin : Io.output_channel
      method stdout : Io.input_channel
    end

  val open_process :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stderr : redirection ->
    command -> process
  val with_process :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    ?stderr : redirection ->
    command -> (process -> 'a Lwt.t) -> 'a Lwt.t

  class process_full :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    command ->
    object
      inherit process_none

      method stdin : Io.output_channel
      method stdout : Io.input_channel
      method stderr : Io.input_channel
    end

  val open_process_full :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    command -> process_full

  val with_process_full :
    ?timeout : float ->
    ?uid:int ->
    ?gid:int ->
    ?env : string array ->
    ?cwd: string ->
    command -> (process_full -> 'a Lwt.t) -> 'a Lwt.t
end
