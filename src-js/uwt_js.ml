include Internal
include Internal.Itypes

let to_unix_error = Int_result.error_to_unix_error
let of_unix_error = Int_result.unix_error_to_error
let err_name n = Int_result.of_error n |> Int_result.err_name

let file_of_file_descr f = Some f

module Timer = Timer
module Handle = Handle
module Signal = Signal
module Handle_fileno = Handle_fileno
module Stream = Stream
module Pipe = Pipe
module Tcp = Tcp
module Dns = Dns
module Subproc = Subproc
module Main = Main
module Fs_sync = Fs.Sync
module Fs = Fs.Async
module Cbytes = Cbytes
module Filename = Filename
module Fs_event = Fs_event
module Fs_poll = Fs_poll
module Sys = Js_sys

module Io = Io
module Process = Process
