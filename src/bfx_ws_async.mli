open Core
open Async

val public_url : Uri.t
val auth_url   : Uri.t

type t = {
  r: Bfx_ws.t Pipe.Reader.t ;
  w: Bfx_ws.t Pipe.Writer.t ;
}

val connect     : ?buf:Bi_outbuf.t -> Uri.t -> t Deferred.Or_error.t
val connect_exn : ?buf:Bi_outbuf.t -> Uri.t -> t Deferred.t

val with_connection :
  ?buf:Bi_outbuf.t -> ?url:Uri.t ->
  (Bfx_ws.t Pipe.Reader.t -> Bfx_ws.t Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.Or_error.t

val with_connection_exn :
  ?buf:Bi_outbuf.t -> ?url:Uri.t ->
  (Bfx_ws.t Pipe.Reader.t -> Bfx_ws.t Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.t

module Persistent : sig
  include Persistent_connection_kernel.S
    with type address = Uri.t
     and type conn = t

  val create' :
    server_name:string ->
    ?on_event:(Event.t -> unit Deferred.t) ->
    ?retry_delay:(unit -> Time_ns.Span.t) ->
    ?buf:Bi_outbuf.t ->
    (unit -> address Or_error.t Deferred.t) -> t
end
