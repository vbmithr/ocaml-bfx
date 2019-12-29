open Core
open Async

val of_string : ?buf:Bi_outbuf.t -> string -> Bfx_ws.t
val to_string : ?buf:Bi_outbuf.t -> Bfx_ws.t -> string

module Persistent : sig
  include Persistent_connection_kernel.S
    with type address = Uri.t
     and type conn = (Bfx_ws.t, Bfx_ws.t) Fastws_async.t

  val create' :
    server_name:string ->
    ?on_event:(Event.t -> unit Deferred.t) ->
    ?retry_delay:(unit -> Time_ns.Span.t) ->
    ?buf:Bi_outbuf.t ->
    (unit -> address Or_error.t Deferred.t) -> t
end
