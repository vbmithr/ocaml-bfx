open Async
open Bfx_ws

val connect :
  ?buf:Bi_outbuf.t ->
  [< `Private | `Public ] ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t,
   [ `Internal of exn | `WS of Fastws_async.error ]) result Deferred.t

val with_connection :
  ?buf:Bi_outbuf.t ->
  [< `Private | `Public ] ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) ->
  ('a, [ `Internal of exn
       | `User_callback of exn
       | `WS of Fastws_async.error ]) result Deferred.t
