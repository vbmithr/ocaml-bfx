val of_string : ?buf:Bi_outbuf.t -> string -> Bfx_ws.t
val to_string : ?buf:Bi_outbuf.t -> Bfx_ws.t -> string

module Persistent : Persistent_connection_kernel.S
  with type address = Uri.t
   and type conn = (Bfx_ws.t, Bfx_ws.t) Fastws_async.t
