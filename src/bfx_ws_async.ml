open Bfx
open Bfx_ws

let of_string ?buf msg = Yojson_encoding.destruct_safe encoding (Yojson.Safe.from_string ?buf msg)
let to_string ?buf cmd = Yojson.Safe.to_string ?buf (Yojson_encoding.construct encoding cmd)

module Conn = (Fastws_async.MakePersistent (struct type r = Bfx_ws.t type w = Bfx_ws.t end))
module Persistent = Persistent_connection_kernel.Make(Conn)
