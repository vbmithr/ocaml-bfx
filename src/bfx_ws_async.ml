open Bfx
open Bfx_ws

let of_string ?buf msg = Yojson_encoding.destruct_safe encoding (Yojson.Safe.from_string ?buf msg)
let to_string ?buf cmd = Yojson.Safe.to_string ?buf (Yojson_encoding.construct encoding cmd)

module Persistent = struct
  module Conn = (Fastws_async.MakePersistent (struct type r = Bfx_ws.t type w = Bfx_ws.t end))
  include Persistent_connection_kernel.Make(Conn)

  let create' ~server_name ?on_event ?retry_delay ?buf =
    create ~server_name ?on_event ?retry_delay
      ~connect:(Fastws_async.connect ~wr:(to_string ?buf) ~rd:(of_string ?buf))
end
