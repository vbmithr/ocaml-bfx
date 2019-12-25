open Core
open Async

open Bfx
open Bfx_ws

let public_url = Uri.make ~scheme:"https" ~host:"api-pub.bitfinex.com" ~path:"ws/2" ()
let auth_url = Uri.make ~scheme:"https" ~host:"api.bitfinex.com" ~path:"ws/2" ()

let src = Logs.Src.create "bfx.ws.async"
module Log = (val Logs.src_log src : Logs.LOG)

module T = struct
  type t = {
    r: Bfx_ws.t Pipe.Reader.t ;
    w: Bfx_ws.t Pipe.Writer.t ;
  }

  module Address = Uri_sexp

  let is_closed { r; w } =
    Pipe.(is_closed r && is_closed w)

  let close { r; w } =
    Pipe.close w ;
    Pipe.close_read r ;
    Deferred.unit

  let close_finished { r; w } =
    Deferred.all_unit [Pipe.closed r; Pipe.closed w]
end
include T

let create_client_read ?buf r =
  Pipe.map r ~f:begin fun msg ->
    Yojson_encoding.destruct_safe encoding
      (Yojson.Safe.from_string ?buf msg)
  end

let create_client_write ?buf w =
  Pipe.create_writer begin fun ws_read ->
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc = Yojson.Safe.to_string ?buf
          (Yojson_encoding.construct encoding cmd) in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end
  end

let connect ?buf url =
  Deferred.Or_error.map
    (Fastws_async.EZ.connect url) ~f:begin fun { r; w; _ } ->
      let client_write = create_client_write ?buf w in
      (Pipe.closed client_write >>> fun () -> Pipe.close w) ;
      { r = create_client_read r; w = client_write }
    end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create' ~server_name ?on_event ?retry_delay ?buf =
    create ~server_name ?on_event ?retry_delay ~connect:(connect ?buf)
end

let connect_exn ?buf url =
  connect ?buf url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection ?buf ?(url=public_url) f =
  Fastws_async.EZ.with_connection url ~f:begin fun r w ->
    f (create_client_read ?buf r) (create_client_write ?buf w)
  end

let with_connection_exn ?buf ?url f =
  with_connection ?buf ?url f >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
