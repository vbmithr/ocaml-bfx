open Core
open Async

open Bfx
open Bfx_ws

let public_url =
  Uri.make ~scheme:"https" ~host:"api-pub.bitfinex.com" ~path:"ws/2" ()
let url =
  Uri.make ~scheme:"https" ~host:"api.bitfinex.com" ~path:"ws/2" ()

let src = Logs.Src.create "bfx.ws.async"
module Log = (val Logs.src_log src : Logs.LOG)

let connect ?buf v =
  let url = match v with
    | `Public -> public_url
    | `Private -> url in
  Deferred.Or_error.map (Fastws_async.EZ.connect url)
    ~f:begin fun { r; w; cleaned_up } ->
      let client_read = Pipe.map r ~f:begin fun msg ->
          Yojson_encoding.destruct_safe encoding
            (Yojson.Safe.from_string ?buf msg)
        end in
      let ws_read, client_write = Pipe.create () in
      don't_wait_for
        (Pipe.closed client_write >>| fun () -> Pipe.close w) ;
      don't_wait_for @@
      Pipe.transfer ws_read w ~f:begin fun cmd ->
        let doc = Yojson.Safe.to_string ?buf
            (Yojson_encoding.construct encoding cmd) in
        Log.debug (fun m -> m "-> %s" doc) ;
        doc
      end ;
      (client_read, client_write, cleaned_up)
    end

let connect_exn ?buf v =
  connect ?buf v >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection ?buf v f =
  let url = match v with
    | `Public -> public_url
    | `Private -> url in
  Fastws_async.EZ.with_connection url ~f:begin fun r w ->
    let client_read = Pipe.map r ~f:begin fun msg ->
        Yojson_encoding.destruct_safe encoding
          (Yojson.Safe.from_string ?buf msg)
      end in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc = Yojson.Safe.to_string ?buf
          (Yojson_encoding.construct encoding cmd) in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end ;
    f client_read client_write
  end

let with_connection_exn ?buf v f =
  with_connection ?buf v f >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
