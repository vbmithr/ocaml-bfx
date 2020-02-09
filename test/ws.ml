open Core
open Async

open Bfx
open Bfx_ws

let src = Logs.Src.create "bfx.cli"
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let process_user_cmd w =
  let process s =
    match String.split s ~on:' ' with
    | ["ping"] ->
      let now = Time_ns.(now () |> to_span_since_epoch |> Span.to_sec) in
      Pipe.write w (Ping now)
    | ["trades" ; symbol ] ->
      Pipe.write w (Subscribe (Trades (Pair.of_string_noprefix_exn symbol)))
    | ["quotes" ; symbol ] ->
      Pipe.write w (Subscribe (Quotes (Pair.of_string_noprefix_exn symbol)))
    | ["unsubscribe" ; chanId] ->
      Pipe.write w (Unsubscribe (Int.of_string chanId))
    | h :: _ ->
      Log_async.err (fun m -> m "Unknown command %s" h)
    | [] ->
      Log_async.err (fun m -> m "Empty command")
  in
  let rec loop () = Reader.(read_line @@ Lazy.force stdin) >>= function
    | `Eof -> Deferred.unit
    | `Ok line -> process line >>= loop
  in
  loop ()

let main () =
  let module Encoding = Json_encoding.Make(Json_repr.Yojson) in
  let buf = Bi_outbuf.create 4096 in
  let of_string s =
    Encoding.destruct encoding (Yojson.Safe.from_string ~buf s) in
  let to_string t =
    Yojson.Safe.to_string ~buf (Encoding.construct encoding t) in
  Fastws_async.with_connection
    ~of_string:of_string ~to_string Bfx_ws.public_url begin fun r w ->
    don't_wait_for (process_user_cmd w) ;
    Deferred.all_unit [
      Pipe.iter r ~f:begin fun msg ->
        Log_async.app (fun m -> m "%a" Sexplib.Sexp.pp_hum (sexp_of_t msg))
      end
    ]
  end

let cmd =
  Command.async ~summary:"BFX shell" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ]
  end

let () =
  Command.run cmd
