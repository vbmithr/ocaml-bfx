open Core
open Async

open Bfx
open Bfx_ws

let src = Logs.Src.create "bfx.cli"

let process_user_cmd w =
  let process s =
    match String.split s ~on:' ' with
    | ["ping"] ->
      let cid = Random.int32 Int32.max_value in
      Pipe.write w (Ping cid)
    | ["trades" ; symbol ] ->
      Pipe.write w (Subscribe (Trades (Pair.of_string_exn symbol)))
    | ["book" ; symbol ] ->
      Pipe.write w (Subscribe (Book (Pair.of_string_exn symbol, `Level25)))
    | h :: _ ->
      Logs_async.err (fun m -> m "Unknown command %s" h)
    | [] ->
      Logs_async.err ~src (fun m -> m "Empty command")
  in
  let rec loop () = Reader.(read_line @@ Lazy.force stdin) >>= function
    | `Eof -> Deferred.unit
    | `Ok line -> process line >>= loop
  in
  loop ()

(* let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
 * let find_auth cfg exchange =
 *   let cfg = Sexplib.Sexp.load_sexp_conv_exn cfg Cfg.t_of_sexp in
 *   let { Cfg.key; secret } =
 *     List.Assoc.find_exn ~equal:String.equal cfg exchange in
 *   key, secret *)

let main () =
  (* let to_ws, _to_ws_w = Pipe.create () in
   * let connected = Condition.create () in
   * don't_wait_for begin
   *   Condition.wait connected >>= fun () ->
   *   Deferred.List.iter topics ~f:(fun t -> Pipe.write to_ws_w (Ws.Repr.Subscribe t))
   * end ; *)
  Bfx_ws_async.with_connection `Public begin fun r w ->
    don't_wait_for (process_user_cmd w) ;
    let transfer_f msg =
      Format.asprintf "%a@." Sexplib.Sexp.pp_hum (sexp_of_t msg) in
    Deferred.all_unit [
      Pipe.transfer r Writer.(pipe @@ Lazy.force stderr) ~f:transfer_f
    ]
  end

let cmd =
  Command.async ~summary:"BFX shell" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ]
  end

let () =
  Command.run cmd
