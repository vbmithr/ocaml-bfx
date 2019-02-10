open Core
open Async

let src = Logs.Src.create "bfx.cli"

module Ws = Bfx_ws.V2

(* let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
 * let find_auth cfg exchange =
 *   let cfg = Sexplib.Sexp.load_sexp_conv_exn cfg Cfg.t_of_sexp in
 *   let { Cfg.key; secret } =
 *     List.Assoc.find_exn ~equal:String.equal cfg exchange in
 *   key, secret *)

let main () =
  (* let dft_log = Lazy.force log in *)
  let to_ws, _to_ws_w = Pipe.create () in
  (* let connected = Condition.create () in *)
  (* don't_wait_for begin
   *   Condition.wait connected >>= fun () ->
   *   Deferred.List.iter topics ~f:(fun t -> Pipe.write to_ws_w (Ws.Repr.Subscribe t))
   * end ; *)
  let ws_r = Ws.open_connection ~to_ws () in
  let transfer_f msg =
      Format.asprintf "%a@." Sexplib.Sexp.pp_hum (Ws.sexp_of_t msg)
  in
  Deferred.all_unit [
    Pipe.transfer ws_r Writer.(pipe @@ Lazy.force stderr) ~f:transfer_f
  ]

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
