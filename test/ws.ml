open Core
open Async
open Log.Global

module Ws = Bfx_ws.V2

(* let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
 * let find_auth cfg exchange =
 *   let cfg = Sexplib.Sexp.load_sexp_conv_exn cfg Cfg.t_of_sexp in
 *   let { Cfg.key; secret } =
 *     List.Assoc.find_exn ~equal:String.equal cfg exchange in
 *   key, secret *)

let base_spec =
  let open Command.Spec in
  empty
  (* +> flag "-cfg" (optional_with_default default_cfg string) ~doc:"path Filepath of cfg (default: ~/.virtu)" *)
  +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"
  +> anon (sequence ("topic" %: string))

let main () =
  (* let dft_log = Lazy.force log in *)
  let to_ws, to_ws_w = Pipe.create () in
  (* let connected = Condition.create () in *)
  (* don't_wait_for begin
   *   Condition.wait connected >>= fun () ->
   *   Deferred.List.iter topics ~f:(fun t -> Pipe.write to_ws_w (Ws.Repr.Subscribe t))
   * end ; *)
  let ws_r = Ws.open_connection ~log:(Lazy.force log) ~to_ws () in
  let transfer_f msg =
      Format.asprintf "%a@." Sexplib.Sexp.pp_hum (Ws.sexp_of_t msg)
  in
  Deferred.all_unit [
    Pipe.transfer ws_r Writer.(pipe @@ Lazy.force stderr) ~f:transfer_f
  ]

let loglevel_of_int = function 2 -> `Info | 3 -> `Debug | _ -> `Error

let cmd =
  let run loglevel topics =
    Option.iter loglevel ~f:(Fn.compose set_level loglevel_of_int);
    (* let key, secret = find_auth cfg "PLNX" in *)
    don't_wait_for @@ main ();
    never_returns @@ Scheduler.go ()
  in
  Command.async ~summary:"BFX shell" base_spec run

let () =
  Command.run cmd
