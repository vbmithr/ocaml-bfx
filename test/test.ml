open Core
open Async

open Bfx_rest

module Cfg = struct
  type cfg = {
    key: string ;
    secret: string ;
    passphrase: string [@default ""];
    quote: (string * int) list [@default []];
  } [@@deriving sexp]

  type t = (string * cfg) list [@@deriving sexp]
end

(* let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
 * let cfg =
 *   List.Assoc.find_exn ~equal:String.equal
 *     (Sexplib.Sexp.load_sexp_conv_exn default_cfg Cfg.t_of_sexp) "BFX" *)

let wrap_request ?(speed=`Quick) n service =
  (* let auth = {
   *   Fastrest.key = cfg.Cfg.key ;
   *   secret = Base64.decode_exn cfg.Cfg.secret ;
   *   meta = [] ;
   * } in *)
  Alcotest_async.test_case
    ~timeout:(Time.Span.of_int_sec 10) n speed begin fun () ->
    Deferred.ignore_m (Fastrest.request service)
  end

let rest = [
  wrap_request "tickers" tickers ;
]

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug) ;
  Alcotest.run "bitfinex" [
    "rest", rest ;
  ]
