open Core
open Async
open Bfx_rest
open Alcotest_async

let wrap_request ?(speed=`Quick) n service =
  test_case
    ~timeout:(Time.Span.of_int_sec 10) n speed begin fun () ->
    Deferred.ignore_m (Fastrest.request service)
  end

let rest = [
  wrap_request "tickers" tickers ;
]

let main () =
  run "bitfinex" [
    "rest", rest ;
  ]

let () =
  don't_wait_for (main ()) ;
  never_returns (Scheduler.go ())
