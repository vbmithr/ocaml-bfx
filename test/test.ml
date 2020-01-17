open Core
open Async

open Bfx_rest

let wrap_request ?(speed=`Quick) n service =
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
