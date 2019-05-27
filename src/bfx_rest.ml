open Bfx

(* let src = Logs.Src.create "bfx.rest" *)

let public_url =
  Uri.make ~scheme:"https" ~host:"api-pub.bitfinex.com" ()

module Ticker = struct
  type t = {
    bid: float ;
    bid_size: float ;
    ask: float ;
    ask_size: float ;
    daily_change: float ;
    daily_change_pct: float ;
    last_price: float ;
    volume: float ;
    high: float ;
    low: float ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { bid ; bid_size ; ask ; ask_size ; daily_change ; daily_change_pct ;
             last_price ; volume ; high ; low } ->
        (bid, bid_size, ask, ask_size, daily_change, daily_change_pct,
         last_price, volume, high, low))
      (fun (bid, bid_size, ask, ask_size, daily_change, daily_change_pct,
            last_price, volume, high, low) ->
        { bid ; bid_size ; ask ; ask_size ; daily_change ; daily_change_pct ;
          last_price ; volume ; high ; low })
      (tup10 float float float float float float float float float float)

end

let ignore_funding_enc =
  let open Json_encoding in
  union [
    case (merge_tups (tup1 Pair.encoding) Ticker.encoding) (fun a -> a) (fun (a, b) -> Some (a, b)) ;
    case unit (fun _ -> None) (fun () -> None) ;
  ]

let always_ok_enc enc =
  let open Json_encoding in
  conv
    (function Ok v -> v | Error _ -> invalid_arg "always_ok_enc")
    (fun v -> Ok v) enc

let tickers =
  let open Json_encoding in
  Fastrest.get (always_ok_enc (list ignore_funding_enc))
    Uri.(with_query' (with_path public_url "v2/tickers") ["symbols", "ALL"])
