open Bfx
open Json_encoding

(* let src = Logs.Src.create "bfx.rest" *)

let public_url =
  Uri.make ~scheme:"https" ~host:"api-pub.bitfinex.com" ()

type ticker = {
  symbol: Pair.t ;
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

let ticker =
  conv
    (fun { symbol; bid ; bid_size ; ask ; ask_size ; daily_change ; daily_change_pct ;
           last_price ; volume ; high ; low } ->
      ((symbol, bid, bid_size, ask, ask_size, daily_change, daily_change_pct,
        last_price, volume, high), low))
    (fun ((symbol, bid, bid_size, ask, ask_size, daily_change, daily_change_pct,
           last_price, volume, high), low) ->
      { symbol; bid ; bid_size ; ask ; ask_size ; daily_change ; daily_change_pct ;
        last_price ; volume ; high ; low })
    (merge_tups
       (tup10 Pair.encoding float float float float float float float float float)
       (tup1 float))

let ignore_funding_enc =
  union [
    case ticker (fun a -> a) (fun a -> Some a) ;
    case unit (fun _ -> None) (fun () -> None) ;
  ]

let always_ok_enc enc =
  conv
    (function Ok v -> v | Error _ -> invalid_arg "always_ok_enc")
    (fun v -> Ok v) enc

let list_not_null e =
  conv
    (fun _ -> assert false)
    (fun l -> List.filter_map (fun a -> a) l)
    (list e)

let tickers =
  Fastrest.get (always_ok_enc (list_not_null ignore_funding_enc))
    Uri.(with_query' (with_path public_url "v2/tickers") ["symbols", "ALL"])
