open Bfx

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

val ticker : ticker Json_encoding.encoding
val tickers : (Fastrest.form, ticker list) Fastrest.service
