open Bfx

module Ticker : sig
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

  val encoding : t Json_encoding.encoding
end

val tickers :
  (Fastrest.form, (Pair.t * Ticker.t) option list, unit) Fastrest.service
