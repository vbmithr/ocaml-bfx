module Msg : sig
  type channel =
    | Book
    | Trades
    | Ticker

  type t = {
    chan: int;
    msg: Yojson.Safe.json list
  }
end
