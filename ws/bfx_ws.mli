open Core
open Async

module Ev : sig
  type t = {
    name: string;
    fields: Yojson.Safe.json String.Map.t
  }

  val create :
    name:string -> fields:(string * Yojson.Safe.json) list -> t

  val of_yojson :
    Yojson.Safe.json -> (t, string) Result.t
end

module Msg : sig
  type channel =
    | Book
    | Trades
    | Ticker

  val channel_of_string : string -> channel
  val channel_to_string : channel -> string

  type chan_descr = {
    chan: channel;
    pair: string;
  }

  val create_chan_descr :
    chan:channel -> pair:string -> chan_descr

  type t = {
    chan: int;
    msg: Yojson.Safe.json list
  }

  val of_yojson : Yojson.Safe.json -> t
end

module Book : sig
  module Raw : sig
    type t = {
      id: int;
      price: float;
      amount: float;
    }

    val of_yojson : Yojson.Safe.json list -> t
  end

  type t = {
    price: float;
    count: int;
    amount: float;
  }

  val of_yojson : Yojson.Safe.json list -> t
end

module Trade : sig
end

module Priv : sig
  type update_type =
    | Snapshot
    | Update
    | New
    | Cancel
    | Execution

  type msg_type =
    | Heartbeat
    | Position
    | Wallet
    | Order
    | HistoricalOrder
    | Trade

  val types_of_msg : string -> msg_type * update_type

  module Position : sig
    type t = {
      pair: string;
      status: [`Active | `Closed];
      amount: float;
      base_price: float;
      margin_funding: float;
      funding_type: [`Daily | `Term]
    }

    val of_yojson : Yojson.Safe.json -> t
  end
end

val open_connection :
  ?buf:Bi_outbuf.t ->
  ?auth:string * Cstruct.t ->
  ?log:Log.t ->
  ?to_ws:Ev.t Pipe.Reader.t ->
  unit -> Yojson.Safe.json Pipe.Reader.t
