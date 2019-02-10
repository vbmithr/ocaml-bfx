open Core
open Async

open Bfx

module Ev : sig
  type t = {
    name: string;
    fields: Yojson.Safe.t String.Map.t
  }

  val create :
    name:string -> fields:(string * Yojson.Safe.t) list -> t

  val of_yojson :
    Yojson.Safe.t -> (t, string) Result.t

  val pp :
    Format.formatter -> t -> unit
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
    msg: Yojson.Safe.t list
  }

  val of_yojson : Yojson.Safe.t -> t
end

module Ticker : sig
  type t = {
    ts : Time_ns.t;
    bid : float;
    bidSize : float;
    ask : float;
    askSize : float;
    dailyChg : float;
    dailyChgPct : float;
    last : float;
    vol : float;
    high : float;
    low : float;
  }
  val create :
    ts:Time_ns.t ->
    bid:float ->
    bidSize:float ->
    ask:float ->
    askSize:float ->
    dailyChg:float ->
    dailyChgPct:float ->
    last:float -> vol:float -> high:float -> low:float -> t
  val of_yojson : ts:Time_ns.t -> [< Yojson.Safe.t ] sexp_list -> t
end

module Book : sig
  module Raw : sig
    type t = {
      id: int;
      price: float;
      amount: float;
    }

    val of_yojson : Yojson.Safe.t list -> t
  end

  type t = {
    price: float;
    count: int;
    amount: float;
  }

  val of_yojson : Yojson.Safe.t list -> t
end

module Trade : sig
  type t = { ts : Time_ns.t; price : float; amount : float; }
  val create : ts:Time_ns.t -> price:float -> amount:float -> t
  val of_yojson : [< Yojson.Safe.t > `Int `String ] sexp_list -> t
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

    val of_yojson : Yojson.Safe.t -> t
  end

  module Wallet : sig
    type t = {
      name : [ `Deposit | `Exchange | `Trading ];
      currency : string;
      balance : float;
      interest_unsettled : float;
    }
    val create :
      name:[ `Deposit | `Exchange | `Trading ] ->
      currency:string -> balance:float -> interest_unsettled:float -> t
    val name_of_string : string -> [> `Deposit | `Exchange | `Trading ]
    val of_yojson : [< Yojson.Safe.t ] -> t
  end

  module Order : sig
    type kind = [ `Limit | `Market | `Stop ]
    type tif = [ `Day | `Fill_or_kill | `Good_till_canceled ]
    type status = [ `Canceled | `Filled | `Open | `Partially_filled ]
    type exchange = Order.exchange = Margin | Exchange
    type spec = Order.spec = { exchange : exchange; kind : kind; tif : tif; }
    val create_spec : exchange -> kind -> tif -> spec
    val spec_of_string : string -> spec
    val status_of_string :
      string -> [> `Canceled | `Filled | `Open | `Partially_filled ]
    type t = {
      id : int;
      pair : string;
      amount : float;
      amount_orig : float;
      spec : spec;
      status : Bfx.Order.status;
      price : float;
      avg_price : float;
      created_at : Time_ns.t;
      notify : bool;
      hidden : bool;
      oco : int sexp_option;
    }
    val create :
      ?oco:int ->
      id:int ->
      pair:string ->
      amount:float ->
      amount_orig:float ->
      spec:spec ->
      status:Bfx.Order.status ->
      price:float ->
      avg_price:float ->
      created_at:Time_ns.t -> notify:bool -> hidden:bool -> unit -> t
    val of_yojson : [< Yojson.Safe.t ] -> t
  end

  module Trade : sig
    type t = {
      id : int;
      pair : string;
      ts : Time_ns.t;
      order_id : int;
      amount : float;
      price : float;
      spec : Order.spec;
      order_price : float;
      fee : float;
      fee_currency : string;
    }
    val create :
      id:int ->
      pair:string ->
      ts:Time_ns.t ->
      order_id:int ->
      amount:float ->
      price:float ->
      spec:Order.spec ->
      order_price:float -> fee:float -> fee_currency:string -> t
    val of_yojson : [< Yojson.Safe.t ] -> t
  end
end

val open_connection :
  ?buf:Bi_outbuf.t ->
  ?auth:string * string ->
  ?to_ws:Ev.t Pipe.Reader.t ->
  unit -> Yojson.Safe.t Pipe.Reader.t

module V2 : sig
  module Event : sig
    type t =
        Info
      | Ping
      | Pong
      | Conf
      | Subscribe
      | Subscribed
      | Error
      | Unsubscribe
      | Unsubscribed
    val encoding : t Json_encoding.encoding
  end

  type error =
    | Unknown_event
    | Unknown_pair

  module Info_message : sig
    module Code : sig
      type t =
        | Please_reconnect
        | Maintenance_start
        | Maintenance_end
    end

    type t = {
      code : Code.t ;
      msg : string ;
    }
  end

  type t =
    | Version of int
    | Info of Info_message.t
    | Ping
    | Pong
  [@@deriving sexp]

  val open_connection :
    ?buf:Bi_outbuf.t ->
    ?auth:string * string ->
    ?to_ws:t Pipe.Reader.t -> unit ->
    t Pipe.Reader.t
end
