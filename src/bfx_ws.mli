open Bfx

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

type version = {
  version: int ;
  serverId: Uuidm.t ;
  platform: [`Operative | `Maintenance] ;
} [@@deriving sexp]

type feed =
  | Trades of Pair.t
  | Quotes of Pair.t
[@@deriving sexp]

module Trade : sig
  type t = {
    id: int64 ;
    ts: Ptime.t ;
    qty: float ;
    price: float ;
  } [@@deriving sexp]

  val encoding : t Json_encoding.encoding
end

module Quote : sig
  type t = {
    id: int64 ;
    price: float ;
    qty: float ;
  } [@@deriving sexp]

  val encoding : t Json_encoding.encoding
end

type t =
  | Version of version
  | Info of Info_message.t
  | Ping of int32
  | Pong of int32 * Ptime.t
  | Subscribe of feed
  | Subscribed of int * feed
  | Heartbeat of int
  | TradesSnap of int * Trade.t list
  | Trade of int * [`Executed | `Updated] * Trade.t
  | QuotesSnap of int * Quote.t list
  | Quote of int * Quote.t
[@@deriving sexp]

val encoding : t Json_encoding.encoding
