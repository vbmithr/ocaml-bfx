open Bfx

val public_url : Uri.t
val auth_url   : Uri.t

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

type error = {
  code: int ;
  msg: string ;
} [@@deriving sexp]

type t =
  | Version of version
  | Error of error
  | Info of Info_message.t
  | Ping of float
  | Pong of float * Ptime.t
  | Subscribe of feed
  | Unsubscribe of int
  | Subscribed of int * feed
  | Unsubscribed of int
  | Heartbeat of int
  | TradesSnap of int * Trade.t list
  | Trade of int * [`Executed | `Updated] * Trade.t
  | QuotesSnap of int * Quote.t list
  | Quote of int * Quote.t
[@@deriving sexp]

val subscribe : feed -> t

val encoding : t Json_encoding.encoding
val of_string : ?buf:Bi_outbuf.t -> string -> t
val to_string : ?buf:Bi_outbuf.t -> t -> string


