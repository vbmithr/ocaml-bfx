open Core
open Async

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
