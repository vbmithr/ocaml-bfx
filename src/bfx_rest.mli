open Core
open Async

open Bfx

module Sym : sig
  type t = {
    pair: string;
    price_precision: int;
    initial_margin: string;
    minimum_margin: string;
    maximum_order_size: string;
    minimum_order_size: string;
    expiration: string;
  }

  val encoding : t Json_encoding.encoding
  val get : unit -> t list Deferred.t
end

(* module Trades : sig
 *   type t = {
 *     ts: Time_ns.t;
 *     price: float;
 *     qty: float;
 *     side: Side.t;
 *   }
 * 
 *   val encoding : t Json_encoding.encoding
 *   val get_exn : ?start:Time_ns.t -> ?count:int -> string -> t list Deferred.t
 * end *)

module Order : sig
  val spec_encoding : Bfx.Order.spec Json_encoding.encoding
  val string_of_buy_sell : [< `Buy | `Sell ] -> string
  val string_of_ord_type_tif :
    [< `Limit | `Market | `Stop ] ->
    [< `Day | `Fill_or_kill | `Good_till_canceled ] -> string
  val submit_exn :
    ?buf:Bi_outbuf.t ->
    key:string ->
    secret:string ->
    side:[< `Buy | `Sell ] sexp_option ->
    tif:[< `Day | `Fill_or_kill | `Good_till_canceled ] ->
    kind:[< `Limit | `Market | `Stop > `Market ] ->
    symbol:string ->
    price:float -> qty:float -> unit -> Yojson.Safe.t Deferred.t
  val cancel_exn :
    ?buf:Bi_outbuf.t ->
    key:string -> secret:string -> int -> unit -> Yojson.Safe.t Deferred.t
  module Response :
  sig
    type t = {
      id : int;
      symbol : string;
      price : float;
      avg_execution_price : float;
      side : Side.t;
      spec : Bfx.Order.spec;
      ts : Time_ns.t;
      is_live : bool;
      is_hidden : bool;
      is_canceled : bool;
      oco_order : int sexp_option;
      was_forced : bool;
      original_amount : float;
      remaining_amount : float;
      executed_amount : float;
    }
    (* val encoding : t Json_encoding.encoding *)
  end
end
