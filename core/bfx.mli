open Core

module Side : sig
  type t = [`Buy | `Sell]

  val encoding : t Json_encoding.encoding
end

module Order : sig
  type kind = [`Market | `Limit | `Stop]
  type tif = [`Day | `Good_till_canceled | `Fill_or_kill]
  type status = [`Open | `Filled | `Partially_filled | `Canceled]

  type exchange = Margin | Exchange

  type spec = {
    exchange : exchange ;
    kind : kind ;
    tif : tif ;
  }

  val create_spec :
    exchange:exchange -> kind:kind -> tif:tif -> spec
end



val time_to_sec : Time_ns.t -> int
val time_of_sec : int -> Time_ns.t

val time_encoding : Time_ns.t Json_encoding.encoding
val fstring_encoding : float Json_encoding.encoding
