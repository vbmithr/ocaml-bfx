module Yojson_encoding : sig
  val construct : 't Json_encoding.encoding -> 't -> Json_repr.yojson
  val destruct : 't Json_encoding.encoding -> Json_repr.yojson -> 't
  val custom :
    ('t -> Json_repr.yojson) ->
    (Json_repr.yojson -> 't) ->
    schema:Json_schema.schema -> 't Json_encoding.encoding
  val destruct_safe : 'a Json_encoding.encoding -> Json_repr.yojson -> 'a
end

module Uuidm : sig
  include module type of Uuidm
    with type t = Uuidm.t

  val t_of_sexp : Sexplib.Sexp.t -> Uuidm.t
  val sexp_of_t : Uuidm.t -> Sexplib.Sexp.t
  val encoding : t Json_encoding.encoding
end

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span

  val t_of_sexp : Sexplib.Sexp.t -> Ptime.t
  val sexp_of_t : Ptime.t -> Sexplib.Sexp.t
  val encoding : t Json_encoding.encoding
end

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

  val create_spec : exchange -> kind -> tif -> spec
end

module Pair : sig
  type t = {
    base: string ;
    quote: string ;
  }  [@@deriving sexp]

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_string : string -> t option
  val of_string_exn : string -> t
  val encoding : t Json_encoding.encoding
end
