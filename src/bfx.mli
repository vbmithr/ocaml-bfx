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
  type t = Fixtypes.Side.t

  val encoding : t Json_encoding.encoding
end

module Order : sig
  type exchange = Margin | Exchange

  type spec = {
    exchange : exchange ;
    kind : Fixtypes.OrdType.t ;
    tif : Fixtypes.TimeInForce.t ;
  }

  val create_spec :
    exchange -> Fixtypes.OrdType.t -> Fixtypes.TimeInForce.t -> spec
end

module Pair : sig
  type t = {
    base: string ;
    quote: string ;
  }  [@@deriving sexp]

  module Set : Set.S with type elt := t
  module Map : Map.S with type key := t
  module Table : Hashtbl.S with type key := t

  val hash : t -> int
  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val to_string_noprefix : t -> string
  val of_string : string -> t option
  val of_string_exn : string -> t
  val of_string_noprefix_exn : string -> t
  val encoding : t Json_encoding.encoding
end
