open Sexplib.Std

module Yojson_encoding = struct
  include Json_encoding.Make(Json_repr.Yojson)

  let pp_json ppf t = Yojson.Safe.pretty_print ppf t
  let pp_decode_error ppf t =
    Json_encoding.print_error ?print_unknown:None ppf t

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Logs.err begin fun m ->
        m "%a@.%a@." pp_json value pp_decode_error exn
      end ;
      raise exn
end

module Uuidm = struct
  include Uuidm

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_string sexp_str with
    | None -> invalid_arg "Uuidm.t_of_sexp"
    | Some u -> u

  let sexp_of_t t =
    sexp_of_string (to_string t)

  let encoding =
    let open Json_encoding in
    conv
      (fun t -> to_string t)
      (fun s -> match of_string s with
         | None -> invalid_arg "encoding"
         | Some u -> u)
      string
end

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)

  let encoding =
    let open Json_encoding in
    conv
      (fun a -> Int64.of_float (Ptime.to_float_s a))
      (fun ts -> match Ptime.of_float_s (Int64.to_float ts /. 1e3) with
         | None -> invalid_arg "Ptime.encoding"
         | Some ts -> ts)
      int53
end

module Side = struct
  type t = [`Buy | `Sell]

  let encoding =
    let open Json_encoding in
    string_enum [
      "buy", `Buy ;
      "sell", `Sell ;
    ]
end

module Order = struct
  type kind = [`Market | `Limit | `Stop]
  type tif = [`Day | `Good_till_canceled | `Fill_or_kill]
  type status = [`Open | `Filled | `Partially_filled | `Canceled]

  type exchange =
    | Margin
    | Exchange

  type spec = {
    exchange : exchange ;
    kind : kind ;
    tif : tif ;
  }

  let create_spec exchange kind tif =
    { exchange ; kind ; tif }
end

module Pair = struct
  type t = {
    base: string ;
    quote: string ;
  } [@@deriving sexp]

  let compare { base ; quote } { base = base' ; quote = quote' } =
    match String.compare base base' with
    | 0 -> String.compare quote quote'
    | n -> n

  let pp ppf { base ; quote } =
    Format.fprintf ppf "t%s%s" base quote

  let to_string { base ; quote } =
    "t" ^ base ^ quote

  let of_string_noprefix_exn s =
    try
      { base = String.sub s 0 3 ; quote = String.sub s 3 3 }
    with _ -> invalid_arg "Pair.of_string_noprefix_exn"

  let to_string_noprefix { base ; quote } = base ^ quote

  let of_string s =
    if String.length s <> 7 then None
    else Some { base = String.sub s 1 3 ; quote = String.sub s 4 3 }

  let of_string_exn s =
    if String.length s <> 7 then invalid_arg "of_string_exn"
    else { base = String.sub s 1 3 ; quote = String.sub s 4 3 }

  let encoding =
    Json_encoding.(conv to_string of_string_exn string)
end
