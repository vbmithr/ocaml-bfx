open Core

let exchange = "BITFINEX"

module Side = struct
  type t = [`Buy | `Sell]

  let encoding =
    let open Json_encoding in
    string_enum [
      "buy", `Buy ;
      "sell", `Sell ;
    ]
end

let side_of_amount amount = match Float.sign_exn amount with
  | Pos -> `Buy
  | Neg -> `Sell
  | Zero -> invalid_arg "side_of_amount"

let time_to_sec t = Time_ns.to_int_ns_since_epoch t / 1_000_000_000
let time_of_sec s = Time_ns.of_int_ns_since_epoch (s * 1_000_000_000)

let time_encoding = Json_encoding.(conv time_to_sec time_of_sec int)

let fstring_encoding =
  let open Json_encoding in
  let open Float in
  conv to_string of_string string


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

  let create_spec ~exchange ~kind ~tif = { exchange ; kind ; tif }
end
