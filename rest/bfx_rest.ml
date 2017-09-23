open Core
open Async

open Bfx

open Cohttp_async
module Yojson_encoding = Json_encoding.Make(Json_repr.Yojson)

let base_uri = Uri.of_string "https://api.bitfinex.com"

exception HTTP_Error of string
exception JSON_Error of string

let handle_rest_call (resp, body) =
  Body.to_string body >>| fun body_str ->
  let status = Response.status resp in
  match status with
  | `OK ->
    Result.try_with (fun () -> Yojson.Safe.(from_string body_str))
  | #Cohttp.Code.status_code as status ->
    Result.fail @@ HTTP_Error Cohttp.Code.(string_of_status status)

module Sym = struct
  type t = {
    pair: string;
    price_precision: int;
    initial_margin: string;
    minimum_margin: string;
    maximum_order_size: string;
    minimum_order_size: string;
    expiration: string;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { pair ; price_precision ; initial_margin ; minimum_margin ;
             maximum_order_size ; minimum_order_size ; expiration } ->
        (pair, price_precision, initial_margin, minimum_margin,
         maximum_order_size, minimum_order_size, expiration))
      (fun (pair, price_precision, initial_margin, minimum_margin,
        maximum_order_size, minimum_order_size, expiration) ->
       { pair ; price_precision ; initial_margin ; minimum_margin ;
         maximum_order_size ; minimum_order_size ; expiration })
      (obj7
         (req "pair" string)
         (req "price_precision" int)
         (req "initial_margin" string)
         (req "minimum_margin" string)
         (req "maximum_order_size" string)
         (req "minimum_order_size" string)
         (req "expiration" string))


  let get () =
    let uri = Uri.with_path base_uri "/v1/symbols_details" in
    Client.get uri >>= handle_rest_call >>| function
    | Error exn -> raise exn
    | Ok (`List syms) ->
      List.map syms ~f:(Yojson_encoding.destruct encoding)
    | Ok #Yojson.Safe.json -> invalid_arg "get_syms"
end

module Trades = struct
  type t = {
    ts: Time_ns.t;
    price: float;
    qty: float;
    side: Side.t;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { ts ; price ; qty ; side } -> ((), (ts, price, qty, side)))
      (fun ((), (ts, price, qty, side)) -> { ts ; price ; qty ; side })
      (merge_objs unit
         (obj4
            (req "timestamp" time_encoding)
            (req "price" fstring_encoding)
            (req "amount" fstring_encoding)
            (req "type" Side.encoding)))

  let get_exn ?log ?(start=Time_ns.epoch) ?(count=50) pair =
    let q = [
      "timestamp", time_to_sec start |> Int.to_string;
      "limit_trades", Int.to_string count;
    ] in
    let uri = Uri.(with_query' (with_path base_uri @@ "/v1/trades/" ^ pair) q) in
    Option.iter log ~f:(fun log -> Log.debug log "GET %s" (Uri.to_string uri));
    Client.get uri >>= handle_rest_call >>| function
    | Error exn -> raise exn
    | Ok (`List ts) -> List.rev_map ts ~f:(Yojson_encoding.destruct encoding)
    | Ok #Yojson.Safe.json -> invalid_arg "Rest.Trades.get"
end

module Priv = struct
  let post_exn ?buf ?log ~key ~secret ~endp ~body () =
    Option.iter log ~f:(fun log -> Log.debug  log "-> %s" Yojson.Safe.(to_string ?buf body));
    let uri = Uri.with_path base_uri endp in
    let nonce = Time_ns.(now () |> to_int63_ns_since_epoch) |> Int63.to_string in
    let payload =
      match body with
      | `Assoc params ->
        `Assoc (["request", `String endp;
                 "nonce", `String nonce;
                ] @ params)
      | _ -> invalid_arg "bitfinex post body must be a json dict"
    in
    let body = Yojson.Safe.to_string ?buf payload in
    let body_b64 = B64.encode body in
    let signature =
      Digestif.SHA384.Bytes.hmac ~key:secret body_b64 |> Hex.of_string in
    let headers = Cohttp.Header.of_list
        ["X-BFX-APIKEY", key;
         "X-BFX-PAYLOAD", body_b64;
         "X-BFX-SIGNATURE", match signature with `Hex sign -> sign;
        ] in
    let body = Cohttp_async.Body.of_string body in
    Client.post ~headers ~body uri >>= fun (resp, body) ->
    Body.to_string body >>| fun body ->
    Option.iter log ~f:(fun log -> Log.debug log "<- %s" body);
    Yojson.Safe.from_string ?buf body

  module Order = struct
    let spec_encoding =
      let open Order in
      let open Json_encoding in
      string_enum [
        "market", create_spec Margin `Market `Good_till_canceled ;
        "limit" , create_spec Margin `Limit `Good_till_canceled ;
        "stop", create_spec  Margin `Stop `Good_till_canceled ;
        "trailing-stop", create_spec  Margin `Market `Good_till_canceled ;
        "fill-or-kill", create_spec  Margin `Limit `Fill_or_kill ;
        "exchange market", create_spec  Exchange `Market `Good_till_canceled ;
        "exchange limit", create_spec Exchange `Limit `Good_till_canceled ;
        "exchange stop", create_spec Exchange `Stop `Good_till_canceled ;
        "exchange trailing-stop", create_spec Exchange `Market `Good_till_canceled ;
        "exchange fill-or-kill", create_spec Exchange `Limit `Fill_or_kill
      ]

    let string_of_buy_sell = function
      | `Buy -> "buy"
      | `Sell -> "sell"

    let string_of_ord_type_tif ord_type tif = match ord_type, tif with
      | `Market, _ -> "market"
      | `Stop, _ -> "stop"
      | `Limit, `Day
      | `Limit, `Good_till_canceled -> "limit"
      | `Limit, `Fill_or_kill -> "fill-or-kill"

    let submit_exn
        ?buf ?log
        ~key ~secret
        ~side ~tif ~kind ~symbol ~price ~qty =
      let side = Option.value_exn ~message:"side is unset" side in
      let side = string_of_buy_sell side in
      let typ = string_of_ord_type_tif kind tif in
      let price = if kind = `Market then 1. else price in
      let body = `Assoc [
          "symbol", `String symbol;
          "exchange", `String "bitfinex";
          "price", `String (Printf.sprintf "%.5f" price);
          "amount", `String (Printf.sprintf "%.2f" qty);
          "side", `String side;
          "type", `String typ;
        ]
      in
      post_exn ?buf ?log ~key ~secret ~endp:"/v1/order/new" ~body

    let cancel_exn ?buf ?log ~key ~secret oid =
      let body = `Assoc ["order_id", `Int oid] in
      post_exn ?buf ?log ~key ~secret ~endp:"/v1/order/cancel" ~body

    module Response = struct
      type t = {
        id: int;
        symbol: string;
        price: float;
        avg_execution_price: float;
        side: Side.t;
        spec: Order.spec ;
        ts: Time_ns.t;
        is_live: bool;
        is_hidden: bool;
        is_canceled: bool;
        oco_order: int option;
        was_forced: bool;
        original_amount: float;
        remaining_amount: float;
        executed_amount: float;
      }

      let encoding =
        let open Json_encoding in
        conv
          (fun _ -> invalid_arg "Response.encoding: Not implemented")
          (fun ((), ((id, symbol, price, avg_execution_price, side,
                      spec, ts, is_live, is_canceled, is_hidden),
                     (oco_order, was_forced, original_amount, remaining_amount,
                      executed_amount))) -> {
              id ; symbol ; price ; avg_execution_price ; side ; spec ;
              ts ; is_live ; is_hidden ; is_canceled ; oco_order ;
              was_forced ; original_amount ; remaining_amount ;
              executed_amount })
          (merge_objs unit
             (merge_objs
                (obj10
                   (req "id" int)
                   (req "symbol" string)
                   (req "price" fstring_encoding)
                   (req "avg_execution_price" fstring_encoding)
                   (req "side" Side.encoding)
                   (req "type" spec_encoding)
                   (req "timestamp" time_encoding)
                   (req "is_live" bool)
                   (req "is_cancelled" bool)
                   (req "is_hidden" bool)
                )
                (obj5
                   (req "oco_order" (option int))
                   (req "was_forced" bool)
                   (req "original_amount" fstring_encoding)
                   (req "remaining_amount" fstring_encoding)
                   (req "executed_amount" fstring_encoding))))
    end
  end
end
