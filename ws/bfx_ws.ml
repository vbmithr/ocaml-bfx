open Core
open Async

open Bfx
module Yojson_encoding = Json_encoding.Make(Json_repr.Yojson)

let float_of_json = function
  | `Int v -> Float.of_int v
  | `Float v -> v
  | `Intlit v -> Float.of_string v
  | #Yojson.Safe.json as json ->
    invalid_arg Printf.(sprintf "float_of_json: %s" Yojson.Safe.(to_string json))

let bool_of_int = function
  | 1 -> true
  | 0 -> false
  | _ -> invalid_arg "Ws.bool_of_int"

let maybe_int = function
  | `Int i -> Some i
  | `Null -> None
  | #Yojson.Safe.json -> invalid_arg "Ws.maybe_int"

module Ev = struct
  type t = {
    name: string;
    fields: Yojson.Safe.json String.Map.t
  }

  let create ~name ~fields = { name; fields = String.Map.of_alist_exn fields }

  let of_yojson json =
    let equal = String.equal in
    match json with
    | `Assoc fields when List.Assoc.mem ~equal fields "event" ->
      create
        ~name:List.Assoc.(find_exn ~equal fields "event" |> Yojson.Safe.to_basic |> Yojson.Basic.Util.to_string)
        ~fields:List.Assoc.(remove ~equal fields "event")
        |> Result.return
    | #Yojson.Safe.json as s -> Result.failf "%s" @@ Yojson.Safe.to_string s

  let to_yojson { name; fields } =
    `Assoc (("event", `String name) :: String.Map.(to_alist fields))
end

module Msg = struct
  type channel =
    | Book
    | Trades
    | Ticker

  let channel_of_string = function
    | "book" -> Book
    | "trades" -> Trades
    | "ticker" -> Ticker
    | _ -> invalid_arg "channel_of_string"

  let channel_to_string = function
    | Book -> "book"
    | Trades -> "trades"
    | Ticker -> "ticker"

  type chan_descr = {
    chan: channel;
    pair: string;
  }

  let create_chan_descr ~chan ~pair = { chan ; pair }

  type t = {
    chan: int;
    msg: Yojson.Safe.json list
  }

  let create ~chan ~msg = { chan ; msg }

  let of_yojson = function
    | `List (`Int chan :: msg) -> create ~chan ~msg
    | #Yojson.Safe.json -> invalid_arg "Msg.on_yojson"
end

module Ticker = struct
  type t = {
    ts: Time_ns.t;
    bid: float;
    bidSize: float;
    ask: float;
    askSize: float;
    dailyChg: float;
    dailyChgPct: float;
    last: float;
    vol: float;
    high: float;
    low: float;
  }

  let create ~ts ~bid ~bidSize ~ask ~askSize ~dailyChg ~dailyChgPct ~last ~vol ~high ~low =
    { ts ; bid ; bidSize ; ask ; askSize ; dailyChg ; dailyChgPct ; last ; vol ; high ; low }

  let of_yojson ~ts msg =
    match List.map msg ~f:float_of_json with
    | [ bid; bidSize; ask; askSize; dailyChg; dailyChgPct; last; vol; high; low] ->
      create ts bid bidSize ask askSize dailyChg dailyChgPct last vol high low
    | _ -> invalid_arg "Ticker.of_yojson"
end

module Book = struct
  module Raw = struct
    type t = {
      id: int;
      price: float;
      amount: float;
    }

    let create ~id ~price ~amount = { id ; price ; amount }

    let of_yojson msg =
      match List.map msg ~f:float_of_json with
      | [id; price; amount] -> create ~id:(Int.of_float id) ~price ~amount
      | _ -> invalid_arg "Book.Raw.of_yojson"
  end

  type t = {
    price: float;
    count: int;
    amount: float;
  }

  let create ~price ~count ~amount = { price ; count ; amount }

  let of_yojson msg =
    match List.map msg ~f:float_of_json with
    | [price; count; amount] -> create ~price ~count:Int.(of_float count) ~amount
    | _ -> invalid_arg "Book.of_yojson"

end

module Trade = struct
  type t = {
    ts: Time_ns.t;
    price: float;
    amount: float;
  }

  let create ~ts ~price ~amount = { ts ; price ; amount }

  let of_yojson = function
    | (`String _) :: tl
    | (`Int _) :: tl -> begin
        match List.map tl ~f:float_of_json with
        | [ ts; price; amount ] ->
          let ts = Int.(of_float ts) * 1_000_000_000 |> Time_ns.of_int_ns_since_epoch in
          create ~ts ~price ~amount
        | _ -> invalid_arg "Trade.of_yojson"
      end
    | _ -> invalid_arg "Trade.of_yojson"
end

module Priv = struct
  type update_type =
    | Snapshot
    | Update
    | New
    | Cancel
    | Execution

  type msg_type =
    | Heartbeat
    | Position
    | Wallet
    | Order
    | HistoricalOrder
    | Trade

  let types_of_msg : string -> msg_type * update_type = function
    | "hb" -> Heartbeat, Update
    | "ps" -> Position, Snapshot
    | "ws" -> Wallet, Snapshot
    | "os" -> Order, Snapshot
    | "hos" -> HistoricalOrder, Snapshot
    | "ts" -> Trade, Snapshot
    | "on" -> Order, New
    | "ou" -> Order, Update
    | "oc" -> Order, Cancel
    | "pn" -> Position, New
    | "pu" -> Position, Update
    | "pc" -> Position, Cancel
    | "wu" -> Wallet, Update
    | "te" -> Trade, Execution
    | "tu" -> Trade, Update
    | _ -> invalid_arg "Ws.Priv.types_of_msg"

  module Position = struct
    type t = {
      pair: string;
      status: [`Active | `Closed];
      amount: float;
      base_price: float;
      margin_funding: float;
      funding_type: [`Daily | `Term]
    }

    let create ~pair ~status ~amount ~base_price ~margin_funding ~funding_type =
      { pair ; status ; amount ; base_price ; margin_funding ; funding_type }

    let of_yojson = function
      | `List [`String pair; `String status; amount; base_price;
               margin_funding; `Int funding_type] ->
        let amount = float_of_json amount in
        let base_price = float_of_json base_price in
        let margin_funding = float_of_json margin_funding in
        let status = match status with "ACTIVE" -> `Active | _ -> `Closed in
        let funding_type = match funding_type with 0 -> `Daily | _ -> `Term in
        create ~pair ~status ~amount ~base_price ~margin_funding ~funding_type
      | #Yojson.Safe.json -> invalid_arg "Position.of_yojson"
  end

  module Wallet = struct
    type t = {
      name: [`Trading | `Exchange | `Deposit];
      currency: string;
      balance: float;
      interest_unsettled: float
    }

    let create ~name ~currency ~balance ~interest_unsettled =
      { name ; currency ; balance ; interest_unsettled }

    let name_of_string = function
      | "trading" -> `Trading
      | "exchange" -> `Exchange
      | "deposit" -> `Deposit
      | _ -> invalid_arg "name_of_string"

    let of_yojson = function
      | `List [ `String name; `String currency; balance; interest_unsettled] ->
        let balance = float_of_json balance in
        let interest_unsettled = float_of_json interest_unsettled in
        let name = name_of_string name in
        create ~name ~currency ~balance ~interest_unsettled
      | #Yojson.Safe.json as json ->
        invalid_arg Printf.(sprintf "Wallet.of_yojson %s" Yojson.Safe.(to_string json))
  end

  module Order = struct
    include Order
    let spec_of_string = function
      | "MARKET" -> create_spec Margin `Market `Good_till_canceled
      | "LIMIT" -> create_spec Margin `Limit `Good_till_canceled
      | "STOP" -> create_spec Margin `Stop `Good_till_canceled
      | "TRAILING STOP" -> create_spec Margin `Market `Good_till_canceled
      | "FOK" -> create_spec Margin `Limit `Fill_or_kill
      | "EXCHANGE MARKET" -> create_spec Exchange `Market `Good_till_canceled
      | "EXCHANGE LIMIT" -> create_spec Exchange `Limit `Good_till_canceled
      | "EXCHANGE STOP" -> create_spec Exchange `Stop `Good_till_canceled
      | "EXCHANGE TRAILING STOP" -> create_spec Exchange `Market `Good_till_canceled
      | "EXCHANGE FOK" -> create_spec Exchange `Limit `Fill_or_kill
      | _ -> invalid_arg "Order.types_of_string"

    let status_of_string status =
      let words = String.split status ~on:' ' in
      match List.hd words with
      | None -> invalid_arg "Order.status_of_string"
      | Some "ACTIVE" -> `Open
      | Some "EXECUTED" -> `Filled
      | Some "PARTIALLY" -> `Partially_filled
      | Some "CANCELED" -> `Canceled
      | Some _ -> invalid_arg "Order.status_of_string"

    type t = {
      id: int;
      pair: string;
      amount: float;
      amount_orig: float;
      spec: Order.spec ;
      status: Order.status;
      price: float;
      avg_price: float;
      created_at: Time_ns.t;
      notify: bool;
      hidden: bool;
      oco: int option;
    }

    let create
        ?oco ~id ~pair ~amount ~amount_orig ~spec
        ~status ~price ~avg_price ~created_at
        ~notify ~hidden () =
      { id ; pair ; amount ; amount_orig ; spec ; status ; price ;
        avg_price ; created_at ; notify ; hidden ; oco }

    let of_yojson = function
      | `List [`Int id; `String pair; amount; amount_orig;
               `String typ; `String status; price; avg_price;
               `String created_at; `Int notify; `Int hidden; oco] ->
        let amount = float_of_json amount in
        let amount_orig = float_of_json amount_orig in
        let price = float_of_json price in
        let avg_price = float_of_json avg_price in
        let spec = spec_of_string typ in
        let status = status_of_string status in
        let created_at = Time_ns.of_string created_at in
        let notify = bool_of_int notify in
        let hidden = bool_of_int hidden in
        let oco = maybe_int oco in
        create ~id ~pair ~amount ~amount_orig ~spec
          ~status ~price ~avg_price ~created_at ~notify ~hidden ?oco ()
      | #Yojson.Safe.json -> invalid_arg "Order.of_yojson"
  end

  module Trade = struct
    type t = {
      id: int;
      pair: string;
      ts: Time_ns.t;
      order_id: int;
      amount: float;
      price: float;
      spec: Order.spec;
      order_price: float;
      fee: float;
      fee_currency: string;
    }

    let create ~id ~pair ~ts ~order_id ~amount ~price ~spec ~order_price ~fee ~fee_currency =
      { id ; pair ; ts ; order_id ; amount ; price ; spec ; order_price ; fee ; fee_currency }

    let of_yojson = function
      | `List [ `Int id; `String pair; `Int ts; `Int order_id; amount; price; `String typ; order_price; fee; `String fee_currency]
      | `List [ `String _; `Int id; `String pair; `Int ts; `Int order_id; amount; price; `String typ; order_price; fee; `String fee_currency] ->
        let ts = Time_ns.of_int_ns_since_epoch (ts * 1_000_000_000) in
        let spec = Order.spec_of_string typ in
        let amount = float_of_json amount in
        let price = float_of_json price in
        let order_price = float_of_json order_price in
        let fee = float_of_json fee in
        create ~id ~pair ~ts ~order_id ~amount
          ~price ~spec ~order_price ~fee ~fee_currency
      | #Yojson.Safe.json -> invalid_arg "Trade.of_yojson"
  end
end

module Auth = struct
  type t = {
    event: string;
    apiKey: string;
    authSig: string;
    authPayload: string;
  }

  let create ~event ~apiKey ~authSig ~authPayload =
    { event ; apiKey ; authSig ; authPayload }

  let encoding =
    let open Json_encoding in
    conv
      (fun { event ; apiKey ; authSig ; authPayload } ->
         (event, apiKey, authSig, authPayload))
      (fun (event, apiKey, authSig, authPayload) ->
         { event ; apiKey ; authSig ; authPayload })
      (obj4
         (req "event" string)
         (req "apiKey" string)
         (req "authSig" string)
         (req "authPayload" string))
end

let sign ~key:apiKey ~secret =
  let open Nocrypto in
  let payload = "AUTH" ^ Time_ns.(now () |> Time_ns.to_int_ns_since_epoch |> fun t -> t / 1_000_000 |> Int.to_string) in
  let `Hex authSig = Hash.SHA384.hmac ~key:secret Cstruct.(of_string payload) |> Hex.of_cstruct in
  Auth.create ~event:"auth" ~apiKey ~authSig ~authPayload:payload

let open_connection ?(buf=Bi_outbuf.create 4096) ?auth ?log ?to_ws () =
  let uri_str = "https://api2.bitfinex.com:3000/ws" in
  let uri = Uri.of_string uri_str in
  let uri_str = Uri.to_string uri in
  let host = Option.value_exn ~message:"no host in uri" Uri.(host uri) in
  let port = Option.value_exn ~message:"no port inferred from scheme"
      Uri_services.(tcp_port_of_uri uri)
  in
  let scheme = Option.value_exn ~message:"no scheme in uri" Uri.(scheme uri) in
  let cur_ws_w = ref None in
  Option.iter to_ws ~f:begin fun to_ws -> don't_wait_for @@
    Monitor.handle_errors (fun () ->
        Pipe.iter ~continue_on_error:true to_ws ~f:begin fun ev ->
          let ev_str = (ev |> Ev.to_yojson |> Yojson.Safe.to_string) in
          Option.iter log ~f:(fun log -> Log.debug log "-> %s" ev_str);
          match !cur_ws_w with
          | None -> Deferred.unit
          | Some w -> Pipe.write_if_open w ev_str
        end
      )
      (fun exn -> Option.iter log ~f:(fun log -> Log.error log "%s" @@ Exn.to_string exn))
  end;
  let client_r, client_w = Pipe.create () in
  let tcp_fun s r w =
    Socket.(setopt s Opt.nodelay true);
    begin if scheme = "https" || scheme = "wss" then
        Conduit_async.ssl_connect ~version:Tlsv1_2 r w
      else return (r, w)
    end >>= fun (r, w) ->
    let ws_r, ws_w = Websocket_async.client_ez uri s r w in
    cur_ws_w := Some ws_w;
    let cleanup () =
      Pipe.close_read ws_r;
      Deferred.all_unit [Reader.close r; Writer.close w]
    in
    Option.iter log ~f:(fun log -> Log.info log "[WS] connecting to %s" uri_str);
    (* AUTH *)
    begin match auth with
      | None -> Deferred.unit
      | Some (key, secret) ->
        let auth = sign key secret in
        let auth_str = (Yojson_encoding.construct Auth.encoding auth |> Yojson.Safe.to_string) in
        Option.iter log ~f:(fun log -> Log.debug log "-> %s" auth_str);
        Pipe.write ws_w auth_str
    end >>= fun () ->
    Monitor.protect ~finally:cleanup (fun () -> Pipe.transfer ws_r client_w ~f:(Yojson.Safe.from_string ~buf))
  in
  let rec loop () = begin
    Monitor.try_with_or_error ~name:"BFX.Ws.with_connection"
      (fun () -> Tcp.(with_connection (to_host_and_port host port) tcp_fun)) >>| function
    | Ok () -> Option.iter log ~f:(fun log -> Log.error log "[WS] connection to %s terminated" uri_str)
    | Error err -> Option.iter log ~f:(fun log -> Log.error  log "[WS] connection to %s raised %s" uri_str (Error.to_string_hum err))
  end >>= fun () ->
    if Pipe.is_closed client_r then Deferred.unit
    else begin
      Option.iter log ~f:(fun log -> Log.error log "[WS] restarting connection to %s" uri_str);
      Clock_ns.after @@ Time_ns.Span.of_int_sec 10 >>= loop
    end
  in
  don't_wait_for @@ loop ();
  client_r
