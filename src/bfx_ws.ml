open Sexplib.Std
open Bfx

module Info_message = struct
  module Code = struct
    type t =
      | Please_reconnect
      | Maintenance_start
      | Maintenance_end
    [@@deriving sexp]

    let of_int = function
      | 20051 -> Please_reconnect
      | 20060 -> Maintenance_start
      | 20061 -> Maintenance_end
      | i -> invalid_arg ("Info.of_int: Got code " ^ string_of_int i)

    let to_int = function
      | Please_reconnect -> 20051
      | Maintenance_start -> 20060
      | Maintenance_end -> 20061

    let encoding = Json_encoding.(conv to_int of_int int)
  end

  type t = {
    code : Code.t ;
    msg : string ;
  } [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    conv
      (fun _ -> invalid_arg "Not implemented")
      (fun ((), (_evt, code, msg)) -> { code ; msg })
      (merge_objs unit
         (obj3
            (req "event" (constant "info"))
            (req "code" Code.encoding)
            (req "msg" string)))
end

type version = {
  version: int ;
  serverId: Uuidm.t ;
  platform: [`Operative | `Maintenance] ;
} [@@deriving sexp]

let status_encoding =
  let open Json_encoding in
  conv
    (function `Operative -> 1 | `Maintenance -> 0)
    (function 1 -> `Operative | _ -> `Maintenance)
    int

let version_encoding =
  let open Json_encoding in
  conv
    (fun _ -> invalid_arg "Not implemented")
    (fun ((), ((), version, serverId, platform)) ->
       { version ; serverId ; platform })
    (merge_objs unit
       (obj4
          (req "event" (constant "info"))
          (req "version" int)
          (req "serverId" Uuidm.encoding)
          (req "platform" (obj1 (req "status" status_encoding)))))

let ping_encoding =
  let open Json_encoding in
  conv (fun i -> (), i) (fun ((), i) -> i)
    (obj2
       (req "event" (constant "ping"))
       (req "cid" int32))

let pong_encoding =
  let open Json_encoding in
  conv (fun (cid, ts) -> (), cid, ts) (fun ((), cid, ts) -> cid, ts)
    (obj3
       (req "event" (constant "pong"))
       (req "cid" int32)
       (req "ts" Ptime.encoding))

type feed =
  | Trades of Pair.t
  | Quotes of Pair.t
[@@deriving sexp]

let trades_encoding =
  let open Json_encoding in
  conv
    (function p -> ((), p))
    (fun ((), p) -> p)
    (obj2
       (req "channel" (constant "trades"))
       (req "symbol" (Pair.encoding)))

let quotes_encoding =
  let open Json_encoding in
  conv
    (function p -> ((), (), p, ()))
    (fun ((), (), p, _) -> p)
    (obj4
       (req "channel" (constant "book"))
       (req "prec" (constant "R0"))
       (req "symbol" (Pair.encoding))
       (req "len" (constant "100")))

let feed_encoding =
  let open Json_encoding in
  union [
    case trades_encoding
      (function Trades i -> Some i | _ -> None)
      (fun i -> Trades i) ;
    case quotes_encoding
      (function Quotes s -> Some s | _ -> None)
      (fun s -> Quotes s) ;
  ]

let subscribe_encoding =
  let open Json_encoding in
  conv (fun a -> (), a) (fun ((), a) -> a)
    (merge_objs (obj1 (req "event" (constant "subscribe"))) feed_encoding)

let unsubscribe_encoding =
  let open Json_encoding in
  conv (fun a -> (), a) (fun ((), a) -> a)
    (obj2
       (req "event" (constant "unsubscribe"))
       (req "chanId" int))

let sub_encoding =
  let open Json_encoding in
  union [
    case (merge_objs unit trades_encoding)
      (function Trades i -> Some ((), i) | _ -> None)
      (fun ((), t) -> Trades t) ;
    case (merge_objs unit quotes_encoding)
      (function Quotes p -> Some ((), p) | _ -> None)
      (fun ((), p) -> Quotes p)
  ]

let subscribed_encoding =
  let open Json_encoding in
  conv
    (fun (chanId, a) -> ((), chanId), a)
    (fun (((), chanId), a) -> chanId, a)
    (merge_objs
       (obj2
          (req "event" (constant "subscribed"))
          (req "chanId" int))
       sub_encoding)

let unsubscribed_encoding =
  let open Json_encoding in
  conv
    (fun chanId -> ((), (), chanId))
    (fun ((), (), chanId) -> chanId)
    (obj3
       (req "event" (constant "unsubscribed"))
       (req "status" (constant "OK"))
       (req "chanId" int))

module Trade = struct
  type t = {
    id: int64 ;
    ts: Ptime.t ;
    qty: float ;
    price: float ;
  } [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    conv
      (fun { id ; ts ; qty ; price } -> (id, ts, qty, price))
      (fun (id, ts, qty, price) -> { id ; ts ; qty ; price })
      (tup4 int53 Ptime.encoding float float)
end

let trade_snap_encoding =
  let open Json_encoding in
  tup2 int (list Trade.encoding)

module Quote = struct
  type t = {
    id: int64 ;
    price: float ;
    qty: float ;
  } [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    conv
      (fun { id ; price ; qty } -> (id, price, qty))
      (fun (id, price, qty) -> { id ; price ; qty })
      (tup3 int53 float float)
end

let quotes_snap_encoding =
  let open Json_encoding in
  tup2 int (list Quote.encoding)

let hb_encoding =
  let open Json_encoding in
  conv (fun i -> i, ()) (fun (i, ()) -> i)
    (tup2 int (constant "hb"))

let trade_update_typ_encoding =
  let open Json_encoding in
  string_enum [
    "te", `Executed ;
    "tu", `Updated ;
  ]

let trade_encoding =
  let open Json_encoding in
  (tup3 int trade_update_typ_encoding Trade.encoding)

let quote_encoding =
  let open Json_encoding in
  tup2 int Quote.encoding

type error = {
  code: int ;
  msg: string ;
} [@@deriving sexp]

let error_encoding =
  let open Json_encoding in
  conv
    (fun { code; msg } -> (), ((), msg, code))
    (fun ((), ((), msg, code)) -> { code ; msg })
    (merge_objs unit
       (obj3
          (req "event" (constant "error"))
          (req "msg" string)
          (req "code" int)))

type t =
  | Version of version
  | Error of error
  | Info of Info_message.t
  | Ping of int32
  | Pong of int32 * Ptime.t
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

let encoding =
  let open Json_encoding in
  union [
    case error_encoding
      (function Error e -> Some e | _ -> None)
      (fun e -> Error e) ;
    case version_encoding
      (function Version i -> Some i | _ -> None)
      (fun i -> Version i) ;
    case Info_message.encoding
      (function Info i -> Some i | _ -> None)
      (fun i -> Info i) ;
    case ping_encoding
      (function Ping cid -> Some cid | _ -> None)
      (fun cid -> Ping cid) ;
    case pong_encoding
      (function Pong (cid, ts)-> Some (cid, ts) | _ -> None)
      (fun (cid, ts) -> Pong (cid, ts)) ;
    case subscribe_encoding
      (function Subscribe a -> Some a | _ -> None)
      (fun a -> Subscribe a) ;
    case unsubscribe_encoding
      (function Unsubscribe a -> Some a | _ -> None)
      (fun a -> Unsubscribe a) ;
    case subscribed_encoding
      (function Subscribed (chanId, feed) -> Some (chanId, feed) | _ -> None)
      (fun (chanId, feed) -> Subscribed (chanId, feed)) ;
    case unsubscribed_encoding
      (function Unsubscribed a -> Some a | _ -> None)
      (fun a -> Unsubscribed a) ;
    case hb_encoding
      (function Heartbeat chanId -> Some chanId | _ -> None)
      (fun chanId -> Heartbeat chanId) ;
    case trade_snap_encoding
      (function TradesSnap (chanId, trades) -> Some (chanId, trades) | _ -> None)
      (fun (chanId, trades) -> TradesSnap (chanId, trades)) ;
    case trade_encoding
      (function Trade (chanId, typ, t) -> Some (chanId, typ, t) | _ -> None)
      (fun (chanId, typ, t) -> Trade (chanId, typ, t)) ;
    case quotes_snap_encoding
      (function QuotesSnap (chanId, trades) -> Some (chanId, trades) | _ -> None)
      (fun (chanId, trades) -> QuotesSnap (chanId, trades)) ;
    case quote_encoding
      (function Quote (chanId, quote) -> Some (chanId, quote) | _ -> None)
      (fun (chanId, quote) -> Quote (chanId, quote)) ;
  ]
