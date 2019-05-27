open Sexplib.Std
open Bfx

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

  let of_string s =
    if String.length s <> 7 then None
    else Some { base = String.sub s 1 3 ; quote = String.sub s 4 3 }

  let of_string_exn s =
    if String.length s <> 7 then invalid_arg "of_string_exn"
    else { base = String.sub s 1 3 ; quote = String.sub s 4 3 }

  let encoding =
    Json_encoding.(conv to_string of_string_exn string)
end

type error =
  | Unknown_event
  | Unknown_pair

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
  | Book of Pair.t * [`Level25 | `Level100]
[@@deriving sexp]

let trades_encoding =
  let open Json_encoding in
  conv
    (function p -> ((), p))
    (fun ((), p) -> p)
    (obj2
       (req "channel" (constant "trades"))
       (req "symbol" (Pair.encoding)))

let lvl_encoding =
  let open Json_encoding in
  conv
    (function `Level25 -> "25" | `Level100 -> "100")
    (function "25" -> `Level25 | _ -> `Level100)
    string

let book_encoding =
  let open Json_encoding in
  conv
    (function (p, len) -> ((), (), (), p, "", len))
    (fun ((), (), (), p, _, len) -> (p, len))
    (obj6
       (req "channel" (constant "book"))
       (req "prec" (constant "R0"))
       (req "freq" (constant "F0"))
       (req "symbol" (Pair.encoding))
       (req "pair" string)
       (req "len" lvl_encoding))

let feed_encoding =
  let open Json_encoding in
  union [
    case trades_encoding
      (function Trades i -> Some i | _ -> None)
      (fun i -> Trades i) ;
    case book_encoding
      (function Book (s, len) -> Some (s, len) | _ -> None)
      (fun (s, len) -> Book (s, len)) ;
  ]

let subscribe_encoding =
  let open Json_encoding in
  conv (fun a -> (), a) (fun ((), a) -> a)
    (merge_objs (obj1 (req "event" (constant "subscribe"))) feed_encoding)

let sub_encoding =
  let open Json_encoding in
  union [
    case (merge_objs unit trades_encoding)
      (function Trades i -> Some ((), i) | _ -> None)
      (fun ((), t) -> Trades t) ;
    case (merge_objs unit book_encoding)
      (function Book (p, len) -> Some ((), (p, len)) | _ -> None)
      (fun ((), (p, len)) -> Book (p, len))
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

module Book = struct
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

let book_snap_encoding =
  let open Json_encoding in
  tup2 int (list Book.encoding)

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

let book_encoding =
  let open Json_encoding in
  tup2 int Book.encoding

type t =
  | Version of version
  | Info of Info_message.t
  | Ping of int32
  | Pong of int32 * Ptime.t
  | Subscribe of feed
  | Subscribed of int * feed
  | Heartbeat of int
  | TradesSnap of int * Trade.t list
  | Trade of int * [`Executed | `Updated] * Trade.t
  | BookSnap of int * Book.t list
  | Book of int * Book.t
[@@deriving sexp]

let encoding =
  let open Json_encoding in
  union [
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
    case subscribed_encoding
      (function Subscribed (chanId, feed) -> Some (chanId, feed) | _ -> None)
      (fun (chanId, feed) -> Subscribed (chanId, feed)) ;
    case hb_encoding
      (function Heartbeat chanId -> Some chanId | _ -> None)
      (fun chanId -> Heartbeat chanId) ;
    case trade_snap_encoding
      (function TradesSnap (chanId, trades) -> Some (chanId, trades) | _ -> None)
      (fun (chanId, trades) -> TradesSnap (chanId, trades)) ;
    case trade_encoding
      (function Trade (chanId, typ, t) -> Some (chanId, typ, t) | _ -> None)
      (fun (chanId, typ, t) -> Trade (chanId, typ, t)) ;
    case book_snap_encoding
      (function BookSnap (chanId, trades) -> Some (chanId, trades) | _ -> None)
      (fun (chanId, trades) -> BookSnap (chanId, trades)) ;
    case book_encoding
      (function Book (chanId, quote) -> Some (chanId, quote) | _ -> None)
      (fun (chanId, quote) -> Book (chanId, quote)) ;
  ]
