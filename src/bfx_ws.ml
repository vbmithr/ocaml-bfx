open Core
open Async

open Bfx

let src = Logs.Src.create "bfx.ws"

let pp_json ppf t = Yojson.Safe.pretty_print ppf t
let pp_decode_error ppf t =
  Json_encoding.print_error ?print_unknown:None ppf t

module Yojson_encoding = struct
  include Json_encoding.Make(Json_repr.Yojson)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Logs.err ~src begin fun m ->
        m "%a@.%a@." pp_json value pp_decode_error exn
      end ;
      raise exn
end

let const_encoding str =
  Json_encoding.(string_enum [str, ()])

module Event = struct
  type t =
    | Info
    | Ping
    | Pong
    | Conf
    | Subscribe
    | Subscribed
    | Error
    | Unsubscribe
    | Unsubscribed

  let encoding =
    let open Json_encoding in
    string_enum [
      "info", Info ;
      "ping", Ping ;
      "pong", Pong ;
      "conf", Conf ;
      "subscribe", Subscribe ;
      "subscribed", Subscribed ;
      "error", Error ;
      "unsubscribe", Unsubscribe ;
      "unsubscribed", Unsubscribed ;
    ]
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
      | i -> invalid_argf "Info.of_int: Got code %d" i ()

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
            (req "event" (const_encoding "info"))
            (req "code" Code.encoding)
            (req "msg" string)))
end

let version_encoding =
  let open Json_encoding in
  conv
    (fun _ -> invalid_arg "Not implemented")
    (fun ((), (_evt, version)) -> version)
    (merge_objs unit
       (obj2
          (req "event" (const_encoding "info"))
          (req "version" int)))

let unit_event_encoding str =
  let open Json_encoding in
  conv
    (fun _ -> invalid_arg "Not implemented")
    (fun _ -> ())
    (obj1
       (req "event" (const_encoding str)))

let ping_encoding = unit_event_encoding "ping"
let pong_encoding = unit_event_encoding "pong"

type t =
  | Version of int
  | Info of Info_message.t
  | Ping
  | Pong
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
      (function Ping -> Some () | _ -> None)
      (fun () -> Ping) ;
    case pong_encoding
      (function Pong -> Some () | _ -> None)
      (fun () -> Pong) ;
  ]
