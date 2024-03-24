type name =
  [ `Accept
  | `Accept_encoding
  | `Access_control_allow_headers
  | `Access_control_allow_methods
  | `Access_control_allow_origin
  | `Access_control_expose_headers
  | `Access_control_request_headers
  | `Authorization
  | `Connection
  | `Content_encoding
  | `Content_length
  | `Content_type
  | `Crypto_key
  | `Date
  | `Digest
  | `Host
  | `Link
  | `Location
  | `Raw of string
  | `Sec_websocket_protocol
  | `Signature
  | `TTL
  | `User_agent ]

let lower_string_of_name : name -> string = function
  | `Accept -> "accept"
  | `Accept_encoding -> "accept-encoding"
  | `Access_control_allow_headers -> "access-control-allow-headers"
  | `Access_control_allow_methods -> "access-control-allow-methods"
  | `Access_control_allow_origin -> "access-control-allow-origin"
  | `Access_control_expose_headers -> "access-control-expose-headers"
  | `Access_control_request_headers -> "access-control-request-headers"
  | `Authorization -> "authorization"
  | `Connection -> "connection"
  | `Content_encoding -> "content-encoding"
  | `Content_length -> "content-length"
  | `Content_type -> "content-type"
  | `Crypto_key -> "crypto-key"
  | `Date -> "date"
  | `Digest -> "digest"
  | `Host -> "host"
  | `Link -> "link"
  | `Location -> "location"
  | `Raw s -> s
  | `Sec_websocket_protocol -> "sec-websocket-protocol"
  | `Signature -> "signature"
  | `TTL -> "ttl"
  | `User_agent -> "user-agent"

let string_of_name = lower_string_of_name

let name_of_string (k : string) : name =
  match String.lowercase_ascii k with
  | "accept" -> `Accept
  | "accept-encoding" -> `Accept_encoding
  | "access-control-allow-headers" -> `Access_control_allow_headers
  | "access-control-allow-methods" -> `Access_control_allow_methods
  | "access-control-allow-origin" -> `Access_control_allow_origin
  | "access-control-expose-headers" -> `Access_control_expose_headers
  | "access-control-request-headers" -> `Access_control_request_headers
  | "authorization" -> `Authorization
  | "connection" -> `Connection
  | "content-encoding" -> `Content_encoding
  | "content-length" -> `Content_length
  | "content-type" -> `Content_type
  | "crypto-key" -> `Crypto_key
  | "date" -> `Date
  | "digest" -> `Digest
  | "host" -> `Host
  | "link" -> `Link
  | "location" -> `Location
  | "sec-websocket-protocol" -> `Sec_websocket_protocol
  | "signature" -> `Signature
  | "ttl" -> `TTL
  | "user-agent" -> `User_agent
  | s -> `Raw s

type t = name * string

let to_tuple ((n, v) : t) : string * string = (string_of_name n, v)
let of_tuple (n, v) : t = (name_of_string n, v)
