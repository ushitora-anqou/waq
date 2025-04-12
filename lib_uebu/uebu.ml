module type Reader = sig
  type t
  type error = [ `Eof ]

  val peek_char : t -> (char, error) result
  val pop_char : t -> (char, error) result
end

(*
  RFC 1945 https://www.rfc-editor.org/rfc/rfc1945.html
    HTTP/0.9 and HTTP/1.0
*)
module Rfc1945 (Reader : Reader) = struct
  module R = struct
    type t = { r : Reader.t }
    type error = [ `Eof | `Unexpected_char ]

    let make ~reader = { r = reader }
    let map_error = Result.map_error (fun e -> (e :> error))
    let ( let* ) = Result.bind

    let check checks chr =
      let rec aux = function
        | [] -> Error `Unexpected_char
        | check :: checks -> (
            match check chr with Ok _ -> Ok chr | Error _ -> aux checks)
      in
      aux checks

    let map_check checks x = Result.bind x (check checks)

    let check_char chr =
      let code = Char.code chr in
      if 0 <= code && code <= 127 then Ok chr else Error `Unexpected_char

    let check_upalpha chr =
      let code = Char.code chr in
      if Char.code 'A' <= code && code <= Char.code 'Z' then Ok chr
      else Error `Unexpected_char

    let check_loalpha chr =
      let code = Char.code chr in
      if Char.code 'a' <= code && code <= Char.code 'z' then Ok chr
      else Error `Unexpected_char

    let peek_octet r = Reader.peek_char r.r |> map_error
    let pop_octet r = Reader.pop_char r.r |> map_error
    let peek_char r = peek_octet r |> map_check [ check_char ]
    let pop_char r = pop_octet r |> map_check [ check_char ]
    let peek_upalpha r = peek_octet r |> map_check [ check_upalpha ]
    let pop_upalpha r = pop_octet r |> map_check [ check_upalpha ]
    let peek_loalpha r = peek_octet r |> map_check [ check_loalpha ]
    let pop_loalpha r = pop_octet r |> map_check [ check_loalpha ]

    let peek_alpha r =
      peek_octet r |> map_check [ check_loalpha; check_upalpha ]

    let pop_alpha r = pop_octet r |> map_check [ check_loalpha; check_upalpha ]
  end

  module URI = struct
    type scheme = string (* 1*( ALPHA | DIGIT | "+" | "-" | "." ) *)
    type absolute_uri = scheme * string (* scheme ":" *( uchar | reserved ) *)
    type segment = string (* *pchar *)
    type fsegment = string (* 1*pchar *)
    type path = fsegment * segment list (* fsegment *( "/" segment ) *)
    type param = string (* *( pchar | "/" ) *)
    type params = param list (* param *( ";" param ) *)
    type query = string (* *( uchar | reserved ) *)

    type rel_path =
      path option
      * params option
      * query option (* [ path ] [ ";" params ] [ "?" query ] *)

    type abs_path = rel_path (* "/" rel_path *)
    type net_loc = string (* *( pchar | ";" | "?" ) *)
    type net_path = net_loc * abs_path option (* "//" net_loc [ abs_path ] *)

    type relative_uri =
      [ `Net_path of net_path | `Abs_path of abs_path | `Rel_path of rel_path ]
    (* net_path | abs_path | rel_path *)

    type fragment = string (* *( uchar | reserved ) *)

    type t =
      [ `Absolute_uri of absolute_uri | `Relative_uri of relative_uri ]
      * fragment option
    (* URI = ( absoluteURI | relativeURI ) [ "#" fragment ] *)
  end

  type token = string (* any CHAR except CTLs or tspecials *)

  module Media_type = struct
    type type_ = token
    type subtype = token
    type attribute = token
    type value = string (* token | quoted-string *)
    type parameter = attribute * value (* attribute "=" value *)
    type t = type_ * subtype * parameter list
    (* media-type = type "/" subtype *( ";" parameter ) *)
  end

  module Http_date = struct
    type month =
      [ `Jan
      | `Feb
      | `Mar
      | `Apr
      | `May
      | `Jun
      | `Jul
      | `Aug
      | `Sep
      | `Oct
      | `Nov
      | `Dec ]

    type date1 = int * month * int (* 2DIGIT SP month SP 4DIGIT *)
    type date2 = int * month * int (* 2DIGIT "-" month "-" 2DIGIT *)
    type date3 = month * int (* month SP ( 2DIGIT | ( SP 1DIGIT )) *)
    type wkday = [ `Mon | `Tue | `Wed | `Thu | `Fri | `Sat | `Sun ]
    (*  "Mon" | "Tue" | "Wed"
      | "Thu" | "Fri" | "Sat" | "Sun" *)

    type weekday = wkday
    (*  "Monday" | "Tuesday" | "Wednesday"
      | "Thursday" | "Friday" | "Saturday" | "Sunday" *)

    type time = int * int * int (* 2DIGIT ":" 2DIGIT ":" 2DIGIT *)

    type rfc1123_date =
      wkday * date1 * time (* wkday "," SP date1 SP time SP "GMT" *)

    type rfc850_date =
      weekday * date2 * time (* weekday "," SP date2 SP time SP "GMT" *)

    type asctime_date =
      wkday * date3 * time * int (* wkday SP date3 SP time SP 4DIGIT *)

    type t =
      [ `Rfc1123_date of rfc1123_date
      | `Rfc850_date of rfc850_date
      | `Asctime_date of asctime_date ]
    (* HTTP-date = rfc1123-date | rfc850-date | asctime-date *)
  end

  module Message = struct
    type word = string (* token | quoted-string *)
    type field_name = token
    type field_value = string (* *( field-content | LWS ) *)
    type http_header = field_name * field_value option

    type pragma_directive =
      [ `No_cache | `Extension_pragma of token * word option ]

    type general_header =
      [ `Date of Http_date.t | `Pragma of pragma_directive list (* non empty *) ]

    type entity_body = bytes
    type method_ = [ `Get | `Head | `Post | `Extension_method of token ]
    type content_coding = [ `X_gzip | `X_compress | `Content_coding of token ]

    type entity_header =
      [ `Allow of method_ list (* non empty *)
      | `Content_encoding of content_coding
      | `Content_length of int
      | `Content_type of Media_type.t
      | `Expires of Http_date.t option (* "expires immediately" if None *)
      | `Last_modified of Http_date.t
      | `Extension_header of http_header ]

    type request_uri =
      [ `Absolute_uri of URI.absolute_uri | `Abs_path of URI.abs_path ]

    type http_version = [ `HTTP_10 ]
    type request_line = method_ * request_uri * http_version
    type auth_scheme = token
    type auth_param = token * string
    type userid_password = token (* user-ID *) * string (* password *)
    type basic_cookie = userid_password (* base64 encoding *)
    type basic_credentials = basic_cookie

    type credentials =
      [ `Basic_credentials of basic_credentials
      | `Credential of auth_scheme * auth_param list ]

    type mailbox = string

    type request_header =
      [ `Authorization of credentials
      | `From of mailbox
      | `If_modified_since of Http_date.t
      | `Referer of
        [ `Absolute_uri of URI.absolute_uri | `Relative_uri of URI.relative_uri ]
      | `User_agent of string ]

    type request =
      [ `Simple_request of request_uri
      | `Full_request of
        request_line
        * [ general_header | request_header | entity_header ] list
        * entity_body option ]

    type status_code =
      [ `SC200 (* OK *)
      | `SC201 (* Created *)
      | `SC202 (* Accepted *)
      | `SC204 (* No Content *)
      | `SC301 (* Moved Permanently *)
      | `SC302 (* Moved Temporarily *)
      | `SC304 (* Not Modified *)
      | `SC400 (* Bad Request *)
      | `SC401 (* Unauthorized *)
      | `SC403 (* Forbidden *)
      | `SC404 (* Not Found *)
      | `SC500 (* Internal Server Error *)
      | `SC501 (* Not Implemented *)
      | `SC502 (* Bad Gateway *)
      | `SC503 (* Service Unavailable *)
      | `Extension_code of int (* 3-digit number *) ]

    type reason_phrase = string
    type status_line = http_version * status_code * reason_phrase
    type realm_value = string
    type realm = realm_value
    type challenge = auth_scheme * realm * auth_param list

    type response_header =
      [ `Location of URI.absolute_uri
      | `Server of string
      | `WWW_authenticate of challenge list (* non empty *) ]

    type response =
      [ `Simple_response of entity_body option
      | `Full_response of
        status_line
        * [ general_header | response_header | entity_header ] list
        * entity_body option ]

    type http_message = [ request | response ]
  end
end
