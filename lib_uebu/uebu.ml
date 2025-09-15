(*
  RFC 1945 https://www.rfc-editor.org/rfc/rfc1945.html
    HTTP/0.9 and HTTP/1.0
*)
module Rfc1945 = struct
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

    module Parser = struct
      let ( .%[] ) = Bytes.get

      let is_invalid_method_octet ch =
        (* FIXME: too strict according to RFC 1945? *)
        (ch < 'A' || ch > 'Z') && ch <> '_' && ch <> '-'

      type state = {
        mutable state : [ `Start | `Method | `Spaces_before_uri ];
        mutable request_start : int;
        mutable method_ : method_ option;
      }

      let parse_request_line buf =
        let method_ = ref None in
        (* FIXME: should we avoid boxing of state for better performance? *)
        let rec loop i state =
          let ch = buf.%[i] in
          match state with
          | `Start when ch = '\r' || ch = '\n' -> loop (i + 1) state
          | `Start when is_invalid_method_octet ch -> Error "invalid method"
          | `Start -> loop (i + 1) (`Method i)
          | `Method request_start when ch = ' ' -> (
              let cmp3 a b c =
                buf.%[i - 3] = a && buf.%[i - 2] = b && buf.%[i - 1] = c
              in
              let cmp4 a b c d =
                buf.%[i - 4] = a
                && buf.%[i - 3] = b
                && buf.%[i - 2] = c
                && buf.%[i - 1] = d
              in
              match i - request_start with
              | 3 when cmp3 'G' 'E' 'T' ->
                  method_ := Some `Get;
                  loop (i + 1) `Spaces_before_uri
              | 4 when cmp4 'P' 'O' 'S' 'T' ->
                  method_ := Some `Post;
                  loop (i + 1) `Spaces_before_uri
              | 4 when cmp4 'H' 'E' 'A' 'D' ->
                  method_ := Some `Head;
                  loop (i + 1) `Spaces_before_uri
              | n ->
                  method_ :=
                    Some
                      (`Extension_header (Bytes.sub_string buf request_start n));
                  loop (i + 1) `Spaces_before_uri)
          | `Method _ when is_invalid_method_octet ch -> Error "invalid method"
          | `Method _ -> loop (i + 1) state
          | _ -> assert false
        in
        loop 0
    end
  end
end
