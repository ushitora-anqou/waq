(*
  RFC 1945 https://www.rfc-editor.org/rfc/rfc1945.html
    HTTP/1.0
*)
module HTTP10 = struct
  module URI = struct
    type absolute_uri = unit (* FIXME *)
    type abs_path = unit (* FIXME *)
  end

  module Message = struct
    type token = string
    type field_name = token
    type field_value = string (* *( field-content | LWS ) *)
    type http_header = field_name * field_value option
    type general_header = [ `Date | `Pragma ]
    type entity_body = bytes

    type entity_header =
      [ `Allow
      | `Content_encoding
      | `Content_length
      | `Content_type
      | `Expires
      | `Last_modified
      | `Extension_header of http_header ]

    type request_uri =
      [ `Absolute_uri of URI.absolute_uri | `Abs_path of URI.abs_path ]

    type method_ = [ `Get | `Head | `Post | `Extension_method of token ]
    type http_version = [ `HTTP_10 ]
    type request_line = method_ * request_uri * http_version

    type request_header =
      [ `Authorization | `From | `If_modified_since | `Referer | `User_agent ]

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
    type response_header = [ `Location | `Server | `WWW_authenticate ]

    type response =
      [ `Simple_response of entity_body option
      | `Full_response of
        status_line
        * [ general_header | response_header | entity_header ] list
        * entity_body option ]

    type http_message = [ request | response ]
  end
end
