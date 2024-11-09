let initialize env f =
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env f

module SecureRandom = struct
  let generate len = Mirage_crypto_rng.generate len

  let unique_token () =
    (* Thanks to: Doorkeeper::OAuth::Helpers::UniqueToken
       https://github.com/doorkeeper-gem/doorkeeper/blob/47037da4def738e4cfd930bd433f35629a5869f6/lib/doorkeeper/oauth/helpers/unique_token.rb *)
    generate 32
    |> Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
end
