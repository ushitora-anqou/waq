(lang dune 3.7)

(generate_opam_files true)

(name waq)

(version 0.1)

(source
 (github ushitora-anqou/waq))

(authors "Ushitora Anqou")

(maintainers "Ushitora Anqou")

(license MIT)

(documentation https://github.com/ushitora-anqou/waq)

(package
 (name waq)
 (synopsis "Yet another ActivityPub server")
 (description "Yet another ActivityPub server")
 (depends
  (alcotest :with-test)
  (alcotest-lwt :with-test)
  (merlin :with-dev-setup)
  (ocamlformat
   (and
    (= 0.25.1)
    :with-dev-setup))
  base64
  (camlimages
   (= 5.0.4-1))
  cmdliner
  cohttp
  cohttp-lwt-unix
  cppo
  cstruct
  dune
  fpath
  jingoo
  lambdasoup
  lwt
  lwt_ppx
  lwt_ssl
  (mirage-crypto-rng
   (<= 0.10.7))
  multipart_form
  multipart_form-lwt
  ocaml
  ocurl
  pcre
  postgresql
  ppx_deriving_yaml
  (ppx_yojson_conv
   (= v0.15.1))
  ptime
  re
  result
  safepass
  sha
  uri
  uuidm
  websocket
  websocket-lwt-unix
  x509
  xml-light
  yaml
  yojson)
 (tags
  (activitypub fediverse)))
