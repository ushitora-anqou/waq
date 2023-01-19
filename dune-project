(lang dune 3.7)

(name waq)

(generate_opam_files true)

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
  (ocamlformat :with-dev-setup)
  base64
  bigstringaf
  cstruct
  dune
  fpath
  httpaf
  httpaf-lwt-unix
  jingoo
  lwt
  lwt_ppx
  mirage-crypto-rng
  ocaml
  postgresql
  ppx_deriving_yaml
  ppx_deriving_yojson
  ptime
  re
  result
  sha
  uri
  uuidm
  x509
  yaml
  yojson)
 (tags
  (activitypub fediverse)))
