(lang dune 3.21)

(name starred_ml)

(generate_opam_files true)

(source
 (github paulosuzart/starred_ml))

(authors "Paulo Suzart")

(maintainers "Paulo Suzart")

(license CC0-1.0)

(package
 (name starred_ml)
 (synopsis "Generates an Awesome-style markdown list from GitHub stars")
 (description "Turn your starred items into a awesomeness list of repos")
 (depends
  (cmdliner
   (>= 1.2.0))
  (re2
   (>= v0.16.0))
  (alcotest
   (and
    (>= 1.9.1)
    :with-test))
  (yojson
   (>= 2.1.2))
  (tls-eio
   (>= 1.0.4))
  (ppx_deriving_yojson
   (>= 3.7.0))
  (ppx_deriving
   (>= 5.2.1))
  (mirage-crypto-rng
   (>= 1.2.0))
  (logs
   (>= 0.7.0))
  (jingoo
   (>= 1.5.2))
  (fmt
   (>= 0.9.0))
  (eio_main
   (>= 1.3))
  (eio
   (>= 1.3))
  (cohttp-eio
   (>= 6.2.1))
  (ocaml
   (>= 5.2.0))
  dune))
