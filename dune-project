(lang dune 3.14)

(name starred_ml)

(generate_opam_files true)

(source
 (github paulosuzart/starred_ml))

(authors "Paulo Suzart")

(maintainers "Paulo Suzart")

(license CC0-1.0)

(package
 (name starred_ml)
 (synopsis "Generates a awesome list markdown")
 (description "Turn your starred items into a awesomeness list of repos")
 (depends
  (cmdliner
   (>= 1.2.0))
  (re2
   (>= v0.16.0))
  (alcotest
   (and
    (>= 1.7.0)
    :with-test))
  (yojson
   (>= 2.1.2))
  (tls-eio
   (>= 0.17.3))
  (ppx_deriving_yojson
   (>= 3.7.0))
  (ppx_deriving
   (>= 5.2.1))
  (mirage-crypto-rng-eio
   (>= 0.11.3))
  (logs
   (>= 0.7.0))
  (jingoo
   (>= 1.5.0))
  (fmt
   (>= 0.9.0))
  (eio_main
   (>= 1.0))
  (eio
   (>= 1.0))
  (cohttp-eio
   (>= 6.0.0~beta2))
  ocaml
  dune))
