(lang dune 3.14)

(name starred_ml)

(generate_opam_files true)

(source
 (github paulosuzart/starred_ml))

(authors "Paulo Suzart")

(maintainers "Paulo Suzart")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name starred_ml)
 (synopsis "Generates a awesome list makdown")
 (description "Turn your starred items into a awesomeness list of repos")
 (depends ocaml dune)
 (tags
  (topics ocaml github makdown)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
