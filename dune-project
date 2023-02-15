(lang dune 3.6)

(name plano)

(generate_opam_files true)

(source
 (github yxqsnz/plano))

(authors "Moizes J. Sousa")

(maintainers "Moizes J. Sousa")

(license LICENSE)

(package
 (name plano)
 (synopsis "A really simple build system")
 (description "A really simple build system written in Ocaml")
 (depends ocaml dune)
 (tags
  (topics build c cpp)))
