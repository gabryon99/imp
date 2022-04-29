(lang dune 3.1)

(name imp)

(generate_opam_files true)

(source
 (github gabryon99/imp))

(authors "Gabriele Pappalardo")

(maintainers "gabryon99")

(license LICENSE)

(using menhir 2.1)

(package
 (name imp)
 (synopsis "The IMP programming language")
 (description "IMP is a small language of while programs.")
 (depends ocaml dune)
 (tags
  (programming_language imperative while semantics)))



