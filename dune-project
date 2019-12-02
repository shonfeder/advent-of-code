(lang dune 2.0)
(generate_opam_files true)

(name aoc_2019)
(version 0.1.0)

(authors "Shon Feder")
; (source (github shonfeder/shonfeder_net))
(maintainers "shon.feder@gmail.com")
; (homepage "https://github.com/shonfeder/shonfeder_net")

(package
 (name aoc_2019)
 (synopsis "TODO")
 (description "TODO")
 (depends
  (dune (>= 2.0.0))

  (ocaml (>= 4.08.0))
  ;; inc to v13 once opium can support
  (core (>= v0.13.0))

  (ppx_inline_test (>= v0.13.0))
  (ppx_deriving (and :with-test (>= 4.4)))
))
