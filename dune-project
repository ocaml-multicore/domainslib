(lang dune 3.0)
(name domainslib)
(formatting disabled)
(generate_opam_files true)

(source (github ocaml-multicore/domainslib))
(authors "KC Sivaramakrishnan <kc@kcsrk.info>")
(maintainers "KC Sivaramakrishnan <kc@kcsrk.info>" "Sudha Parimala")
(documentation "https://ocaml-multicore.github.io/domainslib/doc")
(license "ISC")

(package
 (name domainslib)
 (synopsis "Parallel Structures over Domains for Multicore OCaml")
 (depends
  (ocaml (>= "5.0"))
  (saturn (>= "0.4.0"))
  (domain-local-await (>= 0.1.0))
  (kcas (and (>= 0.3.0) :with-test))
  (mirage-clock-unix (and :with-test (>= "4.2.0")))
  (qcheck-core (and :with-test (>= "0.20")))
  (qcheck-multicoretests-util (and :with-test (>= "0.1")))
  (qcheck-stm (and :with-test (>= "0.1")))))
