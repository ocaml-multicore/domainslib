(lang dune 3.0)
(name domainslib)
(formatting disabled)
(generate_opam_files true)

(source (github ocaml-multicore/domainslib))
(authors "KC Sivaramakrishnan <kc@kcsrk.info>")
(maintainers "KC Sivaramakrishnan <kc@kcsrk.info>")
(documentation "https://kayceesrk.github.io/domainslib/doc")
(license "ISC")

(package
 (name domainslib)
 (synopsis "Parallel Structures over Domains for Multicore OCaml")
 (depends
  (ocaml (>= "5.0"))
  (lockfree (>= "0.2.0"))
  (mirage-clock-unix :with-test)
  (qcheck-core (and :with-test (>= "0.20")))
  (qcheck-multicoretests-util (and :with-test (>= "0.1")))
  (qcheck-stm (and :with-test (>= "0.1")))))
