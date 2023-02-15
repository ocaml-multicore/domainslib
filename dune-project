(lang dune 3.0)
(name domainslib)
(formatting disabled)
(generate_opam_files true)
(cram enable)

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
  iter
  batteries
  (mirage-clock-unix (and :with-test (>= "4.2.0")))
  (qcheck-core (and :with-test (>= "0.20")))
  (qcheck-multicoretests-util (and :with-test (>= "0.1")))
  (qcheck-stm (and :with-test (>= "0.1")))))

(package
 (name multicoretests)
 (synopsis "Experimental multicore test suite of OCaml 5.0")
 (authors "Multiple contributors")
 (description "This package contains a collection of randomized QCheck tests to exercise
the multicore run-time of OCaml 5.0.")
 (tags ("test" "test suite" "property" "qcheck" "quickcheck" "multicore" "non-determinism"))
 (allow_empty)
 (depends
  base-domains
  (ppx_deriving         (>= "5.2.1"))
  (qcheck-core          (>= "0.20"))
  (ppx_deriving_qcheck  (>= "0.2.0"))
  (qcheck-lin           (= :version))
  (qcheck-stm           (= :version))))


(package
 (name qcheck-lin)
 (synopsis "A multicore testing library for OCaml")
 (description
   "A testing library based on QCheck to test interface behaviour under parallel
usage. Lin will generate and run random parallel tests and check the observed
behaviour for sequential consistency, that is, whether they can be linearized
and explained by some sequential interleaving.")
 (tags ("test" "property" "qcheck" "quickcheck" "parallelism" "sequential consistency"))
 (depends
  base-domains
  (qcheck-core           (>= "0.20"))
  (ppx_deriving          (and :with-test (>= "5.2.1")))))

