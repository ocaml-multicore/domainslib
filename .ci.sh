
# setup opam
bash .ci-multicore-ocaml.sh

# use opam to install multicore and dune
opam update
opam switch create 4.12.0+domains+effects --packages=ocaml-variants.4.12.0+domains+effects --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default
eval $(opam config env)
opam install dune

# run the tests
make run_test
