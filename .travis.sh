# If a fork of these scripts is specified, use that GitHub user instead
fork_user=${FORK_USER:-ocaml}

# If a branch of these scripts is specified, use that branch instead of 'master'
fork_branch=${FORK_BRANCH:-master}

### Bootstrap

set -uex

sh .travis-multicore-ocaml.sh

export OPAMYES=1
eval $(opam config env)

opam switch 4.06.1+multicore
eval $(opam config env)
opam install dune.1.9.1
make run_test
