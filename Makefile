all:
	dune build @install

run_test:
	OCAMLRUNPARAM="b=1" dune runtest -f

clean:
	dune clean
