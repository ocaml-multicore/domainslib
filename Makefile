all:
	dune build @install

run_test:
	dune runtest -f

clean:
	dune clean
