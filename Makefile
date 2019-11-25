all:
	dune build @install

run_test:
	dune exec test/test_chan.exe 1 100 1 1

clean:
	dune clean
