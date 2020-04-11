all:
	dune build @install

run_test:
	dune exec test/test_chan.exe 1 100 1 1
	dune exec test/fib.exe 43
	dune exec test/fib_par.exe 1 43
	dune exec test/enumerate_par.exe 1 100
	dune exec test/game_of_life.exe 20 16
	dune exec test/game_of_life_multicore.exe 1 20 16
	dune exec test/spectralnorm2.exe 2000
	dune exec test/spectralnorm2_multicore.exe 1 2000
	dune exec test/sum_par.exe 1 100
	dune exec test/task_exn.exe

clean:
	dune clean
