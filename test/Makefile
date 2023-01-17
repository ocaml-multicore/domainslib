# Counter
counter_stat:
	dune exec -- ./counters.exe true

bench_counter:
	dune exec -- ./test_batch_counting.exe
	dune exec -- ./bench_counters.exe

# Delay of 1 ms to exaggerate parallelism
bench_counter_significant:
	dune exec -- ./test_batch_counting.exe false true
	dune exec -- ./bench_counters.exe false true


# Slist
slist_stat:
	dune exec -- ./slist.exe true

bench_slist:
	dune exec -- ./test_batch_inserts.exe
	dune exec -- ./bench_slist.exe

# 10 million preset, 100_000 inserts
bench_slist_significant:
	dune exec -- ./test_batch_inserts.exe -preset=10000000
	dune exec -- ./bench_slist_inserts.exe false 10000000


test :
	dune exec -- ./playground.exe