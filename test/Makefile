# Counter
bench_counter:
	dune exec -- ./test_batch_counting.exe
	dune exec -- ./bench_counters.exe

# Delay of 1 ms to exaggerate parallelism
bench_counter_significant:
	dune exec -- ./test_batch_counting.exe false true
	dune exec -- ./bench_counters.exe false true

counter_stat:
	dune exec -- ./counters.exe true

# Slist
bench_slist:
	dune exec -- ./test_batch_inserts.exe
	dune exec -- ./bench_slist.exe

# 10 million preset, 100_000 inserts
bench_slist_significant:
	dune exec -- ./test_batch_inserts.exe -preset=10000000
	dune exec -- ./bench_slist.exe false 10000000

slist_stat:
	dune exec -- ./slist.exe true

test :
	dune exec -- ./playground.exe

# make bench_counter
# make bench_counter_significant
# make counter_stat
# make bench_slist
# make bench_slist_significant
# make slist_stat