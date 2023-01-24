# Counter
## Normal conditions run
```
Running Par_Prefix_sum's benchmarks, ops = 1,000,000

        num_domains:      1        2        3        4        5        6        7        8   
     Par_prefix_sum:     39731    65215    76977    87451   114508   116266   124394   125567  ops/ms


Running ImpBatch Counter Benchmarks, batch_size = 4096, ops = 1,000,000
                        (SEQ)
        num_domains:      1        2        3        4        5        6        7        8   
        LockCounter:     61550     9322    13038    10662     7208     7055     6268     5768  ops/ms
    LockfreeCounter:    193161    26155    16051    15833    14187    12869    12549    12139  ops/ms
  ImpBatchedCounter:      7922     2789     3704     4200     4371     4107     4279     4181  ops/ms
```
- The control examples, LockCounter and LockfreeCounter show performance degradation as number of worker domains increase.
- The operations are so cheap that the overhead of batching dominates any observable speedup, Batched operation, Par_prefix_sum have not much speedup and there is also some cost of synchronization between work stealing domains.
## Significant run: Adding delay of 1 ms to exaggerate parallelism
```
Running Par_Prefix_sum's benchmarks, ops = 10,000

        num_domains:      1        2        3        4        5        6        7        8   
     Par_prefix_sum:      18       38       55       76       94      111      127      153  ops/ms


Running ImpBatch Counter Benchmarks, batch_size = 4096, ops = 10,000

        num_domains:      1        2        3        4        5        6        7        8   
  ImpBatchedCounter:      19       37       54       73       90      106      123      143  ops/ms
```
- Significant speedup as number of cores increase with the exaggeration.
- Implicit batching also demostrates that effect

## Implicit batch statistics
```
Running ImpBatchCounter Statistics, batch_size = 4096, ops = 10,000,000
1 -> 3
2 -> 4
3 -> 1
4 -> 1
13 -> 2
28 -> 1
42 -> 1
57 -> 1
83 -> 1
169 -> 1
260 -> 1
573 -> 1
773 -> 1
1064 -> 1
1090 -> 1
1156 -> 1
1290 -> 1
1787 -> 1
2047 -> 1
2810 -> 1
3070 -> 1
3071 -> 3
3072 -> 1
3633 -> 1
3943 -> 1
3958 -> 1
4065 -> 1
4090 -> 1
4091 -> 1
4092 -> 7
4093 -> 41
4094 -> 120
4095 -> 1201
4096 -> 1060
```

# Slist
## Normal conditions run: 1 mil preset
```
Running BatchSlist inserts, preset=1,000,000, inserts=100,000

   num_domains:      1        2        3        4        5        6        7        8   
     Batch_ins:     253      301      312      321      331      334      338      334


Running ImpBatch Slist Benchmarks, batch_size = 127, preset = 1,000,000, additional inserts = 100,000

   num_domains:      1        2        3        4        5        6        7        8   
       Seq_ins:     212      288      247      271      276      293      278      289
  ImpBatch_ins:     201      392      383      534      633      580      675      666
```
- Batched insert shows little observable speedup when increasing number of cores 
- However, against the sequential case, the batched operation is more efficient in general we can perform multiple searches into the list in parallel

## Significant run: 10 mil preset
```
Running BatchSlist inserts, preset=10,000,000, inserts=100,000

 num_domains:        1        2        3        4        5        6        7        8   
   Batch_ins:       226      376      485      572      598      496      548      549

Running ImpBatch Slist Benchmarks, batch_size = 127, preset = 10,000,000, additional inserts = 100,000

 num_domains:        1        2        3        4        5        6        7        8   
     Seq_ins:       158      161      147      155      149      150      146      160
ImpBatch_ins:       131      224      305      373      435      480      509      535
```
- effect of batching is obvious, 3x speedup in implicit batching case.

## Implicit batch statistics
```
Running ImpBatchSlist Statistics, batch_size = 127, preset = 1,000,000, additional inserts = 100,000
1 -> 1564
2 -> 12
5 -> 4
6 -> 2
7 -> 6
9 -> 12
10 -> 4
32 -> 4
101 -> 2
126 -> 260
127 -> 1300
```