## Insert results

```
iex(1)> OtelBench.ets_insert_bench()
Operating System: Linux
CPU Information: Intel(R) Core(TM) i5-7200U CPU @ 2.50GHz
Number of Available Cores: 4
Available memory: 15.49 GB
Elixir 1.13.4
Erlang 25.3.2-1

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 1 min
memory time: 0 ns
reduction time: 0 ns
parallel: 1000
inputs: 1, 100, 1K
Estimated total run time: 6.20 min

Benchmarking ets_duplicate_bag with input 1 ...
Benchmarking ets_duplicate_bag with input 100 ...
Benchmarking ets_duplicate_bag with input 1K ...
Benchmarking ets_set with input 1 ...
Benchmarking ets_set with input 100 ...
Benchmarking ets_set with input 1K ...

##### With input 1 #####
Name                        ips        average  deviation         median         99th %
ets_set                  101.24        9.88 ms     ±8.82%        9.70 ms       14.03 ms
ets_duplicate_bag        100.03       10.00 ms     ±9.28%        9.83 ms       14.24 ms

Comparison: 
ets_set                  101.24
ets_duplicate_bag        100.03 - 1.01x slower +0.119 ms

##### With input 100 #####
Name                        ips        average  deviation         median         99th %
ets_set                    6.82      146.64 ms     ±5.23%      146.46 ms      160.98 ms
ets_duplicate_bag          6.22      160.88 ms     ±6.82%      161.10 ms      177.21 ms

Comparison: 
ets_set                    6.82
ets_duplicate_bag          6.22 - 1.10x slower +14.24 ms

##### With input 1K #####
Name                        ips        average  deviation         median         99th %
ets_set                    0.72         1.38 s    ±12.67%         1.40 s         1.65 s
ets_duplicate_bag          0.63         1.59 s     ±9.63%         1.59 s         1.92 s

Comparison: 
ets_set                    0.72
ets_duplicate_bag          0.63 - 1.15x slower +0.21 s
```

```
iex(1)> OtelBench.ets_insert_to_large_tab_bench()
Operating System: Linux
CPU Information: Intel(R) Core(TM) i5-7200U CPU @ 2.50GHz
Number of Available Cores: 4
Available memory: 15.49 GB
Elixir 1.13.4
Erlang 25.3.2-1

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 10 s
memory time: 0 ns
reduction time: 0 ns
parallel: 100
inputs: 1
Estimated total run time: 24 s

Benchmarking ets_duplicate_bag with input 1 ...
Benchmarking ets_set with input 1 ...

##### With input 1 #####
Name                        ips        average  deviation         median         99th %
ets_duplicate_bag        6.19 K      161.52 μs  ±3867.78%        5.70 μs     6352.91 μs
ets_set                  3.94 K      253.68 μs  ±9495.01%        6.07 μs     8046.54 μs

Comparison: 
ets_duplicate_bag        6.19 K
ets_set                  3.94 K - 1.57x slower +92.16 μs
```

```
iex(1)> OtelBench.ets_insert_bench_less_cleanup()
Operating System: Linux
CPU Information: Intel(R) Core(TM) i5-7200U CPU @ 2.50GHz
Number of Available Cores: 4
Available memory: 15.49 GB
Elixir 1.13.4
Erlang 25.3.2-1

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 30 s
memory time: 0 ns
reduction time: 0 ns
parallel: 1000
inputs: 1
Estimated total run time: 1.07 min

Benchmarking ets_duplicate_bag with input 1 ...
Benchmarking ets_set with input 1 ...

##### With input 1 #####
Name                        ips        average  deviation         median         99th %
ets_set                  571.79        1.75 ms  ±3026.37%     0.00575 ms       90.74 ms
ets_duplicate_bag        540.94        1.85 ms  ±1093.70%     0.00726 ms      110.31 ms

Comparison: 
ets_set                  571.79
ets_duplicate_bag        540.94 - 1.06x slower +0.0997 ms
```

## Traverse results

```
iex(1)> OtelBench.ets_traverse_bench()
Operating System: Linux
CPU Information: Intel(R) Core(TM) i5-7200U CPU @ 2.50GHz
Number of Available Cores: 4
Available memory: 15.49 GB
Elixir 1.13.4
Erlang 25.3.2-1

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 1 min
memory time: 0 ns
reduction time: 0 ns
parallel: 1
inputs: 100K, 10K, 20K
Estimated total run time: 9.30 min

Benchmarking ets_delete with input 100K ...
Benchmarking ets_delete with input 10K ...
Benchmarking ets_delete with input 20K ...
Benchmarking ets_fix_take with input 100K ...
Benchmarking ets_fix_take with input 10K ...
Benchmarking ets_fix_take with input 20K ...
Benchmarking ets_take with input 100K ...
Benchmarking ets_take with input 10K ...
Benchmarking ets_take with input 20K ...

##### With input 100K #####
Name                   ips        average  deviation         median         99th %
ets_take              5.28      189.26 ms     ±0.99%      188.79 ms      199.34 ms
ets_fix_take          5.28      189.35 ms     ±1.64%      188.87 ms      211.17 ms
ets_delete            5.00      200.13 ms     ±7.30%      189.74 ms      249.48 ms

Comparison: 
ets_take              5.28
ets_fix_take          5.28 - 1.00x slower +0.0872 ms
ets_delete            5.00 - 1.06x slower +10.87 ms

##### With input 10K #####
Name                   ips        average  deviation         median         99th %
ets_take             49.56       20.18 ms     ±5.24%       19.81 ms       23.68 ms
ets_fix_take         49.14       20.35 ms     ±4.61%       20.32 ms       23.52 ms
ets_delete           49.14       20.35 ms     ±4.64%       20.41 ms       23.58 ms

Comparison: 
ets_take             49.56
ets_fix_take         49.14 - 1.01x slower +0.175 ms
ets_delete           49.14 - 1.01x slower +0.175 ms

##### With input 20K #####
Name                   ips        average  deviation         median         99th %
ets_take             25.25       39.60 ms     ±6.72%       38.44 ms       47.54 ms
ets_fix_take         25.05       39.91 ms    ±11.61%       38.13 ms       63.72 ms
ets_delete           25.04       39.94 ms     ±6.65%       38.68 ms       46.61 ms

Comparison: 
ets_take             25.25
ets_fix_take         25.05 - 1.01x slower +0.31 ms
ets_delete           25.04 - 1.01x slower +0.34 ms
```

## Non-empty table test benchmark

`ets:info(T, size) >= 0` vs `ets:first(T) =/= '$end_of_Table'`

```
iex(1)> OtelBench.ets_size_bench()
Operating System: Linux
CPU Information: Intel(R) Core(TM) i5-7200U CPU @ 2.50GHz
Number of Available Cores: 4
Available memory: 15.49 GB
Elixir 1.13.4
Erlang 25.3.2-1

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 10 s
memory time: 0 ns
reduction time: 0 ns
parallel: 1
inputs: dup bag, dup bag write_concurrency, dup bag write_concurrency decentralized, set, set bag write_concurrency, set write_concurrency decentralized
Estimated total run time: 4.80 min

Benchmarking ets_first with input dup bag ...
Benchmarking ets_first with input dup bag write_concurrency ...
Benchmarking ets_first with input dup bag write_concurrency decentralized ...
Benchmarking ets_first with input set ...
Benchmarking ets_first with input set bag write_concurrency ...
Benchmarking ets_first with input set write_concurrency decentralized ...
Benchmarking ets_first_empty_tab with input dup bag ...
Benchmarking ets_first_empty_tab with input dup bag write_concurrency ...
Benchmarking ets_first_empty_tab with input dup bag write_concurrency decentralized ...
Benchmarking ets_first_empty_tab with input set ...
Benchmarking ets_first_empty_tab with input set bag write_concurrency ...
Benchmarking ets_first_empty_tab with input set write_concurrency decentralized ...
Benchmarking ets_size with input dup bag ...
Benchmarking ets_size with input dup bag write_concurrency ...
Benchmarking ets_size with input dup bag write_concurrency decentralized ...
Benchmarking ets_size with input set ...
Benchmarking ets_size with input set bag write_concurrency ...
Benchmarking ets_size with input set write_concurrency decentralized ...
Benchmarking ets_size_empty_tab with input dup bag ...
Benchmarking ets_size_empty_tab with input dup bag write_concurrency ...
Benchmarking ets_size_empty_tab with input dup bag write_concurrency decentralized ...
Benchmarking ets_size_empty_tab with input set ...
Benchmarking ets_size_empty_tab with input set bag write_concurrency ...
Benchmarking ets_size_empty_tab with input set write_concurrency decentralized ...

##### With input dup bag #####
Name                          ips        average  deviation         median         99th %
ets_size_empty_tab         6.36 M      157.18 ns  ±2641.20%         151 ns         285 ns
ets_size                   6.32 M      158.32 ns  ±2522.08%         153 ns         200 ns
ets_first_empty_tab        1.74 M      573.07 ns   ±527.39%         564 ns         689 ns
ets_first               0.00964 M   103744.54 ns    ±10.68%      101530 ns   135306.25 ns

Comparison: 
ets_size_empty_tab         6.36 M
ets_size                   6.32 M - 1.01x slower +1.14 ns
ets_first_empty_tab        1.74 M - 3.65x slower +415.89 ns
ets_first               0.00964 M - 660.04x slower +103587.36 ns

##### With input dup bag write_concurrency #####
Name                          ips        average  deviation         median         99th %
ets_size_empty_tab         4.81 M        0.21 μs  ±1221.54%        0.20 μs        0.24 μs
ets_size                   4.75 M        0.21 μs  ±1334.30%        0.21 μs        0.22 μs
ets_first_empty_tab        0.58 M        1.73 μs   ±214.66%        1.72 μs        1.83 μs
ets_first               0.00493 M      202.81 μs     ±4.94%      201.82 μs      217.68 μs

Comparison: 
ets_size_empty_tab         4.81 M
ets_size                   4.75 M - 1.01x slower +0.00263 μs
ets_first_empty_tab        0.58 M - 8.33x slower +1.52 μs
ets_first               0.00493 M - 975.21x slower +202.60 μs

##### With input dup bag write_concurrency decentralized #####
Name                          ips        average  deviation         median         99th %
ets_first_empty_tab      570.22 K        1.75 μs   ±283.50%        1.72 μs        2.21 μs
ets_size_empty_tab       536.64 K        1.86 μs  ±2499.10%        1.65 μs        2.39 μs
ets_size                 528.72 K        1.89 μs  ±2478.72%        1.69 μs        1.87 μs
ets_first                  0.58 K     1713.66 μs     ±3.33%     1696.79 μs     1897.88 μs

Comparison: 
ets_first_empty_tab      570.22 K
ets_size_empty_tab       536.64 K - 1.06x slower +0.110 μs
ets_size                 528.72 K - 1.08x slower +0.138 μs
ets_first                  0.58 K - 977.16x slower +1711.91 μs

##### With input set #####
Name                          ips        average  deviation         median         99th %
ets_size_empty_tab         6.35 M      157.47 ns  ±2886.02%         151 ns         284 ns
ets_size                   6.13 M      163.15 ns  ±2714.96%         153 ns         295 ns
ets_first                  6.10 M      163.90 ns  ±2821.37%         158 ns         210 ns
ets_first_empty_tab        1.75 M      572.97 ns   ±512.95%         564 ns         689 ns

Comparison: 
ets_size_empty_tab         6.35 M
ets_size                   6.13 M - 1.04x slower +5.68 ns
ets_first                  6.10 M - 1.04x slower +6.43 ns
ets_first_empty_tab        1.75 M - 3.64x slower +415.51 ns

##### With input set bag write_concurrency #####
Name                          ips        average  deviation         median         99th %
ets_size_empty_tab         4.81 M      207.97 ns  ±1061.71%         203 ns         237 ns
ets_size                   4.77 M      209.65 ns  ±1052.56%         205 ns         221 ns
ets_first                  4.29 M      232.85 ns  ±1956.18%         226 ns         387 ns
ets_first_empty_tab        0.58 M     1732.33 ns   ±266.60%        1715 ns        1861 ns

Comparison: 
ets_size_empty_tab         4.81 M
ets_size                   4.77 M - 1.01x slower +1.68 ns
ets_first                  4.29 M - 1.12x slower +24.88 ns
ets_first_empty_tab        0.58 M - 8.33x slower +1524.36 ns

##### With input set write_concurrency decentralized #####
Name                          ips        average  deviation         median         99th %
ets_first               4308.78 K        0.23 μs  ±1891.46%        0.23 μs        0.30 μs
ets_first_empty_tab      577.29 K        1.73 μs   ±247.22%        1.72 μs        1.86 μs
ets_size_empty_tab       538.86 K        1.86 μs  ±2519.58%        1.65 μs        2.01 μs
ets_size                 526.41 K        1.90 μs  ±2474.03%        1.70 μs        2.25 μs

Comparison: 
ets_first               4308.78 K
ets_first_empty_tab      577.29 K - 7.46x slower +1.50 μs
ets_size_empty_tab       538.86 K - 8.00x slower +1.62 μs
ets_size                 526.41 K - 8.19x slower +1.67 μs
```
