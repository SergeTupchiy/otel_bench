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
