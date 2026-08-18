```

BenchmarkDotNet v0.13.12, Windows 10 (10.0.19045.7663/22H2/2022Update)
Intel Core i7-8550U CPU 1.80GHz (Kaby Lake R), 1 CPU, 8 logical and 4 physical cores
.NET SDK 10.0.111
  [Host]     : .NET 10.0.11 (10.0.1126.37416), X64 RyuJIT AVX2 DEBUG
  Job-MPKMJS : .NET 10.0.11 (10.0.1126.37416), X64 RyuJIT AVX2

IterationCount=5  WarmupCount=2  

```
| Method                         | Mean          | Error         | StdDev      | Ratio     | RatioSD | Gen0       | Gen1    | Gen2    | Allocated  | Alloc Ratio |
|------------------------------- |--------------:|--------------:|------------:|----------:|--------:|-----------:|--------:|--------:|-----------:|------------:|
| VirtualCsv_FilterRowsBy_Step   |      3.745 μs |     0.2946 μs |   0.0765 μs |      1.00 |    0.00 |     0.7935 |       - |       - |     3345 B |        1.00 |
| VirtualCsv_Slice1000           |      1.407 μs |     0.1937 μs |   0.0503 μs |      0.38 |    0.02 |     0.4120 |       - |       - |     1728 B |        0.52 |
| VirtualCsv_StatsSum            | 49,921.161 μs | 3,369.9498 μs | 521.5035 μs | 13,395.74 |  343.18 | 13090.9091 |       - |       - | 54800762 B |   16,382.89 |
| MaterializedReadCsv_FilterScan |  4,376.527 μs |   854.3347 μs | 221.8680 μs |  1,167.97 |   37.42 |          - |       - |       - |       88 B |        0.03 |
| MaterializedReadCsv_StatsSum   | 25,040.222 μs | 1,195.8305 μs | 310.5534 μs |  6,687.97 |  164.05 |  5781.2500 | 62.5000 | 62.5000 | 26400792 B |    7,892.61 |
