```

BenchmarkDotNet v0.13.12, Windows 10 (10.0.19045.7548/22H2/2022Update)
Intel Core i7-8550U CPU 1.80GHz (Kaby Lake R), 1 CPU, 8 logical and 4 physical cores
.NET SDK 10.0.111
  [Host]     : .NET 10.0.11 (10.0.1126.37416), X64 RyuJIT AVX2 DEBUG
  Job-RXPTLX : .NET 10.0.11 (10.0.1126.37416), X64 RyuJIT AVX2

IterationCount=5  WarmupCount=2  

```
| Method                         | Mean          | Error          | StdDev        | Ratio     | RatioSD  | Gen0       | Gen1    | Gen2    | Allocated  | Alloc Ratio |
|------------------------------- |--------------:|---------------:|--------------:|----------:|---------:|-----------:|--------:|--------:|-----------:|------------:|
| VirtualCsv_FilterRowsBy_Step   |      3.368 μs |      0.7552 μs |     0.1169 μs |      1.00 |     0.00 |     0.5951 |       - |       - |     2512 B |        1.00 |
| VirtualCsv_Slice1000           |      1.571 μs |      1.1037 μs |     0.2866 μs |      0.49 |     0.08 |     0.2899 |       - |       - |     1216 B |        0.48 |
| VirtualCsv_StatsSum            | 61,155.265 μs | 10,566.0928 μs | 2,743.9807 μs | 17,934.42 | 1,223.38 | 13000.0000 |       - |       - | 54800760 B |   21,815.59 |
| MaterializedReadCsv_FilterScan |  5,796.345 μs |  2,676.2818 μs |   695.0219 μs |  1,758.34 |   179.09 |          - |       - |       - |       89 B |        0.04 |
| MaterializedReadCsv_StatsSum   | 37,095.602 μs | 11,793.4372 μs | 1,825.0475 μs | 11,022.82 |   597.37 |  5800.0000 | 66.6667 | 66.6667 | 26400796 B |   10,509.87 |
