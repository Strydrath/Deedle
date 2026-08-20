```

BenchmarkDotNet v0.13.12, Windows 10 (10.0.19045.7548/22H2/2022Update)
Intel Core i7-8550U CPU 1.80GHz (Kaby Lake R), 1 CPU, 8 logical and 4 physical cores
.NET SDK 10.0.111
  [Host]     : .NET 10.0.11 (10.0.1126.37416), X64 RyuJIT AVX2 DEBUG
  Job-FFHHFK : .NET 10.0.11 (10.0.1126.37416), X64 RyuJIT AVX2

IterationCount=5  WarmupCount=2  

```
| Method                          | Mean                | Error               | StdDev            | Ratio        | RatioSD    | Gen0         | Gen1         | Gen2         | Allocated     | Alloc Ratio   |
|-------------------------------- |--------------------:|--------------------:|------------------:|-------------:|-----------:|-------------:|-------------:|-------------:|--------------:|--------------:|
| FilterRowsBy_OrderedStep        |          5,140.4 ns |           803.49 ns |         208.66 ns |         1.00 |       0.00 |       0.6027 |            - |            - |        2536 B |          1.00 |
| FilterRowsBy_OrderedExactFixed  |          5,928.4 ns |         2,726.08 ns |         421.86 ns |         1.15 |       0.10 |       0.6409 |            - |            - |        2688 B |          1.06 |
| FilterRowsBy_OrderedFullFixed   |          4,299.0 ns |         2,186.52 ns |         567.83 ns |         0.84 |       0.11 |       0.6180 |            - |            - |        2592 B |          1.02 |
| FilterRowsBy_SparseIndexList    |         16,007.4 ns |         5,111.86 ns |       1,327.53 ns |         3.12 |       0.35 |       3.6926 |            - |            - |       15568 B |          6.14 |
| FilterRowsBy_SparseWrongStep    |          4,902.4 ns |         2,341.70 ns |         608.13 ns |         0.95 |       0.12 |       0.5951 |            - |            - |        2512 B |          0.99 |
| FilterRowsBy_MappedColumn_Scan  |     67,993,338.9 ns |    77,654,770.97 ns |  12,017,161.77 ns |    13,173.88 |   2,084.20 |    5666.6667 |     444.4444 |            - |    24665620 B |      9,726.19 |
| FilterRowsBy_OrderedStep_Read50 |         55,686.6 ns |        10,543.38 ns |       2,738.08 ns |        10.84 |       0.62 |       9.1553 |            - |            - |       38536 B |         15.20 |
| Slice_VirtualSeries             |          1,927.7 ns |           214.89 ns |          33.25 ns |         0.37 |       0.02 |       0.2937 |            - |            - |        1232 B |          0.49 |
| Lookup_VirtualSeries            |            126.8 ns |            49.35 ns |           7.64 ns |         0.02 |       0.00 |       0.0095 |            - |            - |          40 B |          0.02 |
| MapValues_VirtualSeries         |            338.0 ns |            80.60 ns |          12.47 ns |         0.07 |       0.00 |       0.0858 |            - |            - |         360 B |          0.14 |
| Materialize_SlicedVirtualSeries |        366,928.6 ns |       138,854.96 ns |      36,060.19 ns |        71.37 |       6.36 |      51.7578 |       7.8125 |            - |      218884 B |         86.31 |
| StatsSum_VirtualSeries          |     17,075,089.8 ns |     8,061,836.15 ns |   1,247,578.07 ns |     3,319.49 |     294.47 |    1906.2500 |            - |            - |     8000836 B |      3,154.90 |
| FilterRowsBy_OrdinalLinear      | 33,956,882,075.0 ns | 5,212,982,117.21 ns | 806,714,753.04 ns | 6,605,420.40 | 439,464.14 | 1571000.0000 | 1173000.0000 | 1172000.0000 | 36338138720 B | 14,328,919.05 |
