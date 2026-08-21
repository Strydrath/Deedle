namespace Deedle.Benchmarks

open System
open System.IO
open BenchmarkDotNet.Attributes
open Deedle
open Deedle.Virtual
open Deedle.Virtual.Sources
open Deedle.Parquet
open Deedle.Parquet.Virtual.Sources

/// B6/B8/B13 — file-backed virtual CSV/Parquet benchmarks vs materialized I/O.
[<MemoryDiagnoser>]
[<SimpleJob(warmupCount = 2, iterationCount = 5)>]
type RealSourceBenchmarks() =

    let n = 100_000L
    let searchValue = "lorem"
    let dataDir = Path.Combine(__SOURCE_DIRECTORY__, "data")
    let csvPath = Path.Combine(dataDir, CsvTestData.defaultDatasetName)
    let parquetPath = Path.Combine(dataDir, ParquetTestData.defaultDatasetName)

    let mutable virtualFrame : Frame<DateTimeOffset, string> = Unchecked.defaultof<_>
    let mutable virtualFloatSeries : Series<int64, float> = Unchecked.defaultof<_>
    let mutable materializedFrame : Frame<int, string> = Unchecked.defaultof<_>

    [<GlobalSetup>]
    member _.Setup() =
        if not (Directory.Exists dataDir) then Directory.CreateDirectory dataDir |> ignore
        CsvTestData.ensureSearchCsv csvPath n |> ignore
        ParquetTestData.ensureSearchParquet parquetPath n |> ignore
        virtualFrame <-
            Virtual.ReadCsv(
                csvPath,
                indexColumn = "Timestamp",
                searchColumn = "Category",
                searchLookupRange = VirtualLookupRange.forRepeatingCycle CsvTestData.words8,
                columnKeys = [ "Id"; "Category" ])
        virtualFloatSeries <- CsvTestData.createFloatValueSeries csvPath
        materializedFrame <- Frame.ReadCsv(csvPath, inferRows=100)

    [<Benchmark(Baseline = true)>]
    member _.VirtualCsv_FilterRowsBy_Step() =
        virtualFrame |> Frame.filterRowsBy "Category" searchValue |> ignore

    [<Benchmark>]
    member _.VirtualCsv_Slice1000() =
        virtualFloatSeries.[0L .. 999L] |> ignore

    [<Benchmark>]
    member _.VirtualCsv_StatsSum() =
        Stats.sum virtualFloatSeries |> ignore

    [<Benchmark>]
    member _.MaterializedReadCsv_FilterScan() =
        let col = materializedFrame.GetColumn<string>("Category")
        let mutable count = 0
        for i in 0 .. materializedFrame.RowCount - 1 do
            if col.GetAt(i) = searchValue then count <- count + 1
        count |> ignore

    [<Benchmark>]
    member _.MaterializedReadCsv_StatsSum() =
        Stats.sum (materializedFrame.GetColumn<float>("Value")) |> ignore

    /// B13: load + sum — virtual reads only the Value column.
    [<Benchmark>]
    member _.VirtualParquet_StatsSum() =
        Stats.sum (ParquetTestData.createFloatValueSeries parquetPath) |> ignore

    /// B13: load + sum — materialized reads the full frame then sums Value.
    [<Benchmark>]
    member _.MaterializedReadParquet_StatsSum() =
        let frame = Frame.readParquet parquetPath
        Stats.sum (frame.GetColumn<float>("Value")) |> ignore