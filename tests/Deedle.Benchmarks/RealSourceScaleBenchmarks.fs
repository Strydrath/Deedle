namespace Deedle.Benchmarks

open System
open System.IO
open BenchmarkDotNet.Attributes
open Deedle
open Deedle.TestData
open Deedle.Virtual
open Deedle.Virtual.Sources
open Deedle.Parquet
open Deedle.Parquet.Virtual.Sources

/// Same operations as RealSourceBenchmarks, at a larger fixture size (RQ4: does the file-backed
/// virtual path still avoid full managed-heap loading once the data no longer fits comfortably
/// in a few tens of megabytes?).
[<MemoryDiagnoser>]
[<SimpleJob(warmupCount = 2, iterationCount = 5)>]
type RealSourceScaleBenchmarks() =

    let n = 1_000_000L
    let searchValue = "lorem"
    let dataDir = Path.Combine(__SOURCE_DIRECTORY__, "data")
    let csvPath = Path.Combine(dataDir, "b6-search-1m-random.csv")
    let parquetPath = Path.Combine(dataDir, "b6-search-1m-random.parquet")

    let mutable virtualFrame : Frame<DateTimeOffset, string> = Unchecked.defaultof<_>
    let mutable virtualParquetFrame : Frame<DateTimeOffset, string> = Unchecked.defaultof<_>
    let mutable virtualFloatSeries : Series<int64, float> = Unchecked.defaultof<_>
    let mutable virtualParquetFloatSeries : Series<int64, float> = Unchecked.defaultof<_>

    [<GlobalSetup>]
    member _.Setup() =
        if not (Directory.Exists dataDir) then Directory.CreateDirectory dataDir |> ignore
        CsvTestData.ensureSearchCsv csvPath n |> ignore
        ParquetTestData.ensureSearchParquet parquetPath n |> ignore
        virtualFrame <-
            Virtual.ReadCsv<DateTimeOffset>(
                csvPath,
                indexColumn = "Timestamp",
                searchColumns = [ VirtualSearchColumn.withString "Category" (VirtualLookupRange.forRepeatingCycle CsvTestData.words8) ],
                columnKeys = [ "Id"; "Category"; "Label"; "Value" ])
        virtualFloatSeries <- CsvTestData.createFloatValueSeries csvPath
        virtualParquetFrame <-
            Virtual.ReadParquet(
                parquetPath,
                indexColumn = "Timestamp",
                searchColumns = [ VirtualSearchColumn.withString "Category" (VirtualLookupRange.forRepeatingCycle CsvTestData.words8) ],
                columnKeys = [ "Id"; "Category"; "Label"; "Value" ])
        virtualParquetFloatSeries <- ParquetTestData.createFloatValueSeries parquetPath

    [<Benchmark(Baseline = true)>]
    member _.VirtualCsv_FilterRowsBy_Step() =
        virtualFrame |> Frame.filterRowsBy "Category" searchValue |> ignore

    [<Benchmark>]
    member _.VirtualCsv_StatsSum() =
        Stats.sum virtualFloatSeries |> ignore

    /// Reference point only: materialized full-file CSV read and scan at N = 1,000,000.
    [<Benchmark>]
    member _.MaterializedReadCsv_FilterScan() =
        let materialized = Frame.ReadCsv(csvPath, inferRows=100)
        let col = materialized.GetColumn<string>("Category")
        let mutable count = 0
        for i in 0 .. materialized.RowCount - 1 do
            if col.GetAt(i) = searchValue then count <- count + 1
        count |> ignore

    [<Benchmark>]
    member _.MaterializedReadCsv_StatsSum() =
        let materialized = Frame.ReadCsv(csvPath, inferRows=100)
        Stats.sum (materialized.GetColumn<float>("Value")) |> ignore

    [<Benchmark>]
    member _.VirtualParquet_FilterRowsBy_Step() =
        virtualParquetFrame |> Frame.filterRowsBy "Category" searchValue |> ignore

    /// Sum only: series decoded once in Setup, isolates the aggregation cost from column decode.
    [<Benchmark>]
    member _.VirtualParquet_StatsSum_CachedSeries() =
        Stats.sum virtualParquetFloatSeries |> ignore

    /// Load + sum every iteration, matching the 100k-row RealSourceBenchmarks measurement.
    [<Benchmark>]
    member _.VirtualParquet_StatsSum() =
        Stats.sum (ParquetTestData.createFloatValueSeries parquetPath) |> ignore

    /// Decode only: constructs the file index and reads the Value column, no aggregation.
    [<Benchmark>]
    member _.VirtualParquet_ColumnDecodeOnly() =
        use fileIndex = new ParquetFileIndex(parquetPath)
        fileIndex.ReadFloatColumn "Value" |> ignore

    [<Benchmark>]
    member _.MaterializedReadParquet_StatsSum() =
        let frame = Frame.readParquet parquetPath
        Stats.sum (frame.GetColumn<float>("Value")) |> ignore
