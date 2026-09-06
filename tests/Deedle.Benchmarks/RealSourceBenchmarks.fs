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

/// File-backed virtual CSV/Parquet benchmarks vs materialized I/O.
[<MemoryDiagnoser>]
[<SimpleJob(warmupCount = 2, iterationCount = 5)>]
type RealSourceBenchmarks() =

    let n = 100_000L
    let searchValue = "lorem"
    // Wrong period (5, not the true 8): stays cheap/virtual but returns an incorrect row set.
    let wrongCategoryCycle = CsvTestData.words8.[0 .. 4]
    let dataDir = Path.Combine(__SOURCE_DIRECTORY__, "data")
    let csvPath = Path.Combine(dataDir, CsvTestData.defaultDatasetName)
    let parquetPath = Path.Combine(dataDir, ParquetTestData.defaultDatasetName)

    let mutable virtualFrame : Frame<DateTimeOffset, string> = Unchecked.defaultof<_>
    let mutable virtualParquetFrame : Frame<DateTimeOffset, string> = Unchecked.defaultof<_>
    let mutable virtualFloatSeries : Series<int64, float> = Unchecked.defaultof<_>
    let mutable virtualParquetFloatSeries : Series<int64, float> = Unchecked.defaultof<_>
    let mutable materializedFrame : Frame<int, string> = Unchecked.defaultof<_>
    let mutable virtualCsvWrongStepFrame : Frame<DateTimeOffset, string> = Unchecked.defaultof<_>
    let mutable virtualParquetWrongStepFrame : Frame<DateTimeOffset, string> = Unchecked.defaultof<_>

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
        materializedFrame <- Frame.ReadCsv(csvPath, inferRows=100)
        // Deliberately wrong LookupRange config (RQ2): declared period 5 against a true period-8 column.
        virtualCsvWrongStepFrame <-
            Virtual.ReadCsv<DateTimeOffset>(
                csvPath,
                indexColumn = "Timestamp",
                searchColumns = [ VirtualSearchColumn.withString "Category" (VirtualLookupRange.forRepeatingCycle wrongCategoryCycle) ],
                columnKeys = [ "Id"; "Category"; "Label"; "Value" ])
        virtualParquetWrongStepFrame <-
            Virtual.ReadParquet(
                parquetPath,
                indexColumn = "Timestamp",
                searchColumns = [ VirtualSearchColumn.withString "Category" (VirtualLookupRange.forRepeatingCycle wrongCategoryCycle) ],
                columnKeys = [ "Id"; "Category"; "Label"; "Value" ])

    [<Benchmark>]
    member _.VirtualCsv_FilterRowsBy2_Step() =
        virtualFrame
        |> Frame.filterRowsBy2 "Category" searchValue "Category" searchValue
        |> ignore

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

    /// Load + sum — virtual reads only the Value column.
    [<Benchmark>]
    member _.VirtualParquet_FilterRowsBy_Step() =
        virtualParquetFrame |> Frame.filterRowsBy "Category" searchValue |> ignore

    [<Benchmark>]
    member _.VirtualParquet_FilterRowsBy2_Step() =
        virtualParquetFrame
        |> Frame.filterRowsBy2 "Category" searchValue "Category" searchValue
        |> ignore

    [<Benchmark>]
    member _.VirtualParquet_Slice1000() =
        virtualParquetFloatSeries.[0L .. 999L] |> ignore

    [<Benchmark>]
    member _.VirtualParquet_StatsSum() =
        Stats.sum (ParquetTestData.createFloatValueSeries parquetPath) |> ignore

    /// Load + sum — materialized reads the full frame then sums Value.
    [<Benchmark>]
    member _.MaterializedReadParquet_StatsSum() =
        let frame = Frame.readParquet parquetPath
        Stats.sum (frame.GetColumn<float>("Value")) |> ignore

    // --- RQ2: deliberately wrong LookupRange metadata (declared period 5 vs true period 8) --------

    /// Stays virtual and cheap, but returns the wrong row set (see VirtualCsvWrongLookupRange tests
    /// for the row-count correctness check).
    [<Benchmark>]
    member _.Csv_FilterRowsBy_Category_WrongStep() =
        virtualCsvWrongStepFrame |> Frame.filterRowsBy "Category" searchValue |> ignore

    [<Benchmark>]
    member _.Parquet_FilterRowsBy_Category_WrongStep() =
        virtualParquetWrongStepFrame |> Frame.filterRowsBy "Category" searchValue |> ignore

    // --- Parquet decode vs. sum decomposition (explains VirtualParquet_StatsSum cost) -------------

    /// Sum only: series decoded once in Setup, isolates the aggregation cost from column decode.
    [<Benchmark>]
    member _.VirtualParquet_StatsSum_CachedSeries() =
        Stats.sum virtualParquetFloatSeries |> ignore

    /// Decode only: constructs the file index and reads the Value column, no aggregation.
    [<Benchmark>]
    member _.VirtualParquet_ColumnDecodeOnly() =
        use fileIndex = new ParquetFileIndex(parquetPath)
        fileIndex.ReadFloatColumn "Value" |> ignore