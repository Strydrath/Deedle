namespace Deedle.Benchmarks

open System
open System.IO
open BenchmarkDotNet.Attributes
open Deedle
open Deedle.Virtual
open Deedle.Tests.VirtualInstrumentation
open Deedle.Tests.CsvFileVirtualSource

/// Same operations as RealSourceBenchmarks, at a larger fixture size (RQ4: does the file-backed
/// virtual path still avoid full managed-heap loading once the data no longer fits comfortably
/// in a few tens of megabytes?).
///
/// CHECKPOINT NOTE (CP1 / "infra" era): this checkpoint predates the production
/// Virtual.ReadCsv / Virtual.ReadParquet file-backed API added later in the B8-B20 library
/// work (see CP6 / "benchmarks" branch). Only the CSV virtual-source path — built on the
/// synthetic CsvFileVirtualSource test helper used throughout this checkpoint's benchmarks —
/// is available here. There is no Parquet-backed virtual OR materialized comparison at this
/// checkpoint: Deedle.Parquet was not yet referenced by Deedle.Benchmarks.
[<MemoryDiagnoser>]
[<SimpleJob(warmupCount = 2, iterationCount = 5)>]
type RealSourceScaleBenchmarks() =

    let n = 1_000_000L
    let searchValue = "lorem"
    let csvPath = Path.Combine(__SOURCE_DIRECTORY__, "data", "b6-search-1m-random.csv")

    let mutable virtualFrame : Frame<DateTimeOffset, string> = Unchecked.defaultof<_>
    let mutable virtualFloatSeries : Series<int64, float> = Unchecked.defaultof<_>
    let mutable materializedFrame : Frame<int, string> = Unchecked.defaultof<_>
    let mutable counters : AccessCounters = Unchecked.defaultof<_>

    [<GlobalSetup>]
    member _.Setup() =
        let dataDir = Path.GetDirectoryName csvPath
        if not (Directory.Exists dataDir) then Directory.CreateDirectory dataDir |> ignore
        CsvLineIndex.ensureSearchCsv csvPath n |> ignore
        counters <- AccessCounters()
        let _, vf, _ = CsvFileVirtualSource.createOrderedSearchFrame csvPath counters
        virtualFrame <- vf
        let _, fs = CsvFileVirtualSource.createFloatValueSeries csvPath counters
        virtualFloatSeries <- fs
        materializedFrame <- Frame.ReadCsv(csvPath, inferRows=100)

    [<Benchmark(Baseline = true)>]
    member _.VirtualCsv_FilterRowsBy_Step() =
        counters.Reset()
        virtualFrame |> Frame.filterRowsBy "S2" searchValue |> ignore

    [<Benchmark>]
    member _.VirtualCsv_StatsSum() =
        counters.Reset()
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
