namespace Deedle.Benchmarks

open System
open System.IO
open BenchmarkDotNet.Attributes
open Deedle
open Deedle.Virtual
open Deedle.Tests.VirtualInstrumentation
open Deedle.Tests.CsvFileVirtualSource

/// B6 — phase-2 benchmarks on file-backed CSV virtual sources vs materialized ReadCsv.
[<MemoryDiagnoser>]
[<SimpleJob(warmupCount = 2, iterationCount = 5)>]
type RealSourceBenchmarks() =

    let n = 100_000L
    let searchValue = "lorem"
    let csvPath = Path.Combine(__SOURCE_DIRECTORY__, "data", CsvLineIndex.defaultDatasetName)

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
    member _.VirtualCsv_Slice1000() =
        counters.Reset()
        virtualFloatSeries.[0L .. 999L] |> ignore

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
