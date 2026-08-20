namespace Deedle.Benchmarks

open System
open System.IO
open BenchmarkDotNet.Attributes
open Deedle
open Deedle.Virtual
open Deedle.Virtual.Sources

/// B6/B8 — file-backed virtual CSV benchmarks vs materialized ReadCsv.
[<MemoryDiagnoser>]
[<SimpleJob(warmupCount = 2, iterationCount = 5)>]
type RealSourceBenchmarks() =

    let n = 100_000L
    let searchValue = "lorem"
    let csvPath = Path.Combine(__SOURCE_DIRECTORY__, "data", CsvTestData.defaultDatasetName)

    let mutable virtualFrame : Frame<DateTimeOffset, string> = Unchecked.defaultof<_>
    let mutable virtualFloatSeries : Series<int64, float> = Unchecked.defaultof<_>
    let mutable materializedFrame : Frame<int, string> = Unchecked.defaultof<_>

    [<GlobalSetup>]
    member _.Setup() =
        let dataDir = Path.GetDirectoryName csvPath
        if not (Directory.Exists dataDir) then Directory.CreateDirectory dataDir |> ignore
        CsvTestData.ensureSearchCsv csvPath n |> ignore
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
