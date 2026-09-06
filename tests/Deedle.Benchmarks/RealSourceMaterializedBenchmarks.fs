namespace Deedle.Benchmarks

open System
open System.Globalization
open System.IO
open BenchmarkDotNet.Attributes
open Deedle

/// CHECKPOINT NOTE (CP0 / pre-infra master baseline): this checkpoint predates ALL of the
/// file-backed virtual source work (`CsvFileVirtualSource` was added later at CP1; the
/// production `Virtual.ReadCsv`/`Virtual.ReadParquet` API was added later still, in the B8-B20
/// library work on CP6 / "benchmarks"). There is nothing to compare a materialized read
/// against here — this suite exists purely to give a materialized-only CSV baseline (both at
/// the same 100K-row scale as RealSourceBenchmarks elsewhere, and at 1,000,000 rows) so the
/// "how much did materialized cost change across the project's history" question has an
/// answer at this checkpoint too. No Parquet benchmark exists here: Deedle.Benchmarks does not
/// reference Deedle.Parquet at this checkpoint.
[<AutoOpen>]
module private RealSourceMaterializedFixture =

    let private words8 = "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')
    let private profileVersion = "cp0-materialized-v1"
    let private defaultSeed = 42

    let private metaPath (csvPath: string) = csvPath + ".meta"

    let private shuffleInPlace (rng: Random) (items: int[]) =
        for i in items.Length - 1 .. -1 .. 0 do
            let j = rng.Next(i + 1)
            let tmp = items.[i]
            items.[i] <- items.[j]
            items.[j] <- tmp

    let private generateSearchCsv (path: string) (rowCount: int64) =
        let dir = Path.GetDirectoryName(path)
        if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
            Directory.CreateDirectory dir |> ignore
        let rng = Random(defaultSeed)
        let ids = Array.init (int rowCount) id
        shuffleInPlace rng ids
        use writer = new StreamWriter(path, false)
        writer.WriteLine("Id,Timestamp,Category,Value")
        let start = DateTimeOffset(DateTime(2000, 1, 1), TimeSpan.Zero)
        for i in 0L .. rowCount - 1L do
            let id = ids.[int i]
            let cat = words8.[int (i % int64 words8.Length)]
            let ts = start.AddSeconds(float i).ToString("o", CultureInfo.InvariantCulture)
            let value = rng.NextDouble() * 10000.0
            writer.WriteLine(sprintf "%d,%s,%s,%s" id ts cat (value.ToString("F4", CultureInfo.InvariantCulture)))
        File.WriteAllText(metaPath path, sprintf "version=%s\nrows=%d" profileVersion rowCount)

    /// Regenerates the fixture only if missing or stamped with a different version/row count.
    let ensureSearchCsv (path: string) (rowCount: int64) =
        let valid =
            File.Exists path && File.Exists (metaPath path) &&
            (File.ReadAllText(metaPath path)).Contains(sprintf "rows=%d" rowCount) &&
            (File.ReadAllText(metaPath path)).Contains(profileVersion)
        if not valid then generateSearchCsv path rowCount
        path

[<MemoryDiagnoser>]
[<SimpleJob(warmupCount = 2, iterationCount = 5)>]
type RealSourceMaterializedBenchmarks() =

    let searchValue = "lorem"
    let dataDir = Path.Combine(__SOURCE_DIRECTORY__, "data")
    let csvPath100k = Path.Combine(dataDir, "b6-search-100k-random.csv")
    let csvPath1m = Path.Combine(dataDir, "b6-search-1m-random.csv")

    let mutable frame100k : Frame<int, string> = Unchecked.defaultof<_>
    let mutable frame1m : Frame<int, string> = Unchecked.defaultof<_>

    [<GlobalSetup>]
    member _.Setup() =
        ensureSearchCsv csvPath100k 100_000L |> ignore
        ensureSearchCsv csvPath1m 1_000_000L |> ignore
        frame100k <- Frame.ReadCsv(csvPath100k, inferRows = 100)
        frame1m <- Frame.ReadCsv(csvPath1m, inferRows = 100)

    [<Benchmark(Baseline = true)>]
    member _.MaterializedReadCsv_FilterScan_100k() =
        let col = frame100k.GetColumn<string>("Category")
        let mutable count = 0
        for i in 0 .. frame100k.RowCount - 1 do
            if col.GetAt(i) = searchValue then count <- count + 1
        count |> ignore

    [<Benchmark>]
    member _.MaterializedReadCsv_StatsSum_100k() =
        Stats.sum (frame100k.GetColumn<float>("Value")) |> ignore

    [<Benchmark>]
    member _.MaterializedReadCsv_FilterScan_1m() =
        let col = frame1m.GetColumn<string>("Category")
        let mutable count = 0
        for i in 0 .. frame1m.RowCount - 1 do
            if col.GetAt(i) = searchValue then count <- count + 1
        count |> ignore

    [<Benchmark>]
    member _.MaterializedReadCsv_StatsSum_1m() =
        Stats.sum (frame1m.GetColumn<float>("Value")) |> ignore
