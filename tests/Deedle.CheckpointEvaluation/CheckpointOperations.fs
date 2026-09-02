module Deedle.CheckpointEvaluation.Operations

open System
open System.IO
open Deedle
open Deedle.CheckpointEvaluation.CsvFixture
#if !CHECKPOINT_CP1
open Deedle.TestData
open Deedle.Parquet
open Deedle.Parquet.Virtual.Sources
#endif
open Deedle.Virtual
open Deedle.Vectors.Virtual
open Deedle.Tests.VirtualInstrumentation
open Deedle.CheckpointEvaluation.Measure

module private Fixtures =
  let nSmall = 10_000L
  let nMed = 100_000L
  let searchValue = "lorem"

  let dataDir =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "Deedle.Benchmarks", "data")
    |> Path.GetFullPath

  let csvPath = Path.Combine(dataDir, defaultDatasetName)

#if !CHECKPOINT_CP1
  let parquetPath = Path.Combine(dataDir, ParquetTestData.defaultDatasetName)
#endif

  let ensureFiles () =
    if not (Directory.Exists dataDir) then Directory.CreateDirectory dataDir |> ignore
    ensureSearchCsv csvPath nMed 42 |> ignore
#if !CHECKPOINT_CP1
    ParquetTestData.ensureSearchParquet parquetPath nMed |> ignore
#endif

  let materializedCsv () = Frame.ReadCsv(csvPath, inferRows = 100)

#if !CHECKPOINT_CP1
  let virtualCsvFrame () =
    Virtual.ReadCsv<DateTimeOffset>(
      csvPath,
      indexColumn = "Timestamp",
      searchColumns = [ VirtualSearchColumn.withString "Category" (VirtualLookupRange.forRepeatingCycle CsvTestData.words8) ],
      columnKeys = [ "Id"; "Category"; "Label"; "Value" ])

  let virtualParquetFrame () =
    Virtual.ReadParquet(
      parquetPath,
      indexColumn = "Timestamp",
      searchColumns = [ VirtualSearchColumn.withString "Category" (VirtualLookupRange.forRepeatingCycle CsvTestData.words8) ],
      columnKeys = [ "Id"; "Category"; "Label"; "Value" ])
#endif

  let linearFloatSeries (length: int64) =
    let rnd = Random(0)
    series [ for i in 0L .. length - 1L -> i => rnd.NextDouble() ]

  let linearIntSeries (length: int64) =
    series [ for i in 0L .. length - 1L -> i => i ]

  let linearFrameFromFloatSeries (length: int64) =
    let s = linearFloatSeries length
    Frame.ofColumns [ "A" => s ]

let private syntheticOps (isCp1: bool) =
  [ tryOp "Slice / GetRange" "synthetic" (fun () ->
      measureSeries "Slice / GetRange" "synthetic" "virtual" Fixtures.nMed "GetSubVector only"
        (fun () -> InstrumentedOrdinalSource.createOrdinalSeries Fixtures.nMed)
        (fun s -> s.[100L .. 199L]))

    tryOp "Lookup / TryGet" "synthetic" (fun () ->
      let c, s = InstrumentedOrdinalSource.createOrdinalSeries Fixtures.nMed
      let meanMs, alloc = measureTimed (fun () -> s.TryGet(12345L) |> ignore)
      c.Reset()
      let before = c.Snapshot()
      s.TryGet(12345L) |> ignore
      let d = AccessSnapshot.delta before (c.Snapshot())
      fromCounters "Lookup / TryGet" "synthetic" "virtual" (shapeOfSeries s) "Single lookup" Fixtures.nMed (meanMs, alloc, d))

    tryOp "Metadata (KeyCount)" "synthetic" (fun () ->
      let c, s = InstrumentedOrdinalSource.createOrdinalSeries Fixtures.nMed
      let meanMs, alloc = measureTimed (fun () -> s.KeyCount |> ignore)
      c.Reset()
      let before = c.Snapshot()
      s.KeyCount |> ignore
      let d = AccessSnapshot.delta before (c.Snapshot())
      fromCounters "Metadata (KeyCount)" "synthetic" "virtual" (shapeOfSeries s) "No data touches" Fixtures.nMed (meanMs, alloc, d))

    tryOp "SelectValues / map" "synthetic" (fun () ->
      measureSeries "SelectValues / map" "synthetic" "virtual" Fixtures.nMed "Lazy map"
        (fun () -> InstrumentedOrdinalSource.createFloatSeries Fixtures.nMed)
        (fun s -> s |> Series.mapValues (fun v -> v + 1.0)))

    tryOp "Merge (ordinal slices)" "synthetic" (fun () ->
      let c, s = InstrumentedOrdinalSource.createOrdinalSeries Fixtures.nMed
      let a = s.[0L .. 99L]
      let b = s.[200L .. 299L]
      let meanMs, alloc =
        measureTimed (fun () ->
          c.Reset()
          let before = c.Snapshot()
          Series.merge a b |> ignore
          AccessSnapshot.delta before (c.Snapshot()) |> ignore)
      c.Reset()
      let before = c.Snapshot()
      let merged = Series.merge a b
      let d = AccessSnapshot.delta before (c.Snapshot())
      fromCounters "Merge (ordinal slices)" "synthetic" "virtual" (shapeOfSeries merged) "MergeWith" Fixtures.nMed (meanMs, alloc, d))

    tryOp "filterRowsBy (ordered + LookupRange)" "synthetic" (fun () ->
      measureFrame "filterRowsBy (ordered + LookupRange)" "synthetic" "virtual" Fixtures.nMed "LookupRange"
        (fun () -> InstrumentedOrdinalSource.createOrderedSearchFrame Fixtures.nMed |> fun (c, f, _) -> c, f)
        (fun f -> f |> Frame.filterRowsBy "S2" Fixtures.searchValue))

    tryOp "filterRowsBy (ordinal + LookupRange)" "synthetic" (fun () ->
      measureFrame "filterRowsBy (ordinal + LookupRange)" "synthetic" "virtual" Fixtures.nMed "Ordinal filter"
        (fun () -> InstrumentedOrdinalSource.createOrdinalSearchFrame Fixtures.nMed |> fun (c, f, _) -> c, f)
        (fun f -> f |> Frame.filterRowsBy "S2" Fixtures.searchValue))

    tryOp "filterRowsBy (non-search float, scan)" "synthetic" (fun () ->
      let c, f, _, floatFilter, _ = InstrumentedOrdinalSource.createOrderedSearchWithScanColumnsFrame Fixtures.nMed
      let meanMs, alloc =
        measureTimed (fun () ->
          c.Reset()
          let before = c.Snapshot()
          f |> Frame.filterRowsBy "S3" floatFilter |> ignore
          AccessSnapshot.delta before (c.Snapshot()) |> ignore)
      c.Reset()
      let before = c.Snapshot()
      let filtered = f |> Frame.filterRowsBy "S3" floatFilter
      let d = AccessSnapshot.delta before (c.Snapshot())
      fromCounters "filterRowsBy (non-search float, scan)" "synthetic" "virtual" (shapeOfFrame filtered) "Scan fallback" Fixtures.nMed (meanMs, alloc, d))

    tryOp "filterRowsBy (non-search string, scan)" "synthetic" (fun () ->
      let c, f, _, _, labelFilter = InstrumentedOrdinalSource.createOrderedSearchWithScanColumnsFrame Fixtures.nMed
      let meanMs, alloc =
        measureTimed (fun () ->
          c.Reset()
          let before = c.Snapshot()
          f |> Frame.filterRowsBy "S4" labelFilter |> ignore
          AccessSnapshot.delta before (c.Snapshot()) |> ignore)
      c.Reset()
      let before = c.Snapshot()
      let filtered = f |> Frame.filterRowsBy "S4" labelFilter
      let d = AccessSnapshot.delta before (c.Snapshot())
      fromCounters "filterRowsBy (non-search string, scan)" "synthetic" "virtual" (shapeOfFrame filtered) "Scan fallback" Fixtures.nMed (meanMs, alloc, d))

    tryOp "sampleTimeInto (chunks)" "synthetic" (fun () ->
      let c, s = InstrumentedOrdinalSource.createOrderedFloatSeries Fixtures.nMed
      let meanMs, alloc =
        measureTimed (fun () ->
          c.Reset()
          let before = c.Snapshot()
          s |> Series.sampleTimeInto (TimeSpan.FromDays 365.0) Direction.Forward id |> ignore
          AccessSnapshot.delta before (c.Snapshot()) |> ignore)
      c.Reset()
      let before = c.Snapshot()
      let sampled = s |> Series.sampleTimeInto (TimeSpan.FromDays 365.0) Direction.Forward id
      let chunk = sampled.GetAt(0)
      let d = AccessSnapshot.delta before (c.Snapshot())
      fromCounters "sampleTimeInto (chunks)" "synthetic" "virtual" (shapeOfSeries chunk) "Resample chunks" Fixtures.nMed (meanMs, alloc, d))

    tryOp "GroupBy" "synthetic" (fun () ->
      measureSeries "GroupBy" "synthetic" "virtual" Fixtures.nSmall "Nested groups"
        (fun () -> InstrumentedOrdinalSource.createFloatSeries Fixtures.nSmall)
        (fun s -> s |> Series.groupBy (fun _k v -> int v % 10) |> fun g -> g.GetAt(0)))

    tryOp "WindowSize (nested)" "synthetic" (fun () ->
      measureSeries "WindowSize (nested)" "synthetic" "virtual" 1_000L "Nested windows"
        (fun () -> InstrumentedOrdinalSource.createFloatSeries 1_000L)
        (fun s -> s |> Series.windowSizeInto (5, Boundary.Skip) DataSegment.data))

    tryOp "Window aggregate (sum)" "synthetic" (fun () ->
      measureSeries "Window aggregate (sum)" "synthetic" "virtual" 1_000L "Window sum"
        (fun () -> InstrumentedOrdinalSource.createFloatSeries 1_000L)
        (fun s -> s |> Series.windowSizeInto (5, Boundary.AtEnding) (fun w -> Stats.sum w.Data)))

    tryOp "Shift" "synthetic" (fun () ->
      if isCp1 then
        measurePlain "Shift" "synthetic" "materialized" "FullyLinear" "Eager Frame fallback at CP1"
          (fun () -> Fixtures.linearFloatSeries Fixtures.nSmall |> Series.shift 1 |> ignore)
      else
        measureSeries "Shift" "synthetic" "virtual" Fixtures.nSmall "B9 virtual shift"
          (fun () -> InstrumentedOrdinalSource.createFloatSeries Fixtures.nSmall)
          (fun s -> s |> Series.shift 1))

    tryOp "Diff" "synthetic" (fun () ->
      if isCp1 then
        measurePlain "Diff" "synthetic" "materialized" "FullyLinear" "Eager diff at CP1"
          (fun () -> Fixtures.linearFloatSeries Fixtures.nSmall |> Series.diff 1 |> ignore)
      else
        measureSeries "Diff" "synthetic" "virtual" Fixtures.nSmall "B9 virtual diff"
          (fun () -> InstrumentedOrdinalSource.createFloatSeries Fixtures.nSmall)
          (fun s -> s |> Series.diff 1))

    tryOp "Slice then Stats.sum" "synthetic" (fun () ->
      measurePull "Slice then Stats.sum" "synthetic" "virtual" 100L "Slice-limited pull"
        (fun () -> InstrumentedOrdinalSource.createFloatSeries Fixtures.nMed)
        (fun s -> Stats.sum s.[100L .. 199L] |> ignore))

    tryOp "DropMissing" "synthetic" (fun () ->
      measureSeries "DropMissing" "synthetic" "virtual" Fixtures.nSmall "Drop missing"
        (fun () ->
          let c = AccessCounters()
          let src = InstrumentedOrdinalSource<float>(Fixtures.nSmall, float, c, hasMissing = true)
          c, Virtual.CreateOrdinalSeries(src))
        (fun s -> s |> Series.dropMissing))

    tryOp "SortBy" "synthetic" (fun () ->
      measureSeries "SortBy" "synthetic" "virtual" 1_000L "Value sort"
        (fun () -> InstrumentedOrdinalSource.createFloatSeries 1_000L)
        (fun s -> s |> Series.sortBy (fun v -> -v)))

    tryOp "ZipAlign (identical ordinal)" "synthetic" (fun () ->
      if isCp1 then
        measurePlain "ZipAlign (identical ordinal)" "synthetic" "materialized" "FullyLinear" "Eager series zip at CP1"
          (fun () ->
            let s1 = Fixtures.linearIntSeries Fixtures.nSmall
            let s2 = Fixtures.linearFloatSeries Fixtures.nSmall
            Series.zipAlign JoinKind.Inner Lookup.Exact s1 s2 |> ignore)
      else
        let _, s1 = InstrumentedOrdinalSource.createFloatSeries Fixtures.nSmall
        let _, s2 = InstrumentedOrdinalSource.createFloatSeries Fixtures.nSmall
        let meanMs, alloc = measureTimed (fun () -> Series.zipAlign JoinKind.Inner Lookup.Exact s1 s2 |> ignore)
        let zipped = Series.zipAlign JoinKind.Inner Lookup.Exact s1 s2
        fromCounters "ZipAlign (identical ordinal)" "synthetic" "virtual" (shapeOfSeries zipped) "B9 zip" Fixtures.nSmall (meanMs, alloc, zeroSnapshot))

    tryOp "Frame Join (identical ordinal)" "synthetic" (fun () ->
      if isCp1 then
        measurePlain "Frame Join (identical ordinal)" "synthetic" "materialized" "FrameRowLinear" "Eager frame join at CP1"
          (fun () ->
            let f1 = Fixtures.linearFrameFromFloatSeries Fixtures.nSmall
            let f2 = Fixtures.linearFrameFromFloatSeries Fixtures.nSmall
            f1.Join(f2, JoinKind.Outer) |> ignore)
      else
        let _, s1 = InstrumentedOrdinalSource.createFloats Fixtures.nSmall
        let _, s2 = InstrumentedOrdinalSource.createFloats Fixtures.nSmall
        let f1 = Virtual.CreateOrdinalFrame([ "A" ], [ s1 :> IVirtualVectorSource ])
        let f2 = Virtual.CreateOrdinalFrame([ "B" ], [ s2 :> IVirtualVectorSource ])
        let meanMs, alloc = measureTimed (fun () -> f1.Join(f2, JoinKind.Outer) |> ignore)
        let joined = f1.Join(f2, JoinKind.Outer)
        fromCounters "Frame Join (identical ordinal)" "synthetic" "virtual" (shapeOfFrame joined) "B9 join" Fixtures.nSmall (meanMs, alloc, zeroSnapshot))

    tryOp "Stats.sum (full series)" "synthetic" (fun () ->
      measurePull "Stats.sum (full series)" "synthetic" "virtual" Fixtures.nSmall "Full pull"
        (fun () -> InstrumentedOrdinalSource.createFloatSeries Fixtures.nSmall)
        (fun s -> Stats.sum s |> ignore))

    tryOp "Materialize()" "synthetic" (fun () ->
      measureSeries "Materialize()" "synthetic" "virtual" 500L "Explicit materialize"
        (fun () -> InstrumentedOrdinalSource.createOrdinalSeries 500L)
        (fun s -> s.Materialize()))

    tryOp "FillMissing (Forward)" "synthetic" (fun () ->
      measureSeries "FillMissing (Forward)" "synthetic" "virtual" 1_000L "Forward fill"
        (fun () ->
          let c = AccessCounters()
          let src = InstrumentedOrdinalSource<float>(1_000L, float, c, hasMissing = true)
          c, Virtual.CreateOrdinalSeries(src))
        (fun s -> s |> Series.fillMissing Direction.Forward)) ]

let private fileOps (_isCp1: bool) =
#if CHECKPOINT_CP1
  [ tryOp "File.Csv.FilterRowsBy" "file" (fun () ->
      measurePlain "File.Csv.FilterRowsBy" "file" "materialized" "FrameRowLinear" "Frame.ReadCsv + filter scan"
        (fun () ->
          let frame = Fixtures.materializedCsv()
          let col = frame.GetColumn<string>("Category")
          let mutable count = 0
          for i in 0 .. frame.RowCount - 1 do
            if col.GetAt(i) = Fixtures.searchValue then count <- count + 1
          count |> ignore))

    tryOp "File.Csv.FilterRowsBy2" "file" (fun () ->
      measurePlain "File.Csv.FilterRowsBy2" "file" "materialized" "FrameRowLinear" "Chained filterRowsBy at CP1"
        (fun () ->
          Fixtures.materializedCsv()
          |> Frame.filterRowsBy "Category" Fixtures.searchValue
          |> Frame.filterRowsBy "Category" Fixtures.searchValue
          |> ignore))

    tryOp "File.Csv.Slice1000" "file" (fun () ->
      measurePlain "File.Csv.Slice1000" "file" "materialized" "FullyLinear" "Materialized column slice"
        (fun () ->
          let frame = Fixtures.materializedCsv()
          frame.GetColumn<float>("Value").[0 .. 999] |> ignore))

    tryOp "File.Csv.StatsSum.Materialized" "file" (fun () ->
      measurePlain "File.Csv.StatsSum.Materialized" "file" "materialized" "FullyLinear" "Eager CSV sum"
        (fun () ->
          let frame = Fixtures.materializedCsv()
          Stats.sum (frame.GetColumn<float>("Value")) |> ignore)) ]
#else
  [ tryOp "File.Csv.FilterRowsBy" "file" (fun () ->
      measurePlain "File.Csv.FilterRowsBy" "file" "virtual" "FrameRowVirtual" "Virtual.ReadCsv filter"
        (fun () -> Fixtures.virtualCsvFrame() |> Frame.filterRowsBy "Category" Fixtures.searchValue |> ignore))

    tryOp "File.Csv.FilterRowsBy2" "file" (fun () ->
      measurePlain "File.Csv.FilterRowsBy2" "file" "virtual" "FrameRowVirtual" "Fused virtual filter"
        (fun () ->
          Fixtures.virtualCsvFrame()
          |> Frame.filterRowsBy2 "Category" Fixtures.searchValue "Category" Fixtures.searchValue
          |> ignore))

    tryOp "File.Csv.Slice1000" "file" (fun () ->
      measurePlain "File.Csv.Slice1000" "file" "virtual" "FullyVirtual" "Virtual CSV series slice"
        (fun () ->
          let series = CsvTestData.createFloatValueSeries Fixtures.csvPath
          series.[0L .. 999L] |> ignore))

    tryOp "File.Csv.StatsSum.Materialized" "file" (fun () ->
      measurePlain "File.Csv.StatsSum.Materialized" "file" "materialized" "FullyLinear" "Eager CSV sum"
        (fun () ->
          let frame = Fixtures.materializedCsv()
          Stats.sum (frame.GetColumn<float>("Value")) |> ignore))

    tryOp "File.Csv.StatsSum.Virtual" "file" (fun () ->
      measurePlain "File.Csv.StatsSum.Virtual" "file" "virtual" "VIRTUAL (pull)" "Virtual CSV sum"
        (fun () ->
          let series = CsvTestData.createFloatValueSeries Fixtures.csvPath
          Stats.sum series |> ignore))

    tryOp "File.Parquet.FilterRowsBy" "file" (fun () ->
      measurePlain "File.Parquet.FilterRowsBy" "file" "virtual" "FrameRowVirtual" "Virtual.ReadParquet filter"
        (fun () -> Fixtures.virtualParquetFrame() |> Frame.filterRowsBy "Category" Fixtures.searchValue |> ignore))

    tryOp "File.Parquet.StatsSum.Virtual" "file" (fun () ->
      measurePlain "File.Parquet.StatsSum.Virtual" "file" "virtual" "VIRTUAL (pull)" "Virtual Parquet sum"
        (fun () ->
          let series = ParquetTestData.createFloatValueSeries Fixtures.parquetPath
          Stats.sum series |> ignore))

    tryOp "File.Parquet.StatsSum.Materialized" "file" (fun () ->
      measurePlain "File.Parquet.StatsSum.Materialized" "file" "materialized" "FullyLinear" "Eager Parquet sum"
        (fun () ->
          let frame = Frame.readParquet Fixtures.parquetPath
          Stats.sum (frame.GetColumn<float>("Value")) |> ignore)) ]
#endif

let runAll (checkpoint: string) : OpMetrics list =
  Fixtures.ensureFiles()
  let isCp1 = checkpoint.Equals("CP1", StringComparison.OrdinalIgnoreCase)
  syntheticOps isCp1 @ fileOps isCp1
