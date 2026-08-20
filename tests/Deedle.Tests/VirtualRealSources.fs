#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#load "CsvFileVirtualSource.fs"
#else
module Deedle.Tests.VirtualRealSources
#endif

open System
open System.Diagnostics
open System.Globalization
open System.IO
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Virtual
open Deedle.Tests.VirtualInstrumentation
open Deedle.Virtual.Sources
open Deedle.Tests.CsvFileVirtualSource

// ------------------------------------------------------------------------------------------------
// B6 — real / large-backed virtual sources (phase 2)
// ------------------------------------------------------------------------------------------------

module private B6 =
  let nLarge = 100_000L
  let searchValue = "lorem"
  let dataDir = Path.Combine(__SOURCE_DIRECTORY__, "data")
  let csvPath = Path.Combine(dataDir, CsvHarness.defaultDatasetName)
  let gate = obj()

  let ensureDataset () =
    lock gate (fun () ->
      Directory.CreateDirectory dataDir |> ignore
      CsvHarness.ensureSearchCsv csvPath nLarge)

  let expectedMatchCount (length: int64) (step: int) =
    if length <= 0L then 0
    else int ((length - 1L) / int64 step) + 1

  let elapsedMs (f: unit -> unit) =
    let sw = Stopwatch.StartNew()
    f()
    sw.Stop()
    float sw.ElapsedMilliseconds


[<Test; NonParallelizable>]
let ``B6 generated CSV has expected schema`` () =
  B6.ensureDataset() |> ignore
  let idx = CsvLineIndex(B6.csvPath)
  idx.Length |> shouldEqual B6.nLarge
  let fields = idx.ReadFields 0L
  fields.Length |> shouldEqual 4
  fields.[2] |> shouldEqual "lorem"
  let meta = CsvHarness.readMeta B6.csvPath
  meta.Seed |> shouldEqual CsvHarness.defaultSeed
  meta.RowCount |> shouldEqual B6.nLarge
  // Non-consecutive ids: row ordinals 0 and 1 should not both equal their row index.
  let id0 = Int32.Parse(fields.[0])
  let id1 = Int32.Parse((idx.ReadFields 1L).[0])
  (id0 = 0 && id1 = 1) |> shouldEqual false

[<Test; NonParallelizable>]
let ``B6 CSV virtual frame preserves virtual row index on filter`` () =
  B6.ensureDataset() |> ignore
  let c, frame, words = CsvFileVirtualSource.createOrderedSearchFrame B6.csvPath (AccessCounters())
  c.Reset()
  let filtered = frame |> Frame.filterRowsBy "S2" B6.searchValue
  let d = c.Snapshot()
  d.LookupRangeCount |> should be (greaterThan 0)
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual (B6.expectedMatchCount B6.nLarge words.Length)

[<Test; NonParallelizable>]
let ``B6 CSV virtual filter does not scan all rows at filter time`` () =
  B6.ensureDataset() |> ignore
  let c, frame, _ = CsvFileVirtualSource.createOrderedSearchFrame B6.csvPath (AccessCounters())
  c.Reset()
  frame |> Frame.filterRowsBy "S2" B6.searchValue |> ignore
  let d = c.Snapshot()
  d.ValueAtCount |> should be (lessThan 100)
  d.LookupRangeCount |> shouldEqual 1

[<Test; NonParallelizable>]
let ``B6 materialized ReadCsv loads full dataset`` () =
  B6.ensureDataset() |> ignore
  let frame = Frame.ReadCsv(B6.csvPath, inferRows=100)
  frame.RowCount |> shouldEqual (int B6.nLarge)

[<Test; NonParallelizable>]
let ``B6 CSV virtual slice reads only requested rows`` () =
  B6.ensureDataset() |> ignore
  let c, series = CsvFileVirtualSource.createFloatValueSeries B6.csvPath (AccessCounters())
  c.Reset()
  let sliced = series.[1000L .. 1099L]
  SeriesProbe.isVirtual sliced |> shouldEqual true
  sliced.KeyCount |> shouldEqual 100
  c.Snapshot().ValueAtCount |> shouldEqual 0
  let expectedAt1000 =
    CsvLineIndex(B6.csvPath).ReadFields(1000L).[3]
    |> fun s -> Double.Parse(s, CultureInfo.InvariantCulture)
  sliced.GetAt(0) |> shouldEqual expectedAt1000
  c.Snapshot().ValueAtCount |> shouldEqual 1
  c.Snapshot().ValueAtCount |> shouldEqual 1

[<Test; NonParallelizable>]
let ``B6 CSV virtual Stats.sum materializes full column pull`` () =
  B6.ensureDataset() |> ignore
  let c, series = CsvFileVirtualSource.createFloatValueSeries B6.csvPath (AccessCounters())
  c.Reset()
  let expectedSum = CsvHarness.readMeta(B6.csvPath).ValueSum
  Stats.sum series |> shouldEqual expectedSum
  let d = c.Snapshot()
  d.ValueAtCount |> shouldEqual (int B6.nLarge)
  SeriesProbe.isVirtual series |> shouldEqual true

[<Test; NonParallelizable>]
let ``B6 file-backed filter is faster than materialized full scan`` () =
  B6.ensureDataset() |> ignore
  let virtualMs =
    B6.elapsedMs (fun () ->
      let c, frame, _ = CsvFileVirtualSource.createOrderedSearchFrame B6.csvPath (AccessCounters())
      c.Reset()
      frame |> Frame.filterRowsBy "S2" B6.searchValue |> ignore)
  let materializedMs =
    B6.elapsedMs (fun () ->
      let frame = Frame.ReadCsv(B6.csvPath, inferRows=100)
      let col = frame.GetColumn<string>("Category")
      seq { for i in 0 .. frame.RowCount - 1 do if col.GetAt(i) = B6.searchValue then yield () }
      |> Seq.length
      |> ignore)
  virtualMs |> should be (lessThan materializedMs)
