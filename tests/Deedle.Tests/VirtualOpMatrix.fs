#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.VirtualOpMatrix
#endif

open System
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Virtual
open Deedle.Vectors.Virtual
open Deedle.Tests.VirtualInstrumentation

// ------------------------------------------------------------------------------------------------
// B3 — Operation virtualization matrix
// Assert B1 classifications with VirtualInstrumentation (scheme + access deltas).
// ------------------------------------------------------------------------------------------------

module private Op =
  let nSmall = 10_000L
  let nMed = 100_000L

  let assertVirtual (s: Series<_, _>) = SeriesProbe.isVirtual s |> shouldEqual true
  let assertLinear (s: Series<_, _>) = SeriesProbe.isLinear s |> shouldEqual true

// ------------------------------------------------------------------------------------------------
// VIRTUAL-preserving ops
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``B3 Slice preserves virtual storage without ValueAt`` () =
  let c, s = InstrumentedOrdinalSource.createOrdinalSeries Op.nMed
  c.Reset()
  let sliced = s.[100L .. 199L]
  let d = c.Snapshot()
  d.GetSubVectorCount |> should be (greaterThan 0)
  d.ValueAtCount |> shouldEqual 0
  Op.assertVirtual sliced
  sliced.KeyCount |> shouldEqual 100

[<Test>]
let ``B3 Lookup preserves virtual storage and touches one ValueAt`` () =
  let c, s = InstrumentedOrdinalSource.createOrdinalSeries Op.nMed
  c.Reset()
  s.TryGet(12345L) |> shouldEqual (OptionalValue 12345L)
  let d = c.Snapshot()
  d.ValueAtCount |> shouldEqual 1
  Op.assertVirtual s

[<Test>]
let ``B3 Metadata KeyCount is virtual and zero ValueAt`` () =
  let c, s = InstrumentedOrdinalSource.createOrdinalSeries Op.nMed
  c.Reset()
  s.KeyCount |> shouldEqual (int Op.nMed)
  c.Snapshot().ValueAtCount |> shouldEqual 0
  Op.assertVirtual s

[<Test>]
let ``B3 SelectValues preserves virtual storage without ValueAt`` () =
  let c, s = InstrumentedOrdinalSource.createFloatSeries Op.nMed
  c.Reset()
  let mapped = s |> Series.mapValues (fun v -> v + 1.0)
  let d = c.Snapshot()
  d.ValueAtCount |> shouldEqual 0
  Op.assertVirtual mapped

[<Test>]
let ``B3 Merge of ordinal virtual slices preserves virtual storage`` () =
  let c, s = InstrumentedOrdinalSource.createOrdinalSeries Op.nMed
  let a = s.[0L .. 99L]
  let b = s.[200L .. 299L]
  c.Reset()
  let merged = Series.merge a b
  let d = c.Snapshot()
  d.MergeWithCount |> should be (greaterThan 0)
  d.ValueAtCount |> shouldEqual 0
  Op.assertVirtual merged
  merged.KeyCount |> shouldEqual 200

[<Test>]
let ``B3 Ordered filterRowsBy preserves virtual row index via LookupRange`` () =
  let c, (f: Frame<DateTimeOffset, string>), _ = InstrumentedOrdinalSource.createOrderedSearchFrame Op.nMed
  c.Reset()
  let filtered = f |> Frame.filterRowsBy "S2" "lorem"
  let d = c.Snapshot()
  d.LookupRangeCount |> should be (greaterThan 0)
  d.ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> should be (greaterThan 0)

[<Test>]
let ``B3 sampleTimeInto keeps chunks virtual and does not scan the series`` () =
  let c, s = InstrumentedOrdinalSource.createOrderedFloatSeries Op.nMed
  c.Reset()
  let sampled = s |> Series.sampleTimeInto (TimeSpan.FromDays 365.0) Direction.Forward id
  let d = c.Snapshot()
  // KeyRange / boundary may probe a few index/value addresses; must not scan the series.
  d.ValueAtCount |> should be (lessThan 20)
  d.ValueAtCount |> should be (lessThan (int Op.nMed / 100))
  let chunk = sampled.GetAt(0)
  Op.assertVirtual chunk

// ------------------------------------------------------------------------------------------------
// MATERIALIZING ops
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``B3 GroupBy materializes to linear storage`` () =
  let c, s = InstrumentedOrdinalSource.createFloatSeries Op.nSmall
  let grouped = s |> Series.groupBy (fun _k v -> int v % 10)
  // Nested series values are built under linear builders for virtual sources.
  let first = grouped.GetAt(0)
  Op.assertLinear first
  c.Snapshot().ValueAtCount |> should be (greaterThan 0)

[<Test>]
let ``B3 WindowSize keeps nested window series virtual`` () =
  let _, s = InstrumentedOrdinalSource.createFloatSeries 1_000L
  let windows = s |> Series.windowSizeInto (5, Boundary.Skip) DataSegment.data
  let first = windows.GetAt(0)
  Op.assertVirtual first
  first.KeyCount |> shouldEqual 5

[<Test>]
let ``B3 Window aggregate of sums materializes the result series`` () =
  let _, s = InstrumentedOrdinalSource.createFloatSeries 1_000L
  let windows = s |> Series.windowSizeInto (5, Boundary.AtEnding) (fun w -> Stats.sum w.Data)
  Op.assertLinear windows

[<Test>]
let ``B3 Shift preserves virtual storage without ValueAt`` () =
  let c, s = InstrumentedOrdinalSource.createFloatSeries Op.nSmall
  c.Reset()
  let shifted = s |> Series.shift 1
  SeriesProbe.isVirtual shifted |> shouldEqual true
  c.Snapshot().ValueAtCount |> shouldEqual 0
  shifted.KeyCount |> shouldEqual (int Op.nSmall - 1)
  shifted.[1L] |> shouldEqual s.[0L]

[<Test>]
let ``B3 Slice then Stats.sum pulls only the slice`` () =
  let c, s = InstrumentedOrdinalSource.createFloatSeries Op.nMed
  let sliced = s.[100L .. 199L]
  c.Reset()
  Stats.sum sliced |> shouldEqual 14950.0
  c.Snapshot().ValueAtCount |> shouldEqual 100

[<Test>]
let ``B3 DropMissing materializes to linear storage`` () =
  let c = AccessCounters()
  let src =
    InstrumentedOrdinalSource<float>(Op.nSmall, float, c, hasMissing=true)
  let s = Virtual.CreateOrdinalSeries(src)
  let dropped = s |> Series.dropMissing
  Op.assertLinear dropped
  c.Snapshot().ValueAtCount |> should be (greaterThan 0)

[<Test>]
let ``B3 SortBy materializes to linear storage`` () =
  let c, s = InstrumentedOrdinalSource.createFloatSeries 1_000L
  let sorted = s |> Series.sortBy (fun v -> -v)
  Op.assertLinear sorted
  c.Snapshot().ValueAtCount |> should be (greaterThan 0)

[<Test>]
let ``B3 Intersect materializes to linear storage`` () =
  let _, s = InstrumentedOrdinalSource.createOrdinalSeries Op.nSmall
  let a = s.[0L .. 500L]
  let b = s.[250L .. 750L]
  let inter = Series.intersect a b
  Op.assertLinear inter

[<Test>]
let ``B3 ZipAlign join path materializes typical result`` () =
  let _, s1 = InstrumentedOrdinalSource.createFloatSeries Op.nSmall
  let _, s2 = InstrumentedOrdinalSource.createFloatSeries Op.nSmall
  let zipped = Series.zipAlign JoinKind.Inner Lookup.Exact s1 s2
  Op.assertLinear zipped

[<Test>]
let ``B3 Frame join of ordinal virtual frames materializes row index`` () =
  let _, s1 = InstrumentedOrdinalSource.createFloats Op.nSmall
  let _, s2 = InstrumentedOrdinalSource.createFloats Op.nSmall
  let f1 = Virtual.CreateOrdinalFrame(["A"], [s1 :> IVirtualVectorSource])
  let f2 = Virtual.CreateOrdinalFrame(["B"], [s2 :> IVirtualVectorSource])
  let joined = f1.Join(f2, JoinKind.Outer)
  FrameProbe.rowIndexIsVirtual joined |> shouldEqual false

[<Test>]
let ``B3 Stats.sum pulls values proportional to length (materializing pull)`` () =
  let c, s = InstrumentedOrdinalSource.createFloatSeries Op.nSmall
  c.Reset()
  Stats.sum s |> ignore
  let d = c.Snapshot()
  d.ValueAtCount |> shouldEqual (int Op.nSmall)

[<Test>]
let ``B3 Materialize flips series to linear`` () =
  let c, s = InstrumentedOrdinalSource.createOrdinalSeries 500L
  Op.assertVirtual s
  let mat = s.Materialize()
  Op.assertLinear mat
  c.Snapshot().ValueAtCount |> should be (greaterThan 0)

// ------------------------------------------------------------------------------------------------
// Ordinal filter / incomplete paths
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``B3 Ordinal filterRowsBy does not use virtual Search path`` () =
  let words = "lorem ipsum dolor sit amet".Split(' ')
  let c, s2 = InstrumentedOrdinalSource.createSearchableStrings Op.nSmall words
  let _, s1 = InstrumentedOrdinalSource.createLongs Op.nSmall
  let f = Virtual.CreateOrdinalFrame(["S1"; "S2"], [s1 :> IVirtualVectorSource; s2 :> IVirtualVectorSource])
  c.Reset()
  let filtered = f |> Frame.filterRowsBy "S2" "lorem"
  // Ordinal Search falls back to base builder → non-virtual row index
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual false
  // LookupRange is not used on the ordinal virtual Search short-circuit path
  c.Snapshot().LookupRangeCount |> shouldEqual 0

[<Test>]
let ``B3 FillMissing under virtual scheme stays virtual`` () =
  let c = AccessCounters()
  let src = InstrumentedOrdinalSource<float>(1_000L, float, c, hasMissing=true)
  let s = Virtual.CreateOrdinalSeries(src)
  c.Reset()
  let filled = s |> Series.fillMissing Direction.Forward
  SeriesProbe.isVirtual filled |> shouldEqual true
  // Address 3 is missing (every 3rd); forward-fill copies the previous present value.
  filled.TryGet(3L) |> shouldEqual (OptionalValue 2.0)
  c.Snapshot().ValueAtCount |> should be (greaterThan 0)
