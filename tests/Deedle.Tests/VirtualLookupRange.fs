#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.VirtualLookupRange
#endif

open System
open System.Diagnostics
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Virtual
open Deedle.Tests.VirtualInstrumentation

// ------------------------------------------------------------------------------------------------
// B4 — LookupRange quality sensitivity
// Compare tight Custom/Fixed vs naive full-range vs linear-scan fallback.
// ------------------------------------------------------------------------------------------------

module private B4 =
  let nLarge = 100_000L
  let nTiming = 100_000L
  let searchValue = "lorem"

  let expectedMatchCount (length: int64) (step: int) =
    if length <= 0L then 0
    else int ((length - 1L) / int64 step) + 1

  let filterAndRead (frame: Frame<DateTimeOffset, string>) (c: AccessCounters) (readCount: int) =
    c.Reset()
    let before = c.Snapshot()
    let filtered = frame |> Frame.filterRowsBy "S2" searchValue
    let afterFilter = c.Snapshot()
    let filterDelta = AccessSnapshot.delta before afterFilter
    for i in 0 .. readCount - 1 do
      if int64 i < filtered.RowIndex.KeyCount then
        filtered?S1.GetAt(i) |> ignore
    let afterRead = c.Snapshot()
    let readDelta = AccessSnapshot.delta afterFilter afterRead
    filtered, filterDelta, readDelta

  let filterAndReadOrdinal (frame: Frame<int64, string>) (c: AccessCounters) (readCount: int) =
    c.Reset()
    let before = c.Snapshot()
    let filtered = frame |> Frame.filterRowsBy "S2" searchValue
    let afterFilter = c.Snapshot()
    let filterDelta = AccessSnapshot.delta before afterFilter
    for i in 0 .. readCount - 1 do
      if int64 i < filtered.RowIndex.KeyCount then
        filtered?S1.GetAt(i) |> ignore
    let afterRead = c.Snapshot()
    let readDelta = AccessSnapshot.delta afterFilter afterRead
    filtered, filterDelta, readDelta

  let elapsedMs (f: unit -> unit) =
    let sw = Stopwatch.StartNew()
    f()
    sw.Stop()
    float sw.ElapsedMilliseconds

[<Test>]
let ``B4 Step LookupRange filters virtually with zero ValueAt at filter time`` () =
  let c, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrame B4.nLarge
  let filtered, filterDelta, _ = B4.filterAndRead frame c 0
  filterDelta.LookupRangeCount |> shouldEqual 1
  filterDelta.ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual (B4.expectedMatchCount B4.nLarge words.Length)

[<Test>]
let ``B4 ExactFixed LookupRange is virtual but only retains first match`` () =
  let words = "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')
  let c, frame, _ =
    InstrumentedOrdinalSource.createOrderedSearchFrameWith B4.nLarge (LookupRangeExactFixed (fun v ->
      let o = words |> Array.findIndex ((=) v) |> int64
      o, o))
  let filtered, filterDelta, _ = B4.filterAndRead frame c 0
  filterDelta.LookupRangeCount |> shouldEqual 1
  filterDelta.ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual 1

[<Test>]
let ``B4 FullFixed LookupRange is virtual but retains entire series (naive)`` () =
  let c, frame, _ =
    InstrumentedOrdinalSource.createOrderedSearchFrameWith B4.nLarge LookupRangeFullFixed
  let filtered, filterDelta, _ = B4.filterAndRead frame c 0
  filterDelta.LookupRangeCount |> shouldEqual 1
  filterDelta.ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual (int B4.nLarge)

[<Test>]
let ``B4 Ordinal frame filter scans all rows (linear Search fallback)`` () =
  let c, frame, words = InstrumentedOrdinalSource.createOrdinalSearchFrame B4.nLarge
  let filtered, filterDelta, _ = B4.filterAndReadOrdinal frame c 0
  filterDelta.LookupRangeCount |> shouldEqual 0
  filterDelta.ValueAtCount |> should be (greaterThanOrEqualTo (int B4.nLarge))
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual false
  filtered.RowCount |> shouldEqual (B4.expectedMatchCount B4.nLarge words.Length)

[<Test>]
let ``B4 Step path reads touch only requested rows after filter`` () =
  let c, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrame B4.nLarge
  let readN = 20
  let _, filterDelta, readDelta = B4.filterAndRead frame c readN
  filterDelta.ValueAtCount |> shouldEqual 0
  readDelta.ValueAtCount |> should be (greaterThan 0)
  readDelta.ValueAtCount |> should be (lessThan (readN * 3))

[<Test>]
let ``B4 FullFixed naive range pays full read cost even for few rows`` () =
  let c, frame, _ =
    InstrumentedOrdinalSource.createOrderedSearchFrameWith B4.nLarge LookupRangeFullFixed
  let readN = 20
  let filtered, filterDelta, readDelta = B4.filterAndRead frame c readN
  filterDelta.ValueAtCount |> shouldEqual 0
  filtered.RowCount |> shouldEqual (int B4.nLarge)
  readDelta.ValueAtCount |> should be (lessThan (readN * 3))

[<Test>]
let ``B4 Mapped search column without reverse lookup scans at filter time`` () =
  let c, frame, words = InstrumentedOrdinalSource.createOrderedMappedSearchFrame B4.nLarge
  c.Reset()
  let filtered = frame |> Frame.filterRowsBy "S2" (B4.searchValue.ToUpperInvariant())
  let d = c.Snapshot()
  d.LookupRangeCount |> shouldEqual 0
  d.ValueAtCount |> should be (greaterThanOrEqualTo (int B4.nLarge))
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual (B4.expectedMatchCount B4.nLarge words.Length)

[<Test>]
let ``B4 Step filter is faster than ordinal linear scan on large frame`` () =
  let virtualMs =
    B4.elapsedMs (fun () ->
      let c, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrame B4.nTiming
      c.Reset()
      frame |> Frame.filterRowsBy "S2" B4.searchValue |> ignore)
  let linearMs =
    B4.elapsedMs (fun () ->
      let c, frame, _ = InstrumentedOrdinalSource.createOrdinalSearchFrame B4.nTiming
      c.Reset()
      frame |> Frame.filterRowsBy "S2" B4.searchValue |> ignore)
  virtualMs |> should be (lessThan (linearMs * 0.5))

[<Test>]
let ``B4 Step filter plus partial read is faster than linear scan`` () =
  let virtualMs =
    B4.elapsedMs (fun () ->
      let c, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrame B4.nTiming
      let filtered, _, _ = B4.filterAndRead frame c 50
      filtered.RowCount |> ignore)
  let linearMs =
    B4.elapsedMs (fun () ->
      let c, frame, _ = InstrumentedOrdinalSource.createOrdinalSearchFrame B4.nTiming
      let filtered, _, _ = B4.filterAndReadOrdinal frame c 50
      filtered.RowCount |> ignore)
  virtualMs |> should be (lessThan (linearMs * 0.5))
