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
open System.IO
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

  let filterBy (frame: Frame<DateTimeOffset, string>) (c: AccessCounters) (readCount: int) (value: string) =
    c.Reset()
    let before = c.Snapshot()
    let filtered = frame |> Frame.filterRowsBy "S2" value
    let afterFilter = c.Snapshot()
    let filterDelta = AccessSnapshot.delta before afterFilter
    for i in 0 .. readCount - 1 do
      if int64 i < filtered.RowIndex.KeyCount then
        filtered?S1.GetAt(i) |> ignore
    let afterRead = c.Snapshot()
    let readDelta = AccessSnapshot.delta afterFilter afterRead
    filtered, filterDelta, readDelta

  let filterAndRead frame c readCount =
    filterBy frame c readCount searchValue

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

// ------------------------------------------------------------------------------------------------
// B4 profile baseline reporter — writes metrics for all data profiles
// ------------------------------------------------------------------------------------------------

module B4ProfileReport =
  open System.IO
  open System.Text

  type Row =
    { Profile: string
      LookupRange: string
      N: int64
      Search: string
      VirtualFilter: bool
      FilterValueAt: int
      FilterLookupRange: int
      ResultRows: int
      ExpectedRows: int
      ReadValueAt20: int
      FilterMs: float }

  let private n = B4.nLarge
  let private readN = 20

  let private runFilter (setup: unit -> AccessCounters * Frame<DateTimeOffset, string> * string * int) =
    let c, frame, search, expected = setup ()
    let filterMs =
      B4.elapsedMs (fun () ->
        c.Reset()
        frame |> Frame.filterRowsBy "S2" search |> ignore)
    let filtered, filterDelta, readDelta = B4.filterBy frame c readN search
    { Profile = ""
      LookupRange = ""
      N = n
      Search = search
      VirtualFilter = FrameProbe.rowIndexIsVirtual filtered
      FilterValueAt = filterDelta.ValueAtCount
      FilterLookupRange = filterDelta.LookupRangeCount
      ResultRows = filtered.RowCount
      ExpectedRows = expected
      ReadValueAt20 = readDelta.ValueAtCount
      FilterMs = filterMs }

  let private runOrdinal () =
    let c, frame, words = InstrumentedOrdinalSource.createOrdinalSearchFrame n
    let expected = B4.expectedMatchCount n words.Length
    let filterMs =
      B4.elapsedMs (fun () ->
        c.Reset()
        frame |> Frame.filterRowsBy "S2" B4.searchValue |> ignore)
    let filtered, filterDelta, readDelta = B4.filterAndReadOrdinal frame c readN
    { Profile = "Default 8-word (ordinal index)"
      LookupRange = "Unsupported (linear Search)"
      N = n
      Search = B4.searchValue
      VirtualFilter = FrameProbe.rowIndexIsVirtual filtered
      FilterValueAt = filterDelta.ValueAtCount
      FilterLookupRange = filterDelta.LookupRangeCount
      ResultRows = filtered.RowCount
      ExpectedRows = expected
      ReadValueAt20 = readDelta.ValueAtCount
      FilterMs = filterMs }

  let private runMapped () =
    let c, frame, words = InstrumentedOrdinalSource.createOrderedMappedSearchFrame n
    let expected = B4.expectedMatchCount n words.Length
    let search = B4.searchValue.ToUpperInvariant()
    let filterMs =
      B4.elapsedMs (fun () ->
        c.Reset()
        frame |> Frame.filterRowsBy "S2" search |> ignore)
    c.Reset()
    let before = c.Snapshot()
    let filtered = frame |> Frame.filterRowsBy "S2" search
    let afterFilter = c.Snapshot()
    let filterDelta = AccessSnapshot.delta before afterFilter
    for i in 0 .. readN - 1 do
      if int64 i < filtered.RowIndex.KeyCount then filtered?S1.GetAt(i) |> ignore
    let readDelta = AccessSnapshot.delta afterFilter (c.Snapshot())
    { Profile = "Default 8-word (mapped column)"
      LookupRange = "Scan (no reverse map)"
      N = n
      Search = search
      VirtualFilter = FrameProbe.rowIndexIsVirtual filtered
      FilterValueAt = filterDelta.ValueAtCount
      FilterLookupRange = filterDelta.LookupRangeCount
      ResultRows = filtered.RowCount
      ExpectedRows = expected
      ReadValueAt20 = readDelta.ValueAtCount
      FilterMs = filterMs }

  let collect () : Row list =
    let words11 = "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')
    let expected11 = B4.expectedMatchCount n words11.Length

    let step11 =
      let r =
        runFilter (fun () ->
          let c, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrame n
          c, frame, B4.searchValue, B4.expectedMatchCount n words.Length)
      { r with Profile = "Default 8-word"; LookupRange = "Step (Custom stride)" }

    let exactFixed =
      let r =
        runFilter (fun () ->
          let c, frame, _ =
            InstrumentedOrdinalSource.createOrderedSearchFrameWith n (LookupRangeExactFixed (fun v ->
              let o = words11 |> Array.findIndex ((=) v) |> int64
              o, o))
          c, frame, B4.searchValue, 1)
      { r with Profile = "Default 8-word"; LookupRange = "ExactFixed (first hit)" }

    let fullFixed =
      let r =
        runFilter (fun () ->
          let c, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrameWith n LookupRangeFullFixed
          c, frame, B4.searchValue, int n)
      { r with Profile = "Default 8-word"; LookupRange = "FullFixed (naive [0..N-1])" }

    let vocab256 =
      let r =
        runFilter (fun () ->
          let c, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrameLargeVocab n 256
          let search = words.[0]
          c, frame, search, B4.expectedMatchCount n 256)
      { r with Profile = "Large vocab (256 labels)"; LookupRange = "Step (stride 256)" }

    let sparseIdx =
      let r =
        runFilter (fun () ->
          let c, frame, trueCount = InstrumentedOrdinalSource.createOrderedSearchFrameSparse n 997L 42L
          c, frame, "lorem", trueCount)
      { r with Profile = "Sparse (mod 997)"; LookupRange = "IndexList (precomputed)" }

    let sparseWrong =
      let r =
        runFilter (fun () ->
          let c, frame, trueCount = InstrumentedOrdinalSource.createOrderedSearchFrameSparseWrongStep n 997L 42L
          c, frame, "lorem", trueCount)
      { r with Profile = "Sparse (mod 997)"; LookupRange = "Step (wrong stride 11)" }

    [ step11; exactFixed; fullFixed; runOrdinal(); runMapped(); vocab256; sparseIdx; sparseWrong ]

  let toMarkdown (rows: Row list) (runDate: string) =
    let sb = StringBuilder()
    sb.AppendLine("| Profile | LookupRange | Virtual? | Filter ValueAt | Filter LookupRange | Result rows | Expected | Read ValueAt (20) | Filter ms |") |> ignore
    sb.AppendLine("|---------|-------------|----------|----------------|-------------------|-------------|----------|-------------------|-----------|") |> ignore
    for r in rows do
      let virt = if r.VirtualFilter then "Yes" else "No"
      let ok = if r.ResultRows = r.ExpectedRows then "✓" else "✗"
      sb.AppendLine(
        sprintf "| %s | %s | %s | %d | %d | %d %s | %d | %d | %.0f |"
          r.Profile r.LookupRange virt r.FilterValueAt r.FilterLookupRange r.ResultRows ok r.ExpectedRows r.ReadValueAt20 r.FilterMs)
      |> ignore
    sb.AppendLine() |> ignore
    sb.AppendLine(sprintf "*Generated: %s · N = %d · filter + read 20 rows where applicable*" runDate n) |> ignore
    sb.ToString()

  let writeBigDeedleResults () =
    let rows = collect ()
    let repoRoot = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", ".."))
    let outDir = Path.Combine(repoRoot, "big-deedle")
    let outPath = Path.Combine(outDir, "b4-profile-metrics.md")
    // Optional sibling checkout — CI that clones only Deedle must not fail.
    if Directory.Exists outDir then
      File.WriteAllText(outPath, toMarkdown rows (DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm UTC")))
      Some outPath
    else None

[<Test>]
let ``B4 profile baseline report writes big-deedle metrics when sibling repo exists`` () =
  match B4ProfileReport.writeBigDeedleResults() with
  | Some path ->
      File.Exists(path) |> shouldEqual true
      B4ProfileReport.collect() |> List.length |> shouldEqual 8
  | None ->
      // Sibling big-deedle/ not present (typical CI) — still verify collect() shape.
      B4ProfileReport.collect() |> List.length |> shouldEqual 8

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

// ------------------------------------------------------------------------------------------------
// Additional data profiles (beyond the ideal 8-word cycle)
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``B4 Large vocabulary periodic data works with Step LookupRange`` () =
  let vocabSize = 256
  let c, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrameLargeVocab B4.nLarge vocabSize
  let search = words.[0]
  let filtered, filterDelta, _ = B4.filterBy frame c 0 search
  filterDelta.ValueAtCount |> shouldEqual 0
  filterDelta.LookupRangeCount |> shouldEqual 1
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual (B4.expectedMatchCount B4.nLarge vocabSize)

[<Test>]
let ``B4 Sparse irregular matches work with IndexList LookupRange`` () =
  let modulus = 997L
  let remainder = 42L
  let c, frame, trueCount = InstrumentedOrdinalSource.createOrderedSearchFrameSparse B4.nLarge modulus remainder
  let filtered, filterDelta, _ = B4.filterBy frame c 0 "lorem"
  filterDelta.ValueAtCount |> shouldEqual 0
  filterDelta.LookupRangeCount |> shouldEqual 1
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual trueCount
  trueCount |> should be (lessThan (int B4.nLarge / 100))

[<Test>]
let ``B4 Wrong Step on sparse data over-filters virtual index`` () =
  let modulus = 997L
  let remainder = 42L
  let c, frame, trueCount = InstrumentedOrdinalSource.createOrderedSearchFrameSparseWrongStep B4.nLarge modulus remainder
  let filtered, filterDelta, _ = B4.filterBy frame c 0 "lorem"
  filterDelta.ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> should be (greaterThan trueCount)
  // Wrong Step (period 11 from offset 42) keeps ~N/11 rows, not the ~N/997 true matches
  filtered.RowCount |> should be (greaterThan (int B4.nLarge / 200))
