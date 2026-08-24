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
open Deedle.Addressing
open Deedle.Virtual
open Deedle.Vectors.Virtual
open Deedle.Tests.VirtualInstrumentation

module Address = LinearAddress

module private Range =
  let step offset step =
    RangeRestriction.Custom { Offset = offset; Step = step } : RangeRestriction<Address>

  let fixedRange lo hi =
    RangeRestriction.Fixed(Address.ofInt64 lo, Address.ofInt64 hi)

// ------------------------------------------------------------------------------------------------
// VirtualLookupRange configuration (basic)
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``forRepeatingCycle returns step offset for known value`` () =
  match VirtualLookupRange.forRepeatingCycle [| "a"; "b"; "c" |] with
  | LookupRangeStep f -> f "b" |> shouldEqual (1, 3)
  | _ -> failwith "expected LookupRangeStep"

[<Test>]
let ``forRepeatingCycle returns empty range for unknown value`` () =
  match VirtualLookupRange.forRepeatingCycle [| "a"; "b" |] with
  | LookupRangeStep f -> f "missing" |> shouldEqual (-1, 2)
  | _ -> failwith "expected LookupRangeStep"

[<Test>]
let ``tryInferStringLookupRange returns None for empty column`` () =
  VirtualLookupRange.tryInferStringLookupRange 0L (fun _ -> "")
  |> Option.isNone
  |> shouldEqual true

[<Test>]
let ``tryInferStringLookupRange infers repeating cycle for periodic strings`` () =
  let valueAt i = if i % 2L = 0L then "x" else "y"
  match VirtualLookupRange.tryInferStringLookupRange 10L valueAt with
  | Some(_, desc) -> desc |> should haveSubstring "repeating cycle"
  | None -> failwith "expected inference"

[<Test>]
let ``tryInferStringLookupRange returns None when distinct count exceeds cap`` () =
  let valueAt i = sprintf "value-%d" (int i)
  VirtualLookupRange.tryInferStringLookupRange 100L valueAt
  |> Option.isNone
  |> shouldEqual true

[<Test>]
let ``resolveSearchColumnLookupRange returns None for non-search columns`` () =
  VirtualLookupRange.resolveSearchColumnLookupRange
    "Test.Read"
    (Some("Category", LookupRangeUnsupported))
    "Id"
    false
    (fun () -> Some(VirtualLookupRange.forRepeatingCycle [| "a" |], "cycle"))
  |> Option.isNone
  |> shouldEqual true

// ------------------------------------------------------------------------------------------------
// LookupRange quality sensitivity
// Compare tight Custom/Fixed vs naive full-range vs linear-scan fallback.
// ------------------------------------------------------------------------------------------------

module private LookupRangeFixture =
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
// Profile baseline reporter — writes metrics for all data profiles
// ------------------------------------------------------------------------------------------------

module LookupRangeProfileReport =
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

  let private n = LookupRangeFixture.nLarge
  let private readN = 20

  let private runFilter (setup: unit -> AccessCounters * Frame<DateTimeOffset, string> * string * int) =
    let c, frame, search, expected = setup ()
    let filterMs =
      LookupRangeFixture.elapsedMs (fun () ->
        c.Reset()
        frame |> Frame.filterRowsBy "S2" search |> ignore)
    let filtered, filterDelta, readDelta = LookupRangeFixture.filterBy frame c readN search
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
    let expected = LookupRangeFixture.expectedMatchCount n words.Length
    let filterMs =
      LookupRangeFixture.elapsedMs (fun () ->
        c.Reset()
        frame |> Frame.filterRowsBy "S2" LookupRangeFixture.searchValue |> ignore)
    let filtered, filterDelta, readDelta = LookupRangeFixture.filterAndReadOrdinal frame c readN
    { Profile = "Default 8-word (ordinal index)"
      LookupRange = "Step (Custom stride)"
      N = n
      Search = LookupRangeFixture.searchValue
      VirtualFilter = FrameProbe.rowIndexIsVirtual filtered
      FilterValueAt = filterDelta.ValueAtCount
      FilterLookupRange = filterDelta.LookupRangeCount
      ResultRows = filtered.RowCount
      ExpectedRows = expected
      ReadValueAt20 = readDelta.ValueAtCount
      FilterMs = filterMs }

  let private runMapped () =
    let c, frame, words = InstrumentedOrdinalSource.createOrderedMappedSearchFrame n
    let expected = LookupRangeFixture.expectedMatchCount n words.Length
    let search = LookupRangeFixture.searchValue.ToUpperInvariant()
    let filterMs =
      LookupRangeFixture.elapsedMs (fun () ->
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
    let expected11 = LookupRangeFixture.expectedMatchCount n words11.Length

    let step11 =
      let r =
        runFilter (fun () ->
          let c, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrame n
          c, frame, LookupRangeFixture.searchValue, LookupRangeFixture.expectedMatchCount n words.Length)
      { r with Profile = "Default 8-word"; LookupRange = "Step (Custom stride)" }

    let exactFixed =
      let r =
        runFilter (fun () ->
          let c, frame, _ =
            InstrumentedOrdinalSource.createOrderedSearchFrameWith n (LookupRangeExactFixed (fun v ->
              let o = words11 |> Array.findIndex ((=) v) |> int64
              o, o))
          c, frame, LookupRangeFixture.searchValue, 1)
      { r with Profile = "Default 8-word"; LookupRange = "ExactFixed (first hit)" }

    let fullFixed =
      let r =
        runFilter (fun () ->
          let c, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrameWith n LookupRangeFullFixed
          c, frame, LookupRangeFixture.searchValue, int n)
      { r with Profile = "Default 8-word"; LookupRange = "FullFixed (naive [0..N-1])" }

    let vocab256 =
      let r =
        runFilter (fun () ->
          let c, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrameLargeVocab n 256
          let search = words.[0]
          c, frame, search, LookupRangeFixture.expectedMatchCount n 256)
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
let ``Can write LookupRange profile baseline when sibling repo exists`` () =
  match LookupRangeProfileReport.writeBigDeedleResults() with
  | Some path ->
      File.Exists(path) |> shouldEqual true
      LookupRangeProfileReport.collect() |> List.length |> shouldEqual 8
  | None ->
      // Sibling big-deedle/ not present (typical CI) — still verify collect() shape.
      LookupRangeProfileReport.collect() |> List.length |> shouldEqual 8

[<Test>]
let ``Can filter with Step LookupRange without ValueAt at filter time`` () =
  let c, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrame LookupRangeFixture.nLarge
  let filtered, filterDelta, _ = LookupRangeFixture.filterAndRead frame c 0
  filterDelta.LookupRangeCount |> shouldEqual 1
  filterDelta.ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual (LookupRangeFixture.expectedMatchCount LookupRangeFixture.nLarge words.Length)

[<Test>]
let ``Can filter with ExactFixed LookupRange retaining first match only`` () =
  let words = "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')
  let c, frame, _ =
    InstrumentedOrdinalSource.createOrderedSearchFrameWith LookupRangeFixture.nLarge (LookupRangeExactFixed (fun v ->
      let o = words |> Array.findIndex ((=) v) |> int64
      o, o))
  let filtered, filterDelta, _ = LookupRangeFixture.filterAndRead frame c 0
  filterDelta.LookupRangeCount |> shouldEqual 1
  filterDelta.ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual 1

[<Test>]
let ``Can filter with FullFixed LookupRange retaining entire series`` () =
  let c, frame, _ =
    InstrumentedOrdinalSource.createOrderedSearchFrameWith LookupRangeFixture.nLarge LookupRangeFullFixed
  let filtered, filterDelta, _ = LookupRangeFixture.filterAndRead frame c 0
  filterDelta.LookupRangeCount |> shouldEqual 1
  filterDelta.ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual (int LookupRangeFixture.nLarge)

[<Test>]
let ``Can filter ordinal frame using LookupRange like ordered index`` () =
  let c, frame, words = InstrumentedOrdinalSource.createOrdinalSearchFrame LookupRangeFixture.nLarge
  let filtered, filterDelta, _ = LookupRangeFixture.filterAndReadOrdinal frame c 0
  filterDelta.LookupRangeCount |> shouldEqual 1
  filterDelta.ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual (LookupRangeFixture.expectedMatchCount LookupRangeFixture.nLarge words.Length)

[<Test>]
let ``Can read only requested rows after Step filter`` () =
  let c, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrame LookupRangeFixture.nLarge
  let readN = 20
  let _, filterDelta, readDelta = LookupRangeFixture.filterAndRead frame c readN
  filterDelta.ValueAtCount |> shouldEqual 0
  readDelta.ValueAtCount |> should be (greaterThan 0)
  readDelta.ValueAtCount |> should be (lessThan (readN * 3))

[<Test>]
let ``Can pay full read cost with FullFixed naive range`` () =
  let c, frame, _ =
    InstrumentedOrdinalSource.createOrderedSearchFrameWith LookupRangeFixture.nLarge LookupRangeFullFixed
  let readN = 20
  let filtered, filterDelta, readDelta = LookupRangeFixture.filterAndRead frame c readN
  filterDelta.ValueAtCount |> shouldEqual 0
  filtered.RowCount |> shouldEqual (int LookupRangeFixture.nLarge)
  readDelta.ValueAtCount |> should be (lessThan (readN * 3))

[<Test>]
let ``Can scan mapped search column at filter time without reverse lookup`` () =
  let c, frame, words = InstrumentedOrdinalSource.createOrderedMappedSearchFrame LookupRangeFixture.nLarge
  c.Reset()
  let filtered = frame |> Frame.filterRowsBy "S2" (LookupRangeFixture.searchValue.ToUpperInvariant())
  let d = c.Snapshot()
  d.LookupRangeCount |> shouldEqual 0
  d.ValueAtCount |> should be (greaterThanOrEqualTo (int LookupRangeFixture.nLarge))
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual (LookupRangeFixture.expectedMatchCount LookupRangeFixture.nLarge words.Length)

[<Test>]
let ``Can filter ordinal Step within same order of magnitude as ordered Step`` () =
  let orderedMs =
    LookupRangeFixture.elapsedMs (fun () ->
      let c, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrame LookupRangeFixture.nTiming
      c.Reset()
      frame |> Frame.filterRowsBy "S2" LookupRangeFixture.searchValue |> ignore)
  let ordinalMs =
    LookupRangeFixture.elapsedMs (fun () ->
      let c, frame, _ = InstrumentedOrdinalSource.createOrdinalSearchFrame LookupRangeFixture.nTiming
      c.Reset()
      frame |> Frame.filterRowsBy "S2" LookupRangeFixture.searchValue |> ignore)
  ordinalMs |> should be (lessThan (max 50.0 (orderedMs * 5.0)))

[<Test>]
let ``Can match ordered partial-read cost on ordinal Step filter`` () =
  let orderedMs =
    LookupRangeFixture.elapsedMs (fun () ->
      let c, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrame LookupRangeFixture.nTiming
      let filtered, _, _ = LookupRangeFixture.filterAndRead frame c 50
      filtered.RowCount |> ignore)
  let ordinalMs =
    LookupRangeFixture.elapsedMs (fun () ->
      let c, frame, _ = InstrumentedOrdinalSource.createOrdinalSearchFrame LookupRangeFixture.nTiming
      let filtered, _, _ = LookupRangeFixture.filterAndReadOrdinal frame c 50
      filtered.RowCount |> ignore)
  ordinalMs |> should be (lessThan (max 50.0 (orderedMs * 5.0)))

// ------------------------------------------------------------------------------------------------
// Additional data profiles (beyond the ideal 8-word cycle)
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``Can filter large vocabulary periodic data with Step LookupRange`` () =
  let vocabSize = 256
  let c, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrameLargeVocab LookupRangeFixture.nLarge vocabSize
  let search = words.[0]
  let filtered, filterDelta, _ = LookupRangeFixture.filterBy frame c 0 search
  filterDelta.ValueAtCount |> shouldEqual 0
  filterDelta.LookupRangeCount |> shouldEqual 1
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual (LookupRangeFixture.expectedMatchCount LookupRangeFixture.nLarge vocabSize)

[<Test>]
let ``Can filter sparse irregular matches with IndexList LookupRange`` () =
  let modulus = 997L
  let remainder = 42L
  let c, frame, trueCount = InstrumentedOrdinalSource.createOrderedSearchFrameSparse LookupRangeFixture.nLarge modulus remainder
  let filtered, filterDelta, _ = LookupRangeFixture.filterBy frame c 0 "lorem"
  filterDelta.ValueAtCount |> shouldEqual 0
  filterDelta.LookupRangeCount |> shouldEqual 1
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual trueCount
  trueCount |> should be (lessThan (int LookupRangeFixture.nLarge / 100))

[<Test>]
let ``Can over-filter sparse data with wrong Step LookupRange`` () =
  let modulus = 997L
  let remainder = 42L
  let c, frame, trueCount = InstrumentedOrdinalSource.createOrderedSearchFrameSparseWrongStep LookupRangeFixture.nLarge modulus remainder
  let filtered, filterDelta, _ = LookupRangeFixture.filterBy frame c 0 "lorem"
  filterDelta.ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> should be (greaterThan trueCount)
  // Wrong Step (period 11 from offset 42) keeps ~N/11 rows, not the ~N/997 true matches
  filtered.RowCount |> should be (greaterThan (int LookupRangeFixture.nLarge / 200))

[<Test>]
let ``Can remap IndexList via clipLookupRange after Fixed slice`` () =
  let modulus = 997L
  let remainder = 42L
  let n = 10_000L
  let _, frame, trueCount = InstrumentedOrdinalSource.createOrderedSearchFrameSparse n modulus remainder
  let filtered = frame |> Frame.filterRowsBy "S2" "lorem"
  filtered.RowCount |> shouldEqual trueCount
  let filtered2 = filtered |> Frame.filterRowsBy "S2" "lorem"
  filtered2.RowCount |> shouldEqual trueCount
  FrameProbe.rowIndexIsVirtual filtered2 |> shouldEqual true

[<Test>]
let ``Can return same row count for ordinal and ordered Step filters`` () =
  let n = 10_000L
  let search = "lorem"
  let _, ordered, _ = InstrumentedOrdinalSource.createOrderedSearchFrame n
  let _, ordinal, _ = InstrumentedOrdinalSource.createOrdinalSearchFrame n
  let orderedCount = (ordered |> Frame.filterRowsBy "S2" search).RowCount
  let ordinalCount = (ordinal |> Frame.filterRowsBy "S2" search).RowCount
  ordinalCount |> shouldEqual orderedCount
  ordinalCount |> shouldEqual (int ((n - 1L) / int64 8) + 1)

[<Test>]
let ``ordinal filterRowsBy without LookupRange throws NotSupportedException`` () =
  let words = "lorem ipsum dolor sit amet".Split(' ')
  let c = AccessCounters()
  let s2 = InstrumentedOrdinalSource<string>(100L, (fun i -> words.[int (i % int64 words.Length)]), c, hasMissing=false)
  let _, s1 = InstrumentedOrdinalSource.createLongs 100L
  let frame = Virtual.CreateOrdinalFrame(["S1"; "S2"], [s1 :> IVirtualVectorSource; s2 :> IVirtualVectorSource])
  (fun () -> frame |> Frame.filterRowsBy "S2" "lorem" |> ignore)
  |> should throw typeof<NotSupportedException>

[<Test>]
let ``Can filter ordinal row index when LookupRange is configured`` () =
  let c, frame, words = InstrumentedOrdinalSource.createOrdinalSearchFrame 1000L
  c.Reset()
  let filtered = frame |> Frame.filterRowsBy "S2" words.[0]
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  c.Snapshot().LookupRangeCount |> should be (greaterThan 0)
  c.Snapshot().ValueAtCount |> shouldEqual 0

// ------------------------------------------------------------------------------------------------
// LookupRangeExecutor (src/Deedle/VirtualLookupRange.fs)
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``Can intersect identical Step LookupRanges`` () =
  match LookupRangeExecutor.intersect (Range.step 0 8) (Range.step 0 8) with
  | RangeRestriction.Custom(:? StepRange as s) ->
      s.Offset |> shouldEqual 0
      s.Step |> shouldEqual 8
  | other -> failwithf "expected StepRange, got %A" other

[<Test>]
let ``Can intersect disjoint Step LookupRanges to empty range`` () =
  match LookupRangeExecutor.intersect (Range.step 0 8) (Range.step 1 8) with
  | RangeRestriction.Custom ar -> Seq.length ar |> shouldEqual 0
  | other -> failwithf "expected empty custom range, got %A" other

[<Test>]
let ``Can intersect Step LookupRange with IndexList without enumerating Step`` () =
  let step = RangeRestriction.Custom { Offset = 0; Step = 2 } : RangeRestriction<Address>
  let listAddrs = [ 0L; 2L; 3L; 4L; 7L ] |> List.map Address.ofInt64
  let list =
    ({ new IRangeRestriction<Address> with
        member _.Count = int64 listAddrs.Length
       interface seq<Address> with
         member _.GetEnumerator() = (listAddrs :> seq<_>).GetEnumerator()
       interface System.Collections.IEnumerable with
         member _.GetEnumerator() = (listAddrs :> seq<_>).GetEnumerator() :> System.Collections.IEnumerator }
     |> RangeRestriction.Custom)
  let addrsOf = function
    | RangeRestriction.Custom ar -> ar |> Seq.map Address.asInt64 |> Seq.toList
    | _ -> failwith "expected Custom range"
  addrsOf (LookupRangeExecutor.intersect step list) |> shouldEqual [ 0L; 2L; 4L ]
  addrsOf (LookupRangeExecutor.intersect list step) |> shouldEqual [ 0L; 2L; 4L ]

[<Test>]
let ``Can intersect overlapping Fixed LookupRanges`` () =
  match LookupRangeExecutor.intersect (Range.fixedRange 0 10) (Range.fixedRange 5 15) with
  | RangeRestriction.Fixed(lo, hi) ->
      Address.asInt64 lo |> shouldEqual 5
      Address.asInt64 hi |> shouldEqual 10
  | other -> failwithf "expected Fixed overlap, got %A" other

[<Test>]
let ``Can intersect disjoint Fixed LookupRanges to empty range`` () =
  match LookupRangeExecutor.intersect (Range.fixedRange 0 3) (Range.fixedRange 10 12) with
  | RangeRestriction.Fixed _ -> failwith "expected empty intersection"
  | RangeRestriction.Custom ar -> Seq.isEmpty ar |> shouldEqual true
  | other -> failwithf "expected empty Fixed intersection, got %A" other

[<Test>]
let ``LookupRangeExecutor returns empty range for invalid Step offset`` () =
  let mode = LookupRangeStep (fun _ -> (-1, 4))
  match LookupRangeExecutor.lookupRange 16L mode "x" "test" with
  | RangeRestriction.Custom ar -> Seq.isEmpty ar |> shouldEqual true
  | other -> failwithf "expected empty custom range, got %A" other

[<Test>]
let ``LookupRangeExecutor raises NotSupportedException when LookupRange is unsupported`` () =
  (fun () -> LookupRangeExecutor.lookupRange 8L LookupRangeUnsupported "x" "test" |> ignore)
  |> should throw typeof<NotSupportedException>
