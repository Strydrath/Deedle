#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.VirtualGuardrails
#endif

open System
open System.IO
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Virtual
open Deedle.Virtual.Sources
open Deedle.Vectors.Virtual
open Deedle.Tests.VirtualInstrumentation

[<Test>]
let ``B14 ReadCsv auto-detects ordered datetime row index`` () =
  let path = Path.GetTempFileName() + ".csv"
  try
    System.IO.File.WriteAllLines(path, [| "Timestamp,Id,Category"; "2020-01-01T00:00:00Z,1,lorem"; "2020-01-02T00:00:00Z,2,ipsum" |])
    let frame = Virtual.ReadCsv(path, columnKeys = [ "Id"; "Category" ])
    VirtualFrameDiagnostics.GetRowIndexKind frame |> shouldEqual VirtualRowIndexKind.OrderedVirtual
  finally
    if System.IO.File.Exists path then System.IO.File.Delete path

[<Test>]
let ``B14 ReadCsv infers Step LookupRange for low-cardinality Category column`` () =
  let path = Path.GetTempFileName() + ".csv"
  let words = CsvTestData.words8
  try
    CsvTestData.ensureSearchCsv path 1000L
    let frame = Virtual.ReadCsv(path, indexColumn = "Timestamp", columnKeys = [ "Id"; "Category" ])
    VirtualFrameDiagnostics.IsVirtualColumn(frame, "Category") |> shouldEqual true
    let filtered = frame |> Frame.filterRowsBy "Category" words.[0]
    VirtualFrameDiagnostics.GetRowIndexKind filtered |> shouldEqual VirtualRowIndexKind.OrderedVirtual
    filtered.RowCount |> should be (greaterThan 0)
  finally
    if System.IO.File.Exists path then System.IO.File.Delete path

[<Test>]
let ``B14 filterRowsBy on ordinal virtual row index uses LookupRange when configured`` () =
  let c, frame, words = InstrumentedOrdinalSource.createOrdinalSearchFrame 1000L
  c.Reset()
  let filtered = frame |> Frame.filterRowsBy "S2" words.[0]
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  c.Snapshot().LookupRangeCount |> should be (greaterThan 0)
  c.Snapshot().ValueAtCount |> shouldEqual 0

[<Test>]
let ``B14 ordinal and ordered Step filters return the same row count`` () =
  let n = 10_000L
  let search = "lorem"
  let _, ordered, _ = InstrumentedOrdinalSource.createOrderedSearchFrame n
  let _, ordinal, _ = InstrumentedOrdinalSource.createOrdinalSearchFrame n
  let orderedCount = (ordered |> Frame.filterRowsBy "S2" search).RowCount
  let ordinalCount = (ordinal |> Frame.filterRowsBy "S2" search).RowCount
  ordinalCount |> shouldEqual orderedCount
  ordinalCount |> shouldEqual (int ((n - 1L) / int64 8) + 1)

[<Test>]
let ``B14 ordinal filterRowsBy without LookupRange throws NotSupportedException`` () =
  let words = "lorem ipsum dolor sit amet".Split(' ')
  let c = AccessCounters()
  let s2 = InstrumentedOrdinalSource<string>(100L, (fun i -> words.[int (i % int64 words.Length)]), c, hasMissing=false)
  let _, s1 = InstrumentedOrdinalSource.createLongs 100L
  let frame = Virtual.CreateOrdinalFrame(["S1"; "S2"], [s1 :> IVirtualVectorSource; s2 :> IVirtualVectorSource])
  (fun () -> frame |> Frame.filterRowsBy "S2" "lorem" |> ignore)
  |> should throw typeof<NotSupportedException>

[<Test>]
let ``B14 clipLookupRange remaps IndexList after Fixed slice`` () =
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
let ``B14 chained Step filter same value may shrink rows; use filterRowsBy2`` () =
  let n = 1000L
  let words = "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')
  let _, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrame n
  let once = frame |> Frame.filterRowsBy "S2" words.[0]
  let twice =
    frame
    |> Frame.filterRowsBy "S2" words.[0]
    |> Frame.filterRowsBy "S2" words.[0]
  twice.RowCount |> should be (lessThan once.RowCount)
  let fused = frame |> Frame.filterRowsBy2 "S2" words.[0] "S2" words.[0]
  fused.RowCount |> shouldEqual once.RowCount

[<Test>]
let ``B14 chained filterRowsBy2 still preferred for two predicates`` () =
  let c, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrame 64L
  c.Reset()
  let fused = frame |> Frame.filterRowsBy2 "S2" words.[0] "S2" words.[0]
  let fusedDelta = c.Snapshot()
  c.Reset()
  let chained =
    frame
    |> Frame.filterRowsBy "S2" words.[0]
    |> Frame.filterRowsBy "S2" words.[0]
  chained.RowCount |> should be (lessThan (64 / words.Length))
  fused.RowCount |> shouldEqual (64 / words.Length)
  let chainedDelta = c.Snapshot()
  chainedDelta.GetSubVectorCount |> should be (greaterThan fusedDelta.GetSubVectorCount)

[<Test>]
let ``B14 ReadCsv bundled backend exposes virtual ordered row index`` () =
  let csvPath = Path.GetTempFileName() + ".csv"
  try
    CsvTestData.ensureSearchCsv csvPath 500L |> ignore
    let csv = Virtual.ReadCsv(csvPath, indexColumn = "Timestamp", columnKeys = [ "Id"; "Category" ])
    VirtualFrameDiagnostics.GetRowIndexKind csv |> shouldEqual VirtualRowIndexKind.OrderedVirtual
    VirtualFrameDiagnostics.TryGetRowIndexSchemeId csv |> shouldEqual (Some "csv-file")
    VirtualFrameDiagnostics.IsVirtual csv |> shouldEqual true
  finally
    if File.Exists csvPath then File.Delete csvPath

[<Test>]
let ``B14 VirtualFrameDiagnostics describes ordinal vs ordered frames`` () =
  let _, ordered, _ = InstrumentedOrdinalSource.createOrderedSearchFrame 10L
  let _, ordinal, _ = InstrumentedOrdinalSource.createOrdinalSearchFrame 10L
  VirtualFrameDiagnostics.GetRowIndexKind ordered |> shouldEqual VirtualRowIndexKind.OrderedVirtual
  VirtualFrameDiagnostics.GetRowIndexKind ordinal |> shouldEqual VirtualRowIndexKind.OrdinalVirtual
  VirtualFrameDiagnostics.Describe ordered |> should haveSubstring "ordered virtual"
  VirtualFrameDiagnostics.IsVirtual ordered |> shouldEqual true
  VirtualFrameDiagnostics.IsVirtual ordinal |> shouldEqual true
