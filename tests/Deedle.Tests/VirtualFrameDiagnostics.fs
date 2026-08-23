#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.VirtualFrameDiagnostics
#endif

open System
open System.IO
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Virtual
open Deedle.Tests.VirtualInstrumentation

let fixturesPath = Path.Combine(__SOURCE_DIRECTORY__, "data", "virtual-fixtures.csv")

// ------------------------------------------------------------------------------------------------
// VirtualFrameDiagnostics (src/Deedle/VirtualFrameDiagnostics.fs)
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``Can classify ordered and ordinal virtual row indexes`` () =
  let _, ordered, _ = InstrumentedOrdinalSource.createOrderedSearchFrame 10L
  let _, ordinal, _ = InstrumentedOrdinalSource.createOrdinalSearchFrame 10L
  VirtualFrameDiagnostics.GetRowIndexKind ordered |> shouldEqual VirtualRowIndexKind.OrderedVirtual
  VirtualFrameDiagnostics.GetRowIndexKind ordinal |> shouldEqual VirtualRowIndexKind.OrdinalVirtual
  VirtualFrameDiagnostics.IsVirtual ordered |> shouldEqual true
  VirtualFrameDiagnostics.IsVirtual ordinal |> shouldEqual true

[<Test>]
let ``Can describe virtual frame row index kind`` () =
  let _, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrame 10L
  VirtualFrameDiagnostics.Describe frame |> should haveSubstring "ordered virtual"
  VirtualFrameDiagnostics.Describe frame |> should haveSubstring "columns=2"

[<Test>]
let ``Can detect virtual column and row index scheme id`` () =
  let frame = Virtual.ReadCsv(fixturesPath, indexColumn = "Timestamp", columnKeys = [ "Id"; "Category"; "Label" ])
  VirtualFrameDiagnostics.IsVirtualColumn(frame, "Category") |> shouldEqual true
  VirtualFrameDiagnostics.TryGetRowIndexSchemeId frame |> shouldEqual (Some "csv-file")

[<Test>]
let ``Can report linear row index for materialized frame`` () =
  let frame = Frame.ofColumns [ "A" => series [ 0 => 1; 1 => 2 ] ]
  VirtualFrameDiagnostics.GetRowIndexKind frame |> shouldEqual VirtualRowIndexKind.LinearOrOther
  VirtualFrameDiagnostics.IsVirtual frame |> shouldEqual false
  VirtualFrameDiagnostics.IsVirtualRowIndex frame |> shouldEqual false
  VirtualFrameDiagnostics.Describe frame |> should haveSubstring "linear / materialized"

[<Test>]
let ``IsVirtualColumn returns false for materialized frame columns`` () =
  let frame = Frame.ofColumns [ "A" => series [ 0 => 1.0; 1 => 2.0 ] ]
  VirtualFrameDiagnostics.IsVirtualColumn(frame, "A") |> shouldEqual false
