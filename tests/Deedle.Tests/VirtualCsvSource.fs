#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#load "CsvFileVirtualSource.fs"
#else
module Deedle.Tests.VirtualCsvSource
#endif

open System.IO
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Virtual
open Deedle.Virtual.Sources
open Deedle.Tests.VirtualInstrumentation
open Deedle.Tests.CsvFileVirtualSource

// ------------------------------------------------------------------------------------------------
// B8 — public Virtual.ReadCsv API
// ------------------------------------------------------------------------------------------------

[<Test; NonParallelizable>]
let ``B8 Virtual.ReadCsv loads B6 dataset with virtual row index`` () =
  let csvPath = Path.Combine(__SOURCE_DIRECTORY__, "data", CsvHarness.defaultDatasetName)
  CsvHarness.ensureSearchCsv csvPath 100_000L |> ignore
  let frame =
    Virtual.ReadCsv(
      csvPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      searchLookupRange = VirtualLookupRange.forRepeatingCycle CsvTestData.words8,
      columnKeys = [ "Id"; "Category" ])
  FrameProbe.rowIndexIsVirtual frame |> shouldEqual true
  frame.RowCount |> shouldEqual 100_000
  let filtered = frame |> Frame.filterRowsBy "Category" "lorem"
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual 12_500

[<Test; NonParallelizable>]
let ``B8 Virtual.ReadCsv auto-detects Timestamp index column`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-b8-autodetect.csv")
  CsvHarness.ensureSearchCsv csvPath 1000L |> ignore
  let frame = Virtual.ReadCsv(csvPath, columnKeys = [ "Id" ])
  frame.RowCount |> shouldEqual 1000
  FrameProbe.rowIndexIsVirtual frame |> shouldEqual true

[<Test>]
let ``B8 Virtual.ReadCsv throws when file is missing`` () =
  (fun () -> Virtual.ReadCsv(Path.Combine(Path.GetTempPath(), "deedle-b8-missing.csv")) |> ignore)
  |> should throw typeof<System.Exception>

[<Test; NonParallelizable>]
let ``B8 Virtual.ReadCsv infers remaining columns when columnKeys omitted`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-b8-infer.csv")
  CsvHarness.ensureSearchCsv csvPath 1000L |> ignore
  let frame = Virtual.ReadCsv(csvPath, indexColumn = "Timestamp")
  frame.ColumnCount |> shouldEqual 3
  frame.ColumnKeys |> Seq.toList |> shouldEqual [ "Id"; "Category"; "Value" ]
  frame.GetColumn<int64>("Id").KeyCount |> shouldEqual 1000

[<Test; NonParallelizable>]
let ``B8 VirtualLookupRange.forCategoricalScan filters without Step cycle`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-b8-categorical.csv")
  CsvHarness.ensureSearchCsv csvPath 800L |> ignore
  let lineIndex = CsvLineIndex(csvPath)
  let catIdx =
    lineIndex.HeaderColumns
    |> Array.findIndex (fun h -> h = "Category")
  let valueAt i = lineIndex.ReadFields(i).[catIdx]
  let frame =
    Virtual.ReadCsv(
      csvPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      searchLookupRange = VirtualLookupRange.forCategoricalScan lineIndex.Length valueAt,
      columnKeys = [ "Category" ])
  let filtered = frame |> Frame.filterRowsBy "Category" "lorem"
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual 100
