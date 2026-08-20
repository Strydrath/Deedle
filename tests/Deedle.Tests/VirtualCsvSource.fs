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

[<Test>]
let ``B8 empty and NA cells become missing values`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-b8-missing-cells.csv")
  File.WriteAllText(
    csvPath,
    "Timestamp,Id,Value\r\n" +
    "2000-01-01T00:00:00.0000000+00:00,1,1.5\r\n" +
    "2000-01-01T00:00:01.0000000+00:00,,NA\r\n" +
    "2000-01-01T00:00:02.0000000+00:00,3,\r\n")
  let frame =
    Virtual.ReadCsv(csvPath, indexColumn = "Timestamp", columnKeys = [ "Id"; "Value" ])
  let ids = frame.GetColumn<int64>("Id")
  let values = frame.GetColumn<float>("Value")
  ids.TryGetAt(0).HasValue |> shouldEqual true
  ids.TryGetAt(1).HasValue |> shouldEqual false
  ids.TryGetAt(2).HasValue |> shouldEqual true
  values.TryGetAt(0).HasValue |> shouldEqual true
  values.TryGetAt(1).HasValue |> shouldEqual false
  values.TryGetAt(2).HasValue |> shouldEqual false

[<Test>]
let ``B8 forRepeatingCycle unknown value yields empty filter`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-b8-unknown-cat.csv")
  CsvHarness.ensureSearchCsv csvPath 64L |> ignore
  let frame =
    Virtual.ReadCsv(
      csvPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      searchLookupRange = VirtualLookupRange.forRepeatingCycle CsvTestData.words8,
      columnKeys = [ "Category" ])
  let filtered = frame |> Frame.filterRowsBy "Category" "not-a-category"
  filtered.RowCount |> shouldEqual 0

[<Test>]
let ``B8 quoted CSV fields with commas parse correctly`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-b8-quoted.csv")
  File.WriteAllText(
    csvPath,
    "Timestamp,Label,Value\r\n" +
    "2000-01-01T00:00:00.0000000+00:00,\"hello, world\",2.5\r\n" +
    "2000-01-01T00:00:01.0000000+00:00,\"a \"\"b\"\" c\",3.5\r\n")
  let frame =
    Virtual.ReadCsv(csvPath, indexColumn = "Timestamp", columnKeys = [ "Label"; "Value" ])
  frame.GetColumn<string>("Label").GetAt(0) |> shouldEqual "hello, world"
  frame.GetColumn<string>("Label").GetAt(1) |> shouldEqual "a \"b\" c"
  frame.GetColumn<float>("Value").GetAt(0) |> shouldEqual 2.5
