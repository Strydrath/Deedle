#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.VirtualCsvSource
#endif

open System
open System.Diagnostics
open System.Globalization
open System.IO
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Virtual
open Deedle.Virtual.Sources
open Deedle.Vectors.Virtual
open Deedle.Tests.VirtualInstrumentation

module private SearchDataset =
  let nLarge = 100_000L
  let searchValue = "lorem"
  let dataDir = Path.Combine(__SOURCE_DIRECTORY__, "data")
  let csvPath = Path.Combine(dataDir, CsvTestData.defaultDatasetName)
  let gate = obj()

  let ensureCsv () =
    lock gate (fun () ->
      Directory.CreateDirectory dataDir |> ignore
      CsvTestData.ensureSearchCsv csvPath nLarge |> ignore)

  let expectedMatchCount (length: int64) (step: int) =
    if length <= 0L then 0
    else int ((length - 1L) / int64 step) + 1

  let elapsedMs (f: unit -> unit) =
    let sw = Stopwatch.StartNew()
    f()
    sw.Stop()
    float sw.ElapsedMilliseconds

/// Instrumented low-level CSV virtual sources (for access-counter tests).
module private InstrumentedCsvSource =
  let private wrap (counters: AccessCounters) (source: IVirtualVectorSource) =
    CountingVirtualSource.Wrap counters source

  let createOrderedSearchFrame (csvPath: string) (counters: AccessCounters) =
    let lineIndex = CsvLineIndex(csvPath)
    let idx =
      wrap counters (CsvVirtualSource.createIndexSource lineIndex "Timestamp")
      :?> IVirtualVectorSource<DateTimeOffset>
    let idCol = wrap counters (CsvVirtualSource.createColumnSource lineIndex "Id" None)
    let catCol =
      wrap counters
        (CsvVirtualSource.createColumnSource lineIndex "Category"
          (Some(VirtualLookupRange.forRepeatingCycle CsvTestData.words8)))
    let frame = Virtual.CreateFrame(idx, [ "S1"; "S2" ], [ idCol; catCol ])
    counters, frame, CsvTestData.words8

  let createFloatValueSeries (csvPath: string) (counters: AccessCounters) =
    let lineIndex = CsvLineIndex(csvPath)
    let src =
      wrap counters (CsvVirtualSource.createColumnSource lineIndex "Value" None)
      :?> IVirtualVectorSource<float>
    counters, Virtual.CreateOrdinalSeries(src)

[<Test; NonParallelizable>]
let ``ReadCsv loads search dataset with virtual row index`` () =
  SearchDataset.ensureCsv()
  let frame =
    Virtual.ReadCsv(
      SearchDataset.csvPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      searchLookupRange = VirtualLookupRange.forRepeatingCycle CsvTestData.words8,
      columnKeys = [ "Id"; "Category" ])
  FrameProbe.rowIndexIsVirtual frame |> shouldEqual true
  frame.RowCount |> shouldEqual 100_000
  let filtered = frame |> Frame.filterRowsBy "Category" SearchDataset.searchValue
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual 12_500

[<Test; NonParallelizable>]
let ``ReadCsv auto-detects Timestamp index column`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-csv-autodetect.csv")
  CsvTestData.ensureSearchCsv csvPath 1000L |> ignore
  let frame = Virtual.ReadCsv(csvPath, columnKeys = [ "Id" ])
  frame.RowCount |> shouldEqual 1000
  FrameProbe.rowIndexIsVirtual frame |> shouldEqual true

[<Test>]
let ``ReadCsv throws when file is missing`` () =
  (fun () -> Virtual.ReadCsv(Path.Combine(Path.GetTempPath(), "deedle-csv-missing.csv")) |> ignore)
  |> should throw typeof<System.Exception>

[<Test; NonParallelizable>]
let ``ReadCsv infers remaining columns when columnKeys omitted`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-csv-infer.csv")
  CsvTestData.ensureSearchCsv csvPath 1000L |> ignore
  let frame = Virtual.ReadCsv(csvPath, indexColumn = "Timestamp")
  frame.ColumnCount |> shouldEqual 3
  frame.ColumnKeys |> Seq.toList |> shouldEqual [ "Id"; "Category"; "Value" ]
  frame.GetColumn<int64>("Id").KeyCount |> shouldEqual 1000

[<Test; NonParallelizable>]
let ``forCategoricalScan filters without Step cycle`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-csv-categorical.csv")
  CsvTestData.ensureSearchCsv csvPath 800L |> ignore
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
  let filtered = frame |> Frame.filterRowsBy "Category" SearchDataset.searchValue
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual 100

[<Test>]
let ``empty and NA cells become missing values`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-csv-missing-cells.csv")
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
let ``forRepeatingCycle unknown value yields empty filter`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-csv-unknown-cat.csv")
  CsvTestData.ensureSearchCsv csvPath 64L |> ignore
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
let ``quoted CSV fields with commas parse correctly`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-csv-quoted.csv")
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

[<Test>]
let ``ReadCsv auto-detects ordered datetime row index`` () =
  let path = Path.GetTempFileName() + ".csv"
  try
    File.WriteAllLines(path, [| "Timestamp,Id,Category"; "2020-01-01T00:00:00Z,1,lorem"; "2020-01-02T00:00:00Z,2,ipsum" |])
    let frame = Virtual.ReadCsv(path, columnKeys = [ "Id"; "Category" ])
    VirtualFrameDiagnostics.GetRowIndexKind frame |> shouldEqual VirtualRowIndexKind.OrderedVirtual
  finally
    if File.Exists path then File.Delete path

[<Test>]
let ``ReadCsv infers Step LookupRange for low-cardinality search column`` () =
  let path = Path.GetTempFileName() + ".csv"
  let words = CsvTestData.words8
  try
    CsvTestData.ensureSearchCsv path 1000L |> ignore
    let frame = Virtual.ReadCsv(path, indexColumn = "Timestamp", searchColumn = "Category", columnKeys = [ "Id"; "Category" ])
    VirtualFrameDiagnostics.IsVirtualColumn(frame, "Category") |> shouldEqual true
    let filtered = frame |> Frame.filterRowsBy "Category" words.[0]
    VirtualFrameDiagnostics.GetRowIndexKind filtered |> shouldEqual VirtualRowIndexKind.OrderedVirtual
    filtered.RowCount |> should be (greaterThan 0)
  finally
    if File.Exists path then File.Delete path

[<Test>]
let ``ReadCsv does not infer LookupRange for non-search string columns`` () =
  let path = Path.GetTempFileName() + ".csv"
  try
    CsvTestData.ensureSearchCsv path 1000L |> ignore
    let frame = Virtual.ReadCsv(path, indexColumn = "Timestamp", columnKeys = [ "Id"; "Category" ])
    (fun () -> frame |> Frame.filterRowsBy "Category" SearchDataset.searchValue |> ignore)
    |> should throw typeof<NotSupportedException>
  finally
    if File.Exists path then File.Delete path

[<Test>]
let ``ReadCsv exposes virtual ordered row index and csv-file scheme`` () =
  let csvPath = Path.GetTempFileName() + ".csv"
  try
    CsvTestData.ensureSearchCsv csvPath 500L |> ignore
    let csv = Virtual.ReadCsv(csvPath, indexColumn = "Timestamp", columnKeys = [ "Id"; "Category" ])
    VirtualFrameDiagnostics.GetRowIndexKind csv |> shouldEqual VirtualRowIndexKind.OrderedVirtual
    VirtualFrameDiagnostics.TryGetRowIndexSchemeId csv |> shouldEqual (Some "csv-file")
    VirtualFrameDiagnostics.IsVirtual csv |> shouldEqual true
  finally
    if File.Exists csvPath then File.Delete csvPath

[<Test; NonParallelizable>]
let ``shared row cache decodes each row once across columns`` () =
  SearchDataset.ensureCsv()
  let lineIndex = CsvLineIndex(SearchDataset.csvPath)
  let idSrc = CsvVirtualSource.createColumnSource lineIndex "Id" None :?> IVirtualVectorSource<int64>
  let valSrc = CsvVirtualSource.createColumnSource lineIndex "Value" None :?> IVirtualVectorSource<float>
  let idSeries = Virtual.CreateOrdinalSeries(idSrc)
  let valSeries = Virtual.CreateOrdinalSeries(valSrc)
  lineIndex.ResetSplitCount()
  for row in 1000L .. 1099L do
    idSeries.TryGet row |> ignore
    valSeries.TryGet row |> ignore
  lineIndex.SplitCount |> shouldEqual 100

[<Test; NonParallelizable>]
let ``slice decode count stays within slice bounds`` () =
  SearchDataset.ensureCsv()
  let lineIndex = CsvLineIndex(SearchDataset.csvPath)
  let src =
    CsvVirtualSource.createColumnSource lineIndex "Value" None
    :?> IVirtualVectorSource<float>
  let series = Virtual.CreateOrdinalSeries(src)
  lineIndex.ResetSplitCount()
  let sliced = series.[1000L .. 1099L]
  Stats.sum sliced |> ignore
  lineIndex.SplitCount |> shouldEqual 100

[<Test; NonParallelizable>]
let ``filterRowsBy2 on ReadCsv stays virtual with correct count`` () =
  SearchDataset.ensureCsv()
  let frame =
    Virtual.ReadCsv(
      SearchDataset.csvPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      searchLookupRange = VirtualLookupRange.forRepeatingCycle CsvTestData.words8,
      columnKeys = [ "Id"; "Category" ])
  let fused =
    frame
    |> Frame.filterRowsBy2 "Category" SearchDataset.searchValue "Category" SearchDataset.searchValue
  FrameProbe.rowIndexIsVirtual fused |> shouldEqual true
  fused.RowCount |> shouldEqual 12_500

[<Test; NonParallelizable>]
let ``filterRowsBy2 row count matches single filter on ReadCsv`` () =
  SearchDataset.ensureCsv()
  let frame =
    Virtual.ReadCsv(
      SearchDataset.csvPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      searchLookupRange = VirtualLookupRange.forRepeatingCycle CsvTestData.words8,
      columnKeys = [ "Id"; "Category" ])
  let single = frame |> Frame.filterRowsBy "Category" SearchDataset.searchValue
  let fused =
    frame
    |> Frame.filterRowsBy2 "Category" SearchDataset.searchValue "Category" SearchDataset.searchValue
  fused.RowCount |> shouldEqual single.RowCount

[<Test; NonParallelizable>]
let ``generated CSV has expected schema`` () =
  SearchDataset.ensureCsv()
  let idx = CsvLineIndex(SearchDataset.csvPath)
  idx.Length |> shouldEqual SearchDataset.nLarge
  let fields = idx.ReadFields 0L
  fields.Length |> shouldEqual 4
  fields.[2] |> shouldEqual SearchDataset.searchValue
  let meta = CsvTestData.readMeta SearchDataset.csvPath
  meta.Seed |> shouldEqual CsvTestData.defaultSeed
  meta.RowCount |> shouldEqual SearchDataset.nLarge
  // Non-consecutive ids: row ordinals 0 and 1 should not both equal their row index.
  let id0 = Int32.Parse(fields.[0])
  let id1 = Int32.Parse((idx.ReadFields 1L).[0])
  (id0 = 0 && id1 = 1) |> shouldEqual false

[<Test; NonParallelizable>]
let ``CSV virtual frame preserves virtual row index on filter`` () =
  SearchDataset.ensureCsv()
  let c, frame, words =
    InstrumentedCsvSource.createOrderedSearchFrame SearchDataset.csvPath (AccessCounters())
  c.Reset()
  let filtered = frame |> Frame.filterRowsBy "S2" SearchDataset.searchValue
  let d = c.Snapshot()
  d.LookupRangeCount |> should be (greaterThan 0)
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual (SearchDataset.expectedMatchCount SearchDataset.nLarge words.Length)

[<Test; NonParallelizable>]
let ``CSV virtual filter does not scan all rows at filter time`` () =
  SearchDataset.ensureCsv()
  let c, frame, _ =
    InstrumentedCsvSource.createOrderedSearchFrame SearchDataset.csvPath (AccessCounters())
  c.Reset()
  frame |> Frame.filterRowsBy "S2" SearchDataset.searchValue |> ignore
  let d = c.Snapshot()
  d.ValueAtCount |> should be (lessThan 100)
  d.LookupRangeCount |> shouldEqual 1

[<Test; NonParallelizable>]
let ``materialized ReadCsv loads full dataset`` () =
  SearchDataset.ensureCsv()
  let frame = Frame.ReadCsv(SearchDataset.csvPath, inferRows=100)
  frame.RowCount |> shouldEqual (int SearchDataset.nLarge)

[<Test; NonParallelizable>]
let ``CSV virtual slice reads only requested rows`` () =
  SearchDataset.ensureCsv()
  let c, series =
    InstrumentedCsvSource.createFloatValueSeries SearchDataset.csvPath (AccessCounters())
  c.Reset()
  let sliced = series.[1000L .. 1099L]
  SeriesProbe.isVirtual sliced |> shouldEqual true
  sliced.KeyCount |> shouldEqual 100
  c.Snapshot().ValueAtCount |> shouldEqual 0
  let expectedAt1000 =
    CsvLineIndex(SearchDataset.csvPath).ReadFields(1000L).[3]
    |> fun s -> Double.Parse(s, CultureInfo.InvariantCulture)
  sliced.GetAt(0) |> shouldEqual expectedAt1000
  c.Snapshot().ValueAtCount |> shouldEqual 1

[<Test; NonParallelizable>]
let ``CSV virtual Stats.sum materializes full column pull`` () =
  SearchDataset.ensureCsv()
  let c, series =
    InstrumentedCsvSource.createFloatValueSeries SearchDataset.csvPath (AccessCounters())
  c.Reset()
  let expectedSum = CsvTestData.readMeta(SearchDataset.csvPath).ValueSum
  Stats.sum series |> shouldEqual expectedSum
  let d = c.Snapshot()
  d.ValueAtCount |> shouldEqual (int SearchDataset.nLarge)
  SeriesProbe.isVirtual series |> shouldEqual true

[<Test; NonParallelizable>]
let ``file-backed filter is faster than materialized full scan`` () =
  SearchDataset.ensureCsv()
  let virtualMs =
    SearchDataset.elapsedMs (fun () ->
      let c, frame, _ =
        InstrumentedCsvSource.createOrderedSearchFrame SearchDataset.csvPath (AccessCounters())
      c.Reset()
      frame |> Frame.filterRowsBy "S2" SearchDataset.searchValue |> ignore)
  let materializedMs =
    SearchDataset.elapsedMs (fun () ->
      let frame = Frame.ReadCsv(SearchDataset.csvPath, inferRows=100)
      let col = frame.GetColumn<string>("Category")
      seq { for i in 0 .. frame.RowCount - 1 do if col.GetAt(i) = SearchDataset.searchValue then yield () }
      |> Seq.length
      |> ignore)
  virtualMs |> should be (lessThan materializedMs)
