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
open Deedle.Internal
open Deedle.Addressing
open Deedle.Vectors
open Deedle.Virtual
open Deedle.Virtual.Sources
open Deedle.Vectors.Virtual
open Deedle.TestData
open Deedle.Tests.VirtualInstrumentation

// ------------------------------------------------------------------------------------------------
// Virtual.ReadCsv (src/Deedle/VirtualCsvSource.fs)
// ------------------------------------------------------------------------------------------------

let fixturesPath = Path.Combine(__SOURCE_DIRECTORY__, "data", "virtual-fixtures.csv")

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

module private InstrumentedCsvSource =
  let private wrap (counters: AccessCounters) (source: IVirtualVectorSource) =
    CountingVirtualSource.Wrap counters source

  let createOrderedSearchFrame (csvPath: string) (counters: AccessCounters) =
    let lineIndex = CsvLineIndex(csvPath)
    let idx =
      wrap counters (VirtualCsvSource.createIndexSource lineIndex "Timestamp")
      :?> IVirtualVectorSource<DateTimeOffset>
    let idCol = wrap counters (VirtualCsvSource.createColumnSource lineIndex "Id" None)
    let catCol =
      wrap counters
        (VirtualCsvSource.createColumnSource lineIndex "Category"
          (Some(VirtualLookupRange.forRepeatingCycle CsvTestData.words8)))
    let frame = Virtual.CreateFrame(idx, [ "S1"; "S2" ], [ idCol; catCol ])
    counters, frame, CsvTestData.words8

  let createFloatValueSeries (csvPath: string) (counters: AccessCounters) =
    let lineIndex = CsvLineIndex(csvPath)
    let src =
      wrap counters (VirtualCsvSource.createColumnSource lineIndex "Value" None)
      :?> IVirtualVectorSource<float>
    counters, Virtual.CreateOrdinalSeries(src)

[<Test>]
let ``Can read virtual fixtures CSV with quoted fields and missing cells`` () =
  let frame = Virtual.ReadCsv(fixturesPath, indexColumn = "Timestamp", columnKeys = [ "Id"; "Category"; "Label"; "Value" ])
  frame.RowCount |> shouldEqual 4
  frame.GetColumn<string>("Label").GetAt(0) |> shouldEqual "hello, world"
  frame.GetColumn<string>("Label").GetAt(1) |> shouldEqual "a \"b\" c"
  frame.GetColumn<int64>("Id").TryGetAt(2).HasValue |> shouldEqual false
  frame.GetColumn<float>("Value").TryGetAt(2).HasValue |> shouldEqual false
  Virtual.GetRowIndexKind frame |> shouldEqual VirtualRowIndexKind.OrderedVirtual

[<Test>]
let ``Can throw when ReadCsv file is missing`` () =
  (fun () -> Virtual.ReadCsv(Path.Combine(Path.GetTempPath(), "deedle-csv-missing.csv")) |> ignore)
  |> should throw typeof<FileNotFoundException>

[<Test>]
let ``Can throw when ReadCsv has no data rows`` () =
  let path = Path.Combine(Path.GetTempPath(), "deedle-csv-empty.csv")
  File.WriteAllText(path, "Timestamp,Id\r\n")
  try
    (fun () -> Virtual.ReadCsv(path) |> ignore)
    |> should throw typeof<System.ArgumentException>
  finally
    if File.Exists path then File.Delete path

[<Test; NonParallelizable>]
let ``Can read large search CSV with virtual row index`` () =
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
let ``Can auto-detect Timestamp index column when reading CSV virtually`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-csv-autodetect.csv")
  CsvTestData.ensureSearchCsv csvPath 1000L |> ignore
  let frame = Virtual.ReadCsv(csvPath, columnKeys = [ "Id" ])
  frame.RowCount |> shouldEqual 1000
  FrameProbe.rowIndexIsVirtual frame |> shouldEqual true

[<Test; NonParallelizable>]
let ``Can infer remaining columns when columnKeys omitted`` () =
  let csvPath = Path.Combine(Path.GetTempPath(), "deedle-csv-infer.csv")
  CsvTestData.ensureSearchCsv csvPath 1000L |> ignore
  let frame = Virtual.ReadCsv(csvPath, indexColumn = "Timestamp")
  frame.ColumnCount |> shouldEqual 3
  frame.ColumnKeys |> Seq.toList |> shouldEqual [ "Id"; "Category"; "Value" ]
  frame.GetColumn<int64>("Id").KeyCount |> shouldEqual 1000

[<Test; NonParallelizable>]
let ``Can filter with forCategoricalScan without Step cycle`` () =
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
let ``Can read empty and NA cells as missing values in virtual CSV`` () =
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
let ``Can filter unknown repeating-cycle value to empty result`` () =
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
let ``Can parse quoted CSV fields with commas and escaped quotes`` () =
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
let ``Can auto-detect ordered datetime row index from CSV`` () =
  let path = Path.GetTempFileName() + ".csv"
  try
    File.WriteAllLines(path, [| "Timestamp,Id,Category"; "2020-01-01T00:00:00Z,1,lorem"; "2020-01-02T00:00:00Z,2,ipsum" |])
    let frame = Virtual.ReadCsv(path, columnKeys = [ "Id"; "Category" ])
    Virtual.GetRowIndexKind frame |> shouldEqual VirtualRowIndexKind.OrderedVirtual
  finally
    if File.Exists path then File.Delete path

[<Test>]
let ``Can infer Step LookupRange for low-cardinality search column`` () =
  let path = Path.GetTempFileName() + ".csv"
  let words = CsvTestData.words8
  try
    CsvTestData.ensureSearchCsv path 1000L |> ignore
    let frame = Virtual.ReadCsv(path, indexColumn = "Timestamp", searchColumn = "Category", columnKeys = [ "Id"; "Category" ])
    Virtual.IsVirtualColumn(frame, "Category") |> shouldEqual true
    let filtered = frame |> Frame.filterRowsBy "Category" words.[0]
    Virtual.GetRowIndexKind filtered |> shouldEqual VirtualRowIndexKind.OrderedVirtual
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
let ``Can expose virtual ordered row index and csv-file scheme`` () =
  let csvPath = Path.GetTempFileName() + ".csv"
  try
    CsvTestData.ensureSearchCsv csvPath 500L |> ignore
    let csv = Virtual.ReadCsv(csvPath, indexColumn = "Timestamp", columnKeys = [ "Id"; "Category" ])
    Virtual.GetRowIndexKind csv |> shouldEqual VirtualRowIndexKind.OrderedVirtual
    Virtual.TryGetRowIndexSchemeId csv |> shouldEqual (Some "csv-file")
    Virtual.IsVirtualRowIndex csv |> shouldEqual true
  finally
    if File.Exists csvPath then File.Delete csvPath

[<Test; NonParallelizable>]
let ``Can decode each CSV row once across columns via shared cache`` () =
  SearchDataset.ensureCsv()
  let lineIndex = CsvLineIndex(SearchDataset.csvPath)
  let idSrc = VirtualCsvSource.createColumnSource lineIndex "Id" None :?> IVirtualVectorSource<int64>
  let valSrc = VirtualCsvSource.createColumnSource lineIndex "Value" None :?> IVirtualVectorSource<float>
  let idSeries = Virtual.CreateOrdinalSeries(idSrc)
  let valSeries = Virtual.CreateOrdinalSeries(valSrc)
  lineIndex.ResetSplitCount()
  for row in 1000L .. 1099L do
    idSeries.TryGet row |> ignore
    valSeries.TryGet row |> ignore
  lineIndex.SplitCount |> shouldEqual 100

[<Test; NonParallelizable>]
let ``Can keep slice decode count within slice bounds`` () =
  SearchDataset.ensureCsv()
  let lineIndex = CsvLineIndex(SearchDataset.csvPath)
  let src =
    VirtualCsvSource.createColumnSource lineIndex "Value" None
    :?> IVirtualVectorSource<float>
  let series = Virtual.CreateOrdinalSeries(src)
  lineIndex.ResetSplitCount()
  let sliced = series.[1000L .. 1099L]
  Stats.sum sliced |> ignore
  lineIndex.SplitCount |> shouldEqual 100

[<Test; NonParallelizable>]
let ``Can filterRowsBy2 on ReadCsv staying virtual with correct count`` () =
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
let ``Can match filterRowsBy2 row count to single filter on ReadCsv`` () =
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
let ``Can validate generated search CSV schema and meta`` () =
  SearchDataset.ensureCsv()
  let idx = CsvLineIndex(SearchDataset.csvPath)
  idx.Length |> shouldEqual SearchDataset.nLarge
  let fields = idx.ReadFields 0L
  fields.Length |> shouldEqual 4
  fields.[2] |> shouldEqual SearchDataset.searchValue
  let meta = CsvTestData.readMeta SearchDataset.csvPath
  meta.Seed |> shouldEqual CsvTestData.defaultSeed
  meta.RowCount |> shouldEqual SearchDataset.nLarge
  let id0 = Int32.Parse(fields.[0])
  let id1 = Int32.Parse((idx.ReadFields 1L).[0])
  (id0 = 0 && id1 = 1) |> shouldEqual false

[<Test; NonParallelizable>]
let ``Can preserve virtual row index on CSV filter`` () =
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
let ``Can filter CSV virtually without scanning all rows at filter time`` () =
  SearchDataset.ensureCsv()
  let c, frame, _ =
    InstrumentedCsvSource.createOrderedSearchFrame SearchDataset.csvPath (AccessCounters())
  c.Reset()
  frame |> Frame.filterRowsBy "S2" SearchDataset.searchValue |> ignore
  let d = c.Snapshot()
  d.ValueAtCount |> should be (lessThan 100)
  d.LookupRangeCount |> shouldEqual 1

[<Test; NonParallelizable>]
let ``Can load materialized CSV as baseline for virtual comparisons`` () =
  SearchDataset.ensureCsv()
  let frame = Frame.ReadCsv(SearchDataset.csvPath, inferRows=100)
  frame.RowCount |> shouldEqual (int SearchDataset.nLarge)

[<Test; NonParallelizable>]
let ``Can slice virtual CSV reading only requested rows`` () =
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
let ``Can materialize full column on CSV Stats.sum`` () =
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
let ``Can filter file-backed CSV faster than materialized full scan`` () =
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

[<Test>]
let ``Can throw when ReadCsv column name is unknown`` () =
  let path = Path.Combine(Path.GetTempPath(), "deedle-csv-unknown-col.csv")
  CsvTestData.ensureSearchCsv path 10L |> ignore
  try
    (fun () -> Virtual.ReadCsv(path, indexColumn = "Timestamp", columnKeys = [ "NotAColumn" ]) |> ignore)
    |> should throw typeof<ArgumentException>
  finally
    if File.Exists path then File.Delete path

[<Test>]
let ``Can throw when ReadCsv index column cell is invalid`` () =
  let path = Path.Combine(Path.GetTempPath(), "deedle-csv-bad-index.csv")
  File.WriteAllText(
    path,
    "Timestamp,Id\r\n" +
    "not-a-datetime,1\r\n" +
    "2000-01-01T00:00:01.0000000+00:00,2\r\n")
  try
    let lineIndex = CsvLineIndex(path)
    let idx =
      VirtualCsvSource.createIndexSource lineIndex "Timestamp"
      :?> IVirtualVectorSource<DateTimeOffset>
    (fun () -> idx.ValueAt(KnownLocation(LinearAddress.ofInt64 0L, 0L)) |> ignore)
    |> should throw typeof<System.FormatException>
  finally
    if File.Exists path then File.Delete path

[<Test>]
let ``Can read CSV via byte-offset row index matching the line cache`` () =
  let path = Path.Combine(Path.GetTempPath(), "deedle-csv-byte-offset.csv")
  File.WriteAllText(
    path,
    "Timestamp,Id,Category\r\n" +
    "2000-01-01T00:00:00.0000000+00:00,1,a\r\n" +
    "2000-01-01T00:00:01.0000000+00:00,2,b\r\n" +
    "2000-01-01T00:00:02.0000000+00:00,3,c\r\n")
  try
    let cached = CsvLineIndex(path)
    let seeked = CsvLineIndex(path, byteOffset=true)
    cached.IsByteOffset |> shouldEqual false
    seeked.IsByteOffset |> shouldEqual true
    seeked.Length |> shouldEqual cached.Length
    for i in 0L .. cached.Length - 1L do
      seeked.ReadFields(i) |> shouldEqual (cached.ReadFields(i))
    let frame = Virtual.ReadCsv(path, indexColumn="Timestamp", columnKeys=["Id"; "Category"], byteOffsetIndex=true)
    frame.RowCount |> shouldEqual 3
    frame.GetColumn<int64>("Id").GetAt(2) |> shouldEqual 3L
  finally
    if File.Exists path then File.Delete path

[<Test>]
let ``Can concatenate csv-parts directory as a partitioned virtual frame`` () =
  let dir = Path.Combine(Path.GetTempPath(), "deedle-csv-parts")
  if Directory.Exists dir then Directory.Delete(dir, true)
  Directory.CreateDirectory dir |> ignore
  let p1 = Path.Combine(dir, "part-a.csv")
  let p2 = Path.Combine(dir, "part-b.csv")
  File.WriteAllText(p1, "Id,Category\r\n1,a\r\n2,b\r\n")
  File.WriteAllText(p2, "Id,Category\r\n3,a\r\n4,b\r\n")
  try
    let frame =
      Virtual.ReadCsvDirectory(
        dir,
        searchColumn="Category",
        searchLookupRange=VirtualLookupRange.forRepeatingCycle [| "a"; "b" |],
        byteOffsetIndex=true)
    FrameProbe.rowIndexIsVirtual frame |> shouldEqual true
    frame.RowCount |> shouldEqual 4
    frame.GetColumn<int64>("Id").GetAt(0) |> shouldEqual 1L
    frame.GetColumn<int64>("Id").GetAt(3) |> shouldEqual 4L
    let filtered = frame |> Frame.filterRowsBy "Category" "a"
    filtered.RowCount |> shouldEqual 2
  finally
    if Directory.Exists dir then Directory.Delete(dir, true)
