#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.VirtualParquetSource
#endif

open System
open System.IO
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Virtual
open Deedle.Virtual.Sources
open Deedle.Vectors.Virtual
open Deedle.Parquet
open Deedle.Parquet.Virtual.Sources
open Deedle.Tests.VirtualInstrumentation

module private SearchDataset =
  let nLarge = 100_000L
  let dataDir = Path.Combine(__SOURCE_DIRECTORY__, "data")
  let csvPath = Path.Combine(dataDir, CsvTestData.defaultDatasetName)
  let parquetPath = Path.Combine(dataDir, ParquetTestData.defaultDatasetName)
  let gate = obj()

  let ensureParquet () =
    lock gate (fun () ->
      Directory.CreateDirectory dataDir |> ignore
      CsvTestData.ensureSearchCsv csvPath nLarge |> ignore
      ParquetTestData.ensureSearchParquet parquetPath nLarge |> ignore)

[<Test; NonParallelizable>]
let ``Virtual ReadParquet Stats.sum matches CSV meta and materialized Parquet`` () =
  SearchDataset.ensureParquet()
  let expected = CsvTestData.readMeta(SearchDataset.csvPath).ValueSum
  let csvSum = Stats.sum (CsvTestData.createFloatValueSeries SearchDataset.csvPath)
  Assert.That(abs (csvSum - expected), Is.LessThan(1.0), sprintf "csv=%f expected=%f" csvSum expected)
  let materializedSum =
    Stats.sum ((Frame.readParquet SearchDataset.parquetPath).GetColumn<float>("Value"))
  Assert.That(abs (materializedSum - expected), Is.LessThan(1.0), sprintf "mat=%f expected=%f" materializedSum expected)
  let virtualSum = Stats.sum (ParquetTestData.createFloatValueSeries SearchDataset.parquetPath)
  Assert.That(abs (virtualSum - expected), Is.LessThan(1.0), sprintf "virt=%f expected=%f" virtualSum expected)
  Assert.That(abs (virtualSum - materializedSum), Is.LessThan(0.01), sprintf "virt=%f mat=%f" virtualSum materializedSum)

[<Test; NonParallelizable>]
let ``ReadParquet infers Step LookupRange for Category without searchLookupRange`` () =
  SearchDataset.ensureParquet()
  let frame =
    Virtual.ReadParquet(
      SearchDataset.parquetPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      columnKeys = [ "Id"; "Category" ])
  VirtualFrameDiagnostics.IsVirtualColumn(frame, "Category") |> shouldEqual true
  let filtered = frame |> Frame.filterRowsBy "Category" "lorem"
  VirtualFrameDiagnostics.GetRowIndexKind filtered |> shouldEqual VirtualRowIndexKind.OrderedVirtual
  filtered.RowCount |> shouldEqual 12_500

[<Test; NonParallelizable>]
let ``ReadParquet filterRowsBy on inferred Category stays virtual at filter time`` () =
  SearchDataset.ensureParquet()
  let frame =
    Virtual.ReadParquet(
      SearchDataset.parquetPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      columnKeys = [ "Id"; "Category" ])
  let filtered = frame |> Frame.filterRowsBy "Category" "lorem"
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> shouldEqual 12_500

[<Test; NonParallelizable>]
let ``ReadParquet does not infer LookupRange when searchColumn omitted`` () =
  SearchDataset.ensureParquet()
  let frame =
    Virtual.ReadParquet(
      SearchDataset.parquetPath,
      indexColumn = "Timestamp",
      columnKeys = [ "Id"; "Category" ])
  (fun () -> frame |> Frame.filterRowsBy "Category" "lorem" |> ignore)
  |> should throw typeof<NotSupportedException>

[<Test>]
let ``ReadParquet high-cardinality search column requires explicit LookupRange`` () =
  let path = Path.Combine(Path.GetTempPath(), sprintf "deedle-parquet-hicard-%d.parquet" Environment.TickCount)
  try
    let n = 100
    let schema = Parquet.Schema.ParquetSchema([|
      Parquet.Schema.DataField("Timestamp", typeof<Nullable<DateTime>>) :> Parquet.Schema.Field
      Parquet.Schema.DataField("Category", typeof<string>) :> Parquet.Schema.Field |])
    let fields = schema.GetDataFields()
    let start = DateTime(2000, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let ts = [| for i in 0 .. n - 1 -> Nullable(start.AddDays(float i)) |]
    let cats = [| for i in 0 .. n - 1 -> sprintf "unique-category-%d" i |]
    do
      use stream = File.Create path
      use writer = global.Parquet.ParquetWriter.CreateAsync(schema, stream).GetAwaiter().GetResult()
      use rg = writer.CreateRowGroup()
      rg.WriteColumnAsync(Parquet.Data.DataColumn(fields.[0], ts)).GetAwaiter().GetResult()
      rg.WriteColumnAsync(Parquet.Data.DataColumn(fields.[1], cats)).GetAwaiter().GetResult()
    let assertFilterThrows () =
      let frame = Virtual.ReadParquet(path, indexColumn = "Timestamp", searchColumn = "Category", columnKeys = [ "Category" ])
      (fun () -> frame |> Frame.filterRowsBy "Category" cats.[0] |> ignore)
      |> should throw typeof<NotSupportedException>
    assertFilterThrows()
  finally
    GC.Collect()
    GC.WaitForPendingFinalizers()
    try File.Delete path with :? IOException -> ()

[<Test; NonParallelizable>]
let ``ReadParquet builds searchable frame with explicit LookupRange`` () =
  SearchDataset.ensureParquet()
  let frame =
    Virtual.ReadParquet(
      SearchDataset.parquetPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      searchLookupRange = VirtualLookupRange.forRepeatingCycle CsvTestData.words8,
      columnKeys = [ "Id"; "Category" ])
  frame.RowCount |> shouldEqual (int SearchDataset.nLarge)
  let filtered = frame |> Frame.filterRowsBy "Category" "lorem"
  filtered.RowCount |> shouldEqual 12_500

[<Test; NonParallelizable>]
let ``filterRowsBy2 on ReadParquet stays virtual with correct count`` () =
  SearchDataset.ensureParquet()
  let searchValue = "lorem"
  let frame =
    Virtual.ReadParquet(
      SearchDataset.parquetPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      searchLookupRange = VirtualLookupRange.forRepeatingCycle CsvTestData.words8,
      columnKeys = [ "Id"; "Category" ])
  let fused = frame |> Frame.filterRowsBy2 "Category" searchValue "Category" searchValue
  FrameProbe.rowIndexIsVirtual fused |> shouldEqual true
  fused.RowCount |> shouldEqual 12_500

[<Test; NonParallelizable>]
let ``filterRowsBy2 row count matches single filter on ReadParquet`` () =
  SearchDataset.ensureParquet()
  let searchValue = "lorem"
  let frame =
    Virtual.ReadParquet(
      SearchDataset.parquetPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      searchLookupRange = VirtualLookupRange.forRepeatingCycle CsvTestData.words8,
      columnKeys = [ "Id"; "Category" ])
  let single = frame |> Frame.filterRowsBy "Category" searchValue
  let fused = frame |> Frame.filterRowsBy2 "Category" searchValue "Category" searchValue
  fused.RowCount |> shouldEqual single.RowCount

[<Test>]
let ``Parquet null floats stay missing not NaN`` () =
  let path = Path.Combine(Path.GetTempPath(), sprintf "deedle-parquet-nulls-%d.parquet" Environment.TickCount)
  try
    let schema = Parquet.Schema.ParquetSchema([|
      Parquet.Schema.DataField("Value", typeof<Nullable<float>>) :> Parquet.Schema.Field |])
    let fields = schema.GetDataFields()
    do
      use stream = File.Create path
      use writer = Parquet.ParquetWriter.CreateAsync(schema, stream).GetAwaiter().GetResult()
      use rg = writer.CreateRowGroup()
      let data = [| Nullable(1.0); Nullable(); Nullable(3.0) |]
      rg.WriteColumnAsync(Parquet.Data.DataColumn(fields.[0], data)).GetAwaiter().GetResult()
    let values =
      use idx = new ParquetFileIndex(path)
      idx.ReadFloatColumn "Value"
    values.Length |> shouldEqual 3
    values.[0].HasValue |> shouldEqual true
    values.[0].Value |> shouldEqual 1.0
    values.[1].HasValue |> shouldEqual false
    values.[2].HasValue |> shouldEqual true
    let series =
      Virtual.CreateOrdinalSeries(
        OrdinalVirtualSource(int64 values.Length, (fun i -> values.[int i]), "parquet-file")
        :> IVirtualVectorSource<float>)
    series.Values |> Seq.toList |> shouldEqual [ 1.0; 3.0 ]
    Stats.sum series |> shouldEqual 4.0
  finally
    if File.Exists path then File.Delete path

[<Test>]
let ``ReadParquet supports all Parquet.fs column CLR types`` () =
  let path = Path.Combine(Path.GetTempPath(), sprintf "deedle-parquet-alltypes-%d.parquet" Environment.TickCount)
  try
    let t0 = DateTime(2020, 1, 1, 12, 0, 0, DateTimeKind.Utc)
    let t1 = DateTime(2020, 1, 2, 12, 0, 0, DateTimeKind.Utc)
    let df =
      Frame.ofColumns [
        "Timestamp" => (Series.ofValues [ t0; t1 ] :> ISeries<_>)
        "F64"  => (Series.ofOptionalObservations [ (0, Some 1.5); (1, None) ] :> ISeries<_>)
        "F32"  => (Series.ofValues [ 1.0f; 2.5f ] :> ISeries<_>)
        "I32"  => (Series.ofValues [ 10; 20 ] :> ISeries<_>)
        "I64"  => (Series.ofValues [ 100L; 200L ] :> ISeries<_>)
        "I16"  => (Series.ofValues [ 1s; -2s ] :> ISeries<_>)
        "U8"   => (Series.ofValues [ 1uy; 255uy ] :> ISeries<_>)
        "U16"  => (Series.ofValues [ 1us; 1000us ] :> ISeries<_>)
        "U32"  => (Series.ofValues [ 1u; 100000u ] :> ISeries<_>)
        "U64"  => (Series.ofValues [ 1UL; 123456789UL ] :> ISeries<_>)
        "Flag" => (Series.ofValues [ true; false ] :> ISeries<_>)
        "Name" => (Series.ofValues [ "alpha"; "beta" ] :> ISeries<_>)
        "When" => (Series.ofValues [ t0; t1 ] :> ISeries<_>) ]
    Frame.writeParquet path df
    let keys =
      [ "F64"; "F32"; "I32"; "I64"; "I16"; "U8"; "U16"; "U32"; "U64"; "Flag"; "Name"; "When" ]
    let frame = Virtual.ReadParquet(path, indexColumn = "Timestamp", columnKeys = keys)
    frame.RowCount |> shouldEqual 2
    frame.RowIndex.Keys |> Seq.map (fun dto -> dto.UtcDateTime) |> Seq.toList
    |> shouldEqual [ t0; t1 ]

    let f64 = frame.GetColumn<float>("F64")
    f64.TryGetAt(0).Value |> shouldEqual 1.5
    f64.TryGetAt(1).HasValue |> shouldEqual false
    frame.GetColumn<float32>("F32").Values |> Seq.toList |> shouldEqual [ 1.0f; 2.5f ]
    frame.GetColumn<int>("I32").Values |> Seq.toList |> shouldEqual [ 10; 20 ]
    frame.GetColumn<int64>("I64").Values |> Seq.toList |> shouldEqual [ 100L; 200L ]
    frame.GetColumn<int16>("I16").Values |> Seq.toList |> shouldEqual [ 1s; -2s ]
    frame.GetColumn<byte>("U8").Values |> Seq.toList |> shouldEqual [ 1uy; 255uy ]
    frame.GetColumn<uint16>("U16").Values |> Seq.toList |> shouldEqual [ 1us; 1000us ]
    frame.GetColumn<uint32>("U32").Values |> Seq.toList |> shouldEqual [ 1u; 100000u ]
    frame.GetColumn<uint64>("U64").Values |> Seq.toList |> shouldEqual [ 1UL; 123456789UL ]
    frame.GetColumn<bool>("Flag").Values |> Seq.toList |> shouldEqual [ true; false ]
    frame.GetColumn<string>("Name").Values |> Seq.toList |> shouldEqual [ "alpha"; "beta" ]
    let whenCol = frame.GetColumn<DateTime>("When").Values |> Seq.toList
    abs((whenCol.[0] - t0).TotalSeconds) |> should be (lessThan 1.0)
    abs((whenCol.[1] - t1).TotalSeconds) |> should be (lessThan 1.0)
  finally
    if File.Exists path then
      try File.Delete path with _ -> ()
