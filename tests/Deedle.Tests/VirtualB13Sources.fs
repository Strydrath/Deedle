#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#else
module Deedle.Tests.VirtualB13Sources
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

module private B13 =
  let nLarge = 100_000L
  let dataDir = Path.Combine(__SOURCE_DIRECTORY__, "data")
  let csvPath = Path.Combine(dataDir, CsvTestData.defaultDatasetName)
  let parquetPath = Path.Combine(dataDir, ParquetTestData.defaultDatasetName)
  let gate = obj()

  let ensureCsv () =
    lock gate (fun () ->
      Directory.CreateDirectory dataDir |> ignore
      CsvTestData.ensureSearchCsv csvPath nLarge |> ignore)

  let ensureParquet () =
    lock gate (fun () ->
      ensureCsv ()
      ParquetTestData.ensureSearchParquet parquetPath nLarge |> ignore)

[<Test; NonParallelizable>]
let ``B13 CSV shared row cache decodes each row once across columns`` () =
  B13.ensureCsv()
  let lineIndex = CsvLineIndex(B13.csvPath)
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
let ``B13 CSV slice decode count stays within slice bounds`` () =
  B13.ensureCsv()
  let lineIndex = CsvLineIndex(B13.csvPath)
  let src =
    CsvVirtualSource.createColumnSource lineIndex "Value" None
    :?> IVirtualVectorSource<float>
  let series = Virtual.CreateOrdinalSeries(src)
  lineIndex.ResetSplitCount()
  let sliced = series.[1000L .. 1099L]
  Stats.sum sliced |> ignore
  lineIndex.SplitCount |> shouldEqual 100

[<Test; NonParallelizable>]
let ``B13 virtual Parquet Stats.sum matches CSV meta`` () =
  B13.ensureParquet()
  let expected = CsvTestData.readMeta(B13.csvPath).ValueSum
  let csvSum = Stats.sum (CsvTestData.createFloatValueSeries B13.csvPath)
  Assert.That(abs (csvSum - expected), Is.LessThan(1.0), sprintf "csv=%f expected=%f" csvSum expected)
  let materializedSum =
    Stats.sum ((Frame.readParquet B13.parquetPath).GetColumn<float>("Value"))
  Assert.That(abs (materializedSum - expected), Is.LessThan(1.0), sprintf "mat=%f expected=%f" materializedSum expected)
  let virtualSum = Stats.sum (ParquetTestData.createFloatValueSeries B13.parquetPath)
  Assert.That(abs (virtualSum - expected), Is.LessThan(1.0), sprintf "virt=%f expected=%f" virtualSum expected)
  Assert.That(abs (virtualSum - materializedSum), Is.LessThan(0.01), sprintf "virt=%f mat=%f" virtualSum materializedSum)

[<Test; NonParallelizable>]
let ``B13 Virtual ReadParquet builds searchable frame`` () =
  B13.ensureParquet()
  let frame =
    Virtual.ReadParquet(
      B13.parquetPath,
      indexColumn = "Timestamp",
      searchColumn = "Category",
      searchLookupRange = VirtualLookupRange.forRepeatingCycle CsvTestData.words8,
      columnKeys = [ "Id"; "Category" ])
  frame.RowCount |> shouldEqual (int B13.nLarge)
  let filtered = frame |> Frame.filterRowsBy "Category" "lorem"
  filtered.RowCount |> shouldEqual 12_500

[<Test>]
let ``B13 Parquet null floats stay missing not NaN`` () =
  let path = Path.Combine(Path.GetTempPath(), sprintf "deedle-b13-nulls-%d.parquet" Environment.TickCount)
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
    // Load column, then dispose the index before series use so the temp file can be deleted.
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
