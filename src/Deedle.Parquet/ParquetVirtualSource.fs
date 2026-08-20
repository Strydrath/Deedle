namespace Deedle.Parquet.Virtual.Sources

open System
open System.Collections.Concurrent
open System.Globalization
open System.IO
open Deedle
open Deedle.Virtual
open Deedle.Vectors.Virtual
open Parquet.Schema
open Parquet.Data

/// Options for [`Virtual.ReadParquet`].
type VirtualReadParquetOptions =
  { IndexColumn: string option
    SearchColumn: (string * LookupRangeMode<string>) option
    ColumnKeys: string list option }

  static member Default =
    { IndexColumn = None
      SearchColumn = None
      ColumnKeys = None }

open Deedle.Parquet

module private ColumnValues =
  let append (acc: ResizeArray<OptionalValue<obj>>) (typ: Type) (vec: IVector) =
    let addSeq (seq: seq<OptionalValue<'T>>) =
      for ov in seq do
        acc.Add(if ov.HasValue then OptionalValue(box ov.Value) else OptionalValue.Missing)
    if typ = typeof<float> then addSeq ((vec :?> IVector<float>).DataSequence)
    elif typ = typeof<int> then addSeq ((vec :?> IVector<int>).DataSequence)
    elif typ = typeof<int64> then addSeq ((vec :?> IVector<int64>).DataSequence)
    elif typ = typeof<string> then addSeq ((vec :?> IVector<string>).DataSequence)
    elif typ = typeof<DateTimeOffset> then addSeq ((vec :?> IVector<DateTimeOffset>).DataSequence)
    elif typ = typeof<DateTime> then addSeq ((vec :?> IVector<DateTime>).DataSequence)
    else failwithf "ParquetVirtualSource: unsupported column type '%s'" typ.Name

/// Shared Parquet file handle: schema, row count, and lazily loaded column arrays.
type ParquetFileIndex(path: string) =
  let stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read)
  let reader = global.Parquet.ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
  let dataFields = reader.Schema.GetDataFields()
  // Prefer metadata NumRows — never ReadEntireRowGroup just to count.
  let rowCount =
    if reader.Metadata <> null && reader.Metadata.NumRows > 0L then reader.Metadata.NumRows
    elif reader.RowGroupCount = 0 then 0L
    else
      [| for rgIdx in 0 .. reader.RowGroupCount - 1 ->
           int64 (reader.OpenRowGroupReader(rgIdx).RowCount) |]
      |> Array.sum
  let columnCache = ConcurrentDictionary<string, obj>()

  member _.Path = path
  member _.Length = rowCount
  member _.DataFields = dataFields

  member _.FieldIndex(name: string) =
    match dataFields |> Array.tryFindIndex (fun f -> String.Equals(f.Name, name, StringComparison.OrdinalIgnoreCase)) with
    | Some idx -> idx
    | None -> failwithf "ParquetVirtualSource: column '%s' not found" name

  member private this.ReadColumn (name: string) =
    let field = dataFields.[this.FieldIndex name]
    [| for rgIdx in 0 .. reader.RowGroupCount - 1 do
         let rg = reader.ReadEntireRowGroupAsync(rgIdx).GetAwaiter().GetResult()
         yield rg |> Array.find (fun c -> c.Field.Name = field.Name) |]

  member private this.ReadColumnValues (name: string) =
    let values = ResizeArray<OptionalValue<obj>>()
    for col in this.ReadColumn name do
      let (typ, vec) = Implementation.readColumn col
      ColumnValues.append values typ vec
    values.ToArray()

  member private this.MaterializeColumn (name: string) (convert: OptionalValue<obj>[] -> obj) =
    columnCache.GetOrAdd(name, fun _ -> box (convert (this.ReadColumnValues name)))

  /// Read a float column directly from Parquet.Data arrays (no ObjectSequence boxing).
  member this.ReadFloatColumn(name: string) =
    columnCache.GetOrAdd(name, fun _ ->
      let acc = ResizeArray<float>(int rowCount)
      for col in this.ReadColumn name do
        let data = col.Data
        let n = int col.NumValues
        match data with
        | :? (Nullable<float>[]) as arr ->
            for i in 0 .. n - 1 do
              acc.Add(if arr.[i].HasValue then arr.[i].Value else nan)
        | :? (float[]) as arr ->
            for i in 0 .. n - 1 do acc.Add(arr.[i])
        | :? (string[]) as arr ->
            for i in 0 .. n - 1 do
              acc.Add(
                if isNull arr.[i] then nan
                else Double.Parse(arr.[i], CultureInfo.InvariantCulture))
        | _ ->
            // Fallback through Deedle.Parquet typed reader
            let (_, vec) = Implementation.readColumn col
            for ov in (vec :?> IVector<float>).DataSequence do
              acc.Add(if ov.HasValue then ov.Value else nan)
      box (acc.ToArray()))
    :?> float[]

  member this.ReadInt64Column(name: string) =
    this.MaterializeColumn name (fun values ->
      box (values |> Array.map (fun ov ->
        if ov.HasValue then
          match ov.Value with
          | :? int64 as v -> v
          | :? int as v -> int64 v
          | :? string as s -> Int64.Parse(s, CultureInfo.InvariantCulture)
          | v -> unbox<int64> v
        else 0L)))
    :?> int64[]

  member this.ReadDateTimeOffsetColumn(name: string) =
    this.MaterializeColumn name (fun values ->
      box (values |> Array.map (fun ov ->
        if ov.HasValue then
          match ov.Value with
          | :? DateTimeOffset as dto -> dto
          | :? DateTime as dt -> DateTimeOffset dt
          | :? string as s ->
            DateTimeOffset.Parse(s, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind)
          | v -> unbox<DateTimeOffset> v
        else DateTimeOffset.MinValue)))
    :?> DateTimeOffset[]

  member this.ReadStringColumn(name: string) =
    this.MaterializeColumn name (fun values ->
      box (values |> Array.map (fun ov -> if ov.HasValue then unbox<string> ov.Value else null)))
    :?> string[]

  interface IDisposable with
    member _.Dispose() =
      (reader :> IDisposable).Dispose()
      stream.Dispose()

module internal ParquetColumnSource =
  let createFloat (index: ParquetFileIndex) (name: string) (lookupRange: LookupRangeMode<float> option) =
    let data = index.ReadFloatColumn name
    let lr = defaultArg lookupRange LookupRangeUnsupported
    OrdinalVirtualSource(index.Length, (fun row -> data.[int row]), "parquet-file", lookupRange=lr) :> IVirtualVectorSource

  let createInt64 (index: ParquetFileIndex) (name: string) (lookupRange: LookupRangeMode<int64> option) =
    let data = index.ReadInt64Column name
    let lr = defaultArg lookupRange LookupRangeUnsupported
    OrdinalVirtualSource(index.Length, (fun row -> data.[int row]), "parquet-file", asLong=id, lookupRange=lr) :> IVirtualVectorSource

  let createDateTimeOffset (index: ParquetFileIndex) (name: string) =
    let data = index.ReadDateTimeOffsetColumn name
    OrdinalVirtualSource(index.Length, (fun row -> data.[int row]), "parquet-file", asLong=(fun dto -> dto.UtcTicks)) :> IVirtualVectorSource

  let createString (index: ParquetFileIndex) (name: string) (lookupRange: LookupRangeMode<string> option) =
    let data = index.ReadStringColumn name
    let lr = defaultArg lookupRange LookupRangeUnsupported
    OrdinalVirtualSource(index.Length, (fun row -> data.[int row]), "parquet-file", lookupRange=lr) :> IVirtualVectorSource

  let resolveIndexColumn (fields: DataField[]) (options: VirtualReadParquetOptions) =
    match options.IndexColumn with
    | Some name ->
      match fields |> Array.tryFindIndex (fun f -> String.Equals(f.Name, name, StringComparison.OrdinalIgnoreCase)) with
      | Some idx -> idx
      | None -> failwithf "ParquetVirtualSource: index column '%s' not found" name
    | None ->
      let preferred =
        fields
        |> Array.tryFindIndex (fun f ->
          String.Equals(f.Name, "Timestamp", StringComparison.OrdinalIgnoreCase)
          || String.Equals(f.Name, "DateTime", StringComparison.OrdinalIgnoreCase)
          || f.Name.EndsWith("Time", StringComparison.OrdinalIgnoreCase))
      match preferred with
      | Some idx -> idx
      | None -> 0

module ParquetVirtualSource =
  open ParquetColumnSource

  let private columnKind (fields: DataField[]) (columnIndex: int) =
    let field = fields.[columnIndex]
    let clrType = field.ClrType
    let baseType =
      match Nullable.GetUnderlyingType clrType with
      | null -> clrType
      | ut -> ut
    if baseType = typeof<DateTimeOffset> || baseType = typeof<DateTime> then "datetime"
    elif baseType = typeof<int64> || baseType = typeof<int> then "int64"
    elif baseType = typeof<float> || baseType = typeof<float32> then "float"
    elif baseType = typeof<string> then
      if String.Equals(field.Name, "Value", StringComparison.OrdinalIgnoreCase) then "float"
      elif String.Equals(field.Name, "Timestamp", StringComparison.OrdinalIgnoreCase)
           || String.Equals(field.Name, "DateTime", StringComparison.OrdinalIgnoreCase) then "datetime"
      elif String.Equals(field.Name, "Id", StringComparison.OrdinalIgnoreCase) then "int64"
      else "string"
    else "string"

  let createTypedColumn (index: ParquetFileIndex) (name: string) (kind: string) (lookupRange: LookupRangeMode<string> option) =
    match kind with
    | "datetime" -> createDateTimeOffset index name
    | "int64" -> createInt64 index name None
    | "float" -> createFloat index name None
    | _ -> createString index name lookupRange

  let createFrame (parquetPath: string) (options: VirtualReadParquetOptions) =
    if not (File.Exists parquetPath) then failwithf "ParquetVirtualSource: file not found '%s'" parquetPath
    use fileIndex = new ParquetFileIndex(parquetPath)
    if fileIndex.Length = 0L then invalidArg "parquetPath" "Parquet file has no data rows"
    let fields = fileIndex.DataFields
    if fields.Length = 0 then invalidArg "parquetPath" "Parquet file has no columns"
    let indexCol = resolveIndexColumn fields options
    let indexName = fields.[indexCol].Name
    let indexSource = createDateTimeOffset fileIndex indexName :?> IVirtualVectorSource<DateTimeOffset>
    let valueColumnNames =
      fields
      |> Array.mapi (fun i f -> i, f.Name)
      |> Array.filter (fun (i, _) -> i <> indexCol)
      |> Array.toList
    let keys =
      match options.ColumnKeys with
      | Some ks -> ks
      | None -> valueColumnNames |> List.map snd
    let lookupForColumn (name: string) =
      match options.SearchColumn with
      | Some (searchName, mode) when String.Equals(name, searchName, StringComparison.OrdinalIgnoreCase) -> Some mode
      | _ -> None
    let sources =
      keys
      |> List.map (fun name ->
          let colIdx = fileIndex.FieldIndex name
          let kind = columnKind fields colIdx
          createTypedColumn fileIndex name kind (lookupForColumn name))
    Virtual.CreateFrame(indexSource, keys, sources)

/// Test-data helpers for B13 benchmarks (Parquet counterpart to `CsvTestData`).
module ParquetTestData =
  open Deedle.Parquet

  let defaultDatasetName = "b6-search-100k-random.parquet"

  let createFloatValueSeries (parquetPath: string) =
    use fileIndex = new ParquetFileIndex(parquetPath)
    let data = fileIndex.ReadFloatColumn "Value"
    let src =
      OrdinalVirtualSource(fileIndex.Length, (fun row -> data.[int row]), "parquet-file")
      :> IVirtualVectorSource<float>
    Virtual.CreateOrdinalSeries(src)

  let private parquetValueSumMatches (parquetPath: string) (expectedSum: float) (rowCount: int64) =
    try
      use idx = new ParquetFileIndex(parquetPath)
      if idx.Length <> rowCount then false
      else
        let values = idx.ReadFloatColumn "Value"
        let actual = Array.sum values
        abs (actual - expectedSum) < 1.0
    with _ -> false

  let private writeTypedSearchParquet (parquetPath: string) (csvPath: string) =
    // Stream CSV lines directly — avoid Frame.ReadCsv type inference issues.
    let idAcc = ResizeArray<string>()
    let tsAcc = ResizeArray<string>()
    let catAcc = ResizeArray<string>()
    let valAcc = ResizeArray<Nullable<float>>()
    use reader = new StreamReader(csvPath)
    reader.ReadLine() |> ignore // header
    let mutable line = reader.ReadLine()
    while not (isNull line) do
      let parts = line.Split(',')
      idAcc.Add(parts.[0])
      tsAcc.Add(parts.[1])
      catAcc.Add(parts.[2])
      valAcc.Add(Nullable(Double.Parse(parts.[3].TrimEnd('\r', '\n'), CultureInfo.InvariantCulture)))
      line <- reader.ReadLine()
    let schema = ParquetSchema([|
      DataField("Id", typeof<string>) :> Field
      DataField("Timestamp", typeof<string>) :> Field
      DataField("Category", typeof<string>) :> Field
      DataField("Value", typeof<Nullable<float>>) :> Field |])
    let dataFields = schema.GetDataFields()
    if File.Exists parquetPath then File.Delete parquetPath
    use stream = File.Create parquetPath
    use writer = global.Parquet.ParquetWriter.CreateAsync(schema, stream).GetAwaiter().GetResult()
    use rg = writer.CreateRowGroup()
    rg.WriteColumnAsync(DataColumn(dataFields.[0], idAcc.ToArray())).GetAwaiter().GetResult()
    rg.WriteColumnAsync(DataColumn(dataFields.[1], tsAcc.ToArray())).GetAwaiter().GetResult()
    rg.WriteColumnAsync(DataColumn(dataFields.[2], catAcc.ToArray())).GetAwaiter().GetResult()
    rg.WriteColumnAsync(DataColumn(dataFields.[3], valAcc.ToArray())).GetAwaiter().GetResult()

  let ensureSearchParquet (parquetPath: string) (rowCount: int64) =
    let csvPath = Path.ChangeExtension(parquetPath, ".csv")
    Deedle.Virtual.Sources.CsvTestData.ensureSearchCsv csvPath rowCount |> ignore
    let expectedSum = Deedle.Virtual.Sources.CsvTestData.readMeta(csvPath).ValueSum
    if parquetValueSumMatches parquetPath expectedSum rowCount then parquetPath
    else
      writeTypedSearchParquet parquetPath csvPath
      if not (parquetValueSumMatches parquetPath expectedSum rowCount) then
        failwithf
          "ParquetTestData: regenerated '%s' but Value sum still mismatches CSV meta (expected ~%g)"
          parquetPath expectedSum
      parquetPath

[<AutoOpen>]
module VirtualParquetExtensions =
  type Deedle.Virtual.Virtual with
    /// Load a Parquet file as a virtual frame with an ordered row index.
    static member ReadParquet(path: string, ?indexColumn: string, ?searchColumn: string, ?searchLookupRange: LookupRangeMode<string>, ?columnKeys: string list) =
      let searchCol =
        match searchColumn with
        | None -> None
        | Some name -> Some(name, defaultArg searchLookupRange LookupRangeUnsupported)
      let options : VirtualReadParquetOptions =
        { IndexColumn = indexColumn
          SearchColumn = searchCol
          ColumnKeys = columnKeys }
      ParquetVirtualSource.createFrame path options
