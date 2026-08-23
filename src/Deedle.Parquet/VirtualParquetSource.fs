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

/// Column CLR kinds aligned with [`Implementation.netTypeToDataField`] / `readColumn`.
[<RequireQualifiedAccess>]
type internal ParquetColumnKind =
  | Float | Float32 | Int | Int64 | Int16 | Byte
  | UInt16 | UInt32 | UInt64 | Bool | String | DateTime | DateTimeOffset

module private OptionalArrays =
  let sumPresent (values: OptionalValue<float>[]) =
    let mutable s = 0.0
    for ov in values do
      if ov.HasValue && not (Double.IsNaN ov.Value) then s <- s + ov.Value
    s

  let mapPresent (f: obj -> 'T) (values: OptionalValue<obj>[]) : OptionalValue<'T>[] =
    values |> Array.map (fun ov ->
      if ov.HasValue then OptionalValue(f ov.Value) else OptionalValue.Missing)

/// Shared Parquet file handle: schema, row count, and lazily loaded column arrays.
/// Column sources capture this instance so the file stays open for the virtual frame lifetime;
/// dispose explicitly only for short-lived validation helpers.
type ParquetFileIndex(path: string) =
  let stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read)
  let reader = global.Parquet.ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
  let dataFields = reader.Schema.GetDataFields()
  let mutable disposed = false
  // Prefer metadata NumRows — never ReadEntireRowGroup just to count.
  let rowCount =
    if reader.Metadata <> null && reader.Metadata.NumRows > 0L then reader.Metadata.NumRows
    elif reader.RowGroupCount = 0 then 0L
    else
      let mutable total = 0L
      for rgIdx in 0 .. reader.RowGroupCount - 1 do
        use rgReader = reader.OpenRowGroupReader(rgIdx)
        total <- total + int64 rgReader.RowCount
      total
  let columnCache = ConcurrentDictionary<string, obj>()

  member _.Path = path
  member _.Length = rowCount
  member _.DataFields = dataFields

  member _.FieldIndex(name: string) =
    match dataFields |> Array.tryFindIndex (fun f -> String.Equals(f.Name, name, StringComparison.OrdinalIgnoreCase)) with
    | Some idx -> idx
    | None -> failwithf "VirtualParquetSource: column '%s' not found" name

  /// Read only the named column from each row group (not the entire row group).
  member private this.ReadColumn (name: string) =
    if disposed then invalidOp "ParquetFileIndex: disposed"
    let field = dataFields.[this.FieldIndex name]
    [| for rgIdx in 0 .. reader.RowGroupCount - 1 do
         use rgReader = reader.OpenRowGroupReader(rgIdx)
         yield rgReader.ReadColumnAsync(field).GetAwaiter().GetResult() |]

  /// Boxed optional cells via [`Implementation.readColumn`] + `IVector.ObjectSequence`.
  member private this.ReadColumnValues (name: string) =
    let values = ResizeArray<OptionalValue<obj>>()
    for col in this.ReadColumn name do
      let (_, vec) = Implementation.readColumn col
      for ov in vec.ObjectSequence do
        values.Add(ov)
    values.ToArray()

  /// Cache typed column arrays. `cacheKey` distinguishes conversions of the same field
  /// (e.g. DateTime vs DateTimeOffset for the index).
  member private this.Materialize (cacheKey: string) (build: unit -> obj) =
    columnCache.GetOrAdd(cacheKey, fun _ -> build())

  /// Exact CLR type from `readColumn` (no widening/narrowing).
  member this.ReadTypedColumn<'T>(name: string) =
    this.Materialize (name + "#:" + typeof<'T>.FullName) (fun () ->
      box (OptionalArrays.mapPresent unbox<'T> (this.ReadColumnValues name)))
    :?> OptionalValue<'T>[]

  /// Float column (nulls → Missing). Kept as a named alias for benchmarks/tests.
  member this.ReadFloatColumn(name: string) =
    this.ReadTypedColumn<float>(name)

  /// Index helper: accepts DateTime or DateTimeOffset cells.
  member this.ReadDateTimeOffsetColumn(name: string) =
    this.Materialize (name + "#:DateTimeOffset") (fun () ->
      box (OptionalArrays.mapPresent (fun v ->
        match v with
        | :? DateTimeOffset as dto -> dto
        | :? DateTime as dt -> DateTimeOffset(dt)
        | _ -> unbox<DateTimeOffset> v) (this.ReadColumnValues name)))
    :?> OptionalValue<DateTimeOffset>[]

  interface IDisposable with
    member _.Dispose() =
      if not disposed then
        disposed <- true
        (reader :> IDisposable).Dispose()
        stream.Dispose()

module internal ParquetColumnSource =
  /// Capture `index` in the value closure so the file handle outlives frame construction.
  let private optionalSource
      (index: ParquetFileIndex)
      (data: OptionalValue<'T>[])
      (asLong: ('T -> int64) option)
      (lookupRange: LookupRangeMode<'T>) =
    OrdinalVirtualSource(
      index.Length,
      (fun row ->
        GC.KeepAlive(index)
        data.[int row]),
      "parquet-file",
      ?asLong=asLong, lookupRange=lookupRange)
    :> IVirtualVectorSource

  let private typed<'T> (index: ParquetFileIndex) (name: string) (asLong: ('T -> int64) option) (lookupRange: LookupRangeMode<'T>) =
    optionalSource index (index.ReadTypedColumn<'T>(name)) asLong lookupRange

  let createFloat (index: ParquetFileIndex) (name: string) (lookupRange: LookupRangeMode<float> option) =
    typed<float> index name None (defaultArg lookupRange LookupRangeUnsupported)

  let createFloat32 (index: ParquetFileIndex) (name: string) =
    typed<float32> index name None LookupRangeUnsupported

  let createInt (index: ParquetFileIndex) (name: string) =
    typed<int> index name (Some int64) LookupRangeUnsupported

  let createInt64 (index: ParquetFileIndex) (name: string) (lookupRange: LookupRangeMode<int64> option) =
    typed<int64> index name (Some id) (defaultArg lookupRange LookupRangeUnsupported)

  let createInt16 (index: ParquetFileIndex) (name: string) =
    typed<int16> index name (Some int64) LookupRangeUnsupported

  let createByte (index: ParquetFileIndex) (name: string) =
    typed<byte> index name (Some int64) LookupRangeUnsupported

  let createUInt16 (index: ParquetFileIndex) (name: string) =
    typed<uint16> index name (Some int64) LookupRangeUnsupported

  let createUInt32 (index: ParquetFileIndex) (name: string) =
    typed<uint32> index name (Some int64) LookupRangeUnsupported

  let createUInt64 (index: ParquetFileIndex) (name: string) =
    // Full uint64 range does not fit int64; LookupValue is unsupported for this column.
    typed<uint64> index name None LookupRangeUnsupported

  let createBool (index: ParquetFileIndex) (name: string) =
    typed<bool> index name None LookupRangeUnsupported

  let createString (index: ParquetFileIndex) (name: string) (lookupRange: LookupRangeMode<string> option) =
    typed<string> index name None (defaultArg lookupRange LookupRangeUnsupported)

  let createDateTime (index: ParquetFileIndex) (name: string) =
    typed<DateTime> index name (Some (fun (dt: DateTime) -> DateTimeOffset(dt).UtcTicks)) LookupRangeUnsupported

  let createDateTimeOffset (index: ParquetFileIndex) (name: string) =
    let data = index.ReadDateTimeOffsetColumn name
    optionalSource index data (Some (fun dto -> dto.UtcTicks)) LookupRangeUnsupported

  let resolveIndexColumn (fields: DataField[]) (options: VirtualReadParquetOptions) =
    match options.IndexColumn with
    | Some name ->
      match fields |> Array.tryFindIndex (fun f -> String.Equals(f.Name, name, StringComparison.OrdinalIgnoreCase)) with
      | Some idx -> idx
      | None -> failwithf "VirtualParquetSource: index column '%s' not found" name
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

  let columnKind (field: DataField) =
    let clrType = field.ClrType
    let baseType =
      match Nullable.GetUnderlyingType clrType with
      | null -> clrType
      | ut -> ut
    if baseType = typeof<float> then ParquetColumnKind.Float
    elif baseType = typeof<float32> then ParquetColumnKind.Float32
    elif baseType = typeof<double> then ParquetColumnKind.Float
    elif baseType = typeof<int> then ParquetColumnKind.Int
    elif baseType = typeof<int64> then ParquetColumnKind.Int64
    elif baseType = typeof<int16> then ParquetColumnKind.Int16
    elif baseType = typeof<byte> then ParquetColumnKind.Byte
    elif baseType = typeof<uint16> then ParquetColumnKind.UInt16
    elif baseType = typeof<uint32> then ParquetColumnKind.UInt32
    elif baseType = typeof<uint64> then ParquetColumnKind.UInt64
    elif baseType = typeof<bool> then ParquetColumnKind.Bool
    elif baseType = typeof<string> then ParquetColumnKind.String
    elif baseType = typeof<DateTime> then ParquetColumnKind.DateTime
    elif baseType = typeof<DateTimeOffset> then ParquetColumnKind.DateTimeOffset
    else ParquetColumnKind.String

module VirtualParquetSource =
  open ParquetColumnSource

  let private createTypedColumn (index: ParquetFileIndex) (name: string) (kind: ParquetColumnKind) (lookupRange: LookupRangeMode<string> option) =
    match kind with
    | ParquetColumnKind.Float -> createFloat index name None
    | ParquetColumnKind.Float32 -> createFloat32 index name
    | ParquetColumnKind.Int -> createInt index name
    | ParquetColumnKind.Int64 -> createInt64 index name None
    | ParquetColumnKind.Int16 -> createInt16 index name
    | ParquetColumnKind.Byte -> createByte index name
    | ParquetColumnKind.UInt16 -> createUInt16 index name
    | ParquetColumnKind.UInt32 -> createUInt32 index name
    | ParquetColumnKind.UInt64 -> createUInt64 index name
    | ParquetColumnKind.Bool -> createBool index name
    | ParquetColumnKind.String -> createString index name lookupRange
    | ParquetColumnKind.DateTime -> createDateTime index name
    | ParquetColumnKind.DateTimeOffset -> createDateTimeOffset index name

  let createFrame (parquetPath: string) (options: VirtualReadParquetOptions) =
    if not (File.Exists parquetPath) then failwithf "VirtualParquetSource: file not found '%s'" parquetPath
    // Do not dispose: column sources keep `fileIndex` alive for the frame lifetime.
    let fileIndex = new ParquetFileIndex(parquetPath)
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
    let lookupForColumn (name: string) (kind: ParquetColumnKind) =
      VirtualLookupRange.resolveSearchColumnLookupRange
        "Deedle.Virtual.ReadParquet"
        options.SearchColumn
        name
        (kind = ParquetColumnKind.String)
        (fun () ->
          let data = fileIndex.ReadTypedColumn<string>(name)
          let valueAt row =
            let ov = data.[int row]
            if ov.HasValue then ov.Value else ""
          VirtualLookupRange.tryInferStringLookupRange fileIndex.Length valueAt)
    let sources =
      keys
      |> List.map (fun name ->
          let colIdx = fileIndex.FieldIndex name
          let kind = columnKind fields.[colIdx]
          createTypedColumn fileIndex name kind (lookupForColumn name kind))
    Virtual.CreateFrame(indexSource, keys, sources)

/// Test-data helpers for Parquet virtual benchmarks (counterpart to `CsvTestData`).
module ParquetTestData =
  open Deedle.Parquet

  let defaultDatasetName = "b6-search-100k-random.parquet"

  let createFloatValueSeries (parquetPath: string) =
    // Keep index alive via the value closure (same lifetime rule as Virtual.ReadParquet).
    let fileIndex = new ParquetFileIndex(parquetPath)
    let data = fileIndex.ReadFloatColumn "Value"
    let src =
      OrdinalVirtualSource(
        fileIndex.Length,
        (fun row ->
          GC.KeepAlive(fileIndex)
          data.[int row]),
        "parquet-file")
      :> IVirtualVectorSource<float>
    Virtual.CreateOrdinalSeries(src)

  let private parquetValueSumMatches (parquetPath: string) (expectedSum: float) (rowCount: int64) =
    try
      use idx = new ParquetFileIndex(parquetPath)
      if idx.Length <> rowCount then false
      else
        let actual = OptionalArrays.sumPresent (idx.ReadFloatColumn "Value")
        abs (actual - expectedSum) < 1.0
    with _ -> false

  let private writeTypedSearchParquet (parquetPath: string) (csvPath: string) =
    // Stream CSV fields into typed Parquet columns (schema CLR types drive Virtual.ReadParquet).
    // Parquet.Net rejects DateTimeOffset fields — store UTC DateTime and convert on read.
    // Use CsvLineIndex so quoted commas match Virtual.ReadCsv parsing.
    let idAcc = ResizeArray<Nullable<int64>>()
    let tsAcc = ResizeArray<Nullable<DateTime>>()
    let catAcc = ResizeArray<string>()
    let valAcc = ResizeArray<Nullable<float>>()
    let lineIndex = Deedle.Virtual.Sources.CsvLineIndex(csvPath)
    for row in 0L .. lineIndex.Length - 1L do
      let parts = lineIndex.ReadFields(row)
      idAcc.Add(Nullable(Int64.Parse(parts.[0], CultureInfo.InvariantCulture)))
      let dto =
        DateTimeOffset.Parse(parts.[1], CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind)
      tsAcc.Add(Nullable(dto.UtcDateTime))
      catAcc.Add(parts.[2])
      valAcc.Add(Nullable(Double.Parse(parts.[3], CultureInfo.InvariantCulture)))
    let schema = ParquetSchema([|
      DataField("Id", typeof<Nullable<int64>>) :> Field
      DataField("Timestamp", typeof<Nullable<DateTime>>) :> Field
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
    /// Requested columns are read into memory and cached; the underlying file handle
    /// stays reachable for the lifetime of the returned frame.
    /// Column CLR types match [`Frame.readParquet`] / `Implementation.readColumn`.
    static member ReadParquet(path: string, ?indexColumn: string, ?searchColumn: string, ?searchLookupRange: LookupRangeMode<string>, ?columnKeys: string list) =
      let searchCol =
        match searchColumn with
        | None -> None
        | Some name -> Some(name, defaultArg searchLookupRange LookupRangeUnsupported)
      let options : VirtualReadParquetOptions =
        { IndexColumn = indexColumn
          SearchColumn = searchCol
          ColumnKeys = columnKeys }
      VirtualParquetSource.createFrame path options
