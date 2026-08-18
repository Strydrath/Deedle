namespace Deedle.Virtual.Sources

open System
open System.Globalization
open System.IO
open Deedle
open Deedle.Vectors.Virtual
open Deedle.Virtual

/// Shared line index for one CSV file (built once, reused by column sources).
type CsvLineIndex(path: string, ?skipHeader: bool) =
  let skipHeader = defaultArg skipHeader true
  let lines =
    use reader = new StreamReader(path)
    if skipHeader then reader.ReadLine() |> ignore
    let acc = ResizeArray<string>()
    while not reader.EndOfStream do
      acc.Add(reader.ReadLine())
    acc.ToArray()

  member _.Path = path
  member _.Length = int64 lines.Length

  member _.ReadFields(row: int64) =
    lines.[int row].Split(',') |> Array.map (fun s -> s.TrimEnd('\r', '\n'))

  member _.HeaderColumns =
    use reader = new StreamReader(path)
    match reader.ReadLine() with
    | null -> [||]
    | line -> line.Split(',') |> Array.map (fun s -> s.Trim())

module internal CsvParsing =
  let field (fields: string[]) (columnIndex: int) =
    if columnIndex >= fields.Length then
      failwithf "CsvVirtualSource: column %d missing (fields=%d)" columnIndex fields.Length
    fields.[columnIndex].TrimEnd('\r', '\n')

  let parseInt (s: string) = Int32.Parse(s, CultureInfo.InvariantCulture)
  let parseInt64 (s: string) = Int64.Parse(s, CultureInfo.InvariantCulture)
  let parseFloat (s: string) = Double.Parse(s, CultureInfo.InvariantCulture)
  let parseString (s: string) = s

  let parseDateTime (s: string) =
    DateTimeOffset.Parse(s, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind)

  let tryParseDateTime (s: string) =
    match DateTimeOffset.TryParse(s, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind) with
    | true, dto -> Some dto
    | false, _ -> None

  let columnIndex (header: string[]) (name: string) =
    match header |> Array.tryFindIndex (fun h -> String.Equals(h, name, StringComparison.OrdinalIgnoreCase)) with
    | Some idx -> idx
    | None -> failwithf "CsvVirtualSource: column '%s' not found in header" name

  let private tryInt64 (s: string) =
    match Int64.TryParse(s, NumberStyles.Integer, CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | false, _ -> None

  let private tryFloat (s: string) =
    match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | false, _ -> None

  let inferColumnKind (index: CsvLineIndex) (columnIndex: int) (sampleRows: int) =
    if index.Length = 0L then "string"
    else
      let sampleCount = min sampleRows (int index.Length)
      let samples =
        [ for row in 0 .. sampleCount - 1 ->
            field (index.ReadFields(int64 row)) columnIndex ]
      if List.forall (tryParseDateTime >> Option.isSome) samples then "datetime"
      elif List.forall (tryInt64 >> Option.isSome) samples then "int64"
      elif List.forall (tryFloat >> Option.isSome) samples then "float"
      else "string"

module CsvVirtualSource =
  open CsvParsing

  let private createColumn (lineIndex: CsvLineIndex) columnIndex parse
      (asLong: ('T -> int64) option) (lookupRange: LookupRangeMode<'T> option) =
    let valueAt row =
      let fields = lineIndex.ReadFields row
      field fields columnIndex |> parse
    OrdinalVirtualSource(lineIndex.Length, valueAt, "csv-file", ?asLong=asLong, ?lookupRange=lookupRange) :> IVirtualVectorSource

  let internal createTypedColumn (lineIndex: CsvLineIndex) (columnIndex: int) (kind: string) lookupRange =
    match kind with
    | "datetime" ->
      createColumn lineIndex columnIndex parseDateTime (Some (fun dto -> dto.UtcTicks)) None
    | "int64" ->
      createColumn lineIndex columnIndex (parseInt64 >> id) (Some id) None
    | "float" ->
      createColumn lineIndex columnIndex parseFloat None None
    | _ ->
      createColumn lineIndex columnIndex parseString None lookupRange

  let resolveIndexColumn (header: string[]) (options: VirtualReadCsvOptions) =
    match options.IndexColumn with
    | Some name -> columnIndex header name
    | None ->
      let preferred = header |> Array.tryFindIndex (fun h ->
        String.Equals(h, "Timestamp", StringComparison.OrdinalIgnoreCase)
        || String.Equals(h, "DateTime", StringComparison.OrdinalIgnoreCase)
        || h.EndsWith("Time", StringComparison.OrdinalIgnoreCase))
      match preferred with
      | Some i -> i
      | None -> 0

  /// Build a virtual frame from an indexed CSV file.
  let createFrame (csvPath: string) (options: VirtualReadCsvOptions) =
    if not (File.Exists csvPath) then failwithf "CsvVirtualSource: file not found '%s'" csvPath
    let lineIndex = CsvLineIndex(csvPath)
    if lineIndex.Length = 0L then invalidArg "csvPath" "CSV has no data rows"
    let header = lineIndex.HeaderColumns
    if header.Length = 0 then invalidArg "csvPath" "CSV has no header row"
    let indexCol = resolveIndexColumn header options
    let indexSource =
      createColumn lineIndex indexCol parseDateTime (Some (fun dto -> dto.UtcTicks)) None
      :?> IVirtualVectorSource<DateTimeOffset>
    let valueColumnIndices =
      header
      |> Array.mapi (fun i name -> i, name)
      |> Array.filter (fun (i, _) -> i <> indexCol)
      |> Array.toList
    let keys =
      match options.ColumnKeys with
      | Some ks -> ks
      | None -> valueColumnIndices |> List.map snd
    let lookupForColumn (name: string) =
      match options.SearchColumn with
      | Some (searchName, mode) when String.Equals(name, searchName, StringComparison.OrdinalIgnoreCase) -> Some mode
      | _ -> None
    let sources =
      keys
      |> List.map (fun name ->
          let colIdx = columnIndex header name
          let kind = inferColumnKind lineIndex colIdx 100
          createTypedColumn lineIndex colIdx kind (lookupForColumn name))
    Virtual.CreateFrame(indexSource, keys, sources)

  let createIndexSource (lineIndex: CsvLineIndex) (columnName: string) =
    let colIdx = columnIndex lineIndex.HeaderColumns columnName
    createColumn lineIndex colIdx parseDateTime (Some (fun dto -> dto.UtcTicks)) None

  /// Create a value column source for a CSV file (type inferred from sample rows).
  let createColumnSource (lineIndex: CsvLineIndex) (columnName: string) (lookupRange: LookupRangeMode<string> option) =
    let colIdx = columnIndex lineIndex.HeaderColumns columnName
    let kind = inferColumnKind lineIndex colIdx 100
    createTypedColumn lineIndex colIdx kind lookupRange

  /// Resolve index column using the same rules as [`createFrame`].
  let resolveIndexColumnName (header: string[]) (options: VirtualReadCsvOptions) =
    header.[resolveIndexColumn header options]

/// Test-data helpers (also used by benchmarks). Not required for reading arbitrary CSVs.
module CsvTestData =
  let words8 =
    "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')

  let defaultDatasetName = "b6-search-100k-random.csv"
  let defaultSeed = 42
  let profileVersion = "random-v1"

  type CsvDatasetMeta =
    { Version: string
      Seed: int
      RowCount: int64
      ValueSum: float }

  let metaPath (csvPath: string) = csvPath + ".meta"

  let private writeMeta (csvPath: string) (meta: CsvDatasetMeta) =
    use writer = new StreamWriter(metaPath csvPath, false)
    writer.WriteLine(sprintf "version=%s" meta.Version)
    writer.WriteLine(sprintf "seed=%d" meta.Seed)
    writer.WriteLine(sprintf "rows=%d" meta.RowCount)
    writer.WriteLine(sprintf "valueSum=%s" (meta.ValueSum.ToString("R", CultureInfo.InvariantCulture)))

  let readMeta (csvPath: string) =
    let lines = File.ReadAllLines(metaPath csvPath)
    let lookup key =
      lines
      |> Array.tryFind (fun line -> line.StartsWith(key + "=", StringComparison.Ordinal))
      |> Option.map (fun line -> line.Substring(key.Length + 1))
      |> Option.defaultWith (fun () -> failwithf "CsvTestData meta missing key '%s'" key)
    { Version = lookup "version"
      Seed = Int32.Parse(lookup "seed", CultureInfo.InvariantCulture)
      RowCount = Int64.Parse(lookup "rows", CultureInfo.InvariantCulture)
      ValueSum = Double.Parse(lookup "valueSum", CultureInfo.InvariantCulture) }

  let private shuffleInPlace (rng: Random) (items: int[]) =
    for i in items.Length - 1 .. -1 .. 0 do
      let j = rng.Next(i + 1)
      let tmp = items.[i]
      items.[i] <- items.[j]
      items.[j] <- tmp

  let generateSearchCsv (path: string) (rowCount: int64) (seed: int) =
    let dir = Path.GetDirectoryName(path)
    if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
      Directory.CreateDirectory dir |> ignore
    let rng = Random(seed)
    let ids = Array.init (int rowCount) id
    shuffleInPlace rng ids
    let mutable valueSum = 0.0
    use writer = new StreamWriter(path, false)
    writer.WriteLine("Id,Timestamp,Category,Value")
    let start = DateTimeOffset(DateTime(2000, 1, 1), TimeSpan.Zero)
    for i in 0L .. rowCount - 1L do
      let id = ids.[int i]
      let cat = words8.[int (i % int64 words8.Length)]
      let ts = start.AddSeconds(float i).ToString("o", CultureInfo.InvariantCulture)
      let value = rng.NextDouble() * 10000.0
      let valueStr = value.ToString("F4", CultureInfo.InvariantCulture)
      valueSum <- valueSum + Double.Parse(valueStr, CultureInfo.InvariantCulture)
      writer.WriteLine(sprintf "%d,%s,%s,%s" id ts cat valueStr)
    writeMeta path
      { Version = profileVersion
        Seed = seed
        RowCount = rowCount
        ValueSum = valueSum }
    path

  let ensureSearchCsvWithSeed (path: string) (rowCount: int64) (seed: int) =
    let valid =
      File.Exists path &&
      File.Exists (metaPath path) &&
      try
        let meta = readMeta path
        meta.Version = profileVersion &&
        meta.Seed = seed &&
        meta.RowCount = rowCount &&
        let idx = CsvLineIndex(path)
        idx.Length = rowCount && idx.ReadFields(0L).Length >= 4
      with _ -> false
    if valid then path
    else
      if File.Exists path then File.Delete path
      let metaFile = metaPath path
      if File.Exists metaFile then File.Delete metaFile
      generateSearchCsv path rowCount seed

  let ensureSearchCsv (path: string) (rowCount: int64) =
    ensureSearchCsvWithSeed path rowCount defaultSeed

  /// B6-compatible frame: Timestamp index, Id + searchable Category (8-word cycle Step LookupRange).
  let createB6SearchFrame (csvPath: string) =
    let options =
      { VirtualReadCsvOptions.Default with
          IndexColumn = Some "Timestamp"
          SearchColumn =
            Some("Category", VirtualLookupRange.forRepeatingCycle words8)
          ColumnKeys = Some [ "Id"; "Category" ] }
    CsvVirtualSource.createFrame csvPath options, words8

  let createFloatValueSeries (csvPath: string) =
    let lineIndex = CsvLineIndex(csvPath)
    let src =
      CsvVirtualSource.createColumnSource lineIndex "Value" None
      :?> IVirtualVectorSource<float>
    Virtual.CreateOrdinalSeries(src)

namespace Deedle.Virtual

open Deedle.Virtual.Sources

[<AutoOpen>]
module VirtualCsvExtensions =
  type Virtual with
    /// Load a CSV file as a virtual frame with an ordered row index.
    /// The index column defaults to `Timestamp` / `DateTime` / first date-like column when not specified.
    static member ReadCsv(path: string, ?indexColumn: string, ?searchColumn: string, ?searchLookupRange: LookupRangeMode<string>, ?columnKeys: string list) =
      let searchCol =
        match searchColumn with
        | None -> None
        | Some name -> Some(name, defaultArg searchLookupRange LookupRangeUnsupported)
      let options : VirtualReadCsvOptions =
        { IndexColumn = indexColumn
          SearchColumn = searchCol
          ColumnKeys = columnKeys }
      CsvVirtualSource.createFrame path options
