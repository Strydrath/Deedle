namespace Deedle.Virtual.Sources

open System
open System.Globalization
open System.IO
open System.Text
open FSharp.Data
open Deedle
open Deedle.Vectors.Virtual
open Deedle.Virtual

module internal CsvParsing =
  /// RFC4180-ish CSV field split (quotes, escaped quotes). Does not handle embedded newlines.
  let splitCsvLine (line: string) =
    let acc = ResizeArray<string>()
    let sb = StringBuilder()
    let mutable i = 0
    let mutable inQuotes = false
    while i < line.Length do
      let c = line.[i]
      if inQuotes then
        if c = '"' then
          if i + 1 < line.Length && line.[i + 1] = '"' then
            sb.Append('"') |> ignore
            i <- i + 2
          else
            inQuotes <- false
            i <- i + 1
        else
          sb.Append(c) |> ignore
          i <- i + 1
      else
        match c with
        | '"' ->
            inQuotes <- true
            i <- i + 1
        | ',' ->
            acc.Add(sb.ToString())
            sb.Clear() |> ignore
            i <- i + 1
        | _ ->
            sb.Append(c) |> ignore
            i <- i + 1
    acc.Add(sb.ToString().TrimEnd('\r', '\n'))
    acc.ToArray()

  let field (fields: string[]) (columnIndex: int) =
    if columnIndex >= fields.Length then
      failwithf "CsvVirtualSource: column %d missing (fields=%d)" columnIndex fields.Length
    fields.[columnIndex].TrimEnd('\r', '\n')

  let isMissingCell (s: string) =
    let t = s.Trim()
    String.IsNullOrEmpty t ||
    Array.exists (fun m -> String.Equals(t, m, StringComparison.OrdinalIgnoreCase)) TextConversions.DefaultMissingValues

  let tryParseInt64 (s: string) =
    match Int64.TryParse(s.Trim(), NumberStyles.Integer, CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | false, _ -> None

  let tryParseFloat (s: string) =
    match Double.TryParse(s.Trim(), NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | false, _ -> None

  let tryParseDateTime (s: string) =
    match DateTimeOffset.TryParse(s.Trim(), CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind) with
    | true, dto -> Some dto
    | false, _ -> None

  let parseDateTimeStrict (s: string) =
    DateTimeOffset.Parse(s.Trim(), CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind)

  let columnIndex (header: string[]) (name: string) =
    match header |> Array.tryFindIndex (fun h -> String.Equals(h, name, StringComparison.OrdinalIgnoreCase)) with
    | Some idx -> idx
    | None -> failwithf "CsvVirtualSource: column '%s' not found in header" name

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
  let fieldCache : string[][] = Array.create lines.Length null
  let cacheLock = obj()
  let mutable splitCount = 0

  member _.Path = path
  member _.Length = int64 lines.Length
  /// Number of CSV rows split since construction or last [`ResetSplitCount`].
  member _.SplitCount = splitCount
  /// Reset [`SplitCount`] (for tests and diagnostics).
  member _.ResetSplitCount() = splitCount <- 0

  member _.ReadFields(row: int64) =
    let i = int row
    if i < 0 || i >= fieldCache.Length then
      invalidArg "row" (sprintf "CsvLineIndex: row %d out of range [0, %d)" row fieldCache.Length)
    match fieldCache.[i] with
    | null ->
        lock cacheLock (fun () ->
          match fieldCache.[i] with
          | null ->
              splitCount <- splitCount + 1
              let fields = CsvParsing.splitCsvLine lines.[i]
              fieldCache.[i] <- fields
              fields
          | fields -> fields)
    | fields -> fields

  member _.HeaderColumns =
    use reader = new StreamReader(path)
    match reader.ReadLine() with
    | null -> [||]
    | line -> CsvParsing.splitCsvLine line |> Array.map (fun s -> s.Trim())

module CsvVirtualSource =
  open CsvParsing

  let private looksLikeDateTime (s: string) =
    s.IndexOf('-') >= 0 || s.IndexOf('/') >= 0 || s.IndexOf('T') >= 0

  let private inferColumnKind (index: CsvLineIndex) (columnIndex: int) (sampleRows: int) =
    if index.Length = 0L then "string"
    else
      let sampleCount = min sampleRows (int index.Length)
      let samples =
        [ for row in 0 .. sampleCount - 1 ->
            field (index.ReadFields(int64 row)) columnIndex ]
        |> List.filter (not << isMissingCell)
      if samples.IsEmpty then "string"
      // Prefer numerics over DateTimeOffset.TryParse, which accepts bare integers like "1".
      elif List.forall (tryParseInt64 >> Option.isSome) samples then "int64"
      elif List.forall (tryParseFloat >> Option.isSome) samples then "float"
      elif List.forall (fun s -> Option.isSome (tryParseDateTime s) && looksLikeDateTime s) samples then "datetime"
      else "string"

  let private createOptionalColumn (lineIndex: CsvLineIndex) columnIndex (tryParse: string -> 'T option)
      (asLong: ('T -> int64) option) (lookupRange: LookupRangeMode<'T> option) =
    let valueAt row =
      let s = field (lineIndex.ReadFields row) columnIndex
      if isMissingCell s then OptionalValue.Missing
      else
        match tryParse s with
        | Some v -> OptionalValue(v)
        | None -> OptionalValue.Missing
    OrdinalVirtualSource(lineIndex.Length, valueAt, "csv-file", ?asLong=asLong, ?lookupRange=lookupRange) :> IVirtualVectorSource

  /// Index columns stay strict: empty/invalid index cells throw (row keys must be present).
  let private createStrictColumn (lineIndex: CsvLineIndex) columnIndex (parse: string -> 'T)
      (asLong: ('T -> int64) option) =
    let valueAt row =
      let s = field (lineIndex.ReadFields row) columnIndex
      OptionalValue(parse s)
    OrdinalVirtualSource(lineIndex.Length, valueAt, "csv-file", ?asLong=asLong) :> IVirtualVectorSource

  let private maxInferredSearchCardinality = 64

  let private stringValueAt (lineIndex: CsvLineIndex) (columnIndex: int) =
    fun (row: int64) ->
      let s = field (lineIndex.ReadFields row) columnIndex
      if isMissingCell s then "" else s.Trim()

  /// Infer LookupRange for low-cardinality string columns (B14).
  let internal tryInferStringLookupRange (lineIndex: CsvLineIndex) (columnIndex: int) (columnName: string) =
    let length = lineIndex.Length
    if length = 0L then None
    else
      let valueAt = stringValueAt lineIndex columnIndex
      let values = [| for i in 0L .. length - 1L -> valueAt i |]
      let distinct =
        values |> Array.filter ((<>) "") |> Array.distinct
      if distinct.Length = 0 || distinct.Length > maxInferredSearchCardinality then None
      else
        let period = distinct.Length
        let isRepeatingCycle =
          values
          |> Array.mapi (fun i v -> v = "" || v = distinct.[i % period])
          |> Array.forall id
        if isRepeatingCycle then
          Some(VirtualLookupRange.forRepeatingCycle distinct, sprintf "repeating cycle (period %d)" period)
        else
          Some(
            VirtualLookupRange.forCategoricalScan length valueAt,
            sprintf "categorical IndexList (%d distinct; one-time O(N) scan per filter value)" distinct.Length)

  let private resolveSearchLookupRange (lineIndex: CsvLineIndex) (_header: string[]) (colIdx: int) (name: string) (kind: string) (options: VirtualReadCsvOptions) =
    match options.SearchColumn with
    | Some (searchName, LookupRangeUnsupported) when String.Equals(name, searchName, StringComparison.OrdinalIgnoreCase) && kind = "string" ->
        match tryInferStringLookupRange lineIndex colIdx name with
        | Some (mode, desc) ->
            System.Diagnostics.Trace.WriteLine(
              sprintf "Deedle.Virtual.ReadCsv: inferred %s LookupRange for search column '%s'." desc name)
            Some mode
        | None ->
            System.Diagnostics.Trace.WriteLine(
              sprintf "Deedle.Virtual.ReadCsv: search column '%s' has high cardinality; configure searchLookupRange explicitly (e.g. VirtualLookupRange.scan)." name)
            None
    | Some (searchName, mode) when String.Equals(name, searchName, StringComparison.OrdinalIgnoreCase) ->
        Some mode
    | _ -> None

  let private createTypedColumn (lineIndex: CsvLineIndex) (columnIndex: int) (kind: string) lookupRange =
    match kind with
    | "datetime" ->
      createOptionalColumn lineIndex columnIndex tryParseDateTime (Some (fun dto -> dto.UtcTicks)) None
    | "int64" ->
      createOptionalColumn lineIndex columnIndex tryParseInt64 (Some id) None
    | "float" ->
      createOptionalColumn lineIndex columnIndex tryParseFloat None None
    | _ ->
      createOptionalColumn lineIndex columnIndex (fun s -> Some s) None lookupRange

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
      createStrictColumn lineIndex indexCol parseDateTimeStrict (Some (fun dto -> dto.UtcTicks))
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
    let lookupForColumn (name: string) (colIdx: int) (kind: string) =
      resolveSearchLookupRange lineIndex header colIdx name kind options
    let sources =
      keys
      |> List.map (fun name ->
          let colIdx = columnIndex header name
          let kind = inferColumnKind lineIndex colIdx 100
          createTypedColumn lineIndex colIdx kind (lookupForColumn name colIdx kind))
    Virtual.CreateFrame(indexSource, keys, sources)

  let createIndexSource (lineIndex: CsvLineIndex) (columnName: string) =
    let colIdx = columnIndex lineIndex.HeaderColumns columnName
    createStrictColumn lineIndex colIdx parseDateTimeStrict (Some (fun dto -> dto.UtcTicks))

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
        | Some name ->
            match searchLookupRange with
            | Some mode -> Some(name, mode)
            | None ->
                // Defer inference to createFrame (needs file content).
                Some(name, LookupRangeUnsupported)
      let options : VirtualReadCsvOptions =
        { IndexColumn = indexColumn
          SearchColumn = searchCol
          ColumnKeys = columnKeys }
      CsvVirtualSource.createFrame path options
