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

  /// Scan a UTF-8 CSV for physical line starts (CRLF/LF). Does not treat quoted embedded newlines as one record.
  let indexPhysicalLineOffsets (path: string) (skipHeader: bool) =
    use fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read)
    if fs.Length >= 3L then
      let bom = Array.zeroCreate 3
      fs.Read(bom, 0, 3) |> ignore
      if bom.[0] <> 0xEFuy || bom.[1] <> 0xBBuy || bom.[2] <> 0xBFuy then
        fs.Seek(0L, SeekOrigin.Begin) |> ignore
    let offs = ResizeArray<int64>()
    let mutable skip = skipHeader
    let mutable lineStart = fs.Position
    let rec consume () =
      let b = fs.ReadByte()
      if b < 0 then
        if not skip && fs.Position > lineStart then offs.Add(lineStart)
      elif b = 10 then
        if not skip then offs.Add(lineStart)
        skip <- false
        lineStart <- fs.Position
        consume ()
      else consume ()
    consume ()
    offs.ToArray()

  let readPhysicalLineAt (path: string) (offset: int64) =
    use fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read)
    fs.Seek(offset, SeekOrigin.Begin) |> ignore
    let buf = ResizeArray<byte>()
    let mutable b = fs.ReadByte()
    while b >= 0 && b <> 10 do
      if b <> 13 then buf.Add(byte b)
      b <- fs.ReadByte()
    Encoding.UTF8.GetString(buf.ToArray())

  let field (fields: string[]) (columnIndex: int) =
    if columnIndex >= fields.Length then
      invalidOp (sprintf "VirtualCsvSource: column %d missing (fields=%d)" columnIndex fields.Length)
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
    | None -> invalidArg "name" (sprintf "VirtualCsvSource: column '%s' not found in header" name)

/// Shared row index for one CSV file (built once, reused by column sources).
/// Default backend caches physical line text. [`byteOffset`] stores only start offsets and seeks on read.
type CsvLineIndex(path: string, ?skipHeader: bool, ?byteOffset: bool) =
  let skipHeader = defaultArg skipHeader true
  let byteOffset = defaultArg byteOffset false
  let lines, offsets =
    if byteOffset then
      [||], CsvParsing.indexPhysicalLineOffsets path skipHeader
    else
      use reader = new StreamReader(path)
      if skipHeader then reader.ReadLine() |> ignore
      let acc = ResizeArray<string>()
      while not reader.EndOfStream do
        acc.Add(reader.ReadLine())
      acc.ToArray(), [||]
  let rowCount = if byteOffset then offsets.Length else lines.Length
  let fieldCache : string[][] = Array.create rowCount null
  let cacheLock = obj()
  let mutable splitCount = 0

  member _.Path = path
  member _.Length = int64 rowCount
  member _.IsByteOffset = byteOffset
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
              let raw = if byteOffset then CsvParsing.readPhysicalLineAt path offsets.[i] else lines.[i]
              let fields = CsvParsing.splitCsvLine raw
              fieldCache.[i] <- fields
              fields
          | fields -> fields)
    | fields -> fields

  member _.HeaderColumns =
    use reader = new StreamReader(path)
    match reader.ReadLine() with
    | null -> [||]
    | line -> CsvParsing.splitCsvLine line |> Array.map (fun s -> s.Trim())

module VirtualCsvSource =
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
      match samples with
      | [] -> "string"
      // Prefer numerics over DateTimeOffset.TryParse, which accepts bare integers like "1".
      | _ when List.forall (tryParseInt64 >> Option.isSome) samples -> "int64"
      | _ when List.forall (tryParseFloat >> Option.isSome) samples -> "float"
      | _ when List.forall (fun s -> Option.isSome (tryParseDateTime s) && looksLikeDateTime s) samples -> "datetime"
      | _ -> "string"

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

  let private stringValueAt (lineIndex: CsvLineIndex) (columnIndex: int) =
    fun (row: int64) ->
      let s = field (lineIndex.ReadFields row) columnIndex
      if isMissingCell s then "" else s.Trim()

  let private resolveSearchLookupRange (lineIndex: CsvLineIndex) (_header: string[]) (colIdx: int) (name: string) (kind: string) (options: VirtualReadCsvOptions) =
    VirtualLookupRange.resolveSearchColumnLookupRange
      "Deedle.Virtual.ReadCsv"
      options.SearchColumn
      name
      (kind = "string")
      (fun () -> VirtualLookupRange.tryInferStringLookupRange lineIndex.Length (stringValueAt lineIndex colIdx))

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
    if not (File.Exists csvPath) then raise (FileNotFoundException(sprintf "VirtualCsvSource: file not found '%s'" csvPath, csvPath))
    let lineIndex = CsvLineIndex(csvPath, byteOffset=options.ByteOffsetIndex)
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

  /// Map a global ordinal row to (part index, row-in-part).
  let private locatePartRow (partSizes: int[]) (i: int64) =
    let rec loop part acc =
      let n = int64 partSizes.[part]
      if i < acc + n then part, i - acc
      else loop (part + 1) (acc + n)
    loop 0 0L

  /// Concatenate CSV files as one ordinal virtual frame (sorted paths). Shared schema required.
  /// Uses linear 0..N-1 addressing so existing [`OrdinalVirtualSource`] LookupRange applies as-is.
  let createConcatenatedFrame (csvPaths: string[]) (options: VirtualReadCsvOptions) =
    if csvPaths.Length = 0 then invalidArg "csvPaths" "At least one CSV file is required"
    let indexes =
      csvPaths |> Array.map (fun p ->
        if not (File.Exists p) then raise (FileNotFoundException(sprintf "VirtualCsvSource: file not found '%s'" p, p))
        CsvLineIndex(p, byteOffset=options.ByteOffsetIndex))
    if indexes |> Array.exists (fun i -> i.Length = 0L) then invalidArg "csvPaths" "CSV part has no data rows"
    let header = indexes.[0].HeaderColumns
    if header.Length = 0 then invalidArg "csvPaths" "CSV has no header row"
    for i in 1 .. indexes.Length - 1 do
      if indexes.[i].HeaderColumns <> header then
        invalidArg "csvPaths" (sprintf "VirtualCsvSource: schema mismatch in '%s'" csvPaths.[i])
    let partSizes = indexes |> Array.map (fun i -> int i.Length)
    let total = partSizes |> Array.sumBy int64
    let keys =
      match options.ColumnKeys with
      | Some ks -> ks
      | None -> Array.toList header
    let makeColumn name =
      let colIdx = columnIndex header name
      let kind = inferColumnKind indexes.[0] colIdx 100
      let lookup = resolveSearchLookupRange indexes.[0] header colIdx name kind options
      let cell i =
        let part, row = locatePartRow partSizes i
        field (indexes.[part].ReadFields row) colIdx
      let sourceOf parse asLong lookupRange =
        let valueAt i =
          let s = cell i
          if isMissingCell s then OptionalValue.Missing
          else match parse s with Some v -> OptionalValue(v) | None -> OptionalValue.Missing
        OrdinalVirtualSource(total, valueAt, "csv-file", ?asLong=asLong, ?lookupRange=lookupRange)
        :> IVirtualVectorSource
      match kind with
      | "datetime" -> sourceOf tryParseDateTime (Some (fun dto -> dto.UtcTicks)) None
      | "int64" -> sourceOf tryParseInt64 (Some id) None
      | "float" -> sourceOf tryParseFloat None None
      | _ -> sourceOf (fun s -> Some s) None lookup
    Virtual.CreateOrdinalFrame(keys, keys |> List.map makeColumn)

namespace Deedle.Virtual

open System.IO
open Deedle.Virtual.Sources

[<AutoOpen>]
module VirtualCsvExtensions =
  let private searchColumnOption searchColumn searchLookupRange =
    match searchColumn with
    | None -> None
    | Some name ->
        match searchLookupRange with
        | Some mode -> Some(name, mode)
        | None -> Some(name, LookupRangeUnsupported)

  type Virtual with
    /// Load a CSV file as a virtual frame with an ordered row index.
    /// The index column defaults to `Timestamp` / `DateTime` / first date-like column when not specified.
    static member ReadCsv(path: string, ?indexColumn: string, ?searchColumn: string, ?searchLookupRange: LookupRangeMode<string>, ?columnKeys: string list, ?byteOffsetIndex: bool) =
      let options : VirtualReadCsvOptions =
        { IndexColumn = indexColumn
          SearchColumn = searchColumnOption searchColumn searchLookupRange
          ColumnKeys = columnKeys
          ByteOffsetIndex = defaultArg byteOffsetIndex false }
      VirtualCsvSource.createFrame path options

    /// Load matching CSV files in a directory as one ordinal virtual frame (files sorted by name).
    /// Rows are addressed 0 .. N-1 across files; all files must share the first file's header.
    static member ReadCsvDirectory
        ( directory: string,
          ?searchPattern: string,
          ?searchColumn: string,
          ?searchLookupRange: LookupRangeMode<string>,
          ?columnKeys: string list,
          ?byteOffsetIndex: bool ) =
      if not (Directory.Exists directory) then
        raise (DirectoryNotFoundException(sprintf "VirtualCsvSource: directory not found '%s'" directory))
      let pattern = defaultArg searchPattern "*.csv"
      let files = Directory.GetFiles(directory, pattern) |> Array.sort
      if files.Length = 0 then invalidArg "directory" (sprintf "No files matching '%s' in '%s'" pattern directory)
      let options : VirtualReadCsvOptions =
        { IndexColumn = None
          SearchColumn = searchColumnOption searchColumn searchLookupRange
          ColumnKeys = columnKeys
          ByteOffsetIndex = defaultArg byteOffsetIndex false }
      VirtualCsvSource.createConcatenatedFrame files options
