#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.CsvFileVirtualSource
#endif

open System
open System.Globalization
open System.IO
open Deedle
open Deedle.Internal
open Deedle.Addressing
open Deedle.Vectors
open Deedle.Vectors.Virtual
open Deedle.Virtual
open Deedle.Tests.VirtualInstrumentation

module Address = LinearAddress

// ------------------------------------------------------------------------------------------------
// B6 — file-backed CSV virtual source (phase 2)
// ------------------------------------------------------------------------------------------------

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

module CsvLineIndex =
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
      |> Option.defaultWith (fun () -> failwithf "CsvLineIndex meta missing key '%s'" key)
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

  /// Write a large CSV with shuffled ids, random values, and the same 8-word category cycle as B4/B5.
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

/// Pull-on-read column backed by CSV line offsets (`IVirtualVectorSource`).
type CsvFileVirtualSource<'T>
    ( length: int64,
      valueAt: int64 -> 'T,
      counters: AccessCounters,
      ?asLong: 'T -> int64,
      ?lookupRange: LookupRangeMode<'T> ) =

  let lookupRangeMode = defaultArg lookupRange LookupRangeUnsupported
  let addressing = Indices.Linear.LinearAddressOperations(0L, length - 1L) :> IAddressOperations

  interface IVirtualVectorSource with
    member x.Length = length
    member x.AddressingSchemeID = "csv-file"
    member x.ElementType = typeof<'T>
    member x.AddressOperations = addressing
    member x.Invoke(op) = op.Invoke(x)

  interface IVirtualVectorSource<'T> with
    member _.MergeWith(sources) =
      counters.MergeWithCount <- counters.MergeWithCount + 1
      let parts =
        (length, valueAt)
        :: [ for s in sources ->
               match s with
               | :? CsvFileVirtualSource<'T> as src -> src.Length, src.RawValueAt
               | _ -> failwith "MergeWith: expected CsvFileVirtualSource" ]
      let total = parts |> List.sumBy fst
      let mergedValueAt (i: int64) =
        let mutable offset = 0L
        let mutable result = None
        for (len, vat) in parts do
          if result.IsNone then
            if i < offset + len then result <- Some(vat (i - offset))
            else offset <- offset + len
        match result with
        | Some v -> v
        | None -> failwithf "MergeWith: index %d out of range (len=%d)" i total
      CsvFileVirtualSource<'T>(total, mergedValueAt, counters, ?asLong=asLong, lookupRange=lookupRangeMode) :> _

    member _.LookupRange(v) =
      counters.LookupRangeCount <- counters.LookupRangeCount + 1
      match lookupRangeMode with
      | LookupRangeUnsupported -> failwith "LookupRange: not configured on CsvFileVirtualSource"
      | LookupRangeExactFixed f ->
          let lo, hi = f v
          RangeRestriction.Fixed(Address.ofInt64 lo, Address.ofInt64 hi)
      | LookupRangeStep f ->
          let offset, step = f v
          RangeRestriction.Custom { Offset = offset; Step = step }
      | LookupRangeFullFixed ->
          RangeRestriction.Fixed(Address.ofInt64 0L, Address.ofInt64(length - 1L))
      | LookupRangeIndexList f ->
          let addrs = f v |> List.map Address.ofInt64
          let count = int64 addrs.Length
          ({ new IRangeRestriction<Address> with
              member _.Count = count
             interface seq<Address> with
               member _.GetEnumerator() = (addrs :> seq<_>).GetEnumerator()
             interface System.Collections.IEnumerable with
               member _.GetEnumerator() = (addrs :> seq<_>).GetEnumerator() :> System.Collections.IEnumerator }
           |> RangeRestriction.Custom)

    member _.LookupValue(k, l, check) =
      counters.LookupValueCount <- counters.LookupValueCount + 1
      let asLong =
        match asLong with
        | Some g -> g
        | None -> failwith "LookupValue: asLong not configured"
      let c = Func<int64, bool>(fun i -> check.Invoke(Address.ofInt64 i))
      IndexUtilsModule.binarySearch length (Func<_, _>(fun i -> asLong (valueAt i))) (asLong k) l c
      |> OptionalValue.map (fun i -> valueAt i, Address.ofInt64 i)

    member _.ValueAt(loc) =
      let absAddr = Address.asInt64 loc.Address
      counters.RecordValueAt(absAddr)
      OptionalValue(valueAt absAddr)

    member _.GetSubVector(range) =
      counters.GetSubVectorCount <- counters.GetSubVectorCount + 1
      match range.AsAbsolute(length) with
      | Choice1Of2(nlo, nhi) ->
          let lo = Address.asInt64 nlo
          let hi = Address.asInt64 nhi
          if hi < lo then invalidOp "GetSubVector: hi < lo"
          let newLen = hi - lo + 1L
          let subValueAt i = valueAt (lo + i)
          let subLookup =
            match lookupRangeMode with
            | LookupRangeUnsupported -> LookupRangeUnsupported
            | LookupRangeExactFixed f ->
                LookupRangeExactFixed(fun v ->
                  let a, b = f v
                  max 0L (a - lo), min (newLen - 1L) (b - lo))
            | LookupRangeStep f -> LookupRangeStep f
            | LookupRangeFullFixed -> LookupRangeFullFixed
            | LookupRangeIndexList _ -> lookupRangeMode
          CsvFileVirtualSource<'T>(newLen, subValueAt, counters, ?asLong=asLong, lookupRange=subLookup) :> _
      | Choice2Of2(:? StepRange as lr) ->
          let subValueAt i = valueAt (int64 lr.Offset + int64 lr.Step * i)
          let count =
            if length = 0L then 0L
            else
              let span = length
              let baseCount = span / int64 lr.Step
              if span % int64 lr.Step > int64 lr.Offset then baseCount + 1L else baseCount
          let newLen = max 0L count
          CsvFileVirtualSource<'T>(newLen, subValueAt, counters, ?asLong=asLong, lookupRange=lookupRangeMode) :> _
      | Choice2Of2 ar ->
          let addrs = ar |> Seq.map Address.asInt64 |> List.ofSeq
          let subValueAt i = valueAt addrs.[int i]
          CsvFileVirtualSource<'T>(int64 addrs.Length, subValueAt, counters, ?asLong=asLong, lookupRange=lookupRangeMode) :> _

  member _.Length = length
  member _.RawValueAt(i: int64) = valueAt i

module CsvFileVirtualSource =
  let private field (fields: string[]) (columnIndex: int) =
    if columnIndex >= fields.Length then
      failwithf "CsvFileVirtualSource: column %d missing (fields=%d)" columnIndex fields.Length
    fields.[columnIndex].TrimEnd('\r', '\n')

  let private parseInt (s: string) = Int32.Parse(s, CultureInfo.InvariantCulture)
  let private parseFloat (s: string) = Double.Parse(s, CultureInfo.InvariantCulture)
  let private parseString (s: string) = s
  let private parseDateTime (s: string) =
    DateTimeOffset.Parse(s, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind)

  let private createColumn (index: CsvLineIndex) columnIndex (parse: string -> 'T) counters
      (asLong: ('T -> int64) option) (lookupRange: LookupRangeMode<'T> option) =
    let valueAt row =
      let fields = index.ReadFields row
      field fields columnIndex |> parse
    CsvFileVirtualSource(index.Length, valueAt, counters, ?asLong=asLong, ?lookupRange=lookupRange)

  /// Ordered virtual frame backed by CSV columns (Timestamp index, Id + Category search).
  let createOrderedSearchFrame (csvPath: string) (counters: AccessCounters) =
    let lineIndex = CsvLineIndex(csvPath)
    let words = CsvLineIndex.words8
    let idx =
      createColumn lineIndex 1 parseDateTime counters
        (Some (fun dto -> dto.UtcTicks)) (Some LookupRangeUnsupported)
    let s1 =
      createColumn lineIndex 0 (fun s -> int64 (parseInt s)) counters (Some id) None
    let s2 =
      createColumn lineIndex 2 parseString counters None
        (Some (LookupRangeStep (fun v -> words |> Array.findIndex ((=) v), words.Length)))
    let frame =
      Virtual.CreateFrame(idx, ["S1"; "S2"], [s1 :> IVirtualVectorSource; s2 :> IVirtualVectorSource])
    counters, frame, words

  let createFloatValueSeries (csvPath: string) (counters: AccessCounters) =
    let lineIndex = CsvLineIndex(csvPath)
    let src = createColumn lineIndex 3 parseFloat counters None None
    counters, Virtual.CreateOrdinalSeries(src)
