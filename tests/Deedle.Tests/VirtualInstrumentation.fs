#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#else
module Deedle.Tests.VirtualInstrumentation
#endif

open System
open System.Collections.Generic
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Internal
open Deedle.Addressing
open Deedle.Vectors
open Deedle.Vectors.Virtual
open Deedle.Virtual

module Address = LinearAddress

// ------------------------------------------------------------------------------------------------
// Access counters & snapshots (deterministic metrics — no wall clock)
// ------------------------------------------------------------------------------------------------

/// Mutable counters shared across GetSubVector / MergeWith clones of a source.
type AccessCounters() =
  let valueAtAddresses = ResizeArray<int64>()
  member val ValueAtCount = 0 with get, set
  member val LookupValueCount = 0 with get, set
  member val LookupRangeCount = 0 with get, set
  member val GetSubVectorCount = 0 with get, set
  member val MergeWithCount = 0 with get, set
  member x.ValueAtAddresses = valueAtAddresses :> IReadOnlyList<_>
  member x.RecordValueAt(addr: int64) =
    x.ValueAtCount <- x.ValueAtCount + 1
    valueAtAddresses.Add(addr)
  member x.Reset() =
    x.ValueAtCount <- 0
    x.LookupValueCount <- 0
    x.LookupRangeCount <- 0
    x.GetSubVectorCount <- 0
    x.MergeWithCount <- 0
    valueAtAddresses.Clear()
  member x.Snapshot() =
    { ValueAtCount = x.ValueAtCount
      LookupValueCount = x.LookupValueCount
      LookupRangeCount = x.LookupRangeCount
      GetSubVectorCount = x.GetSubVectorCount
      MergeWithCount = x.MergeWithCount
      ValueAtAddressList = List.ofSeq valueAtAddresses }

and AccessSnapshot =
  { ValueAtCount: int
    LookupValueCount: int
    LookupRangeCount: int
    GetSubVectorCount: int
    MergeWithCount: int
    ValueAtAddressList: int64 list }
  member x.TouchedData = x.ValueAtCount > 0
  member x.TotalOps =
    x.ValueAtCount + x.LookupValueCount + x.LookupRangeCount + x.GetSubVectorCount + x.MergeWithCount
  static member delta (before: AccessSnapshot) (after: AccessSnapshot) =
    { ValueAtCount = after.ValueAtCount - before.ValueAtCount
      LookupValueCount = after.LookupValueCount - before.LookupValueCount
      LookupRangeCount = after.LookupRangeCount - before.LookupRangeCount
      GetSubVectorCount = after.GetSubVectorCount - before.GetSubVectorCount
      MergeWithCount = after.MergeWithCount - before.MergeWithCount
      ValueAtAddressList =
        // Addresses recorded after `before` (suffix)
        let n = before.ValueAtAddressList.Length
        after.ValueAtAddressList |> List.skip n }

// ------------------------------------------------------------------------------------------------
// Virtual vs materialised classification
// ------------------------------------------------------------------------------------------------

type StorageKind =
  | VirtualStorage
  | LinearStorage
  | OtherStorage of string

type SeriesShape =
  | FullyVirtual
  | FullyLinear
  | Mixed of index: StorageKind * vector: StorageKind

module SchemeProbe =
  let kind (scheme: IAddressingScheme) =
    match scheme with
    | :? VirtualAddressingScheme -> VirtualStorage
    | :? LinearAddressingScheme -> LinearStorage
    | other -> OtherStorage(other.GetType().Name)

  let isVirtualScheme scheme =
    match kind scheme with
    | VirtualStorage -> true
    | _ -> false

module SeriesProbe =
  let indexKind (s: Series<'K, 'V>) = SchemeProbe.kind s.Index.AddressingScheme
  let vectorKind (s: Series<'K, 'V>) = SchemeProbe.kind s.Vector.AddressingScheme

  let classify (s: Series<'K, 'V>) =
    match indexKind s, vectorKind s with
    | VirtualStorage, VirtualStorage -> FullyVirtual
    | LinearStorage, LinearStorage -> FullyLinear
    | i, v -> Mixed(i, v)

  let isVirtual (s: Series<'K, 'V>) =
    match classify s with
    | FullyVirtual -> true
    | _ -> false

  let isLinear (s: Series<'K, 'V>) =
    match classify s with
    | FullyLinear -> true
    | _ -> false

module FrameProbe =
  /// True when the row index uses a virtual addressing scheme.
  let rowIndexIsVirtual (f: Frame<'R, 'C>) =
    SchemeProbe.isVirtualScheme f.RowIndex.AddressingScheme

// ------------------------------------------------------------------------------------------------
// Instrumented ordinal IVirtualVectorSource
// ------------------------------------------------------------------------------------------------

/// Strided custom range used by filter/Search (same shape as VirtualFrame.LinearSubRange).
type StepRange =
  { Offset: int
    Step: int }
  interface IRangeRestriction<Address> with
    member x.Count = failwith "Count not supported"
  interface seq<Address> with
    member x.GetEnumerator() : System.Collections.Generic.IEnumerator<Address> =
      failwith "enumeration not supported"
  interface System.Collections.IEnumerable with
    member x.GetEnumerator() : System.Collections.IEnumerator =
      failwith "enumeration not supported"

/// How LookupRange behaves (B4 quality axis).
type LookupRangeMode<'T> =
  | LookupRangeUnsupported
  /// Return a tight Fixed absolute index range for the searched value.
  | LookupRangeExactFixed of ('T -> int64 * int64)
  /// Return a Custom strided range (offset, step) over the ordinal domain.
  | LookupRangeStep of ('T -> int * int)
  /// Naive over-approximation: entire ordinal domain (wrong for sparse matches).
  | LookupRangeFullFixed

/// Linear ordinal source over [0 .. length-1] with shared access counters.
type InstrumentedOrdinalSource<'T>
    ( length: int64,
      valueAt: int64 -> 'T,
      counters: AccessCounters,
      ?asLong: 'T -> int64,
      ?lookupRange: LookupRangeMode<'T>,
      ?hasMissing: bool ) =

  let hasMissing = defaultArg hasMissing false
  let lookupRangeMode = defaultArg lookupRange LookupRangeUnsupported
  let addressing = Indices.Linear.LinearAddressOperations(0L, length - 1L) :> IAddressOperations

  member x.Counters = counters
  member x.Length = length

  interface IVirtualVectorSource with
    member x.Length = length
    member x.AddressingSchemeID = "instrumented-ordinal"
    member x.ElementType = typeof<'T>
    member x.AddressOperations = addressing
    member x.Invoke(op) = op.Invoke(x)

  interface IVirtualVectorSource<'T> with
    member x.MergeWith(sources) =
      counters.MergeWithCount <- counters.MergeWithCount + 1
      let parts =
        (length, valueAt)
        :: [ for s in sources ->
               match s with
               | :? InstrumentedOrdinalSource<'T> as src -> src.Length, src.RawValueAt
               | _ -> failwith "MergeWith: expected InstrumentedOrdinalSource" ]
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
      InstrumentedOrdinalSource<'T>
        (total, mergedValueAt, counters, ?asLong=asLong, lookupRange=lookupRangeMode, hasMissing=hasMissing) :> _

    member x.LookupRange(v) =
      counters.LookupRangeCount <- counters.LookupRangeCount + 1
      match lookupRangeMode with
      | LookupRangeUnsupported -> failwith "LookupRange: not configured on InstrumentedOrdinalSource"
      | LookupRangeExactFixed f ->
          let lo, hi = f v
          RangeRestriction.Fixed(Address.ofInt64 lo, Address.ofInt64 hi)
      | LookupRangeStep f ->
          let offset, step = f v
          RangeRestriction.Custom { Offset = offset; Step = step }
      | LookupRangeFullFixed ->
          RangeRestriction.Fixed(Address.ofInt64 0L, Address.ofInt64(length - 1L))

    member x.LookupValue(k, l, check) =
      counters.LookupValueCount <- counters.LookupValueCount + 1
      let asLong =
        match asLong with
        | Some g -> g
        | None -> failwith "LookupValue: asLong not configured"
      let c = Func<int64, bool>(fun i -> check.Invoke(Address.ofInt64 i))
      let found =
        IndexUtilsModule.binarySearch length (Func<_, _>(fun i -> asLong (valueAt i))) (asLong k) l c
      found
      |> OptionalValue.map (fun i -> valueAt i, Address.ofInt64 i)

    member x.ValueAt(loc) =
      let absAddr = Address.asInt64 loc.Address
      counters.RecordValueAt(absAddr)
      if hasMissing && absAddr % 3L = 0L then OptionalValue.Missing
      else OptionalValue(valueAt absAddr)

    member x.GetSubVector(range) =
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
                  // Clip / shift into sub-range coordinates when possible
                  max 0L (a - lo), min (newLen - 1L) (b - lo))
            | LookupRangeStep f ->
                // Absolute domain still uses original valueAt via subValueAt remapping;
                // step search offsets stay relative to the sub-source ordinal domain.
                LookupRangeStep f
            | LookupRangeFullFixed -> LookupRangeFullFixed
          InstrumentedOrdinalSource<'T>(newLen, subValueAt, counters, ?asLong=asLong, lookupRange=subLookup, hasMissing=hasMissing) :> _
      | Choice2Of2(:? StepRange as lr) ->
          let subValueAt i = valueAt (int64 lr.Offset + int64 lr.Step * i)
          let count =
            if length = 0L then 0L
            else
              let span = length
              let baseCount = span / int64 lr.Step
              if span % int64 lr.Step > int64 lr.Offset then baseCount + 1L else baseCount
          let newLen = max 0L count
          InstrumentedOrdinalSource<'T>(newLen, subValueAt, counters, ?asLong=asLong, lookupRange=lookupRangeMode, hasMissing=hasMissing) :> _
      | Choice2Of2 ar ->
          let addrs = ar |> Seq.map Address.asInt64 |> List.ofSeq
          let subValueAt i = valueAt addrs.[int i]
          InstrumentedOrdinalSource<'T>(int64 addrs.Length, subValueAt, counters, ?asLong=asLong, lookupRange=lookupRangeMode, hasMissing=hasMissing) :> _

  /// Read without recording (for MergeWith composition).
  member x.RawValueAt(i: int64) = valueAt i

module InstrumentedOrdinalSource =
  let createFloats (length: int64) =
    let c = AccessCounters()
    c, InstrumentedOrdinalSource<float>(length, float, c, hasMissing=false)

  let createLongs (length: int64) =
    let c = AccessCounters()
    c, InstrumentedOrdinalSource<int64>(length, id, c, asLong=id, hasMissing=false)

  let createStrings (length: int64) (words: string[]) =
    let c = AccessCounters()
    let valueAt i = words.[int (i % int64 words.Length)]
    let indexOf v =
      let o = words |> Array.findIndex ((=) v) |> int64
      // Fixed first-hit window (B4 can study ExactFixed quality separately)
      o, o
    c, InstrumentedOrdinalSource<string>(length, valueAt, c, lookupRange=LookupRangeExactFixed indexOf, hasMissing=false)

  let createSearchableStrings (length: int64) (words: string[]) =
    let c = AccessCounters()
    let valueAt i = words.[int (i % int64 words.Length)]
    let search v =
      let o = words |> Array.findIndex ((=) v)
      o, words.Length
    c, InstrumentedOrdinalSource<string>(length, valueAt, c, lookupRange=LookupRangeStep search, hasMissing=false)

  let createTimes (length: int64) =
    let c = AccessCounters()
    let start = DateTimeOffset(DateTime(2000, 1, 1), TimeSpan.FromHours(-1.0))
    let valueAt i = start.AddTicks(i * 123456789L)
    let asLong (dto: DateTimeOffset) = dto.UtcTicks
    c, InstrumentedOrdinalSource<DateTimeOffset>(length, valueAt, c, asLong=asLong, hasMissing=false)

  let createOrdinalSeries (length: int64) =
    let c, src = createLongs length
    c, Virtual.CreateOrdinalSeries(src)

  let createFloatSeries (length: int64) =
    let c, src = createFloats length
    c, Virtual.CreateOrdinalSeries(src)

  /// Ordered DateTimeOffset index + float values sharing one AccessCounters.
  let createOrderedFloatSeries (length: int64) =
    let c = AccessCounters()
    let start = DateTimeOffset(DateTime(2000, 1, 1), TimeSpan.FromHours(-1.0))
    let idx =
      InstrumentedOrdinalSource<DateTimeOffset>
        (length, (fun i -> start.AddTicks(i * 123456789L)), c, asLong=(fun dto -> dto.UtcTicks), hasMissing=false)
    let vals = InstrumentedOrdinalSource<float>(length, float, c, hasMissing=false)
    c, Virtual.CreateSeries(idx, vals)

  /// Ordered time frame; `lookupRange` controls search-column LookupRange quality (B4).
  let createOrderedSearchFrameWith (length: int64) (lookupRange: LookupRangeMode<string>) =
    let words = "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')
    let c = AccessCounters()
    let start = DateTimeOffset(DateTime(2000, 1, 1), TimeSpan.FromHours(-1.0))
    let idx =
      InstrumentedOrdinalSource<DateTimeOffset>
        (length, (fun i -> start.AddTicks(i * 123456789L)), c, asLong=(fun dto -> dto.UtcTicks), hasMissing=false)
    let s1 = InstrumentedOrdinalSource<int64>(length, id, c, asLong=id, hasMissing=false)
    let search v =
      let o = words |> Array.findIndex ((=) v)
      o, words.Length
    let s2Lookup =
      match lookupRange with
      | LookupRangeStep _ -> LookupRangeStep search
      | other -> other
    let s2 =
      InstrumentedOrdinalSource<string>
        (length, (fun i -> words.[int (i % int64 words.Length)]), c, lookupRange=s2Lookup, hasMissing=false)
    let frame = Virtual.CreateFrame(idx, ["S1"; "S2"], [s1 :> IVirtualVectorSource; s2 :> IVirtualVectorSource])
    c, frame, words

  /// Ordered time frame with a searchable string column (for filterRowsBy).
  let createOrderedSearchFrame (length: int64) =
    createOrderedSearchFrameWith length (LookupRangeStep (fun _ -> 0, 0))

  /// Ordered frame where the search column is a mapped virtual series (no reverse lookup).
  let createOrderedMappedSearchFrame (length: int64) =
    let c, frame, words = createOrderedSearchFrame length
    let mapped =
      frame.GetColumn<string>("S2")
      |> Series.mapValues (fun s -> s.ToUpperInvariant())
    let rebuilt = frame |> Frame.replaceCol "S2" mapped
    c, rebuilt, words

  /// Ordinal-index frame (linear Search fallback — no virtual LookupRange path).
  let createOrdinalSearchFrame (length: int64) =
    let words = "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')
    let c = AccessCounters()
    let search v =
      let o = words |> Array.findIndex ((=) v)
      o, words.Length
    let s1 = InstrumentedOrdinalSource<int64>(length, id, c, asLong=id, hasMissing=false)
    let s2 =
      InstrumentedOrdinalSource<string>
        (length, (fun i -> words.[int (i % int64 words.Length)]), c, lookupRange=LookupRangeStep search, hasMissing=false)
    let frame = Virtual.CreateOrdinalFrame(["S1"; "S2"], [s1 :> IVirtualVectorSource; s2 :> IVirtualVectorSource])
    c, frame, words

// ------------------------------------------------------------------------------------------------
// Smoke tests
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``KeyCount does not touch ValueAt`` () =
  let c, series = InstrumentedOrdinalSource.createOrdinalSeries 1_000_000L
  c.Reset()
  series.KeyCount |> shouldEqual 1_000_000
  c.Snapshot().ValueAtCount |> shouldEqual 0
  SeriesProbe.isVirtual series |> shouldEqual true

[<Test>]
let ``Formatting touches only a few ValueAt calls`` () =
  let c, series = InstrumentedOrdinalSource.createOrdinalSeries 1_000_000L
  c.Reset()
  series.Format(3, 3, false) |> ignore
  let snap = c.Snapshot()
  snap.ValueAtCount |> should be (lessThan 20)
  SeriesProbe.isVirtual series |> shouldEqual true

[<Test>]
let ``Slicing preserves virtual storage and records GetSubVector`` () =
  let c, series = InstrumentedOrdinalSource.createOrdinalSeries 1_000_000L
  c.Reset()
  let sliced = series.[10L .. 20L]
  let snap = c.Snapshot()
  snap.GetSubVectorCount |> should be (greaterThan 0)
  snap.ValueAtCount |> shouldEqual 0
  SeriesProbe.isVirtual sliced |> shouldEqual true
  sliced.KeyCount |> shouldEqual 11

[<Test>]
let ``Materialize flips series to linear storage`` () =
  let c, series = InstrumentedOrdinalSource.createOrdinalSeries 100L
  SeriesProbe.isVirtual series |> shouldEqual true
  let mat = series.Materialize()
  SeriesProbe.isLinear mat |> shouldEqual true
  // Materialize reads data
  c.Snapshot().ValueAtCount |> should be (greaterThan 0)

[<Test>]
let ``Delta snapshot isolates operation cost`` () =
  let c, series = InstrumentedOrdinalSource.createOrdinalSeries 10_000L
  let before = c.Snapshot()
  series.Format(2, 2, false) |> ignore
  let after = c.Snapshot()
  let d = AccessSnapshot.delta before after
  d.ValueAtCount |> should be (greaterThan 0)
  d.ValueAtCount |> shouldEqual (after.ValueAtCount - before.ValueAtCount)
