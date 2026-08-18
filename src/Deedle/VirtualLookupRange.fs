namespace Deedle.Virtual

open System
open Deedle
open Deedle.Addressing
open Deedle.Vectors.Virtual

module Address = LinearAddress

/// Strided custom range used by filter / Search (same shape as step-based LookupRange).
type StepRange =
  { Offset: int
    Step: int }
  interface IRangeRestriction<Address> with
    member _.Count = failwith "Count not supported on StepRange"
  interface seq<Address> with
    member _.GetEnumerator() = failwith "enumeration not supported on StepRange"
  interface System.Collections.IEnumerable with
    member _.GetEnumerator() = failwith "enumeration not supported on StepRange"

/// How `LookupRange` behaves on searchable virtual columns (quality / correctness axis).
type LookupRangeMode<'T> =
  | LookupRangeUnsupported
  /// Return a tight Fixed absolute index range for the searched value.
  | LookupRangeExactFixed of ('T -> int64 * int64)
  /// Return a Custom strided range (offset, step) over the ordinal domain.
  | LookupRangeStep of ('T -> int * int)
  /// Naive over-approximation: entire ordinal domain (wrong for sparse matches).
  | LookupRangeFullFixed
  /// Precomputed absolute indices (irregular/sparse matches).
  | LookupRangeIndexList of ('T -> int64 list)

/// Helpers for configuring searchable columns on virtual sources.
[<RequireQualifiedAccess>]
module VirtualLookupRange =
  /// Step LookupRange for values repeating on a fixed cycle (B4/B5/B6 ideal case).
  let forRepeatingCycle (values: 'T[]) =
    LookupRangeStep (fun v -> values |> Array.findIndex ((=) v), values.Length)

  /// IndexList LookupRange from a pre-built map of value → row indices.
  let forCategorical (indicesByValue: Map<'T, int64 list>) =
    LookupRangeIndexList (fun v ->
      match indicesByValue.TryGetValue v with
      | true, xs -> xs
      | false, _ -> [])

  /// Build categorical IndexList by scanning column values once at frame construction.
  let forCategoricalScan (length: int64) (valueAt: int64 -> 'T) =
    let buckets = System.Collections.Generic.Dictionary<'T, ResizeArray<int64>>()
    for i in 0L .. length - 1L do
      let v = valueAt i
      let bucket =
        match buckets.TryGetValue v with
        | true, b -> b
        | false, _ ->
          let b = ResizeArray()
          buckets.[v] <- b
          b
      bucket.Add(i)
    let map =
      buckets
      |> Seq.map (fun (KeyValue(k, v)) -> k, List.ofSeq v)
      |> Map.ofSeq
    forCategorical map

  /// Correct but O(N) per filter — scans all rows when LookupRange is invoked.
  let scan (length: int64) (valueAt: int64 -> 'T) =
    LookupRangeIndexList (fun v ->
      [ for i in 0L .. length - 1L do if valueAt i = v then i ])

  let exactFixed (selector: 'T -> int64 * int64) = LookupRangeExactFixed selector
  let fullFixed = LookupRangeFullFixed

/// Shared LookupRange / GetSubVector logic for ordinal virtual sources.
[<RequireQualifiedAccess>]
module LookupRangeExecutor =
  open Deedle.Internal

  let lookupRange (length: int64) (mode: LookupRangeMode<'T>) (value: 'T) (context: string) =
    match mode with
    | LookupRangeUnsupported -> failwithf "LookupRange: not configured on %s" context
    | LookupRangeExactFixed f ->
        let lo, hi = f value
        RangeRestriction.Fixed(Address.ofInt64 lo, Address.ofInt64 hi)
    | LookupRangeStep f ->
        let offset, step = f value
        RangeRestriction.Custom { Offset = offset; Step = step }
    | LookupRangeFullFixed ->
        RangeRestriction.Fixed(Address.ofInt64 0L, Address.ofInt64(length - 1L))
    | LookupRangeIndexList f ->
        let addrs = f value |> List.map Address.ofInt64
        let count = int64 addrs.Length
        ({ new IRangeRestriction<Address> with
            member _.Count = count
           interface seq<Address> with
             member _.GetEnumerator() = (addrs :> seq<_>).GetEnumerator()
           interface System.Collections.IEnumerable with
             member _.GetEnumerator() = (addrs :> seq<_>).GetEnumerator() :> System.Collections.IEnumerator }
         |> RangeRestriction.Custom)

  let clipLookupRange (mode: LookupRangeMode<'T>) (lo: int64) (newLen: int64) =
    match mode with
    | LookupRangeUnsupported -> LookupRangeUnsupported
    | LookupRangeExactFixed f ->
        LookupRangeExactFixed(fun v ->
          let a, b = f v
          max 0L (a - lo), min (newLen - 1L) (b - lo))
    | LookupRangeStep f -> LookupRangeStep f
    | LookupRangeFullFixed -> LookupRangeFullFixed
    | LookupRangeIndexList f -> LookupRangeIndexList f

  type SubVectorSpec<'T> =
    { Length: int64
      ValueAt: int64 -> 'T
      AsLong: ('T -> int64) option
      LookupRange: LookupRangeMode<'T> }

  let getSubVector (length: int64) (valueAt: int64 -> 'T) (mode: LookupRangeMode<'T>) (asLong: ('T -> int64) option) (range: RangeRestriction<Address>) =
    match range.AsAbsolute(length) with
    | Choice1Of2(nlo, nhi) ->
        let lo = Address.asInt64 nlo
        let hi = Address.asInt64 nhi
        if hi < lo then invalidOp "GetSubVector: hi < lo"
        let newLen = hi - lo + 1L
        let subValueAt i = valueAt (lo + i)
        Choice1Of2
          { Length = newLen
            ValueAt = subValueAt
            AsLong = asLong
            LookupRange = clipLookupRange mode lo newLen }
    | Choice2Of2(:? StepRange as lr) ->
        let subValueAt i = valueAt (int64 lr.Offset + int64 lr.Step * i)
        let count =
          if length = 0L then 0L
          else
            let span = length
            let baseCount = span / int64 lr.Step
            if span % int64 lr.Step > int64 lr.Offset then baseCount + 1L else baseCount
        let newLen = max 0L count
        Choice1Of2
          { Length = newLen
            ValueAt = subValueAt
            AsLong = asLong
            LookupRange = mode }
    | Choice2Of2 ar ->
        let addrs = ar |> Seq.map Address.asInt64 |> List.ofSeq
        let subValueAt i = valueAt addrs.[int i]
        Choice1Of2
          { Length = int64 addrs.Length
            ValueAt = subValueAt
            AsLong = asLong
            LookupRange = mode }
