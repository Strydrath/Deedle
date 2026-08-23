namespace Deedle.Virtual

open System
open Deedle
open Deedle.Addressing
open Deedle.Vectors.Virtual
open Deedle.VectorHelpers

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
  /// Step LookupRange for values repeating on a fixed cycle (periodic categorical data).
  /// Unknown values yield an empty range (negative offset) instead of throwing.
  let forRepeatingCycle (values: 'T[]) =
    LookupRangeStep (fun v ->
      match values |> Array.tryFindIndex ((=) v) with
      | Some i -> i, values.Length
      | None -> -1, max 1 values.Length)

  /// IndexList LookupRange from a pre-built map of value â†’ row indices.
  let forCategorical (indicesByValue: Map<'T, int64 list>) =
    LookupRangeIndexList (fun v ->
      match indicesByValue.TryGetValue v with
      | true, xs -> xs
      | false, _ -> [])

  /// Build categorical IndexList by scanning column values once at frame construction.
  let forCategoricalScan (length: int64) (valueAt: int64 -> 'T) =
    [ for i in 0L .. length - 1L -> valueAt i, i ]
    |> List.groupBy fst
    |> List.map (fun (k, pairs) -> k, List.map snd pairs)
    |> Map.ofList
    |> forCategorical

  /// Correct but O(N) per filter - scans all rows when LookupRange is invoked.
  let scan (length: int64) (valueAt: int64 -> 'T) =
    LookupRangeIndexList (fun v ->
      [ for i in 0L .. length - 1L do if valueAt i = v then i ])

  let exactFixed (selector: 'T -> int64 * int64) = LookupRangeExactFixed selector
  let fullFixed = LookupRangeFullFixed

  /// Maximum distinct non-empty string values for automatic LookupRange inference.
  [<Literal>]
  let MaxInferredSearchCardinality = 64

  /// Infer Step or categorical IndexList LookupRange from column values (ReadCsv / ReadParquet).
  let tryInferStringLookupRange (length: int64) (valueAt: int64 -> string) =
    if length = 0L then None
    else
      let values = [| for i in 0L .. length - 1L -> valueAt i |]
      let distinct =
        values |> Array.filter ((<>) "") |> Array.distinct
      if distinct.Length = 0 || distinct.Length > MaxInferredSearchCardinality then None
      else
        let period = distinct.Length
        let isRepeatingCycle =
          values
          |> Array.mapi (fun i v -> v = "" || v = distinct.[i % period])
          |> Array.forall id
        if isRepeatingCycle then
          Some(forRepeatingCycle distinct, sprintf "repeating cycle (period %d)" period)
        else
          Some(
            forCategoricalScan length valueAt,
            sprintf "categorical IndexList (%d distinct; one-time O(N) scan per filter value)" distinct.Length)

  /// Resolve LookupRange for one column when `searchColumn` is set on ReadCsv / ReadParquet.
  let resolveSearchColumnLookupRange
      (apiName: string)
      (searchColumn: (string * LookupRangeMode<string>) option)
      (columnName: string)
      (isStringColumn: bool)
      (infer: unit -> (LookupRangeMode<string> * string) option) =
    match searchColumn with
    | Some (searchName, LookupRangeUnsupported) when isStringColumn && String.Equals(columnName, searchName, StringComparison.OrdinalIgnoreCase) ->
        match infer() with
        | Some (mode, desc) ->
            System.Diagnostics.Trace.WriteLine(
              sprintf "%s: inferred %s LookupRange for search column '%s'." apiName desc columnName)
            Some mode
        | None ->
            System.Diagnostics.Trace.WriteLine(
              sprintf "%s: search column '%s' has high cardinality; configure searchLookupRange explicitly (e.g. VirtualLookupRange.scan)." apiName columnName)
            None
    | Some (searchName, mode) when String.Equals(columnName, searchName, StringComparison.OrdinalIgnoreCase) ->
        Some mode
    | _ -> None

/// Shared LookupRange / GetSubVector logic for ordinal virtual sources.
[<RequireQualifiedAccess>]
module LookupRangeExecutor =
  open Deedle.Internal

  let private emptyAddressRange () =
    let addrs: Address list = []
    ({ new IRangeRestriction<Address> with
        member _.Count = 0L
       interface seq<Address> with
         member _.GetEnumerator() = (addrs :> seq<_>).GetEnumerator()
       interface System.Collections.IEnumerable with
         member _.GetEnumerator() = (addrs :> seq<_>).GetEnumerator() :> System.Collections.IEnumerator }
     |> RangeRestriction.Custom)

  let lookupRange (length: int64) (mode: LookupRangeMode<'T>) (value: 'T) (context: string) =
    match mode with
    | LookupRangeUnsupported ->
        raise (NotSupportedException(
          sprintf
            "%s: LookupRange is not configured on this virtual column. Configure searchLookupRange (e.g. VirtualLookupRange.forRepeatingCycle, forCategorical, or scan) or use Virtual.ReadCsv / Virtual.ReadParquet with a low-cardinality string search column (<=64 distinct values are inferred automatically)."
            context))
    | LookupRangeExactFixed f ->
        let lo, hi = f value
        RangeRestriction.Fixed(Address.ofInt64 lo, Address.ofInt64 hi)
    | LookupRangeStep f ->
        let offset, step = f value
        if offset < 0 || step <= 0 then emptyAddressRange ()
        else RangeRestriction.Custom { Offset = offset; Step = step }
    | LookupRangeFullFixed ->
        RangeRestriction.Fixed(Address.ofInt64 0L, Address.ofInt64(length - 1L))
    | LookupRangeIndexList f ->
        let addrs = f value |> List.map Address.ofInt64
        let count = int64 addrs.Length
        if count = 0L then emptyAddressRange ()
        else
          ({ new IRangeRestriction<Address> with
              member _.Count = count
             interface seq<Address> with
               member _.GetEnumerator() = (addrs :> seq<_>).GetEnumerator()
             interface System.Collections.IEnumerable with
               member _.GetEnumerator() = (addrs :> seq<_>).GetEnumerator() :> System.Collections.IEnumerator }
           |> RangeRestriction.Custom)

  let clipLookupRange (mode: LookupRangeMode<'T>) (lo: int64) (newLen: int64) =
    let hi = lo + newLen - 1L
    match mode with
    | LookupRangeUnsupported -> LookupRangeUnsupported
    | LookupRangeExactFixed f ->
        LookupRangeExactFixed(fun v ->
          let a, b = f v
          max 0L (a - lo), min (newLen - 1L) (b - lo))
    | LookupRangeStep f ->
        LookupRangeStep (fun v ->
          let offset, step = f v
          if offset < 0 || step <= 0 then (offset, step)
          else
            let firstAbs =
              if int64 offset >= lo then int64 offset
              else int64 offset + (lo - int64 offset + int64 step - 1L) / int64 step * int64 step
            if firstAbs > hi then (-1, step)
            else (int (firstAbs - lo), step))
    | LookupRangeFullFixed -> LookupRangeFullFixed
    | LookupRangeIndexList f ->
        LookupRangeIndexList (fun v ->
          f v
          |> List.choose (fun abs ->
              let local = abs - lo
              if local >= 0L && local < newLen then Some local else None))

  /// Sub-vector plan: callers compose `valueAt << MapRow` so OptionalValue sources stay typed.
  type SubVectorSpec<'T> =
    { Length: int64
      MapRow: int64 -> int64
      AsLong: ('T -> int64) option
      LookupRange: LookupRangeMode<'T> }

  let getSubVector (length: int64) (mode: LookupRangeMode<'T>) (asLong: ('T -> int64) option) (range: RangeRestriction<Address>) =
    match range.AsAbsolute(length) with
    | Choice1Of2(nlo, nhi) ->
        let lo = Address.asInt64 nlo
        let hi = Address.asInt64 nhi
        if hi < lo then invalidOp "GetSubVector: hi < lo"
        let newLen = hi - lo + 1L
        Choice1Of2
          { Length = newLen
            MapRow = fun i -> lo + i
            AsLong = asLong
            LookupRange = clipLookupRange mode lo newLen }
    | Choice2Of2(:? StepRange as lr) ->
        let count =
          if length = 0L || lr.Offset < 0 || lr.Step <= 0 then 0L
          else
            let span = length
            let baseCount = span / int64 lr.Step
            if span % int64 lr.Step > int64 lr.Offset then baseCount + 1L else baseCount
        let newLen = max 0L count
        Choice1Of2
          { Length = newLen
            MapRow = fun i -> int64 lr.Offset + int64 lr.Step * i
            AsLong = asLong
            LookupRange = mode }
    | Choice2Of2 ar ->
        let addrs = ar |> Seq.map Address.asInt64 |> List.ofSeq
        Choice1Of2
          { Length = int64 addrs.Length
            MapRow = fun i -> addrs.[int i]
            AsLong = asLong
            LookupRange = mode }

  let private gcd (a: int) (b: int) =
    let rec loop x y = if y = 0 then abs x else loop y (x % y)
    loop a b

  let private emptyRange =
    RangeRestriction.ofSeq 0L Array.empty

  /// Intersect two LookupRange results (same original address domain).
  /// Used to fuse two `filterRowsBy` predicates into one sub-vector restriction.
  /// Never enumerates [`StepRange`] (its enumerator throws by design).
  let intersect (a: RangeRestriction<Address>) (b: RangeRestriction<Address>) =
    let fromAddrs addrs =
      let arr = addrs |> Seq.distinct |> Array.ofSeq
      RangeRestriction.ofSeq (int64 arr.Length) arr
    let tryStep = function
      | RangeRestriction.Custom(:? StepRange as s) -> Some s
      | _ -> None
    let matchesStep (s: StepRange) (addr: Address) =
      let i = Address.asInt64 addr
      s.Step <> 0 &&
      i >= int64 s.Offset &&
      (i - int64 s.Offset) % int64 s.Step = 0L
    let filterAddrsByStep (s: StepRange) (addrs: seq<Address>) =
      fromAddrs (addrs |> Seq.filter (matchesStep s))
    let collectStepInFixed (s: StepRange) (lo64: int64) (hi64: int64) =
      if s.Step = 0 then emptyRange
      else
        let rec collect i acc =
          let addr = int64 s.Offset + int64 s.Step * i
          if addr > hi64 then List.rev acc
          elif addr < lo64 then collect (i + 1L) acc
          else collect (i + 1L) (Address.ofInt64 addr :: acc)
        fromAddrs (collect 0L [])
    match tryStep a, tryStep b, a, b with
    | Some s1, Some s2, _, _ ->
        if s1.Step = 0 || s2.Step = 0 then emptyRange
        else
          let p, q, ao, bo = s1.Step, s2.Step, s1.Offset, s2.Offset
          let g = gcd p q
          if (ao - bo) % g <> 0 then emptyRange
          else
            let lcm = p / g * q
            let m = max ao bo
            let rem = ((m - ao) % p + p) % p
            let startA = if rem = 0 then m else m + (p - rem)
            // Congruence guarantees a hit within one period of the other stride.
            let maxSteps = abs q / g + 1
            let rec loop x guard =
              if guard <= 0 then None
              elif (x - bo) % q = 0 then Some x
              else loop (x + p) (guard - 1)
            match loop startA maxSteps with
            | Some offset -> RangeRestriction.Custom { Offset = offset; Step = lcm }
            | None -> emptyRange
    | _, _, RangeRestriction.Fixed(lo1, hi1), RangeRestriction.Fixed(lo2, hi2) ->
        let lo = if lo1 > lo2 then lo1 else lo2
        let hi = if hi1 < hi2 then hi1 else hi2
        if lo <= hi then RangeRestriction.Fixed(lo, hi) else emptyRange
    | Some s, None, _, RangeRestriction.Fixed(lo, hi)
    | None, Some s, RangeRestriction.Fixed(lo, hi), _ ->
        collectStepInFixed s (Address.asInt64 lo) (Address.asInt64 hi)
    // Step ∩ IndexList (or any enumerable Custom): filter the enumerable; never enumerate Step.
    | Some s, None, _, RangeRestriction.Custom ar
    | None, Some s, RangeRestriction.Custom ar, _ ->
        filterAddrsByStep s ar
    | _, _, RangeRestriction.Custom ar1, RangeRestriction.Custom ar2 ->
        // Both non-Step Customs (IndexList ∩ IndexList, etc.)
        let set2 = System.Collections.Generic.HashSet<_>(ar2)
        fromAddrs (ar1 |> Seq.filter set2.Contains)
    | _, _, RangeRestriction.Custom ar, RangeRestriction.Fixed(lo, hi)
    | _, _, RangeRestriction.Fixed(lo, hi), RangeRestriction.Custom ar ->
        fromAddrs (ar |> Seq.filter (fun addr -> addr >= lo && addr <= hi))
    | _ -> emptyRange

