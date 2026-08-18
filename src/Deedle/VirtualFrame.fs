namespace Deedle.Virtual

// ------------------------------------------------------------------------------------------------
// Helpers that can be used when implementing Lookup in your own Deedle sources
// ------------------------------------------------------------------------------------------------

module IndexUtilsModule =
  open Deedle
  open System

  /// Binary search in range [ 0L .. count ]. The function is generic in ^T and
  /// is 'inline' so that the comparison on ^T is optimized.
  ///
  ///  - `count` specifies the upper bound for the binary search
  ///  - `valueAt` is a function that returns value ^T at the specified location
  ///  - `value` is the ^T value that we are looking for
  ///  - `lookup` is the lookup semantics as used in Deedle
  ///  - `check` is a function that tests whether we want a given location
  ///    (if no, we scan - this can be used to find the first available value in a series)
  ///
  let inline binarySearch count (valueAt:Func<int64, ^T>) value (lookup:Lookup) (check:Func<_, _>) =

    /// Binary search the 'asOfTicks' series, looking for the
    /// specified 'asOf' (the invariant is that: lo <= res < hi)
    /// The result is index 'idx' such that: 'asOfAt idx <= asOf && asOf (idx+1) > asOf'
    let rec binarySearch lo hi =
      let mid = (lo + hi) / 2L
      if lo + 1L = hi then lo
      else
        if valueAt.Invoke mid > value then binarySearch lo mid
        else binarySearch mid hi

    /// Scan the series, looking for first value that passes 'check'
    let rec scan next idx =
      if idx < 0L || idx >= count then OptionalValue.Missing
      elif check.Invoke idx then OptionalValue(idx)
      else scan next (next idx)

    if count = 0L then OptionalValue.Missing
    else
      let found = binarySearch 0L count
      match lookup with
      | Lookup.Exact ->
          // We're looking for an exact value, if it's not the one at 'idx' then Nothing
          if valueAt.Invoke found = value && check.Invoke found then OptionalValue(found)
          else OptionalValue.Missing
      | Lookup.ExactOrGreater | Lookup.ExactOrSmaller when valueAt.Invoke found = value && check.Invoke found ->
          // We found an exact match and we the lookup behaviour permits that
          OptionalValue(found)
      | Lookup.Greater | Lookup.ExactOrGreater ->
          // Otherwise we need to scan (because the found value does not work or is not allowed)
          scan ((+) 1L) (if valueAt.Invoke found <= value then found + 1L else found)
      | Lookup.Smaller | Lookup.ExactOrSmaller ->
          scan ((-) 1L) (if valueAt.Invoke found >= value then found - 1L else found)
      | _ -> invalidArg "lookup" "Unexpected Lookup behaviour"

/// Helpers that can be used when implementing Lookup
type IndexUtils =
  /// See the comment for `IndexUtilsModule.binarySearch`
  static member BinarySearch(count, valueAt, (value:int64), lookup, check) =
    IndexUtilsModule.binarySearch count valueAt value lookup check


// ------------------------------------------------------------------------------------------------
// Public API for creating virtual frames and series
// ------------------------------------------------------------------------------------------------

open Deedle
open Deedle.Ranges
open Deedle.Internal
open Deedle.Addressing
open Deedle.Vectors.Virtual
open Deedle.Indices.Virtual
open System

module Address = LinearAddress

/// <exclude />
///
/// Helper that is invoked via Reflection to create generic virtual vectors.
type VirtualVectorHelper =
  static member Create<'T>(source:IVirtualVectorSource<'T>) =
    VirtualVector<'T>(source)

/// Options for [`Virtual.ReadCsv`].
type VirtualReadCsvOptions =
  { /// Column used as ordered row index. When `None`, auto-detects `Timestamp` / `DateTime` / first parseable date column.
    IndexColumn: string option
    /// Optional searchable string column and its LookupRange mode.
    SearchColumn: (string * LookupRangeMode<string>) option
    /// Explicit column keys (defaults to all CSV columns except index column).
    ColumnKeys: string list option }

  static member Default =
    { IndexColumn = None
      SearchColumn = None
      ColumnKeys = None }

/// Provides static methods for creating virtual series and virtual frames.
/// Those provide necessary wrapping around `IVirtualVectorSource` values
type Virtual private () =
  static let createMi = typeof<VirtualVectorHelper>.GetMethod("Create")

  static let createFrame rowIndex columnIndex (sources:seq<IVirtualVectorSource>) =
    let data =
      sources
      |> Seq.map (fun source ->
          createMi.MakeGenericMethod(source.ElementType).Invoke(null, [| source |]) :?> IVector)
      |> Vector.ofValues
    Frame<_, _>(rowIndex, columnIndex, data, VirtualIndexBuilder.Instance, VirtualVectorBuilder.Instance)

  /// Creates a virtual series with ordinal index. The parameter is `IVirtualVectorSource`
  /// that specifies how to access values in the series (and is also used to determine the size
  /// of the series index)
  static member CreateOrdinalSeries(source) =
    let vector = VirtualVector(source)
    let index = VirtualOrdinalIndex(Ranges.inlineCreate (+) [ 0L, source.Length-1L ], source)
    Series(index, vector, VirtualVectorBuilder.Instance, VirtualIndexBuilder.Instance)


  /// Create a virtual series with an index and values specified by two `IVirtualVectorSource` values.
  /// The index source should support lookup (which is used for series lookup, slicing etc.)
  /// The value source does not need to implement lookup - mainly `ValueAt`, merging and getting sub-source
  static member CreateSeries(indexSource:IVirtualVectorSource<_>, valueSource:IVirtualVectorSource<_>) =
    let vector = VirtualVector(valueSource)
    let index = VirtualOrderedIndex(indexSource)
    Series(index, vector, VirtualVectorBuilder.Instance, VirtualIndexBuilder.Instance)

  /// Create a frame with ordinal index, containing the specified sources as columns.
  static member CreateOrdinalFrame(keys:seq<_>, sources:seq<IVirtualVectorSource>) =
    let count = sources |> Seq.fold (fun st src ->
      match st with
      | None -> Some(src.Length)
      | Some n when n = src.Length -> Some(n)
      | _ -> invalidArg "sources" "Sources should have the same length!" ) None
    if count = None then invalidArg "sources" "At least one column is required"
    let count = count.Value
    let source = sources |> Seq.head
    createFrame (VirtualOrdinalIndex(Ranges.inlineCreate (+) [0L, count-1L], source)) (Index.ofKeys (ReadOnlyCollection.ofSeq keys)) sources

  /// Create a frame with ordinal index, containing the specified sources as columns.
  /// The index source should support lookup (which is used for series lookup, slicing etc.)
  /// The value source does not need to implement lookup - mainly `ValueAt`, merging and getting sub-source
  static member CreateFrame(indexSource:IVirtualVectorSource<_>, keys, sources:seq<IVirtualVectorSource>) =
    createFrame (VirtualOrderedIndex indexSource) (Index.ofKeys (ReadOnlyCollection.ofSeq keys)) sources

/// Ordinal pull-on-read virtual source with optional LookupRange semantics.
type OrdinalVirtualSource<'T>
    ( length: int64,
      valueAt: int64 -> 'T,
      schemeId: string,
      ?asLong: 'T -> int64,
      ?lookupRange: LookupRangeMode<'T> ) =

  let lookupRangeMode = defaultArg lookupRange LookupRangeUnsupported
  let addressing = Indices.Linear.LinearAddressOperations(0L, length - 1L) :> IAddressOperations
  let context = sprintf "OrdinalVirtualSource<%s>" (typeof<'T>.Name)

  let rec createFromSpec (spec: LookupRangeExecutor.SubVectorSpec<'T>) =
    OrdinalVirtualSource<'T>(spec.Length, spec.ValueAt, schemeId, ?asLong=spec.AsLong, lookupRange=spec.LookupRange) :> IVirtualVectorSource<'T>

  interface IVirtualVectorSource with
    member this.Length = length
    member this.AddressingSchemeID = schemeId
    member this.ElementType = typeof<'T>
    member this.AddressOperations = addressing
    member this.Invoke(op) = op.Invoke(this :> IVirtualVectorSource<'T>)

  interface IVirtualVectorSource<'T> with
    member _.MergeWith(sources) =
      let parts =
        (length, valueAt)
        :: [ for s in sources ->
               match s with
               | :? OrdinalVirtualSource<'T> as src -> src.Length, src.RawValueAt
               | _ -> failwith "MergeWith: expected OrdinalVirtualSource" ]
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
      OrdinalVirtualSource<'T>(total, mergedValueAt, schemeId, ?asLong=asLong, lookupRange=lookupRangeMode) :> _

    member _.LookupRange(v) =
      LookupRangeExecutor.lookupRange length lookupRangeMode v context

    member _.LookupValue(k, l, check) =
      let asLongFn =
        match asLong with
        | Some g -> g
        | None -> failwith "LookupValue: asLong not configured"
      let c = Func<int64, bool>(fun i -> check.Invoke(Address.ofInt64 i))
      IndexUtilsModule.binarySearch length (Func<_, _>(fun i -> asLongFn (valueAt i))) (asLongFn k) l c
      |> OptionalValue.map (fun i -> valueAt i, Address.ofInt64 i)

    member _.ValueAt(loc) =
      OptionalValue(valueAt (Address.asInt64 loc.Address))

    member _.GetSubVector(range) =
      match LookupRangeExecutor.getSubVector length valueAt lookupRangeMode asLong range with
      | Choice1Of2 spec -> createFromSpec spec
      | Choice2Of2 _ -> invalidOp "GetSubVector: unexpected result"

  member _.Length = length
  member _.RawValueAt(i: int64) = valueAt i