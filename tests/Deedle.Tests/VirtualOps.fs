#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.VirtualOps
#endif

open System
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Addressing
open Deedle.Virtual
open Deedle.Vectors
open Deedle.Vectors.Virtual
open Deedle.Tests.VirtualInstrumentation

module Address = LinearAddress

module private Range =
  let step offset step =
    RangeRestriction.Custom { Offset = offset; Step = step } : RangeRestriction<Address>

[<Test>]
let ``Diff stays virtual and matches materialized values`` () =
  let c, s = InstrumentedOrdinalSource.createFloatSeries 32L
  c.Reset()
  let d = s |> Series.diff 1
  SeriesProbe.isVirtual d |> shouldEqual true
  c.Snapshot().ValueAtCount |> shouldEqual 0
  d.[1L] |> shouldEqual (s.[1L] - s.[0L])

[<Test>]
let ``Shift on ordered virtual series stays virtual`` () =
  let c, s = InstrumentedOrdinalSource.createOrderedFloatSeries 64L
  c.Reset()
  let shifted = s |> Series.shift 1
  SeriesProbe.isVirtual shifted |> shouldEqual true
  c.Snapshot().ValueAtCount |> shouldEqual 0
  shifted.KeyCount |> shouldEqual 63

[<Test>]
let ``Shift with negative offset stays virtual`` () =
  let c, s = InstrumentedOrdinalSource.createOrderedFloatSeries 64L
  c.Reset()
  let shifted = s |> Series.shift -1
  SeriesProbe.isVirtual shifted |> shouldEqual true
  c.Snapshot().ValueAtCount |> shouldEqual 0
  shifted.KeyCount |> shouldEqual 63
  shifted.GetAt(0) |> shouldEqual (s.GetAt(1))

[<Test>]
let ``Frame.shift stays virtual without ValueAt`` () =
  let c, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrameWith 64L (LookupRangeStep (fun _ -> 0, 1))
  c.Reset()
  let shifted = frame |> Frame.shift 1
  FrameProbe.rowIndexIsVirtual shifted |> shouldEqual true
  c.Snapshot().ValueAtCount |> shouldEqual 0
  shifted.RowCount |> shouldEqual 63

[<Test>]
let ``Frame.diff stays virtual until read`` () =
  let n = 32L
  let c = AccessCounters()
  let start = DateTimeOffset(DateTime(2000, 1, 1), TimeSpan.FromHours(-1.0))
  let idx =
    InstrumentedOrdinalSource<DateTimeOffset>
      (n, (fun i -> start.AddTicks(i * 123456789L)), c, asLong=(fun dto -> dto.UtcTicks), hasMissing=false)
  let vals = InstrumentedOrdinalSource<float>(n, float, c, hasMissing=false)
  let frame = Virtual.CreateFrame(idx, ["V"], [vals :> IVirtualVectorSource])
  c.Reset()
  let d = frame |> Frame.diff 1
  FrameProbe.rowIndexIsVirtual d |> shouldEqual true
  c.Snapshot().ValueAtCount |> shouldEqual 0
  d.GetColumn<float>("V").GetAt(0) |> shouldEqual 1.0

[<Test>]
let ``LookupRangeExecutor.intersect of identical Step ranges is identity`` () =
  match LookupRangeExecutor.intersect (Range.step 0 8) (Range.step 0 8) with
  | RangeRestriction.Custom(:? StepRange as s) ->
      s.Offset |> shouldEqual 0
      s.Step |> shouldEqual 8
  | other -> failwithf "expected StepRange, got %A" other

[<Test>]
let ``LookupRangeExecutor.intersect of disjoint Step ranges is empty`` () =
  match LookupRangeExecutor.intersect (Range.step 0 8) (Range.step 1 8) with
  | RangeRestriction.Custom ar -> Seq.length ar |> shouldEqual 0
  | other -> failwithf "expected empty custom range, got %A" other

[<Test>]
let ``WindowSizeInto id windows stay virtual until aggregated`` () =
  let c, s = InstrumentedOrdinalSource.createFloatSeries 64L
  c.Reset()
  let windows = s |> Series.windowSizeInto (4, Boundary.Skip) DataSegment.data
  c.Snapshot().ValueAtCount |> shouldEqual 0
  let w0 = windows.GetAt(0)
  SeriesProbe.isVirtual w0 |> shouldEqual true
  Stats.sum w0 |> shouldEqual (0.0 + 1.0 + 2.0 + 3.0)

[<Test>]
let ``filterRowsBy2 fuses two LookupRanges into one frame rebuild`` () =
  let c, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrameWith 64L (LookupRangeStep (fun _ -> 0, 1))
  let words = "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')
  c.Reset()
  let fused = frame |> Frame.filterRowsBy2 "S2" words.[0] "S2" words.[0]
  let fusedDelta = c.Snapshot()
  FrameProbe.rowIndexIsVirtual fused |> shouldEqual true
  fusedDelta.LookupRangeCount |> shouldEqual 2
  c.Reset()
  let once = frame |> Frame.filterRowsBy "S2" words.[0]
  fused.RowCount |> shouldEqual once.RowCount

  c.Reset()
  let chained =
    frame
    |> Frame.filterRowsBy "S2" words.[0]
    |> Frame.filterRowsBy "S2" words.[0]
  let chainedDelta = c.Snapshot()
  chainedDelta.GetSubVectorCount |> should be (greaterThan fusedDelta.GetSubVectorCount)

[<Test>]
let ``filterRowsBy2 of disjoint values on the same column is empty`` () =
  let _, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrameWith 64L (LookupRangeStep (fun _ -> 0, 1))
  let empty = frame |> Frame.filterRowsBy2 "S2" words.[0] "S2" words.[1]
  empty.RowCount |> shouldEqual 0

[<Test>]
let ``filterRowsBy2 Step intersect IndexList does not throw`` () =
  let n = 64L
  let words = "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')
  let valueAt i = words.[int (i % int64 words.Length)]
  let c = AccessCounters()
  let start = DateTimeOffset(DateTime(2000, 1, 1), TimeSpan.FromHours(-1.0))
  let idx =
    InstrumentedOrdinalSource<DateTimeOffset>
      (n, (fun i -> start.AddTicks(i * 123456789L)), c, asLong=(fun dto -> dto.UtcTicks), hasMissing=false)
  let stepCol =
    InstrumentedOrdinalSource<string>
      (n, valueAt, c,
       lookupRange=LookupRangeStep (fun v -> words |> Array.findIndex ((=) v), words.Length),
       hasMissing=false)
  let listCol =
    InstrumentedOrdinalSource<string>
      (n, valueAt, c, lookupRange=VirtualLookupRange.forCategoricalScan n valueAt, hasMissing=false)
  let frame =
    Virtual.CreateFrame(idx, ["Step"; "List"], [stepCol :> IVirtualVectorSource; listCol :> IVirtualVectorSource])
  let fused = frame |> Frame.filterRowsBy2 "Step" "lorem" "List" "lorem"
  FrameProbe.rowIndexIsVirtual fused |> shouldEqual true
  let expected = frame |> Frame.filterRowsBy "Step" "lorem"
  fused.RowCount |> shouldEqual expected.RowCount

[<Test>]
let ``LookupRangeExecutor.intersect Step with IndexList stays enumerable`` () =
  let step = RangeRestriction.Custom { Offset = 0; Step = 2 } : RangeRestriction<Address>
  let listAddrs = [ 0L; 2L; 3L; 4L; 7L ] |> List.map Address.ofInt64
  let list =
    ({ new IRangeRestriction<Address> with
        member _.Count = int64 listAddrs.Length
       interface seq<Address> with
         member _.GetEnumerator() = (listAddrs :> seq<_>).GetEnumerator()
       interface System.Collections.IEnumerable with
         member _.GetEnumerator() = (listAddrs :> seq<_>).GetEnumerator() :> System.Collections.IEnumerator }
     |> RangeRestriction.Custom)
  let hit = LookupRangeExecutor.intersect step list
  let addrsOf = function
    | RangeRestriction.Custom ar -> ar |> Seq.map Address.asInt64 |> Seq.toList
    | _ -> failwith "expected Custom range"
  // Even addresses from the list: 0, 2, 4
  addrsOf hit |> shouldEqual [ 0L; 2L; 4L ]
  // Symmetric order
  addrsOf (LookupRangeExecutor.intersect list step) |> shouldEqual [ 0L; 2L; 4L ]

[<Test>]
let ``Projection pushdown does not ValueAt unused columns at filter time`` () =
  let n = 64L
  let words = "lorem ipsum dolor sit amet".Split(' ')
  let cIdx = AccessCounters()
  let cUnused = AccessCounters()
  let cSearch = AccessCounters()
  let start = DateTimeOffset(DateTime(2000, 1, 1), TimeSpan.FromHours(-1.0))
  let idx =
    InstrumentedOrdinalSource<DateTimeOffset>
      (n, (fun i -> start.AddTicks(i * 123456789L)), cIdx, asLong=(fun dto -> dto.UtcTicks), hasMissing=false)
  let unused =
    InstrumentedOrdinalSource<int64>(n, id, cUnused, asLong=id, hasMissing=false)
  let search =
    InstrumentedOrdinalSource<string>
      (n, (fun i -> words.[int (i % int64 words.Length)]), cSearch,
       lookupRange=LookupRangeStep (fun v -> words |> Array.findIndex ((=) v), words.Length), hasMissing=false)
  let frame = Virtual.CreateFrame(idx, ["U"; "S2"], [unused :> IVirtualVectorSource; search :> IVirtualVectorSource])
  cUnused.Reset()
  cSearch.Reset()
  let filtered = frame |> Frame.filterRowsBy "S2" "lorem"
  cUnused.Snapshot().ValueAtCount |> shouldEqual 0
  cSearch.Snapshot().ValueAtCount |> shouldEqual 0
  filtered.GetColumn<int64>("U").GetAt(0) |> ignore
  cUnused.Snapshot().ValueAtCount |> should be (greaterThan 0)
  // Reading U must not decode the search column.
  cSearch.Snapshot().ValueAtCount |> shouldEqual 0

[<Test>]
let ``Inner zip of overlapping ordinal slices stays virtual`` () =
  let _, s = InstrumentedOrdinalSource.createOrdinalSeries 1_000L
  let a = s.[0L .. 500L]
  let b = s.[250L .. 750L]
  let zipped = Series.zipAlign JoinKind.Inner Lookup.Exact a b
  SeriesProbe.isVirtual zipped |> shouldEqual true
  zipped.KeyCount |> shouldEqual 251
  zipped.GetAt(0) |> shouldEqual (OptionalValue 250L, OptionalValue 250L)

[<Test>]
let ``Outer join of mismatched ordinal lengths materializes`` () =
  let _, s1 = InstrumentedOrdinalSource.createFloats 64L
  let _, s2 = InstrumentedOrdinalSource.createFloats 32L
  let f1 = Virtual.CreateOrdinalFrame(["A"], [s1 :> IVirtualVectorSource])
  let f2 = Virtual.CreateOrdinalFrame(["B"], [s2 :> IVirtualVectorSource])
  let joined = f1.Join(f2, JoinKind.Outer)
  FrameProbe.rowIndexIsVirtual joined |> shouldEqual false

[<Test>]
let ``GroupBy nested series are linear after a full pull`` () =
  let c, s = InstrumentedOrdinalSource.createFloatSeries 64L
  let grouped = s |> Series.groupBy (fun _k v -> int v % 4)
  SeriesProbe.isLinear (grouped.GetAt(0)) |> shouldEqual true
  c.Snapshot().ValueAtCount |> should be (greaterThan 0)

[<Test>]
let ``chained Step filter same value may shrink rows; use filterRowsBy2`` () =
  let n = 1000L
  let words = "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')
  let _, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrame n
  let once = frame |> Frame.filterRowsBy "S2" words.[0]
  let twice =
    frame
    |> Frame.filterRowsBy "S2" words.[0]
    |> Frame.filterRowsBy "S2" words.[0]
  twice.RowCount |> should be (lessThan once.RowCount)
  let fused = frame |> Frame.filterRowsBy2 "S2" words.[0] "S2" words.[0]
  fused.RowCount |> shouldEqual once.RowCount

[<Test>]
let ``chained filterRowsBy2 still preferred for two predicates on same column`` () =
  let c, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrame 64L
  c.Reset()
  let fused = frame |> Frame.filterRowsBy2 "S2" words.[0] "S2" words.[0]
  let fusedDelta = c.Snapshot()
  c.Reset()
  let chained =
    frame
    |> Frame.filterRowsBy "S2" words.[0]
    |> Frame.filterRowsBy "S2" words.[0]
  chained.RowCount |> should be (lessThan (64 / words.Length))
  fused.RowCount |> shouldEqual (64 / words.Length)
  let chainedDelta = c.Snapshot()
  chainedDelta.GetSubVectorCount |> should be (greaterThan fusedDelta.GetSubVectorCount)
