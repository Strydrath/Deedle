#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.VirtualIndex
#endif

open System
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Virtual
open Deedle.Vectors.Virtual
open Deedle.Tests.VirtualInstrumentation

// ------------------------------------------------------------------------------------------------
// Virtual index builder (src/Deedle/Indices/VirtualIndex.fs)
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``Can shift virtual frame without ValueAt at shift time`` () =
  let c, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrameWith 64L (LookupRangeStep (fun _ -> 0, 1))
  c.Reset()
  let shifted = frame |> Frame.shift 1
  FrameProbe.rowIndexIsVirtual shifted |> shouldEqual true
  c.Snapshot().ValueAtCount |> shouldEqual 0
  shifted.RowCount |> shouldEqual 63

[<Test>]
let ``Can diff virtual frame staying virtual until read`` () =
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
let ``Can filterRowsBy2 fusing two predicates on same column`` () =
  let _, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrame 64L
  let fused = frame |> Frame.filterRowsBy2 "S2" words.[0] "S2" words.[0]
  FrameProbe.rowIndexIsVirtual fused |> shouldEqual true
  fused.RowCount |> shouldEqual (64 / words.Length)

[<Test>]
let ``Can filterRowsBy2 on disjoint values yielding empty frame`` () =
  let _, frame, words = InstrumentedOrdinalSource.createOrderedSearchFrameWith 64L (LookupRangeStep (fun _ -> 0, 1))
  (frame |> Frame.filterRowsBy2 "S2" words.[0] "S2" words.[1]).RowCount |> shouldEqual 0

[<Test>]
let ``Chained filterRowsBy on Step index shrinks rows; filterRowsBy2 preserves count (regression)`` () =
  let n = 1000L
  let words = "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')
  let _, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrame n
  let once = frame |> Frame.filterRowsBy "S2" words.[0]
  let twice = frame |> Frame.filterRowsBy "S2" words.[0] |> Frame.filterRowsBy "S2" words.[0]
  twice.RowCount |> should be (lessThan once.RowCount)
  (frame |> Frame.filterRowsBy2 "S2" words.[0] "S2" words.[0]).RowCount |> shouldEqual once.RowCount

[<Test>]
let ``Can filterRowsBy2 combining Step and IndexList columns`` () =
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
      (n, valueAt, c, lookupRange=VirtualLookupRangeTest.repeatingCycle words, hasMissing=false)
  let listCol =
    InstrumentedOrdinalSource<string>
      (n, valueAt, c, lookupRange=VirtualLookupRange.forCategoricalScan n valueAt, hasMissing=false)
  let frame = Virtual.CreateFrame(idx, ["Step"; "List"], [stepCol :> IVirtualVectorSource; listCol :> IVirtualVectorSource])
  let fused = frame |> Frame.filterRowsBy2 "Step" "lorem" "List" "lorem"
  FrameProbe.rowIndexIsVirtual fused |> shouldEqual true
  fused.RowCount |> shouldEqual (frame |> Frame.filterRowsBy "Step" "lorem").RowCount

[<Test>]
let ``Can filter virtual frame without reading unused columns`` () =
  let n = 64L
  let words = "lorem ipsum dolor sit amet".Split(' ')
  let cUnused = AccessCounters()
  let cSearch = AccessCounters()
  let start = DateTimeOffset(DateTime(2000, 1, 1), TimeSpan.FromHours(-1.0))
  let idx =
    InstrumentedOrdinalSource<DateTimeOffset>
      (n, (fun i -> start.AddTicks(i * 123456789L)), AccessCounters(),
       asLong=(fun dto -> dto.UtcTicks), hasMissing=false)
  let unused = InstrumentedOrdinalSource<int64>(n, id, cUnused, asLong=id, hasMissing=false)
  let search =
    InstrumentedOrdinalSource<string>
      (n, (fun i -> words.[int (i % int64 words.Length)]), cSearch,
       lookupRange=VirtualLookupRangeTest.repeatingCycle words, hasMissing=false)
  let frame = Virtual.CreateFrame(idx, ["U"; "S2"], [unused :> IVirtualVectorSource; search :> IVirtualVectorSource])
  cUnused.Reset()
  cSearch.Reset()
  frame |> Frame.filterRowsBy "S2" "lorem" |> ignore
  cUnused.Snapshot().ValueAtCount |> shouldEqual 0
  cSearch.Snapshot().ValueAtCount |> shouldEqual 0

[<Test>]
let ``Can outer join mismatched ordinal virtual frames materializing index`` () =
  let _, s1 = InstrumentedOrdinalSource.createFloats 64L
  let _, s2 = InstrumentedOrdinalSource.createFloats 32L
  let f1 = Virtual.CreateOrdinalFrame(["A"], [s1 :> IVirtualVectorSource])
  let f2 = Virtual.CreateOrdinalFrame(["B"], [s2 :> IVirtualVectorSource])
  FrameProbe.rowIndexIsVirtual (f1.Join(f2, JoinKind.Outer)) |> shouldEqual false

[<Test>]
let ``Can outer join identical ordinal virtual frames staying virtual`` () =
  let _, s1 = InstrumentedOrdinalSource.createFloats 10_000L
  let _, s2 = InstrumentedOrdinalSource.createFloats 10_000L
  let f1 = Virtual.CreateOrdinalFrame(["A"], [s1 :> IVirtualVectorSource])
  let f2 = Virtual.CreateOrdinalFrame(["B"], [s2 :> IVirtualVectorSource])
  FrameProbe.rowIndexIsVirtual (f1.Join(f2, JoinKind.Outer)) |> shouldEqual true

[<Test>]
let ``Can filter ordered virtual frame via LookupRange without ValueAt`` () =
  let c, (f: Frame<DateTimeOffset, string>), _ = InstrumentedOrdinalSource.createOrderedSearchFrame 100_000L
  c.Reset()
  let filtered = f |> Frame.filterRowsBy "S2" "lorem"
  c.Snapshot().LookupRangeCount |> should be (greaterThan 0)
  c.Snapshot().ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true

[<Test>]
let ``Can filter ordinal virtual frame via LookupRange without ValueAt`` () =
  let words = "lorem ipsum dolor sit amet".Split(' ')
  let c, s2 = InstrumentedOrdinalSource.createSearchableStrings 10_000L words
  let _, s1 = InstrumentedOrdinalSource.createLongs 10_000L
  let f = Virtual.CreateOrdinalFrame(["S1"; "S2"], [s1 :> IVirtualVectorSource; s2 :> IVirtualVectorSource])
  c.Reset()
  let filtered = f |> Frame.filterRowsBy "S2" "lorem"
  c.Snapshot().LookupRangeCount |> should be (greaterThan 0)
  c.Snapshot().ValueAtCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true

[<Test>]
let ``Ordinal filterRowsBy without LookupRange throws NotSupportedException`` () =
  let words = "lorem ipsum dolor sit amet".Split(' ')
  let s2 = InstrumentedOrdinalSource<string>(100L, (fun i -> words.[int (i % int64 words.Length)]), AccessCounters(), hasMissing=false)
  let _, s1 = InstrumentedOrdinalSource.createLongs 100L
  let frame = Virtual.CreateOrdinalFrame(["S1"; "S2"], [s1 :> IVirtualVectorSource; s2 :> IVirtualVectorSource])
  (fun () -> frame |> Frame.filterRowsBy "S2" "lorem" |> ignore)
  |> should throw typeof<NotSupportedException>

[<Test>]
let ``Can filter virtual frame to empty result when no rows match`` () =
  let _, frame, _ = InstrumentedOrdinalSource.createOrderedSearchFrame 64L
  let filtered = frame |> Frame.filterRowsBy "S2" "definitely-not-in-vocabulary"
  filtered.RowCount |> shouldEqual 0
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
