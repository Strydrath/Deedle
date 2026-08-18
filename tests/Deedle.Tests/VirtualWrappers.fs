#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.VirtualWrappers
#endif

open System
open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Addressing
open Deedle.Vectors
open Deedle.Vectors.Virtual
open Deedle.Virtual
open Deedle.Tests.VirtualInstrumentation

module Address = LinearAddress

let private customCount (range: RangeRestriction<Address>) =
  match range with
  | RangeRestriction.Custom ar -> Seq.length ar
  | RangeRestriction.Fixed(lo, hi) -> int (Address.asInt64 hi - Address.asInt64 lo + 1L)
  | RangeRestriction.Start n | RangeRestriction.End n -> int n

[<Test>]
let ``B10 Boxed LookupRange delegates to the inner source`` () =
  let words = [| "lorem"; "ipsum"; "dolor" |]
  let c, src = InstrumentedOrdinalSource.createSearchableStrings 30L words
  c.Reset()
  let boxed = VirtualVectorSource.boxSource src
  boxed.LookupRange(box "lorem") |> ignore
  c.Snapshot().LookupRangeCount |> shouldEqual 1

[<Test>]
let ``B10 Mapped LookupRange without reverse mapping scans and stays virtual`` () =
  let n = 64L
  let words = "lorem ipsum dolor sit amet".Split(' ')
  let c = AccessCounters()
  let start = DateTimeOffset(DateTime(2000, 1, 1), TimeSpan.FromHours(-1.0))
  let idx =
    InstrumentedOrdinalSource<DateTimeOffset>
      (n, (fun i -> start.AddTicks(i * 123456789L)), c, asLong=(fun dto -> dto.UtcTicks), hasMissing=false)
  let inner =
    InstrumentedOrdinalSource<string>
      (n, (fun i -> words.[int (i % int64 words.Length)]), c, lookupRange=LookupRangeStep (fun v -> words |> Array.findIndex ((=) v), words.Length), hasMissing=false)
  let mapped =
    VirtualVectorSource.map None (fun _ (ov: OptionalValue<string>) ->
      ov |> OptionalValue.map (fun s -> s.ToUpperInvariant())) (inner :> IVirtualVectorSource<string>)
  let frame = Virtual.CreateFrame(idx, ["UP"], [mapped :> IVirtualVectorSource])
  c.Reset()
  let filtered = frame |> Frame.filterRowsBy "UP" "LOREM"
  FrameProbe.rowIndexIsVirtual filtered |> shouldEqual true
  filtered.RowCount |> should be (greaterThan 0)
  let snap = c.Snapshot()
  snap.ValueAtCount |> should be (greaterThan 0)
  snap.LookupRangeCount |> shouldEqual 0

[<Test>]
let ``B10 Combined LookupRange scans instead of throwing`` () =
  let c, s1 = InstrumentedOrdinalSource.createFloats 16L
  let s2 = InstrumentedOrdinalSource<float>(16L, (fun i -> float (i + 1L)), c)
  let combined =
    VirtualVectorSource.combine
      (function
        | [a; b] when a.HasValue && b.HasValue -> OptionalValue(a.Value + b.Value)
        | _ -> OptionalValue.Missing)
      [ s1 :> IVirtualVectorSource<_>; s2 :> IVirtualVectorSource<_> ]
  let range = combined.LookupRange(5.0) // i + (i+1) = 2i+1 = 5 → i=2
  customCount range |> shouldEqual 1

[<Test>]
let ``B10 Row-reader LookupRange does not throw`` () =
  let _, s1 = InstrumentedOrdinalSource.createFloats 8L
  let irt =
    { new IRowReaderTransform with
        member _.ColumnAddressAt(i) = Address.ofInt64 i
      interface INaryTransform with
        member _.GetFunction<'R>() = fun (_: OptionalValue<'R> list) -> OptionalValue.Missing }
  let ctor (src: IVirtualVectorSource<float>) = VirtualVector(src) :> IVector
  let vectors = Vector.ofValues [ ctor s1 ]
  let reader =
    VirtualVectorSource.createRowReader ctor VectorBuilder.Instance irt vectors [ s1 :> IVirtualVectorSource<_> ]
  customCount (reader.LookupRange(Unchecked.defaultof<_>)) |> shouldEqual 0

[<Test>]
let ``B10 FillMissingWith constant stays virtual`` () =
  let c = AccessCounters()
  let src = InstrumentedOrdinalSource<float>(32L, float, c, hasMissing=true)
  let s = Virtual.CreateOrdinalSeries(src)
  c.Reset()
  let filled = s |> Series.fillMissingWith 0.0
  SeriesProbe.isVirtual filled |> shouldEqual true
  filled.TryGet(0L) |> shouldEqual (OptionalValue 0.0)
  filled.TryGet(1L) |> shouldEqual (OptionalValue 1.0)

[<Test>]
let ``B10 FillMissing direction stays virtual and copies the previous value`` () =
  let c = AccessCounters()
  let src = InstrumentedOrdinalSource<float>(32L, float, c, hasMissing=true)
  let s = Virtual.CreateOrdinalSeries(src)
  let filled = s |> Series.fillMissing Direction.Forward
  SeriesProbe.isVirtual filled |> shouldEqual true
  filled.TryGet(3L) |> shouldEqual (OptionalValue 2.0)

[<Test>]
let ``B10 AsyncMaterialize on a virtual series succeeds with a linear result`` () =
  let _, s = InstrumentedOrdinalSource.createOrdinalSeries 32L
  let mat = s.AsyncMaterialize() |> Async.RunSynchronously
  SeriesProbe.isLinear mat |> shouldEqual true
  mat.TryGet(7L) |> shouldEqual (OptionalValue 7L)

[<Test>]
let ``B10 AsyncBuild with a virtual scheme raises NotSupportedException`` () =
  let _, s = InstrumentedOrdinalSource.createOrdinalSeries 8L
  let ex =
    Assert.Throws<NotSupportedException>(fun () ->
      (VirtualVectorBuilder.Instance :> IVectorBuilder)
        .AsyncBuild(s.Vector.AddressingScheme, Return 0, [| s.Vector |])
      |> Async.RunSynchronously
      |> ignore)
  ex.Message.Contains("Materialize") |> shouldEqual true
