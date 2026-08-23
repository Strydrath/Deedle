#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.VirtualVector
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

// ------------------------------------------------------------------------------------------------
// VirtualVectorSource wrappers (src/Deedle/Vectors/VirtualVector.fs)
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``Can read ValueAt through VirtualVector wrapper`` () =
  let src = OrdinalVirtualSource(4L, (fun i -> OptionalValue(i + 10L)), "test")
  let vec = VirtualVector(src) :> IVector<int64>
  vec.GetValueAtLocation(KnownLocation(Address.ofInt64 2L, 2L)) |> shouldEqual (OptionalValue 12L)

[<Test>]
let ``Can delegate LookupRange through boxed virtual source`` () =
  let words = [| "lorem"; "ipsum"; "dolor" |]
  let c, src = InstrumentedOrdinalSource.createSearchableStrings 30L words
  c.Reset()
  VirtualVectorSource.boxSource(src).LookupRange(box "lorem") |> ignore
  c.Snapshot().LookupRangeCount |> shouldEqual 1

[<Test>]
let ``Can scan LookupRange on mapped virtual source without reverse mapping`` () =
  let c, src = InstrumentedOrdinalSource.createFloats 16L
  let mapped = VirtualVectorSource.map None (fun _ ov -> OptionalValue.map (fun v -> v + 1.0) ov) src
  c.Reset()
  customCount (mapped.LookupRange(3.0)) |> shouldEqual 1
  c.Snapshot().ValueAtCount |> should be (greaterThanOrEqualTo 1)

[<Test>]
let ``Can scan combined virtual source LookupRange instead of throwing`` () =
  let c, s1 = InstrumentedOrdinalSource.createFloats 16L
  let s2 = InstrumentedOrdinalSource<float>(16L, (fun i -> float (i + 1L)), c)
  let combined =
    VirtualVectorSource.combine
      (function
        | [a; b] when a.HasValue && b.HasValue -> OptionalValue(a.Value + b.Value)
        | _ -> OptionalValue.Missing)
      [ s1 :> IVirtualVectorSource<_>; s2 :> IVirtualVectorSource<_> ]
  customCount (combined.LookupRange(5.0)) |> shouldEqual 1

[<Test>]
let ``Row reader virtual source LookupRange does not throw`` () =
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
let ``AsyncBuild on virtual scheme raises NotSupportedException`` () =
  let _, s = InstrumentedOrdinalSource.createOrdinalSeries 8L
  let ex =
    Assert.Throws<NotSupportedException>(fun () ->
      (VirtualVectorBuilder.Instance :> IVectorBuilder)
        .AsyncBuild(s.Vector.AddressingScheme, Return 0, [| s.Vector |])
      |> Async.RunSynchronously
      |> ignore)
  ex.Message.Contains("Materialize") |> shouldEqual true
