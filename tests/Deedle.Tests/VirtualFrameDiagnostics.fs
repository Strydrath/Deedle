#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#load "VirtualInstrumentation.fs"
#else
module Deedle.Tests.VirtualFrameDiagnostics
#endif

open FsUnit
open NUnit.Framework
open Deedle
open Deedle.Virtual
open Deedle.Tests.VirtualInstrumentation

[<Test>]
let ``GetRowIndexKind and Describe distinguish ordered vs ordinal virtual frames`` () =
  let _, ordered, _ = InstrumentedOrdinalSource.createOrderedSearchFrame 10L
  let _, ordinal, _ = InstrumentedOrdinalSource.createOrdinalSearchFrame 10L
  VirtualFrameDiagnostics.GetRowIndexKind ordered |> shouldEqual VirtualRowIndexKind.OrderedVirtual
  VirtualFrameDiagnostics.GetRowIndexKind ordinal |> shouldEqual VirtualRowIndexKind.OrdinalVirtual
  VirtualFrameDiagnostics.Describe ordered |> should haveSubstring "ordered virtual"
  VirtualFrameDiagnostics.IsVirtual ordered |> shouldEqual true
  VirtualFrameDiagnostics.IsVirtual ordinal |> shouldEqual true
