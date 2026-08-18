#if INTERACTIVE
#I "../../bin/netstandard2.0"
#load "Deedle.fsx"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#r "../../packages/FsUnit/lib/net45/FsUnit.NUnit.dll"
#load "../Common/FsUnit.fs"
#else
module Deedle.Tests.CsvFileVirtualSource
#endif

open System
open Deedle
open Deedle.Virtual
open Deedle.Virtual.Sources
open Deedle.Vectors.Virtual
open Deedle.Tests.VirtualInstrumentation

/// Instrumented wrappers around library CSV virtual sources (B6 harness).
module CsvHarness =
  let defaultDatasetName = CsvTestData.defaultDatasetName
  let defaultSeed = CsvTestData.defaultSeed
  let words8 = CsvTestData.words8
  let ensureSearchCsv = CsvTestData.ensureSearchCsv
  let ensureSearchCsvWithSeed = CsvTestData.ensureSearchCsvWithSeed
  let readMeta = CsvTestData.readMeta
  let generateSearchCsv = CsvTestData.generateSearchCsv

module CsvFileVirtualSource =
  let private wrap (counters: AccessCounters) (source: IVirtualVectorSource) =
    CountingVirtualSource.Wrap counters source

  let createOrderedSearchFrame (csvPath: string) (counters: AccessCounters) =
    let lineIndex = Deedle.Virtual.Sources.CsvLineIndex(csvPath)
    let idx =
      wrap counters (CsvVirtualSource.createIndexSource lineIndex "Timestamp")
      :?> IVirtualVectorSource<DateTimeOffset>
    let idCol = wrap counters (CsvVirtualSource.createColumnSource lineIndex "Id" None)
    let catCol =
      wrap counters
        (CsvVirtualSource.createColumnSource lineIndex "Category"
          (Some(VirtualLookupRange.forRepeatingCycle CsvTestData.words8)))
    let frame = Virtual.CreateFrame(idx, [ "S1"; "S2" ], [ idCol; catCol ])
    counters, frame, CsvTestData.words8

  let createFloatValueSeries (csvPath: string) (counters: AccessCounters) =
    let lineIndex = Deedle.Virtual.Sources.CsvLineIndex(csvPath)
    let src =
      wrap counters (CsvVirtualSource.createColumnSource lineIndex "Value" None)
      :?> IVirtualVectorSource<float>
    counters, Virtual.CreateOrdinalSeries(src)
