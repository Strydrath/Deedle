module Deedle.CheckpointEvaluation.Measure

open System
open System.Diagnostics
open Deedle.Tests.VirtualInstrumentation

type OpMetrics =
  { Operation: string
    Category: string
    Path: string
    Shape: string
    Verdict: string
    MeanMs: float
    AllocBytes: int64
    ValueAt: int
    LookupValue: int
    LookupRange: int
    GetSubVector: int
    MergeWith: int
    Notes: string }

let private warmup = 2
let private iterations = 5

let private measureOnce (action: unit -> unit) =
  GC.Collect()
  GC.WaitForPendingFinalizers()
  GC.Collect()
  let beforeAlloc = GC.GetAllocatedBytesForCurrentThread()
  let sw = Stopwatch.StartNew()
  action()
  sw.Stop()
  let alloc = GC.GetAllocatedBytesForCurrentThread() - beforeAlloc
  sw.Elapsed.TotalMilliseconds, alloc

let measureTimed (action: unit -> unit) =
  for _ = 1 to warmup do action()
  let samples = [| for _ in 1 .. iterations -> measureOnce action |]
  samples |> Array.averageBy fst, samples |> Array.averageBy (fun (_, alloc) -> float alloc) |> int64

let verdict shape valueAt (length: int64) =
  let fullPull = int64 valueAt >= length && length > 0L
  match shape with
  | "FullyVirtual" | "FrameRowVirtual" when fullPull -> "VIRTUAL (pull)"
  | "FullyVirtual" | "FrameRowVirtual" -> "VIRTUAL"
  | _ when fullPull -> "MATERIALIZE"
  | _ -> "MATERIALIZE"

let shapeOfSeries (s: Deedle.Series<'K, 'V>) =
  match SeriesProbe.classify s with
  | FullyVirtual -> "FullyVirtual"
  | FullyLinear -> "FullyLinear"
  | Mixed(i, v) -> sprintf "Mixed(%A,%A)" i v

let shapeOfFrame (f: Deedle.Frame<'R, 'C>) =
  if FrameProbe.rowIndexIsVirtual f then "FrameRowVirtual" else "FrameRowLinear"

let fromCounters op category path shape notes length (meanMs, alloc, d: AccessSnapshot) =
  { Operation = op
    Category = category
    Path = path
    Shape = shape
    Verdict = verdict shape d.ValueAtCount length
    MeanMs = meanMs
    AllocBytes = alloc
    ValueAt = d.ValueAtCount
    LookupValue = d.LookupValueCount
    LookupRange = d.LookupRangeCount
    GetSubVector = d.GetSubVectorCount
    MergeWith = d.MergeWithCount
    Notes = notes }

let measureSeries op category path length notes (setup: unit -> AccessCounters * Deedle.Series<'K, 'V>) (run: Deedle.Series<'K, 'V> -> Deedle.Series<'K2, 'V2>) =
  let c, s = setup()
  let meanMs, alloc =
    measureTimed (fun () ->
      c.Reset()
      let before = c.Snapshot()
      let result = run s
      shapeOfSeries result |> ignore
      AccessSnapshot.delta before (c.Snapshot()) |> ignore)
  c.Reset()
  let before = c.Snapshot()
  let result = run s
  let d = AccessSnapshot.delta before (c.Snapshot())
  fromCounters op category path (shapeOfSeries result) notes length (meanMs, alloc, d)

let measureFrame op category path length notes (setup: unit -> AccessCounters * Deedle.Frame<'R, 'C>) (run: Deedle.Frame<'R, 'C> -> Deedle.Frame<'R2, 'C2>) =
  let c, f = setup()
  let meanMs, alloc =
    measureTimed (fun () ->
      c.Reset()
      let before = c.Snapshot()
      let result = run f
      shapeOfFrame result |> ignore
      AccessSnapshot.delta before (c.Snapshot()) |> ignore)
  c.Reset()
  let before = c.Snapshot()
  let result = run f
  let d = AccessSnapshot.delta before (c.Snapshot())
  fromCounters op category path (shapeOfFrame result) notes length (meanMs, alloc, d)

let measurePull op category path length notes (setup: unit -> AccessCounters * Deedle.Series<'K, 'V>) (run: Deedle.Series<'K, 'V> -> unit) =
  let c, s = setup()
  let meanMs, alloc =
    measureTimed (fun () ->
      c.Reset()
      let before = c.Snapshot()
      run s
      AccessSnapshot.delta before (c.Snapshot()) |> ignore)
  c.Reset()
  let before = c.Snapshot()
  run s
  let d = AccessSnapshot.delta before (c.Snapshot())
  fromCounters op category path (shapeOfSeries s) notes length (meanMs, alloc, d)

let measurePlain op category path shape notes (action: unit -> unit) =
  let meanMs, alloc = measureTimed action
  { Operation = op
    Category = category
    Path = path
    Shape = shape
    Verdict = "MATERIALIZE"
    MeanMs = meanMs
    AllocBytes = alloc
    ValueAt = -1
    LookupValue = 0
    LookupRange = 0
    GetSubVector = 0
    MergeWith = 0
    Notes = notes }

let tryOp op category f =
  try f()
  with e ->
    { Operation = op
      Category = category
      Path = "n/a"
      Shape = "n/a"
      Verdict = "INCOMPLETE"
      MeanMs = 0.0
      AllocBytes = 0L
      ValueAt = 0
      LookupValue = 0
      LookupRange = 0
      GetSubVector = 0
      MergeWith = 0
      Notes = e.GetType().Name + ": " + e.Message }

let zeroSnapshot =
  { ValueAtCount = 0
    LookupValueCount = 0
    LookupRangeCount = 0
    GetSubVectorCount = 0
    MergeWithCount = 0
    ValueAtAddressList = [] }
