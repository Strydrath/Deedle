module Deedle.CheckpointEvaluation.Main

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json
open Deedle.CheckpointEvaluation.Measure
open Deedle.CheckpointEvaluation.Operations

module private Report =
  let toMarkdown (checkpoint: string) (sha: string) (rows: OpMetrics list) =
    let sb = StringBuilder()
    let append (line: string) = sb.AppendLine(line) |> ignore
    append "# Checkpoint evaluation report"
    append ""
    append (sprintf "- **Checkpoint:** %s" checkpoint)
    append (sprintf "- **Generated:** %s" (DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")))
    append (sprintf "- **Deedle commit:** `%s`" sha)
    append "- **Harness:** `tests/Deedle.CheckpointEvaluation` (warmup=2, iterations=5)"
    append ""
    append "## Results (time + memory + ValueAt)"
    append ""
    append "| Operation | Category | Path | Verdict | Mean ms | Alloc bytes | ValueAt | LookupRange | GetSubVector | Notes |"
    append "|-----------|----------|------|---------|--------:|------------:|--------:|------------:|-------------:|-------|"
    for r in rows do
      append (
        sprintf "| %s | %s | %s | **%s** | %.3f | %d | %d | %d | %d | %s |"
          r.Operation r.Category r.Path r.Verdict r.MeanMs r.AllocBytes r.ValueAt r.LookupRange r.GetSubVector r.Notes)
    append ""
    append "ValueAt = -1 for file/materialized paths without instrumented counters."
    append ""
    sb.ToString()

  let toJson (checkpoint: string) (sha: string) (rows: OpMetrics list) =
    let payload =
      {| checkpoint = checkpoint
         commit = sha
         generated = DateTime.Now
         rows = rows |}
    JsonSerializer.Serialize(payload, JsonSerializerOptions(WriteIndented = true))

[<EntryPoint>]
let main argv =
  let checkpoint =
    argv
    |> Array.tryFind (fun a -> a.StartsWith("--checkpoint=", StringComparison.OrdinalIgnoreCase))
    |> Option.map (fun a -> a.Substring("--checkpoint=".Length))
    |> Option.defaultValue "CP6"

  let outBase =
    argv
    |> Array.tryFind (fun a -> a.StartsWith("--out=", StringComparison.OrdinalIgnoreCase))
    |> Option.map (fun a -> a.Substring("--out=".Length))
    |> Option.defaultValue (
      Path.GetFullPath(
        Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", "..", "big-deedle", "harness", "results", sprintf "checkpoint-%s-evaluation" checkpoint)))

  let sha =
    try
      let deedleRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", ".."))
      let psi =
        ProcessStartInfo(FileName = "git", Arguments = "rev-parse --short HEAD", RedirectStandardOutput = true, UseShellExecute = false)
      psi.WorkingDirectory <- deedleRoot
      use p = Process.Start(psi)
      let text = p.StandardOutput.ReadToEnd().Trim()
      p.WaitForExit()
      if String.IsNullOrWhiteSpace text then "unknown" else text
    with _ ->
      "unknown"

  printfn "Running checkpoint evaluation (%s)..." checkpoint
  let rows = runAll checkpoint
  let mdPath = if outBase.EndsWith(".md", StringComparison.OrdinalIgnoreCase) then outBase else outBase + ".md"
  let jsonPath = Path.ChangeExtension(mdPath, ".json")
  let dir = Path.GetDirectoryName mdPath
  if not (String.IsNullOrEmpty dir) then Directory.CreateDirectory dir |> ignore
  File.WriteAllText(mdPath, Report.toMarkdown checkpoint sha rows, Encoding.UTF8)
  File.WriteAllText(jsonPath, Report.toJson checkpoint sha rows, Encoding.UTF8)
  printfn "Wrote %s" mdPath
  printfn "Wrote %s" jsonPath
  for r in rows do
    printfn "  [%s/%s] %s  %.2f ms  alloc=%d  ValueAt=%d" r.Path r.Verdict r.Operation r.MeanMs r.AllocBytes r.ValueAt
  0
