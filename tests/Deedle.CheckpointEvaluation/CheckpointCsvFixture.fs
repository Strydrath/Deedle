module Deedle.CheckpointEvaluation.CsvFixture

open System
open System.Globalization
open System.IO

let words8 =
  "lorem ipsum dolor sit amet consectetur adipiscing elit".Split(' ')

let words4 =
  "alpha beta gamma delta".Split(' ')

let defaultDatasetName = "b6-search-100k-random.csv"

let private shuffleInPlace (rng: Random) (items: int[]) =
  for i in items.Length - 1 .. -1 .. 0 do
    let j = rng.Next(i + 1)
    let tmp = items.[i]
    items.[i] <- items.[j]
    items.[j] <- tmp

let ensureSearchCsv (path: string) (rowCount: int64) (seed: int) =
  let dir = Path.GetDirectoryName(path)
  if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
    Directory.CreateDirectory dir |> ignore
  if File.Exists path then path
  else
    let rng = Random(seed)
    let ids = Array.init (int rowCount) id
    shuffleInPlace rng ids
    use writer = new StreamWriter(path, false)
    writer.WriteLine("Id,Timestamp,Category,Label,Value")
    let start = DateTimeOffset(DateTime(2000, 1, 1), TimeSpan.Zero)
    for i in 0L .. rowCount - 1L do
      let id = ids.[int i]
      let cat = words8.[int (i % int64 words8.Length)]
      let label = words4.[int (i % int64 words4.Length)]
      let ts = start.AddSeconds(float i).ToString("o", CultureInfo.InvariantCulture)
      let value = rng.NextDouble() * 10000.0
      writer.WriteLine(sprintf "%d,%s,%s,%s,%s" id ts cat label (value.ToString("F4", CultureInfo.InvariantCulture)))
    path
