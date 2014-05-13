module JGN

open System.IO
open System.Text.RegularExpressions
open System.Text

let file = "//192.168.1.3/csv1f/SOSIKI.csv"
let outputFile = "f:/20131025/法人事業主.csv"

let lineString (line:string list) =
    Printf.sprintf "%07d01\n" (line.[0] |> int)

let lineString2 (line:string list) =
    Printf.sprintf "%07d\n" (line.[0] |> int)

[<EntryPoint>]
let main arg =
    use _w = new System.IO.StreamWriter(outputFile, false, Encoding.GetEncoding("utf-8")) in
    Util.CSVseq.read file "sjis"
    |> Seq.iter (fun line ->
                 match line.[21] with
                 | "1" -> _w.Write (lineString2 line)
                 | _ -> ignore());
    0
