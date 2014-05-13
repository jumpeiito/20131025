module OTO

let file = "f:/20131025/2013年度支払分新河端病院.csv"

let numbers () =
    Util.CSVseq.read_map file "sjis" (fun x -> x.[0])

let data () =
    let map = Zenken.jmap() in
    numbers ()
    |> Seq.map (fun j -> map.[j])
    |> Seq.filter (function
                   | Zenken.Off _ | Zenken.Other -> false
                   | Zenken.On x -> match Zenken.jnum_shibu x with "85" -> false | _ -> true)
