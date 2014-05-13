module Sakyo

let file2012 = "f:/20130628/2012特定健診全件データ.csv"
let file2013 = "f:/20130628/特定健診全件データ.csv"

let counterToString code c =
    let shibu, bunkai = Util.Shibu.bunkai code in
    Zenken.Statistic.counterToStringList c
    |> List.mapi (fun i x ->
                  match i with
                  | 0 -> Printf.sprintf "%s,%s,合計,%s" shibu bunkai x
                  | 1 -> Printf.sprintf "%s,%s,本人,%s" shibu bunkai x
                  | 2 -> Printf.sprintf "%s,%s,家族,%s" shibu bunkai x
                  | 3 -> Printf.sprintf "%s,%s,男性,%s" shibu bunkai x
                  | 4 -> Printf.sprintf "%s,%s,女性,%s" shibu bunkai x)
    |> Util.Str._join "\n"

let output file =
    Zenken.Statistic.bunkaiInitial (Zenken.fileToData file)
    |> Seq.iter (fun (code, c) ->
                 match code / 100 with
                 | 15 -> printfn "%s" (counterToString code c)
                 | _ -> ignore())


output file2012;;
output file2013;;
