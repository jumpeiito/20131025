module NS

(* 被保険者全体、 20歳以上被保険者、 40歳以上被保険者をそれぞれ出す必要がある。 *)

let testfile = "f:/20131025/20140401/特定健診用氏名一覧10北_20140401.csv"

module Cseq = Util.CSVseq
module UD   = Util.Date

let how_old (birthday:string) (nendo:int) =
    let b = (UD.strdt birthday).Value in
    let n = (UD.nendo_end nendo) in
    UD.how_old b n

let filter nendo (line:string list) lowerLimit =
    let year = how_old line.[4] nendo in
    if lowerLimit <= year && year < 75 then true else false

let all    nendo (line:string list) = true
let over20 nendo (line:string list) = filter nendo line 20
let over40 nendo (line:string list) = filter nendo line 40

let count (c:int []) (line:string list) =
    let (<+>) (ary:int []) id = ary.[id] <- ary.[id] + 1 in
    let plus x1 x2 = (c <+> x1); (c <+> x2); (c <+> 4) in
    match line.[3], line.[8] with
    | "男", "本人" -> plus 0 2
    | "女", "本人" -> plus 1 2
    | "男", "家族" -> plus 0 3
    | "女", "家族" -> plus 1 3
    | _ -> failwith "in Count"

let countCalc filter (s:seq<string list>) =
    let counter = Array.init 5 (fun n -> 0) in
    s
    |> Seq.filter filter
    |> Seq.iter (count counter);
    counter

let fileToData (nendo:int) (file:string) =
    Cseq.read file "sjis"
    |> Seq.filter (function "受診券整理番号"::_ -> false | _ -> true)

let parseFileName (filename:string) =
    let basename = Util.File.Basename filename in
    let namelist = Util.Str.Split basename ',' in
    let shibu = namelist.[0] in
    (shibu.[9..10], shibu.[11..11])

let fileToSeq (nendo:int) (file:string) =
    let func = [all; over20; over40] in
    let data = fileToData nendo file in
    func
    |> List.map (fun f -> data |> countCalc (f nendo))
