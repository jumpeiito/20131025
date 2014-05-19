module Ax

open Zenken
open System.Collections.Generic

let file = "f:/20130628/特定健診全件データ.csv"
let jgnfile = "f:/20131025/法人事業主.csv"

let shibu_filter (t:t) =
    match t.shibu with
    | 15, _ | 50, _ | 95, _ -> ignore()
    | _ -> Util.Str._join "," t.raw |> printfn "%s"

let jgnmap() =
    Util.CSVseq.read jgnfile "sjis"
    |> Seq.map (fun n -> n.[0], 1)
    |> Map.ofSeq

let hoge() =
    data()
    |> Seq.iter (function
                 | On x | Off x -> shibu_filter x
                 | Other _ -> ignore())

let is_hojin_jnum str =
    let f = (Util.Str.String_Take 5 >> Util.Str.take_right 2) in
    match f str with
    | "85" -> true
    | _ -> false

let is_hojin = function
    | On x | Off x -> is_hojin_jnum (jnum x)
    | _ -> failwith "is_hojin other"

let (@) map key = Map.containsKey key map

let is_hojin_ok jgnmap = function
    | On t as x ->
        match is_hojin x, t.jnum, (jgnmap@t.kid) with
        | true, _, false -> false
        | true, H _, true -> true
        | true, K _, true -> false
        | _ -> true
    | _ -> false

let hojin_string jgnmap = function
    | On t as x ->
        match is_hojin x, t.jnum, (jgnmap@t.kid) with
        | true, _, false -> "法人"
        | true, H _, true -> "法人事業主"
        | true, K _, true -> "法人事業主家族"
        | _ -> ""
    | _ -> ""

let is_dock_or_sc dmap smap = function
    | On t as x ->
        match dmap@(jnum t), smap@(jnum t) with
        | true, _ -> true
        | _, true -> true
        | _ -> false
    | _ -> false

let is_ok jgnmap dmap smap = function
    | On t as x ->
        let has map = Map.containsKey t.kid map in
        let (@) map key = Map.containsKey key map in
        match (is_hojin_ok jgnmap x), (is_dock_or_sc dmap smap x) with // if is_hojin x then false else true
        | true, false -> true
        | _ -> false
    | x -> false

let csv() =
    let jmap = Zenken.jmap() in
    let jgnmap = jgnmap() in
    Util.CSVseq.read "f:/20130628/50乙訓新河端病院.csv" "sjis"
    |> Seq.map (fun n ->
                try jmap.[n.[0]] with keynotfoundexception -> printfn "%A" n; Other)

let checker (data:seq<string list>) =
    let jmap = Zenken.jmap() in
    let jgnmap = jgnmap() in
    let dmap = Zenken.Dock.map() in
    let smap = Zenken.SC.map() in
    data
    |> Seq.map (fun n ->
                try jmap.[n.[0]] with keynotfoundexception -> printfn "%A" n; Other)
    |> Seq.filter (is_ok jgnmap dmap smap)
    
let sfilter (shibun:int) (hpcode:string) dmap smap = function
    | On x | Off x ->
        let has map = Map.containsKey (jnum x) map in
        match x.shibu, x.hp, (has dmap), (has smap) with
        | (s, _), Hp (h, _), false, false when s = shibun && h = hpcode -> true
        | _ -> false
    | _ -> false

// 上京診療所 "2610204741"
let outputLine jgnmap dmap smap = function
    | Off x as t -> printfn "%s,%s,%s" (Util.Str._join "," x.raw) "除外" ""
    | On x as t ->
        let s1 = hojin_string jgnmap t
        let s2 = if is_dock_or_sc dmap smap t then "ドック/集合契約" else "" in
        printfn "%s,%s,%s" (Util.Str._join "," x.raw) s1 s2
    | _ -> ignore()

let output jgnmap dmap smap (s:seq<board>) =
    s
    |> Seq.iter (outputLine jgnmap dmap smap)

let kita () =
    let dmap = Zenken.Dock.map() in
    let smap = Zenken.SC.map() in
    let jgnmap = jgnmap() in
    Zenken.data()
    |> Seq.filter (sfilter 15 "2610604080" dmap smap)
    |> Seq.iter (outputLine jgnmap dmap smap)

let kita2 shibu hcode =
    let dmap = Zenken.Dock.map() in
    let smap = Zenken.SC.map() in
    let jgnmap = jgnmap() in
    Zenken.fileToData "f:/20130628/2012特定健診全件データ.csv"
    |> Seq.filter (sfilter shibu hcode dmap smap)
    |> Seq.iter (outputLine jgnmap dmap smap)

let _csvToData (csv:string) (jmap:Map<string, board>) =
    let map = jmap in
    Util.CSVseq.read csv "sjis"
    |> Seq.map (fun n -> map.[n.[0]])

let csvToData (csv:string) =
    Zenken.jmap()
    |> _csvToData csv

let csvToData2012 (csv:string) =
    Zenken.jmap2 (Zenken.fileToData "f:/20130628/2012特定健診全件データ.csv")
    |> _csvToData csv

let hoku data =
    let dmap = Zenken.Dock.map() in
    let smap = Zenken.SC.map() in
    let jgnmap = jgnmap() in
    data
    |> Seq.iter (outputLine jgnmap dmap smap)

let alist =
    [(17, "2614102230")
     (12, "2610307411")
     (51, "2620700027")
     (16, "2610803013")
     (50, "2613100656")
     (59, "2610903946")
     (62, "2611202348")
     (14, "2610500916")
     (19, "2610903946")
     (50, "2613000625")
     (61, "2610903946")
     (63, "2610903946")]

let starter = function
    | Other -> failwith "starter"
    | On x | Off x as b ->
        match x.jnum with
        | H _ -> (0, b)
        | K _ -> (1, b)

let starter2013() =
    data()
    |> Seq.filter starter_filter
    |> Seq.map starter
    |> Seq.groupBy fst
    |> Seq.map (fun (x, y) -> (x, Seq.length y))
