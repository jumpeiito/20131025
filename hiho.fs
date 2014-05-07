module Hiho

let file = "f:/20131025/00263129-201403-25.xls"

(* #r "OFFICE.DLL"; #r "Microsoft.Office.Interop.Excel.dll"; #r "util.dll"; #r "zenken.dll"; #load "hiho.fs";; *)

open System
open System.IO
open System.Reflection
open Microsoft.Office.Interop.Excel
open Zenken

// ["00263129"; null; "杉山　聖名子"; "建１３舞５８"; "０３２２２"; "0130404802"; 19721215.0; 2.0; 41.0; "13158028402"; "受診者"; "情報提供"; null; "未実施"]

// (ファイルの全数) - (odd_data_hiho) + (odd_data_zenken)
// ---------------------------------------↑ 除外者

let reader (file:string) =
    let app   = ApplicationClass(Visible = false) in
    let book  = app.Workbooks.Open(file) in
    let sheet = book.Worksheets.[1] :?> _Worksheet in
    let v = sheet.UsedRange.Value2 :?> obj[,] in
    book.Close();
    app.Quit();
    v

let contents (file:string) =
    let init = reader file in
    let imap = Zenken.imap() in
    seq { for i in [init.GetLowerBound(0) + 1..init.GetUpperBound(0)] do
          let id = Array2D.get init i 6 :?> string |> int |> Printf.sprintf "%09d" in
          let info = [3..5] |> List.map (fun n -> Array2D.get init i n :?> string) in
          if Map.containsKey id imap
          then yield (id, imap.[id], info)
          else yield (id, Zenken.Other, info)}

let map (s:seq<string*Zenken.board*string list>) =
    s
    |> Seq.map (fun (s, z, l) -> (s, (z, l)))
    |> Map.ofSeq

let odd_data_hiho (s:seq<string*Zenken.board*string list>) =
    s
    |> Seq.filter (fun (x, y, z) ->
                   match y with | Zenken.Other | Zenken.Off _ -> true | _ -> false)

let id = function
    | Zenken.On x | Zenken.Off x -> x.id
    | _ -> failwith "hiho.id"

let od_zenken_filter (m:Map<string,(Zenken.board * string list)>) = function
    | Zenken.On x ->
        if Map.containsKey x.id m
        then false
        else true
    | _ -> false

let odd_data_zenken (m:Map<string,(Zenken.board * string list)>) =
    Zenken.data ()
    |> Seq.filter (od_zenken_filter m)

let odd_data_zenken2 (m:Map<string,(Zenken.board * string list)>) =
    odd_data_zenken m
    |> Seq.filter (function
                   | Zenken.On x -> match x.hp with | Zenken.Hp _ -> true | _ -> false
                   | _ -> false)
    
let rightDate (t:Zenken.t) =
    match t.right with
    | RightOFF(d', d'') -> Printf.sprintf "%A,%A" d' d''
    | _ -> ","

let describeT = function
    | On t | Off t -> Printf.printf "%s\n" (t.raw |> Util.Str._join ",")
    | _ -> ignore()

let describe truple =
    let id, (zb:Zenken.board), sl = truple in
    describeT zb

[<EntryPoint>]
let main (arg:string []) =
    printfn "%A" arg.[0];
    let file = arg.[0] in
    let content = contents file in
    let map = map content in
    printfn "要チェック";
    odd_data_hiho content
    |> Seq.iter describe;
    printfn "除外者になっているが健診を受診したもの";
    odd_data_zenken2 map
    |> Seq.iter describeT;
    0
