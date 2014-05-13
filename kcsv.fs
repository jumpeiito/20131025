module KCSV

// #if INTERACTIVE
//     #r "util.dll"
//     #r "shibu.dll"
//     #r "fkca172.dll"
//     #r "Office"
//     #r "Microsoft.Office.Interop.Excel"
// #endif

open System
open System.IO
open System.Reflection
open Microsoft.Office.Interop.Excel
open System.Text
open System.Text.RegularExpressions
open Util
open Util.File
open Util.Str
open Util.Date
open System.Collections.Generic
open FKCA172.Types

let csvfile = "f:/20131025/temp2.csv"
// let csvdir  = "d:/特定健診結果データ/連合会に送付するもの/2013/"
let csvdir  = "f:/kcsv2/"
let csv_body (cont:string list list) =
    List.foldBack (fun (x:string list) y ->
                   match x.[0] with | "5" -> x :: y; | _ -> y) cont []

let get_year (number:string) (birth:string) =
    let nendo = 2000 + (String_Take 2 number |> int) in
    let nendo_end = DateTime(nendo + 1, 3, 31) in
    (how_old (strdt birth).Value nendo_end) |> string

let csv_info (line:string list) =
    match line with
    | _::_::name::bango::birth::number::xs ->
        Some [number;name;(List.rev xs |> List.head);
              bango;birth;(get_year number birth)]
    | _ -> None

// "f:/kcsv2/00263129_FKAC522_20130612_010.csv"
type CSV(filename:string) =
    let contents = CSVread filename "shift-jis"
    let body     = csv_body contents
    member csv.Name      = file_basename filename
    member csv.Contents  = contents
    member csv.Body      = body
    member csv.CreateDay = contents.[0].[3]
    member csv.OccurDay  = contents.[1].[3]
    member csv.HpName    = contents.[1].[2]
    member csv.HpCode    = contents.[1].[1]
    member csv.BaseInfo  = [csv.OccurDay;csv.HpCode;csv.HpName;csv.Name]
    member csv.Info      = List.foldBack (fun x y ->
                                          match csv_info x with
                                          | Some ys -> (csv.BaseInfo @ ys) :: y
                                          | None -> y) body []
    member csv.InfoStr   = List.map (fun x -> join x ",") csv.Info // |> List.toArray
    member csv.Output    = csv.Info |> List.iter (fun x -> printfn "%s" (join x ","))

let is_kcsv_path (path:string) : bool =
    Regex.Match(path, "csv$").Success

let files =
    Util.File.Allf csvdir |> Seq.filter is_kcsv_path

let simplify (file : string) =
    CSVread file "shift-jis"
    |> List.fold (fun x y ->
                  match y with
                  | ig1::ig2::name::bango::birth::number::xs when y.[0] = "5" -> [file;name;bango;birth;number] :: x
                  | _   -> x) []
    |> List.rev;;

let csvlist () =
    let rec inner subf r =
        match subf with
            | [] -> r
            | x :: xs -> inner xs (r @ (CSV x).InfoStr)
    in
    inner (Seq.toList files) []

let csvmain() =
    let op = new System.IO.StreamWriter(csvfile, false, Encoding.GetEncoding("shift-jis"))
    op.WriteLine "受診日,医療機関CD,医療機関,ファイル,整理番号,氏名,記号,番号,生年月日"
    files
    |> Seq.iter (fun x ->
                 Seq.iter (fun (line:string) -> op.WriteLine line) (CSV x).InfoStr);
    op.Dispose();;

let excol (i:int) =
    let rec inner subi r =
        match subi with
        | 0 -> Implode r
        | j when j < 26 -> Implode ((char (65 + j - 1)) :: r)
        | n' ->
            match (n' % 26) with
            | 0 -> inner ((n' / 26) - 1) ('Z' :: r)
            | y -> inner (n' / 26) ((char (65 + (y - 1))) :: r)
    in
    inner i []

let rec pow(num, digit) =
    match digit with
    | 0 -> 1
    | _ -> num * pow(num, (digit - 1))

let colex (s:string) :int =
    let rec inner subl c r =
        match subl with
        | [] -> r
        | x :: xs -> inner xs (c + 1) (r + pow(26, c) * ((int x) - 64))
    in
    inner (List.rev (Explode s)) 0 0

let excol2 (i:int) =
    let c = excol i in
    c + ":" + c

type Alignment =
    | Center
    | Right
    | Left

let setAlign((sheet:_Worksheet), (range:string), (align:Alignment)) =
    let al =
        match align with
        | Center -> -4108
        | Right  -> -4152
        | Left   -> -4131
    sheet.Range(range).HorizontalAlignment <- al

let setWidth((sheet:_Worksheet), (range:string), (width:float)) =
    sheet.Range(range).EntireColumn.ColumnWidth <- width
    
let lastrow((sheet:_Worksheet), (start:string)) =
    sheet.Range(start).End(XlDirection.xlDown).Row

let width = [|10.0; 12.0; 25.0; 32.0; 12.50; 13.75; 10.0; 6.0; 11.0; 3.0|]
let align = [|Left; Center; Left; Center; Center; Left; Center; Right; Right; Center|]

let cell (sheet:_Worksheet) (y:int) (x:int) =
    sheet.Range((excol x) + (string y)).Value2

let range (sheet:_Worksheet) (rbegin:int*int) (rend:int*int) =
    let (by, bx) = rbegin in
    let (ey, ex) = rend in
    sheet.Range((excol bx) + (string by) + ":" + (excol ex) + (string ey))

let cell_color (jdict:Dictionary<string,fkca172>) (sheet:_Worksheet) (row:int) =
    let jnum = string (cell sheet row 5) in
    match jdict.ContainsKey jnum with
    | true  ->
        let kmes = jdict.[jnum].Kmes in
        match kmes with
        | "" -> (range sheet (row, 1) (row, 10)).Interior.ColorIndex <- 15
        | _  ->
            (range sheet (row, 1) (row, 10)).Interior.ColorIndex <- 12;
            sheet.Range((sprintf "K%d" row)).Value2 <- kmes
    | false ->
        let year = sheet.Range((sprintf "J%d" row)).Value2 |> string in
        match  year with
        | "75" -> (range sheet (row, 1) (row, 10)).Interior.ColorIndex <- 10;
        | _    -> printf "×---%s %s\n" jnum year;;


let xlsmain() =
    let fkca  = FKCA172.Types.jdict in
    let app   = ApplicationClass(Visible = true, DisplayAlerts = false) in
    let book  = app.Workbooks.Open(csvfile) in
    // let book  = app.Workbooks.Add() in
    let sheet = book.Worksheets.[1] :?> _Worksheet in
    let row   = ref 2 in
    let lr = lastrow(sheet, "A1") in
    (* 幅と横位置の調整 *)
    [ for i in 1..(Array.length width) -> (excol2 i) ]
    |> List.iteri (fun c col ->
                   setWidth(sheet, col, width.[c]);
                   setAlign(sheet, col, align.[c]))

    setAlign(sheet, "1:1", Center);
    sheet.Range(sprintf "A1:J%d" lr).Borders.LineStyle <- 1;
    ignore [ for i in 2..lr -> cell_color fkca sheet i];;

[<EntryPoint>]
let main (arg:string []) =
    match arg.[0] with
    | "-c" -> csvmain()
    | "-x" -> xlsmain()
    | _ -> ignore()
    0;;

// Local Variables:
// coding:utf-8
// End:
