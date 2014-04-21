module SinQ

open Microsoft.Office.Interop.Excel

(* #r "util.dll"; #r "Microsoft.Office.Interop.Excel.dll"; #r "OFFICE.DLL"; #load "sq.fs";; *)

let file = "y:/32薗部/鍼灸計算表/鍼灸2014年/鍼灸2014.01診療月.xlsm"
let file2 = "f:/20131025/鍼灸2013.12診療月.xlsx"
// let file2 = "y:/32薗部/鍼灸計算表/鍼灸2013年/鍼灸2013.12診療月.xlsx"
let ofile = "f:/20131025/療養費等の支給に係る国一部負担金等の一部相当額の請求（第1062号).xlsx"
let osheet = "【療養費】連名簿"
let gsheet = "表紙2"

type t = {
    hp:     string;
    shibu:  string;
    number: string;
    name:   string;
    gender: string;
    birth:  string list;
    month:  string * string;
    kh:     string;
    rh:     string;
    exflag: string;
    flag:   string;}

type zw = Zenki of t | Other of t

let make_gender = function
    | "男" -> "1"
    | "女" -> "2"
    | p -> failwith "make_gender"
    
(* "3550714" -> ["3"; "55"; "7"; "14"] *)
let make_birth (f:string) =
    let rec inner i r = function
        | [] -> List.rev r
        | x::xs -> inner (i % x) ((i / x |> string)::r) xs
    in
    inner (int f) [] [1000000;10000;100;1]

let judge t =
    match t.flag, t.exflag with
    | "1", "返戻" -> Other t
    | "1", _ -> Zenki t
    | _ -> Other t

let make_line (s:string list) =
    { hp     = s.[1];
      shibu  = s.[16];
      number = s.[17];
      name   = s.[5];
      gender = make_gender s.[7];
      birth  = make_birth s.[8];
      month  = (s.[14], s.[15]);
      kh     = s.[31];
      rh     = s.[29];
      exflag = s.[0];
      flag   = s.[26];}
    |> judge
    
let read _file =
    Util.Excel.get _file "入力"
    |> Seq.filter (fun x ->
                   match string x.[2] with
                   | "" | "口座番号" -> false
                   | _ -> true)
    |> Seq.map ((List.map string) >> make_line)
    |> Seq.filter (function
                   | Zenki t when t.kh <> "" -> true
                   | _ -> false)

let getSheet app book =
    Util.Excel.getSheet 2 book

let putValue (sheet:_Worksheet) counter = function
    | Other _ -> failwith "putValue"
    | Zenki t ->
        let col id = id + (Printf.sprintf "%A" (counter + 14)) in
        [("B", t.hp)
         ("D", t.shibu)
         ("F", t.number)
         ("J", t.birth.[0])
         ("K", t.birth.[1])
         ("L", t.birth.[2])
         ("M", t.birth.[3])
         ("N", t.gender)
         ("O", t.kh)
         ("T", t.rh)
         ("X", fst t.month)
         ("Y", snd t.month)]
        |> List.iter (fun (sym, v) -> sheet.Range(col sym).Value2 <- v)

let getDate (app:ApplicationClass) _file =
    let book = Util.Excel.getBook _file app in
    let sheet = Util.Excel.getSheet gsheet book in
    let get id = sheet.Range(id).Value2 |> string in
    let v = (get "F21", get "H21") in
    book.Saved <- true;
    book.Close();
    v

let putTitleValue (sheet:_Worksheet) (_year, _month) =
    let year, month = (int _year, int _month) in
    let putIn id v = sheet.Range(id).Value2 <- (string v) in
    putIn "G5" year;
    putIn "I5" month;
    match month with
    | 12 -> putIn "R10" (year + 1); putIn "U10" 1
    | _ -> putIn "R10" year; putIn "U10" (month + 1)


[<EntryPoint>]
let main (arg:string []) =
    let _file = arg.[0] in
    let app = ApplicationClass(Visible=true) in
    let book = Util.Excel.getBook ofile app in
    let sheet = getSheet app book in
    putTitleValue sheet (getDate app _file);
    read _file |> Seq.iteri (putValue sheet);
    0
