module Dock
// #if INTERACTIVE
// #r "util.dll"
// #r "shibu.dll"
// #r "fkca172.dll"
// #r "Office"
// #r "Microsoft.Office.Interop.Excel"
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

let file = "f:/util2/kserv/.dock";;

type Person = { Appoint  : string;
                Occur    : string;
                Hospital : string;
                Bango    : string;
                Name     : string;
                Birth    : string;
                Number   : string;
                Pay      : string;
                Flag     : string list;
                Shibu    : int;
                Gender   : int;
                Year     : int;
                Nendo    : int;
               }
    
type MetaPerson =
    | Male of Person
    | Female of Person


let dock_shibu (bg:string) :int =
    if Regex.Match("^85.+$", bg).Success
    then int(String_Take 2 (String_Drop 2 bg))
    else int(String_Take 2 bg)

let select_number = [1;2;4;5;6;7;16;17;20;22]

let person_create (line:string list) =
    let l = select_number |> List.map (fun n -> line.[n])
    match l with
    | [ap;hp;bg;nm;bi;num;f1;f2;oc;pay] ->
        match (oc, pay) with
        | (_, "")  -> None
        | ("", _)  -> None
        | _  -> Some { Appoint = ap; Occur = oc; Hospital = hp; Bango = bg;
                       Name = nm; Birth = bi; Number = num; Pay = pay; Flag = [f1; f2];
                       Nendo = nendo2 oc;
                       Shibu = dock_shibu bg;
                       Gender = 0; Year = 0}
    | _ -> None


let is_tokutei (p: Person option) (year: int) :bool =
    match p with
    | Some _ ->
        match p.Value with
        | person when (String.length person.Number) <> 11 -> false
        | person when person.Nendo <> year                -> false
        | person when person.Flag.[0] <> "1"              -> false
        | _ -> true
    | None -> false

let data (year:int) =
    (CSVread file "utf-8")
    |> List.fold (fun x y ->
                  let p = person_create y in
                  match is_tokutei p year with
                  | true -> p.Value :: x; | false -> x) []

