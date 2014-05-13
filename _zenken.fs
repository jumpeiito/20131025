(* -*- coding:utf-8 -*- *)
module Zenken

open System.Collections.Generic
open System

module CSV   = Util.CSV
module Hosp  = Util.Hospital
module Shibu = Util.Shibu

let file = "f:/20130628/特定健診全件データ.csv"

type category = | H | K
type sex      = | Male | Female
type right    = | True | False of string * string
type hospital = | Hospital_Null | Hospital of string * string
type flag     = | IN | OUT of int
type person = {
    number: string;
    name:   string;
    types:  (string * string * string * string) * category * sex;
    year:   int;
    right:  right;
    jnumber: string;
    hospital: hospital;
    id: string;
    flag: flag;
    }
type on_board = | ON of person | OFF of person | Other

let make_category = function | "01" -> H | _ -> K

let make_sex = function | "男" -> Male | _ -> Female

let make_flag = function | "0" -> IN | n' -> OUT (int n')

let make_type shibu (cat:string) (sex:string) =
    (shibu, make_category cat, make_sex sex)

let make_right (get:string) (lost:string) =
    match (get.[0] = ' '), (lost.[0] = ' ') with
    | true, true   -> True
    | true, false  -> False ("", lost)
    | false, true  -> False (get, "")
    | false, false -> False (get, lost)
    
let make_hospital = function
    | "0000000000" -> Hospital_Null
    | n' ->
        try
            let long, short = Hosp.get n' in
            Hospital (n', long)
        with
            keynotfoundexception -> Hospital (n', "")

let make_shibu scode bcode =
    try
        let code = int (scode + bcode) in
        let (shibu, bunkai) = Shibu.bunkai code in
        (scode, bcode, shibu, bunkai)
    with
        formatexception -> (scode, bcode, "", "")

let judgement (obj:person) :on_board =
    match obj.right, obj.year with
    | True, s when s >= 40 && s < 75 -> ON obj
    | _ -> OFF obj

// let make_line (line:string array) =
let make_line (line:string list) =
    try
        let s = make_shibu line.[23] line.[24] in
        { number   = line.[2];
          name     = line.[4];
          types    = make_type s line.[3] line.[6];
          year     = int line.[8]
          right    = make_right line.[9] line.[10]
          jnumber  = line.[12];
          id       = line.[21];
          flag     = make_flag line.[11]
          hospital = make_hospital line.[15];
          } |> judgement
    with
        formatexception -> Other
    
let data() =
    CSV.read_map file "shift-jis" make_line

let make_hash f =
    let d = Dictionary<string, on_board>(40000) in
    CSV.read_iter file "shift-jis" (fun line ->
                                    let p = make_line line in
                                    match p with
                                    | ON x | OFF x -> d.[(f x)] <- p
                                    | Other -> ignore());
    d

let jhash() =
    make_hash (fun x -> x.jnumber)

let ihash() =
    make_hash (fun x -> x.id)

let iter f =
    CSV.read_iter file "shift_jis" (fun x -> f (make_line x))

// let investigate() =
//     data()
//     |> Array.filter (fun x ->
//                      match x with | ON y | OFF y -> (match y.flag with | IN -> true | _ -> false) | _ -> false
//                      )
