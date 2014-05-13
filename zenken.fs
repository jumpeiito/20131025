(* -*- coding:utf-8 -*- *)
module Zenken

open System.Collections.Generic
open System

module CSV   = Util.CSV
module Cseq  = Util.CSVseq
module Hosp  = Util.Hospital
module Shibu = Util.Shibu

let file =
    Util.File.returnExists ["d:/特定健診システム/特定健診CSV/特定健診全件データ.csv"; "f:/20130628/特定健診全件データ.csv"]

type jnum  = | H of string | K of string
type sex   = | M | F
type right = | Cont | RightOFF of string * string
type hp    = | Apply | HpNull | Hp of string * string
type flag  = | F0 | Flag of int

type t =
    { number: string;
      name:   string;
      shibu:  int * string;
      bunkai: int * string;
      jnum:   jnum;
      right:  right;
      sex:    sex;
      hp:     hp;
      year:   int;
      id:     string;
      kid:    string;
      flag:   flag;
      raw:    string list}

type board = | On of t | Off of t | Other

module Dock =
    let file = "f:/util2/kserv/.dock"
    
    let map() =
        Cseq.read file "utf-8"
        |> Seq.map (fun l -> (l.[5], (l.[10], l.[12])))
        |> Map.ofSeq

module SC =
    let file = "f:/util2/kserv/.setcontract"

    let map() =
        Cseq.read file "utf-8"
        |> Seq.map (fun l -> (l.[0], l.[5]))
        |> Map.ofSeq

module Jnumber = begin
    let _info (j:string) =
        let rec loop r = function
            | j' when Seq.length j' = 3 -> List.rev r
            | j' -> loop ((Util.Str.String_Take 2 j')::r) ((Seq.skip 2 >> Seq.toList >> Util.Str.Implode) j')
        in
        loop [] j

    let info (j:string) =
        match _info j with
        | _::shibu::bunkai::_ -> (shibu, bunkai)
        | _ -> failwith "Zenken.Jnumber.info"

    let toShibu (j:string) =
        let i = (info >> fst >> int) j in
        (i, Util.Shibu.get i |> fst)

    let toBunkai (j:string) =
        let i = info j |> (fun (x, y) -> x + y) |> int in
        (i, Util.Shibu.bunkai i |> snd)
end

let make_sex = function | "男" -> M | "女" -> F | _ -> failwith "make_sex"
let make_flag = function | "0" -> F0 | n'  -> Flag (int n')

let make_shibu (line:string list) =
    let _shibu = Util.Shibu.get >> snd in
    match Jnumber.info line.[2] with
    | "85", "90" | "90", _ -> (90, "表具")
    | "85", "95" | "95", _ -> (95, "電工")
    | "85", _ -> let s = line.[24] |> int in (s, _shibu s)
    | (x, _)  -> let x' = int x in (x', _shibu x')

let make_bunkai (line:string list) =
    match Jnumber.info line.[2] with
    | "85", "90" | "90", _ -> (9000, "表具")
    | "85", "95" | "95", _ -> (9500, "電工")
    | "85", _ ->
        let b = (line.[24] + line.[25]) |> int in
        (b, Util.Shibu.bunkai b |> snd)
    | _ -> Jnumber.toBunkai line.[2]

let make_jnum (line:string list) =
    let jnum = line.[12] in
    match Util.Str.take_right 2 jnum with
    | "01" -> H jnum
    | _ -> K jnum

let make_right (line:string list) =
    match line.[9], line.[10] with
    | "          ", "          " -> Cont
    | "          ", y' -> RightOFF("", y')
    | x', "          " -> RightOFF(x', "")
    | x', y' -> RightOFF(x', y')

let make_hp (line:string list) =
    match line.[15], line.[16] with
    | "受付　　　", _ -> Apply
    | _, "0000000000" -> HpNull
    | _, code -> Hp (code, Util.Hospital.get code |> fst)

let judge (t:t) =
    match t.right, t.year, t.flag with
    | Cont, y, F0 when 40 <= y && y < 75 -> On t
    | _ -> Off t

let make_line (line:string list) =
    { number = line.[2];
      name   = line.[4];
      shibu  = make_shibu line;
      bunkai = make_bunkai line;
      jnum   = make_jnum line;
      right  = make_right line;
      sex    = make_sex line.[6];
      hp     = make_hp line;
      year   = line.[8] |> int;
      id     = (int >> Printf.sprintf "%09d") line.[22];
      kid    = (int >> Printf.sprintf "%07d") line.[23];
      flag   = make_flag line.[11];
      raw    = line;}
    |> judge

let filter = function
    | "年度末支部"::_ | [""] -> false
    | _ -> true

let data() =
    Util.CSVseq.read_filter_map file "sjis" make_line filter

let fileToData file =
    Util.CSVseq.read_filter_map file "sjis" make_line filter

let jnum (t:t) =
    match t.jnum with | H j | K j -> j

let board_jnum = function
    | On t | Off t -> jnum t
    | _ -> failwith "board_jnum other"

let kid (t:t) = t.kid

let board_kid = function
    | On t | Off t -> kid t
    | _ -> failwith "board_kid other"

let jnum_shibu (t:t) =
    let f = (Util.Str.String_Take 5 >> Util.Str.take_right 2) in
    f (jnum t)

let makeMap f =
    data()
    |> Seq.map (function | On x | Off x as b -> (f x, b) | Other -> failwith "makeMap")
    |> Map.ofSeq

let jmap() = makeMap jnum

let imap() = makeMap (fun x -> x.id)

let idict() =
    let d = Dictionary<string, board>() in
    data()
    |> Seq.iter (function
                 | On x | Off x as b -> d.[x.id] <- b
                 | Other -> ignore());
    d

let bunkai_investigate() =
    data()
    |> Seq.filter (function
                   | On x | Off x -> match x.bunkai with | _, "" -> true | _ -> false
                   | _ -> false)

let occd = function
    | On x | Off x ->
        let d = (Util.Date.strdt x.raw.[14]).Value in
        (Util.Date.nendo d, d.Month)
    | _ -> (0, 0)
    

module Applied =
    let on () =
        data()
        |> Seq.filter (function
                       | On x -> match x.hp with Apply -> true | _ -> false
                       | _ -> false)

    let shibu_on () =
        on()
        |> Seq.groupBy (function On x -> x.shibu | _ -> failwith "err")
        |> Seq.map (fun (shibu, seq) -> (shibu, Seq.length seq))

module Statistic =
    type counter =
        { all : int [];
          h   : int [];
          k   : int [];
          m   : int [];
          f   : int []; }

    let initCounter size =
        let init() = Array.init size (fun x -> 0) in
        { all = init();
          h   = init();
          k   = init();
          m   = init();
          f   = init(); }
    
    let counterToList (c:counter) =
        [c.all; c.h; c.k; c.m; c.f]

    let counterToStringList (c:counter) =
        counterToList c
        |> List.map (Array.toList >> List.map string >> (fun list' -> Util.Str.join list' ","))

    let _countCounter (t:t) (c:counter) =
        let year = t.year / 10 in
        let take2 = Util.Str.take_right 2 in
        let plus (ary:int []) =
            ary.[year] <- ary.[year] + 1;
            ary.[8] <- ary.[8] + 1;
            match year with
            | y when y >= 4 ->
                ary.[10] <- ary.[10] + 1;
                ary.[9] <- ary.[9] + 1;
                ignore()
            | y when y >= 2 -> ary.[9] <- ary.[9] + 1; ignore()
            | _ -> ignore()
        in
        plus c.all
        match t.sex, take2 t.id with
        | M, "01" -> plus c.m; plus c.h
        | M, _    -> plus c.m; plus c.k
        | F, "01" -> plus c.f; plus c.h
        | F, _    -> plus c.f; plus c.k

    let countCounter (c:counter) = function
        | Other -> ignore()
        | On t  -> _countCounter t c
        | Off t ->
            match t.right, t.flag with
            | Cont, F0 -> _countCounter t c
            | _ -> ignore()

    let countCounter_on_initial (c:counter) = function
        | Other -> ignore()
        | On t | Off t ->
            match t.right with
            | RightOFF (got, _) when got <> "" -> ignore()
            | _ -> _countCounter t c

    let classify (s:seq<board>) counterFunction =
        let c = initCounter 11 in
        s |> Seq.iter (counterFunction c);
        c

    let _sequence f data counterFunction =
        data
        |> Seq.groupBy (function
                        | On x | Off x -> match f x with code, name -> code
                        | Other -> 0) 
        |> Seq.map (fun (code, seq) -> code, classify seq counterFunction)

    let shibu data =
        _sequence (fun x -> x.shibu) data countCounter

    let bunkai data =
        _sequence (fun x -> x.bunkai) data countCounter

    let shibuInitial data =
        _sequence (fun x -> x.shibu) data countCounter_on_initial

    let bunkaiInitial data =
        _sequence (fun x -> x.bunkai) data countCounter_on_initial

    let toMap f data =
        f data
        |> Seq.map (fun (code, c) -> code, counterToStringList c)
        |> Map.ofSeq

    let shibuMap data = toMap shibu data

    let bunkaiMap data = toMap bunkai data

    let monthCounter (c:counter) = function
        | Other -> failwith "monthCounter raises error."
        | On t | Off t as b ->
            let nendo, _month = occd b in
            let month = match _month with | 1 | 2 | 3 -> _month + 8 | _ -> _month - 4 in
            let plus (ary:int []) =
                ary.[month] <- ary.[month] + 1;
                ary.[12] <- ary.[12] + 1;
            in
            plus c.all
            match t.sex, Util.Str.take_right 2 t.id with
            | M, "01" -> plus c.m; plus c.h
            | M, _    -> plus c.m; plus c.k
            | F, "01" -> plus c.f; plus c.h
            | F, _    -> plus c.f; plus c.k

    let month_classify (s:seq<board>) =
        let c = initCounter 13 in
        s |> Seq.iter (monthCounter c);
        c

let testline = Util.Str.Split "10,北,131002314,01,薬師川　尚弘　　　　,本人,男,1968/01/08,046,          ,          ,0,13110011301,          ,          ,受付　　　,0000000000,　　　　,　　　　　,          , ,10,041200101,0409707,10,02,07,004.00" ','

