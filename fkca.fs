module FKCA

module Str  = Util.Str
module HZ   = Util.HankakuZenkaku
module Date = Util.Date

type flag    = | F0 | Flag of int
type level   = | S | D | LVNone | LVOther
type message = | Blank | Message of string * string
type hk      = | H of string | K of string
type gender  = | M | F

let make_hk = function
    | s when Str.take_right 1 s = "1" -> H s
    | s -> K s

let make_sex   = function | "1" -> M | "2" -> F | _ -> failwith "make_sex"
let make_flag  = function | "0" -> F0 | n'  -> Flag (int n')
let make_level = function | "1" -> S | "2" -> D | "3" -> LVNone | _ -> LVOther

let make_message = function
    | "", "" -> Blank
    | m', n' -> Message (m', n')

module R167 =
    let file = "f:/FKAC167.csv"

    type t =
        { jnumber : hk;
          date    : System.DateTime;
          hp      : string * string * string;
          metabo  : string;
        }

    let make_hospital hp =
        let long, short = Util.Hospital.get hp in
        (hp, long, short)

    let toLine (line:string list) =
        let mtb = if line.[48] = "" then "4" else line.[48] in
        { jnumber = make_hk line.[9];
          date    = (Date.strdt line.[10]).Value;
          hp      = make_hospital line.[11];
          metabo  = mtb}

    let filter = function
        | _::"FKAC167"::_
        | "保険者番号"::_
        | [_]  -> false
        | line ->
            match line.[9] with
            | "" -> false
            | _ -> true

    let _map _file =
        Util.CSVseq.read_filter_map _file "sjis" (fun x -> x.[9], toLine x) filter
        |> Map.ofSeq

    let map() = _map file
    let map2  = _map

module R172 =
    let file = "f:/FKCA172.csv"

    type t =
        { number: int * string * string;
          id: string;
          sex: gender;
          year: int;
          name: string;
          right: flag;
          except: flag;
          jnumber: hk;
          hlv: level;
          kmes: message;
          hmes: message;
          r167: R167.t;
          zenken: Zenken.board;}

    type board =
    | ON of t
    | OFF of t
    | Ignore of string list

    let make_number (line:string list) =
        let line1 = line.[1] in
        let line2 = line.[2] in
        let f func = (func >> HZ._hankaku >> int) in
        let k = (f (Str.take_right 2)) line1 in
        let b = (f (Str.String_Take 2)) line2 in
        match k with
        | 85 -> (b, line1, line2)
        | _  -> (k, line1, line2)

    let calc_year_old (jnum:string) (birthday:string) =
        let nendoEnd = Date.jnum_nendo_end jnum in
        let birth'   = (Date.strdt birthday).Value in
        Date.how_old birth' nendoEnd
        
    let judge (t:t) =
        match t.right, t.except, t.kmes, t.hmes with
        | F0, F0, Blank, Blank -> ON t
        | _ -> OFF t

    let make_zenken (mapz:Map<string,Zenken.board>) id =
        let key = id |> int |> Printf.sprintf "%09d" in
        if Map.containsKey key mapz
        then mapz.[key]
        else Zenken.Other

    let _make_line_init (map167:Map<string,R167.t>) (mapz:Map<string,Zenken.board>) (line:string list) gap =
        let jnum = line.[11] in
        { number  = make_number line;
          id      = line.[3];
          sex     = make_sex line.[5];
          jnumber = make_hk jnum;
          year    = calc_year_old jnum line.[6];
          name    = line.[8];
          right   = make_flag line.[9];
          except  = make_flag line.[10];
          hlv     = make_level line.[12];
          kmes    = make_message (line.[13+gap], line.[14+gap]);
          hmes    = make_message (line.[19+gap], line.[20+gap]);
          r167    = map167.[jnum];
          zenken  = make_zenken mapz line.[3]}
        |> judge

    let _make_line (map167:Map<string,R167.t>) (mapz:Map<string,Zenken.board>) (line:string list) =
        _make_line_init map167 mapz line 1

    let _make_line2 (map167:Map<string,R167.t>) (mapz:Map<string,Zenken.board>) (line:string list) =
        _make_line_init map167 mapz line 0

    let make_line_init (map167:Map<string,R167.t>) (mapz:Map<string,Zenken.board>) func = function
        | _ :: "FKCA172" :: _
        | "保険者番号" :: _
        | [_] as line -> Ignore line
        | line' ->
            // 受診券整理番号が入っていないものは対象外とする。
            match line'.[11] with
            | "" -> Ignore line'
            | _  -> func map167 mapz line'

    let make_line (map167:Map<string,R167.t>) (mapz:Map<string,Zenken.board>) = make_line_init map167 mapz _make_line

    let make_line2 (map167:Map<string,R167.t>) (mapz:Map<string,Zenken.board>) = make_line_init map167 mapz _make_line2
        
    let _get func err = function
        | Ignore _ -> failwith err
        | ON x | OFF x ->
            match x.zenken with
            | Zenken.On x' | Zenken.Off x' -> func x'
            | _ -> (0, "")

    let getShibu  = _get (fun x -> x.shibu) "getShibu"

    let getBunkai = _get (fun x -> x.bunkai) "getBunkai"

    // ON, OFF, Ignore含めたもの
    let data() =
        let map = R167.map() in
        let mapz = Zenken.imap() in
        Util.CSVseq.read_map file "sjis" (make_line map mapz)

    let filter = function
        | ON _ | OFF _ -> true
        | _ -> false

    let _data file func pred =
        let map = R167.map() in
        let mapz = Zenken.imap() in
        Util.CSVseq.read_map_filter file "sjis" (func map mapz) pred
        
    // ON, OFF含めたもの
    let data2() = _data file make_line filter
    // ONのみ
    let data3() = _data file make_line (function ON _ -> true | _ -> false)

    let data_iter func =
        let map = R167.map() in
        let mapz = Zenken.imap() in
        Util.CSV.read_iter file "sjis" (fun x ->
                                        match make_line map mapz x with
                                        | ON x' -> func x'
                                        | _ -> ignore())

    let data4 file172 file167 =
        let map = R167.map2 file167 in
        let mapz = Zenken.imap() in
        Util.CSVseq.read_map_filter file172 "sjis" (make_line2 map mapz) (function OFF _ -> true | _ -> false)// (function ON _ -> true | _ -> false)

    let makeMap f =
        data3()
        |> Seq.groupBy (function
                        | ON x -> match f x with | y,_,_ -> y
                        | _ -> failwith "shibuMap")
        |> Map.ofSeq

    let shibuMap() = makeMap (fun x -> x.number)

    let bunkaiMap() =
        data3()
        |> Seq.groupBy (fun x -> match getBunkai x with c, n -> c)
        |> Map.ofSeq

    let makeMap2 (data:seq<board>) f =
        data
        |> Seq.groupBy f
        |> Map.ofSeq

    let sMap (data:seq<board>) =
        makeMap2 data (function
                       | ON x -> match x.number with | y,_,_ -> y
                       | _ -> failwith "sMap")

    let bMap (data:seq<board>) =
        makeMap2 data (fun x -> match getBunkai x with c, n -> c)

    let investigate f =
        data()
        |> Seq.filter (function | ON x | OFF x -> f x | Ignore _ -> false)

    let is_shibu (s:int) = function
        | Ignore _ -> false
        | ON t | OFF t ->
            match t.number with
            | num, _, _ when num = s -> true
            | _ -> false
        

    let kame () =
        data3()
        |> Seq.filter (function | ON _ as b -> is_shibu 53 b | _ -> false)
