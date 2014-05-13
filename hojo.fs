module Hojo

module US = Util.Str
module UD = Util.Date

let dockFile = "f:/util2/kserv/.dock"

type kigo = Hojin of string | Other of string
type jnum = H of string | K of string

let testLine = ["2010/11/02"; "工場保健会"; "5103384"; "藤井　寿雄"; "1944/06/08"; "10151011302";
      "1"; "1"; "1"; ""; "2010/11/02"; "61740"; "2011/04/25"; "4月"]

type t =
    { kigo: kigo;
      name: string;
      jnum: jnum;
      nendo: int;
    }

type board = ON of t | OFF of t

let makeKigo (s:string) =
    match US.String_Take 2 s with
    | "85" -> Hojin s
    | _ -> Other s

let makeJnum (s:string) =
    match US.take_right 2 s with
    | "01" -> H s
    | _ -> K s

let makeNendo (s:string) =
    match UD.strdt s with
    | Some x -> UD.nendo x
    | None -> 0

let judge (t:t) =
    match t.nendo, t.kigo, t.jnum with
    | 2013, Other _, _ -> ON t
    | 2013, Hojin _, K _ -> ON t
    | _, _, _ -> OFF t

let makeLine (line:string list) =
    { kigo  = makeKigo line.[2];
      name  = line.[3];
      jnum  = makeJnum line.[5];
      nendo = makeNendo line.[12]}
    |> judge

let data = Util.CSVseq.read_map dockFile "utf-8" makeLine
