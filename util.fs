// -*- coding:utf-8 -*-
// シグネチャを作成するのは、fsc --sig:util.fsi util.fs
module Util

open System;;
open System.IO;;
open System.Text;;
open System.Text.RegularExpressions;;
open System.Collections;;
open System.Collections.Generic;;
open Microsoft.Office.Interop.Excel

let combination x1 x2 =
    let rec loop x1' x2' r =
        match x1', x2' with
        | [], _    -> List.rev r
        | x::xs, [] -> loop xs x2 r
        | x::xs as x', y::ys -> loop x' ys ([x;y]::r)
    in
    loop x1 x2 []

let combination3 x1 x2 x3 =
    let xx = combination x1 x2 in
    let rec loop xx' x3' r =
        match xx', x3' with
        | [], _ -> List.rev r
        | x::xs, [] -> loop xs x3 r
        | x::xs as x', y::ys -> loop x' ys ((x@[y])::r)
    in
    loop xx x3 []
    
let combination_pair x1 x2 =
    let rec loop x1' x2' r =
        match x1', x2' with
        | [], _    -> List.rev r
        | x::xs, [] -> loop xs x2 r
        | x::xs as x', y::ys -> loop x' ys ((x, y)::r)
    in
    loop x1 x2 []

let combination_triple x1 x2 x3 =
    let xx = combination_pair x1 x2 in
    let rec loop xx' x3' r =
        match xx', x3' with
        | [], _ -> List.rev r
        | x::xs, [] -> loop xs x3 r
        | (xfst, xsnd)::xs as x', y::ys -> loop x' ys ((xfst, xsnd, y)::r)
    in
    loop xx x3 []

let combination_quad x1 x2 x3 x4 =
    let xx = combination_triple x1 x2 x3 in
    let rec loop xx' x4' r =
        match xx', x4' with
        | [], _ -> List.rev r
        | x::xs, [] -> loop xs x4 r
        | (f,s,t)::xs as x', y::ys -> loop x' ys ((f,s,t,y)::r)
    in
    loop xx x4 []


module Str = begin
    let Explode (str : string) =
        List str |> Seq.toList

    let Implode (l : char list) =
        let rec inner subl r =
            match subl with
            | [] -> r
            | x :: xs -> inner xs (r + (string x))
        in
        inner l ""

    let String_Take (n : int) (str : string) =
        Seq.take n str |> Seq.toList |> Implode

    let take (n: int) (str: string) =
        Seq.take n str |> Seq.toList |> Implode

    let String_Drop (n : int) (str : string) =
        Seq.skip n str |> Seq.toList |> Implode

    let take_right (n:int) (str:string) :string =
        let len = String.length str in
        String_Drop (len - n) str
        
    let Split (str : string) (sep : char) =
        let rec inner subst r =
            try
                (let pos = Seq.findIndex (fun x -> x = sep) subst in
                 inner (String_Drop (1 + pos) subst) ((String_Take pos subst)::r))
            with
            | keynotfoundexception -> List.rev (subst::r)
        in
        inner str []

    let join (l:string list) (joiner:string) :string =
        let rec inner subl r =
            match subl with
            | []      -> r
            | [x]     -> r + x
            | x :: xs -> inner xs (r + (x + joiner))
        in
        inner l ""

    let _join (joiner:string) l =
        join l joiner
end

module HankakuZenkaku = begin
    let zal   = [ for d in 'Ａ'..'ｚ' -> d ]
    let hal   = [ for d in 'A'..'z' -> d ]
    let znum  = [ for d in '０'..'９' -> d ]
    let hnum  = [ for d in '0'..'9' -> d ]
    let zkat  = "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲンッャュョァィゥェォ" |> Seq.toList
    let hkat  = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝｯｬｭｮｧｨｩｪｫ" |> Seq.toList
    let zkigo = "！”＃＄％＆’（）＝～｜＠｛｝＊＋；：＜＞、、。？・／＿￥－－ー　　　　　　" |> Seq.toList
    let hkigo = "!\"#$%&’()=~|@{}＊+;:<>､,.?･/_\\ｰ-- ⅠⅡⅢⅣⅤ" |> Seq.toList
    let z     = zal @ znum @ zkat @ zkigo
    let h     = hal @ hnum @ hkat @ hkigo
    let zdict =
        let d = Dictionary<char, char>(List.length z) in
        Seq.iter2 (fun x y -> d.[x] <- y) h z
        d
    let hdict =
        let d = Dictionary<char, char>(List.length z) in
        Seq.iter2 (fun x y -> d.[x] <- y) z h
        d

    let group (n : int) (l : 'a list) =
        let rec inner subl r =
            match List.length subl with
            | len when len < n ->
                match subl with
                | [] -> List.rev r
                | _  -> (List.rev (subl :: r))
            | _ -> inner (Seq.skip n subl |> Seq.toList) ((Seq.take n subl |> Seq.toList) :: r)
        in
        inner l []

    let zkat1_gen = "ヴガギグゲゴザジズゼゾダヂヅデドバビブベボ" |> Seq.toList
    let hkat1_gen = "ｳﾞｶﾞｷﾞｸﾞｹﾞｺﾞｻﾞｼﾞｽﾞｾﾞｿﾞﾀﾞﾁﾞﾂﾞﾃﾞﾄﾞﾊﾞﾋﾞﾌﾞﾍﾞﾎﾞ" |> Seq.toList |> group 2
    let zkat2_gen = "パピプペポ" |> Seq.toList
    let hkat2_gen = "ﾊﾟﾋﾟﾌﾟﾍﾟﾎﾟ" |> Seq.toList |> group 2

    let zk = zkat1_gen @ zkat2_gen
    let hk = (hkat1_gen @ hkat2_gen) |> List.map (fun [x; y] -> (y, x))

    let zdict2 =
        let d = Dictionary<char*char, char>(List.length zk) in
        Seq.iter2 (fun x y -> d.[x] <- y) hk zk
        d

    let hdict2 =
        let d = Dictionary<char, char*char>(List.length hk) in
        Seq.iter2 (fun x y -> d.[x] <- y) zk hk
        d

    let zd (ch : char) =
        try zdict.[ch] with
            keynotfoundexception -> ch

    let hd (ch :char) =
        try hdict.[ch] with
            keynotfoundexception -> ch

    let _zenkaku (str : string) =
        let rec inner subst r =
            match subst with
            | [] -> Str.Implode r
            | [x] -> Str.Implode ((zd x) :: r)
            | x :: y :: xs ->
                match x with
                | 'ﾞ' | 'ﾟ' -> inner xs (zdict2.[(x, y)] :: r)
                | _ ->
                    try inner (y :: xs) ((zd x) :: r) with
                        keynotfoundexception -> inner (y :: xs) (x :: r)
        in
        inner (Seq.toList str |> List.rev) []

    let _hankaku (str : string) =
        let rec inner subst r =
            match subst with
            | [] -> Str.Implode (List.rev r)
            | x :: xs ->
                try let (h1, h2) = hdict2.[x] in inner xs (h1 :: h2 :: r) with
                    keynotfoundexception -> inner xs ((hd x) :: r)
        in
        inner (Seq.toList str) []
end

module Date = begin
    let (|Regex|_|) pattern str =
        let r = Regex.Match(str, pattern)
        if r.Success then Some (List.tail [ for g in r.Groups -> g.Value ])
        else None

    let (|Regex0|_|) pattern str =
        let r = Regex.Match(str, pattern)
        if r.Success then Some r.Value
        else None

    let english_month (month : string) :int =
        match month with
        | "Jan" | "January"   -> 1
        | "Feb" | "February"  -> 2
        | "Mar" | "March"     -> 3
        | "Apr" | "April"     -> 4
        | "May"               -> 5
        | "Jun" | "June"      -> 6
        | "Jul" | "July"      -> 7
        | "Aug" | "August"    -> 8
        | "Sep" | "September" -> 9
        | "Oct" | "October"   -> 10
        | "Nov" | "November"  -> 11
        | "Dev" | "December"  -> 12
        | _ -> 0

    let gengou (g : string) (year : string) =
        match g with
        | "m" | "M" | "明" | "明治" -> 1867 + (int year)
        | "t" | "T" | "大" | "大正" -> 1911 + (int year)
        | "s" | "S" | "昭" | "昭和" -> 1925 + (int year)
        | "h" | "H" | "平" | "平成" -> 1988 + (int year)
        | _ -> 0

    let strdt (str : string) =
        match str with
        | Regex0 "\\d{4}[-/\. 年]\\d{1,2}[-/\. 月]\\d{1,2}" s -> Some (DateTime.Parse s)
        | Regex0 "\\d{4}\\d{2}\\d{2}" s -> Some (DateTime.ParseExact(s, "yyyyMMdd", null))
        | Regex "([MTSHmtsh明大昭平])[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\. 月][ ]*(\\d{1,2})[日]*" [g; year; month; day] ->
            Some (DateTime((gengou g year), (int month), (int day)))
        | Regex "^(明治|大正|昭和|平成)[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\. 月][ ]*(\\d{1,2})[-/\. 日]$" [g; year; month; day] ->
            Some (DateTime((gengou g year), (int month), (int day)))
        | Regex "^(\\d{4})[-/\. 年][ ]*(\\d{1,2})[-/\. 月]*[ ]*$" [year; month] ->
            Some (DateTime((int year), (int month), 1))
        | Regex "^([MTSHmtsh明大昭平])[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\.月][ ]*" [g; year; month] ->
            Some (DateTime((gengou g year), (int month), 1))
        | Regex "^(明治|大正|昭和|平成)[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\. 月][ ]*" [g; year; month] ->
            Some (DateTime((gengou g year), (int month), 1))
    // | Regex "[A-Za-z]+ ([A-Za-z]+) ([0-9]+) 00:00:00 ........ ([0-9]+)" [month; day; year] ->
    //     Some (DateTime(year, (english_month month), day))
        | Regex "^(\\d{4})(\\d{2})" [year; month] ->
            Some (DateTime((int year), (int month), 1))
        | Regex "^(\\d{4})-(\\d{2})-(\\d{2}) 00:00:00 .+" [year; month; day] ->
            Some (DateTime((int year), (int month), (int day)))
        | _ -> None

    let how_old (f:System.DateTime) (e:System.DateTime) =
        let fd = f.Month * 100 + f.Day
        let ed = e.Month * 100 + e.Day
        match ed - fd with
        | x when x >= 0 -> e.Year - f.Year
        | _ -> e.Year - f.Year - 1

    let how_old2 (f:string) (e:string) =
        how_old (strdt f).Value (strdt e).Value

    let nendo (d:System.DateTime) =
        match d.Month with
        | 1 | 2 | 3 -> d.Year - 1
        | _ -> d.Year

    let nendo2 (s:string) =
        try (nendo (strdt s).Value) with
            nullreferenceexception -> 0

    let today8 () :string =
        let today = DateTime.Today in
        Printf.sprintf "%4d%02d%02d" today.Year today.Month today.Day

    let nendo_end (nendo:int) =
        System.DateTime((nendo + 1), 3, 31)

    let jnum_nendo jnum =
        2000 + (int (Str.String_Take 2 jnum))

    let jnum_nendo_end jnum =
        nendo_end (jnum_nendo jnum)
end

module File = 
    let Readfile path =
        File.ReadAllLines path

    let Basename (filename:string) =
        Str.Split filename '/' |> List.rev |> List.head

    let DirectoryName (filename:string) =
        let rec inner subl r =
            match subl with
            | []      -> r
            | [x]     -> r
            | x :: xs -> inner xs (r + x + "/")
        in
        inner (Str.Split filename '/') ""

    let Directory_List dir =
        Seq.append (Directory.GetFiles dir) (Directory.GetDirectories dir)

    let rec Alld dir =
        let rec inner subd (r : list<string>) =
            match subd with
            | [||] -> dir :: r
            | n'   -> inner n'.[1..] (r @ Alld n'.[0])
        in
        inner (Directory.GetDirectories dir) []

    let rec Allf dir =
        let rec inner subd (r : list<string>) =
            match subd with
            | [||] -> dir :: r
            | n'   -> inner n'.[1..] (r @ Allf n'.[0])
        in
        inner (Directory.GetDirectories dir) (Array.toList (Directory.GetFiles dir))

    let sjis_reader (pathname:string) =
        new StreamReader(pathname, Encoding.GetEncoding("shift-jis"))// .ReadToEnd()

    let UTFfile path =
        Readfile path

    let SJISfile (path:string) =
        let stream = new StreamReader(path, Encoding.GetEncoding("shift-jis"))
        let rec inner r =
            match stream.ReadLine() with
            | null -> List.rev r
            | n' -> inner (n'::r)
        in
        inner []

    // d:/特定健診結果データ/連合会に送付するもの/2013/00263129_FKAC522_20130612_010.csv
    let CSVread (path:string) (code:string) =
        // code ... "shift-jis"
        let stream = new StreamReader(path, Encoding.GetEncoding(code)) in
        let rec inner r =
            match stream.ReadLine() with
            | null -> stream.Dispose(); List.rev r
            | n   -> inner ((Str.Split n ',') :: r)
        in
        inner []

module CSV =
    type CHAR =
    | End | Quote | Return | Newline | Comma
    | Other of char

    let newbuf() =
        new System.IO.StringWriter()

    let examine (i:int) =
        if i < 0
        then End
        else
            match char i with
            | '\"' -> Quote
            | '\r' -> Return
            | '\n' -> Newline
            | ','  -> Comma
            | c    -> Other c
            
    let _read (path:string) (code:string) func pred =
        let ip = new StreamReader(path, Encoding.GetEncoding(code)) in
        let rec inner subst line r =
            let line_set () = List.rev (subst.ToString()::line) in
            let i = ip.Read() in
            match examine i with
            | Quote | Return -> inner subst line r
            | Comma          -> inner (newbuf()) (subst.ToString()::line) r
            | Other c        -> subst.Write c; inner subst line r
            | End            ->
                match pred (func (line_set())) with
                | Some x -> List.rev (x::r)
                | None   -> List.rev r
            | Newline        ->
                match pred (func (line_set())) with
                | Some x -> inner (newbuf()) [] (x::r)
                | None   -> inner (newbuf()) [] r
        in
        inner (newbuf()) [] []

    // let read_seq (path:string) (code:string) =
    //     let ip = new StreamReader(path, Encoding.GetEncoding(code)) in
    //     // seq { yield ip.Read() }
    //     seq { let rec loop subst line =              
    //         }

    let read (path:string) (code:string) =
        _read path code id (fun x -> Some x)

    let read_map (path:string) (code:string) f =
        _read path code f (fun x -> Some x)

    let read_filter_map (path:string) (code:string) func pred =
        _read path code func pred

    let read_iter (path:string) (code:string) func :unit =
        let ip = new StreamReader(path, Encoding.GetEncoding(code)) in
        let rec loop subst line =
            let i = ip.Read() in
            match examine i with
            | Quote | Return -> loop subst line
            | Comma          -> loop (newbuf()) (subst.ToString()::line)
            | Other c        -> subst.Write c; loop subst line
            | End            -> ignore()
            | Newline        -> (func (List.rev (subst.ToString()::line))); loop (newbuf()) []
        in
        loop (newbuf()) []
        
    let excol (i:int) =
        let rec inner subi r =
            match subi with
            | 0 -> Str.Implode r
            | j when j < 26 -> Str.Implode ((char (65 + j - 1)) :: r)
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
        inner (List.rev (Str.Explode s)) 0 0

module CSVseq =
    type CHAR =
    | End | Quote | Return | Comma
    | Other of char

    let newbuf() =
        new System.IO.StringWriter()

    let examine (i:int) =
        if i < 0
        then End
        else
            match char i with
            | '\"' -> Quote
            | '\r' -> Return
            | ','  -> Comma
            | c    -> Other c
            
    let csv_split string =
        let str = new System.IO.StringReader(string) in
        let rec inner buf r =
            let c = str.Read() in
            let (<+>) buf' r' = buf'.ToString()::r in
            match examine c with
            | End -> List.rev (buf <+> r)
            | Quote | Return -> inner buf r
            | Comma -> inner (newbuf()) (buf <+> r)
            | Other c -> buf.Write c; inner buf r
        in
        inner (newbuf()) []

    let _read (path:string) (code:string) =
        seq { for line in File.ReadAllLines(path, Encoding.GetEncoding code) -> (csv_split line)}

    let _read_fm (path:string) (code:string) func pred =
        seq { for line in File.ReadAllLines(path, Encoding.GetEncoding code) do
              let l  = csv_split line in
              let l' = func l in
              if pred l' then yield l'
            }

    let read = _read

    let read_map path code f =
        _read_fm path code f (fun x -> true)

    let read_filter path code p =
        _read_fm path code id p

    let read_map_filter = _read_fm

    let read_filter_map path (code:string) func pred =
        seq { for line in File.ReadAllLines(path, Encoding.GetEncoding code) do
              let l  = csv_split line in
              if pred l then yield func l
            }

module CSV2 =
    type CHAR =
    | End | Quote | Return | Newline | Comma
    | Other of char

    let newbuf() =
        new System.IO.StringWriter()

    let examine (i:int) =
        if i < 0
        then End
        else
            let c = char i in
            match c with
            | '\"' -> Quote
            | '\r' -> Return
            | '\n' -> Newline
            | ','  -> Comma
            | _    -> Other c
            
    let read (path:string) (code:string) =
        let ip = new StreamReader(path, Encoding.GetEncoding(code)) in
        let rec inner subst line r =
            let i = ip.Read() in
            match (examine i) with
            | End            -> Array.append r [|line|]
            | Quote | Return -> inner subst line r
            | Newline        -> inner (newbuf()) [||] (Array.append r [|line|])
            | Comma          -> inner (newbuf()) (Array.append line [|subst.ToString()|]) r
            | Other c        -> subst.Write c; inner subst line r
        in
        inner (newbuf()) [||] [||]

    let read_func (path:string) (code:string) f =
        let ip = new StreamReader(path, Encoding.GetEncoding(code)) in
        let rec inner subst line r =
            let i = ip.Read() in
            match (examine i) with
            | End            -> Array.append r [|f line|]
            | Quote | Return -> inner subst line r
            | Newline        -> inner (newbuf()) [||] (Array.append r [|f line|])
            | Comma          -> inner (newbuf()) (Array.append line [|subst.ToString()|]) r
            | Other c        -> subst.Write c; inner subst line r
        in
        inner (newbuf()) [||] [||]

    let read_iter (path:string) (code:string) f =
        let ip = new StreamReader(path, Encoding.GetEncoding(code)) in
        let rec inner subst line =
            let i = ip.Read() in
            match (examine i) with
            | End            -> ignore()
            | Quote | Return -> inner subst line
            | Newline        -> f line; inner (newbuf()) [||]
            | Comma          -> inner (newbuf()) (Array.append line [|subst.ToString()|])
            | Other c        -> subst.Write c; inner subst line
        in
        inner (newbuf()) [||]

    let excol (i:int) =
        let rec inner subi r =
            match subi with
            | 0 -> Str.Implode r
            | j when j < 26 -> Str.Implode ((char (65 + j - 1)) :: r)
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
        inner (List.rev (Str.Explode s)) 0 0

module Shibu =
    let alist = [(10, "北", "北");
                 (11, "上京", "上");
                 (12, "中京", "中");
                 (13, "下京", "下");
                 (14, "南", "南");
                 (15, "左京", "左");
                 (16, "東山", "東");
                 (17, "山科", "山");
                 (18, "右京", "右");
                 (19, "西京", "西");
                 (20, "伏見", "伏");
                 (21, "醍醐", "醍");
                 (50, "乙訓", "乙");
                 (51, "宇治", "宇");
                 (53, "亀岡", "亀");
                 (54, "船井", "船");
                 (56, "綾部", "綾");
                 (57, "福知山", "福");
                 (58, "舞鶴", "舞");
                 (59, "宮津", "宮");
                 (60, "奥丹後", "奥");
                 (61, "相楽", "相");
                 (62, "洛南", "洛");
                 (63, "綴喜八幡", "綴");
                 (85, "法人", "法");
                 (90, "表具組合", "表");
                 (95, "電工組合", "電")]

    let _hash =
        let d = Dictionary<int, string * string>(27) in
        List.iter (fun (code, long, short) -> d.[code] <- (long, short)) alist;
        d

    let get (code:int) :string * string =
        if _hash.ContainsKey code
        then _hash.[code]
        else "", ""

    let _bunkai_hash =
        let gen = File.Readfile "f:/20131025/bunkai.csv"
        let d = Dictionary<int, string*string>(Seq.length gen)
        gen |> Seq.iter (fun x ->
                         let [code; shibu; bunkai] = Str.Split x ',' in
                         d.[int code] <- (shibu, bunkai))
        d

    let bunkai code =
        if _bunkai_hash.ContainsKey code
        then _bunkai_hash.[code]
        else "", ""

    let what_shibu k b =
        let _k = k |> Str.String_Drop 4 |> HankakuZenkaku._hankaku in
        let _b = b |> Str.String_Take 2 |> HankakuZenkaku._hankaku in
        match _k with
        | "85" -> int _b
        | _ -> int _k

module Hospital =
    let _hospital =
        [("2610903946", "城南診療所", "城南");
         ("2610803013", "東山診療所", "東山");
         ("2614102230", "大宅診療所", "大宅");
         ("2610201358", "上京病院", "上京");
         ("2613000625", "新河端病院", "新河端");
         ("2610403590", "西七条診療所", "西七条");
         ("2610500916", "吉祥院病院", "吉祥院");
         ("2620700092", "京都府交通労働等災害救済事業団", "労災事業団");
         ("2610601342", "第二中央病院", "第二中央");
         ("2610406569", "西七条診療所", "西七条");
         ("2610606093", "洛北診療所", "洛北");
         ("2613300108", "たんご協立診療所", "たんご協立");
         ("2610307411", "太子道診療所", "太子道");
         ("2611801123", "京都協立病院", "京都協立");
         ("2612701488", "まいづる協立診療所", "まいづる");
         ("2611202348", "あさくら診療所", "あさくら");
         ("2613100656", "医誠会診療所", "医誠会");
         ("2613000625", "新河端病院", "新河端");
         ("2614101075", "洛和会音羽病院", "洛和会");
         ("2610204741", "上京診療所", "上京診");
         ("2610604080", "川端診療所", "川端");
         ("2610303667", "京都工場保健会", "工場保健会");
         ("2620700027", "京都工場保健会宇治支所", "工場宇治");
         ("2612800710", "きづ川病院", "きづ川");
         ("2613200209", "田辺中央病院", "田辺中央");
         ("2619700053", "社会保険京都病院", "社会保険");
         ("2610405124", "康生会武田病院", "武田");
         ("2610405231", "武田健診センター", "武田");
         ("2619600311", "京都市立病院", "京都市立");
         ("2614001234", "洛西シミズ病院", "洛西シミズ");
         ("2610305993", "大和健診センター", "大和");
         ("2619700038", "京都第一赤十字病院", "第一赤十字");
         ("2619700012", "京都第二赤十字病院", "第二赤十字");
         ("2613100987", "向日回生病院", "向日回生");
         ("2619600154", "公立山城総合医療センター", "公立山城");
         ("2614000095", "桂病院", "桂");
         ("2610503118", "同仁会クリニック", "同仁会");
         ("2610306454", "御池クリニック", "御池");
         ("2612600938", "京都ルネス病院", "ルネス");
         ("2613400288", "明治国際医療大学附属病院", "明治国際");
         ("2610306868", "シミズ四条大宮クリニック", "四条大宮");
         ("2614102552", "ラクト健診センター", "ラクト");
         ("2610308393", "四条烏丸クリニック", "四条烏丸");
         ("2610301430", "京都予防医学センター", "予防医学");
         ("2619700095", "舞鶴共済病院", "舞鶴共済");
         ("2610906766", "金井病院", "金井");
         ("2619600212", "福知山市民病院", "福知山市民");
         ("2610902245", "大島病院", "大島");
         ("2611800950", "綾部市立病院", "綾部市立");
         ("2619700129", "済生会京都府病院", "済生会");
         ("2610903045", "蘇生会総合病院", "蘇生会");
         ("2614002539", "洛西ニュータウン病院", "洛西NT");
         ("2619600063", "公立南丹病院", "公立南丹");
         ("2610200251", "堀川病院", "堀川");
         ("2610405348", "西村診療所", "西村");
         ("2610402832", "京都南病院", "京都南");
         ("2610406627", "新京都南病院", "新京都南");
         
         ("2613344", "洛北診療所", "洛北")]
        
    let _hash =
        let d = Dictionary<string, string * string>(100) in
        List.iter (fun (code, long, short) -> d.[code] <- (long, short)) _hospital;
        d

    let get (code:string) :string * string =
        if _hash.ContainsKey code
        then _hash.[code]
        else ("", "")

module Excel =
    let application () = ApplicationClass(Visible = false)

    let getBook pathname (app:ApplicationClass) =
        app.Workbooks.Open(pathname)

    let getSheet index (book:Workbook) =
        book.Worksheets.[index] :?> _Worksheet

    let getValue (sh:_Worksheet) =
        let v = sh.UsedRange.Value2 :?> obj[,] in
        seq { for i in [v.GetLowerBound(0)..v.GetUpperBound(0)] do
              yield ([v.GetLowerBound(1)..v.GetUpperBound(1)] |> List.map (fun n -> Array2D.get v i n))
              }

    let get (path:string) index =
        let app   = application () in
        let book  = getBook path app in
        let sheet = getSheet index book in
        let value = getValue sheet in
        book.Saved <- true;
        book.Close();
        app.Quit();
        value
