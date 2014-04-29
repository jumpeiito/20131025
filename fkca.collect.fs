module FKCA.Collect

(* #r "util.dll"; #r "zenken.dll"; #r "fkca.dll"; #load "fkca.collect.fs";;  *)
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Text

open FKCA
module F172 = FKCA.R172
module ZS   = Zenken.Statistic
module US   = Util.Shibu
module HP   = Util.Hospital

type counter =
    { all : int [];
      h   : int [];
      k   : int [];
      m   : int [];
      f   : int []; }

let initCounter (size:int) =
    let init() = Array.init size (fun x -> 0) in
    { all = init();
      h   = init();
      k   = init();
      m   = init();
      f   = init(); }

let shibuList =
    US.alist
    |> List.map (fun (x, y, z) -> x)
    |> List.filter (fun n -> n <> 85)

let bunkaiList =
    let d = US._bunkai_hash in
    d.Keys
    |> Seq.filter (fun x ->
                   match x / 100 with
                   | 90 | 95 -> false
                   | _ -> true)
    |> List.ofSeq

let counterToList (c:counter) = [c.all;c.h;c.k;c.m;c.f]
let counterIndex = ["合計"; "本人"; "家族"; "男性"; "女性"]

let counterToStringList (code, name) (c:counter) =
    let array_to_string_list = Array.toList >> List.map string in
    let concat_with_comma = Util.Str._join "," in
    counterToList c
    |> List.map array_to_string_list
    |> List.map concat_with_comma
    |> List.map2 (Printf.sprintf "%s,%s,%s,%s" code name) counterIndex
    
let counterToString (code, name) (c:counter) =
    let str = counterToStringList (code, name) c |> Util.Str._join "\n" in
    str + "\n"

let total (c:counter) = c.all.[4]

let _run (c:counter) (b':F172.t) (dec:int) (final:int) =
    let plus (ary:int []) =
        ary.[dec] <- ary.[dec] + 1; // 年代別のところに足す
        ary.[final] <- ary.[final] + 1      // 合計のところを足す
    in
    plus c.all;
    match b'.sex, b'.jnumber with
    | M, H _ -> plus c.m; plus c.h
    | M, K _ -> plus c.m; plus c.k
    | F, H _ -> plus c.f; plus c.h
    | F, K _ -> plus c.f; plus c.k

let run c b dec = _run c b dec 4

let filter_iter (s:seq<F172.board>) f =
    let c = initCounter 5 in
    Seq.iter (fun x ->
              match x with
              | F172.ON y' | F172.OFF y' -> run c y' (f y')
              | _ -> ignore()) s;
    c

let _count = filter_iter

let sample() = Seq.nth 1000 (F172.data3())

let writer (file:string) f =
    use _w = new System.IO.StreamWriter(file, false, Encoding.GetEncoding("utf-8")) in
    (f _w) |> ignore
    
module Base =
    let count (s:seq<F172.board>) =
        _count s (fun x -> x.year / 10 - 4)

module Metabo =
    let metabo (t:F172.t) = t.r167.metabo |> int

    let count (s:seq<F172.board>) =
        _count s (metabo >> (fun x -> x - 1))

module HLV =
    let level (t:F172.t) =
        match t.hlv with
        | S -> 0
        | D -> 1
        | LVNone -> 2
        | LVOther -> 3

    let count (s:seq<F172.board>) =
        _count s level

module Month =
    let month (t:F172.t) =
        match t.r167.date.Month with
        | 1 | 2 | 3 as m -> m + 8
        | m -> m - 4

    let run c t dec = _run c t dec 12

    let count (s:seq<F172.board>) =
        let c = initCounter 13 in
        Seq.iter (function
                  | F172.ON y' | F172.OFF y' -> run c y' (month y')
                  | _ -> ignore()) s;
        c

module HP =
    let grouping = function
        | F172.ON x -> let c, l, s = x.r167.hp in c
        | _ -> failwith "HP.map"

    let pairSeq (s:seq<F172.board>) =
        s
        |> Seq.groupBy grouping
        |> Seq.map (fun (code, seq) ->
                    let long, short = HP.get code in
                    counterToString (code, long) (Month.count seq))

    let write (s:seq<F172.board>) =
        writer "病院月別.csv" (fun op -> pairSeq s |> Seq.iter op.Write)


module Out =
    let shibuGet code =
        let long, short = US.get code in long

    let bunkaiGet code =
        let shibu, bunkai = US.bunkai code in bunkai

    let codeToKey code f =
        (string code, f code)

    let counterOut code f (c:counter) =
        counterToString (codeToKey code f) c

    let _Write fseq file f set (title:string) =
        let genList, mapf, getf = set in
        let (map:Map<int,seq<F172.board>>) = mapf fseq in
        use sw = new System.IO.StreamWriter(file, false, Encoding.GetEncoding("utf-8")) in
        sw.Write title;
        genList
        |> Seq.filter (fun n -> Map.containsKey n map) 
        |> Seq.map (fun n -> f map.[n] |> counterOut n getf)
        |> List.ofSeq
        |> Util.Str._join "\n"
        |> sw.Write

    let shibu_set = (shibuList, F172.sMap, shibuGet)

    let bunkai_set = (bunkaiList, F172.bMap, bunkaiGet)

    let codeToString (zmap:Map<int,string list>) (map:Map<int,seq<F172.board>>) getf code =
        let base' = (Base.count map.[code]) |> counterToStringList (string code, getf code) in
        let (<+>) x y = x + "," + y in
        List.map2 (<+>) base' zmap.[code]
        |> Util.Str._join "\n"
        |> (fun n -> n + "\n")

    let baseTitle1 =
        [40..10..70]
        |> List.map (fun n -> Printf.sprintf "受診者%d歳台" n)
        |> Util.Str._join ","
        |> (fun n -> ",,," + n + ",受診者合計,")

    let baseTitle2 =
        [0..10..70]
        |> List.map (fun n -> Printf.sprintf "被保険者%d歳台" n)
        |> Util.Str._join ","
        |> (fun n -> n + ",被保険者合計\n")

    let baseTitle = baseTitle1 + baseTitle2

    let _base zseq seq file set zmapf =
        let genList, mapf, getf = set in
        let zmap = zmapf zseq in
        let (map:Map<int,seq<F172.board>>) = mapf seq in
        writer file (fun op ->
                     op.Write baseTitle;
                     genList
                     |> List.iter (fun code -> try (codeToString zmap map getf code |> op.Write)
                                               with
                                               | :? KeyNotFoundException -> ignore()) // (codeToString zmap map getf >> op.Write)
                     )

    let baseShibu zseq fseq =
        _base zseq fseq "基礎支部.csv" shibu_set Zenken.Statistic.shibuMap

    let baseBunkai zseq fseq =
        _base zseq fseq "基礎分会.csv" bunkai_set Zenken.Statistic.bunkaiMap

    let monthTitle  = ",,,4月,5月,6月,7月,8月,9月,10月,11月,12月,1月,2月,3月,合計\n"
    let metaboTitle = ",,,基準該当,予備群該当,該当なし,判定不能,合計\n"
    let hlvTitle  = ",,,積極的,動機付,なし,判定不能,合計\n"

    let monthShibu (fseq:seq<F172.board>) =
        _Write fseq "月別支部.csv" Month.count shibu_set monthTitle

    let monthBunkai (fseq:seq<F172.board>) =
        _Write fseq "月別分会.csv" Month.count bunkai_set monthTitle

    let metaboShibu (fseq:seq<F172.board>) =
        _Write fseq "メタボ支部.csv" Metabo.count shibu_set metaboTitle

    let metaboBunkai (fseq:seq<F172.board>) =
        _Write fseq "メタボ分会.csv" Metabo.count bunkai_set metaboTitle

    let hlvShibu (fseq:seq<F172.board>) =
        _Write fseq "保健指導支部.csv" HLV.count shibu_set hlvTitle

    let hlvBunkai (fseq:seq<F172.board>) =
        _Write fseq "保健指導分会.csv" HLV.count bunkai_set hlvTitle

    let _all zd d =
        printfn "%s" "基礎";
        baseShibu zd d;
        baseBunkai zd d;
        printfn "%s" "月別";
        monthShibu d;
        monthBunkai d;
        printfn "%s" "メタボ";
        metaboShibu d;
        metaboBunkai d;
        printfn "%s" "保健指導レベル";
        hlvShibu d;
        hlvBunkai d;
        printfn "%s" "病院月別";
        HP.write d;

    let all () =
        let zd = Zenken.data() |> List.ofSeq in
        let d  = FKCA.R172.data3() |> List.ofSeq in
        _all zd d

    let abbrev(shibuF, bunkaiF) =
        let d = F172.data3() in shibuF d; bunkaiF d

    let month() = abbrev(monthShibu, monthBunkai)

    let metabo() = abbrev(metaboShibu, metaboBunkai)

    let hlv() = abbrev(hlvShibu, hlvBunkai)

[<EntryPoint>]
let main(args:string []) =
    match args.[0] with
    | "-a"  | "--all"  -> Out.all()
    | "-k"  | "--kiso" ->
        let d = Zenken.data() in
        let d' = F172.data3() in
        Out.baseShibu d d'; Out.baseBunkai d d'
    | "-m"  | "--month"    -> Out.month()
    | "-mt" | "--metabo"   -> Out.metabo()
    | "-h"  | "--hlv"      -> Out.hlv()
    | "-hp" | "--hospital" -> HP.write (F172.data3())
    | _ -> ignore()
    0
