// namespace Kensin

module Shibu

// #r "util.dll";;
open System;;
open System.IO;;
open System.Text;;
open System.Collections;;
open System.Collections.Generic;;
open Util.Str;;
open Util.File;;

module Internal = 
    let Alist = [(10, "北");
                 (11, "上京");
                 (12, "中京");
                 (13, "下京");
                 (14, "南");
                 (15, "左京");
                 (16, "東山");
                 (17, "山科");
                 (18, "右京");
                 (19, "西京");
                 (20, "伏見");
                 (21, "醍醐");
                 (50, "乙訓");
                 (51, "宇治");
                 (53, "亀岡");
                 (54, "船井");
                 (56, "綾部");
                 (57, "福知山");
                 (58, "舞鶴");
                 (59, "宮津");
                 (60, "奥丹後");
                 (61, "相楽");
                 (62, "洛南");
                 (63, "綴喜八幡");
                 (85, "法人");
                 (90, "表具組合");
                 (95, "電工組合")]

module Dict = begin
    let long_shibu_dictionary =
        let d = Dictionary<int, string>(26) in
        List.iter (fun (k, v) -> d.[k] <- v) Internal.Alist;
        d

    let short_shibu_dictionary =
        let d = Dictionary<int, char>(26) in
        List.iter (fun (k, (v : string)) -> d.[k] <- v.[0]) Internal.Alist;
        d

    let bunkai_code =
        let gen = Util.File.Readfile "bunkai.csv"
        let d = Dictionary<int, string*string>(Seq.length gen)
        gen |> Seq.iter (fun x ->
                         let [code; shibu; bunkai] = Util.Str.Split x ',' in
                         d.[int code] <- (shibu, bunkai))
        d
end

module Core = begin
    let long_shibu (query : int) =
        Dict.long_shibu_dictionary.[query]

    let short_shibu (query : int) =
        Dict.short_shibu_dictionary.[query]

    let bunkai (query : int) =
        Dict.bunkai_code.[query]

end

// // end    


