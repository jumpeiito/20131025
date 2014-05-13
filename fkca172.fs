module FKCA172

// #r "util.dll"
// #r "shibu.dll"
// #r "strdt.dll"
open Util
open Util.File
open Util.Str
open Util.HankakuZenkaku
open Shibu
open Microsoft.FSharp.Collections
open System.Collections
open System.Collections.Generic

let file = "f:/FKCA172.csv"

module Types =
    let shibu_match (k:string) (b:string) =
        match String_Drop 4 k |> _hankaku with
        | "85" -> (String_Take 2 b) |> _hankaku |> int
        | n' -> int (_hankaku n')

    type fkca172(init:list<string>) =
        let [_; kigo; bango; id; _; gender; birth; _; name; skflag; exflag; 
             number; hsidolv; fk; kmesid; kmes; rnumber; rdate; rflag01; rflag02
             rmesid; rmes] = init
        member obj.Kigo    = kigo
        member obj.Bango   = bango
        member obj.ID      = id
        member obj.Gender  = int gender
        member obj.Birth   = birth
        member obj.Name    = name
        member obj.SKFLAG  = skflag
        member obj.EXFLAG  = exflag
        member obj.Number  = number
        member obj.HsidoLV = hsidolv
        member obj.FK      = fk
        member obj.KmesID  = kmesid
        member obj.Kmes    = kmes
        member obj.RNumber = rnumber
        member obj.RDate   = rdate
        member obj.RFLAG01 = rflag01
        member obj.RFLAG02 = rflag02
        member obj.RmesID  = rmesid
        member obj.Rmes    = rmes
        member obj.Shibu   = shibu_match kigo bango
        member obj.Year    = Util.Date.how_old2 birth "2014/3/31"
        
    let jnum (obj:fkca172) = obj.Number
    let rnum (obj:fkca172) = obj.RNumber
    let id   (obj:fkca172) = obj.ID

    let core =
        CSVread file "shift-jis"
        |> List.fold (fun x y ->
                      match y.[0] with
                      | "00263129" when (List.length y) > 10 -> (fkca172 y) :: x
                      | _  -> x) []

    let push (d:Dictionary<string, fkca172>) (x:fkca172) f =
        match d.ContainsKey (f x) with
        | false -> d.Add((f x), x)
        | true  ->
            if (x.Kmes = "") && (x.Rmes = "")
            then d.Add((f x), x)

    let make_dict f =
        let d = Dictionary<string, fkca172>()
        List.iter (fun (x:fkca172) -> push d x f) core
        d

    let jdict = make_dict jnum
    let idict = make_dict id
