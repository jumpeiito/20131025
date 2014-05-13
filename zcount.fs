module ZC

open Zenken

let file = "f:/20130628/特定健診全件データ.csv"

let data = Zenken.fileToData file

let is_hojin (kg:string) =
    let sb = (Util.Str.String_Take 5 >> Util.Str.take_right 2) kg in
    sb = "85"

let filter (shibu:int) (hcode:string) (st:string) (en:string) = function
    | Off _ | Other _ -> false
    | On t ->
        match t.shibu, t.hp with
        | _, HpNull -> false
        | (scode, sname), Hp (code, name) when scode = shibu && code = hcode && not(is_hojin t.raw.[12])->
            let uv s = (Util.Date.strdt s).Value in
            let st' = uv st in
            let en' = uv en in
            let date = uv t.raw.[14] in
            if st' <= date && date <= en' && not(is_hojin t.raw.[2])
            then true
            else false
        | _ -> false
