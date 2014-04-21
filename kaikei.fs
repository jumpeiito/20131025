module Kaikei

open Zenken
module US = Util.Str
(* #r "util.dll"; #r "zenken.dll";; *)
let dockMap = Zenken.Dock.map()

let scMap = Zenken.SC.map()

let dockToShibu = US.String_Take 5 >> US.take_right 2

// 集合契約と人間ドックを除外
let dock_or_sc jnum =
    if Map.containsKey jnum dockMap || Map.containsKey jnum scMap
    then false
    else true

let filtering (t:t) =
    let jnum = jnum t in
    match t.hp, dockToShibu jnum with
    | _ , "85" -> false
    | Hp _, _ -> dock_or_sc jnum
    | _ -> false

let counterToAllString code (counter:Statistic.counter) =
    let short, long = Util.Shibu.get code in
    Printf.sprintf "%A,%s,%s" code short (counter.all |> Array.toList |> List.map string |> Util.Str._join ",")

let data =
    Zenken.data()
    |> Seq.filter (function
                   | On x -> filtering x
                   | _ -> false)
    |> Seq.groupBy (function
                    | On x -> let code, name = x.shibu in code
                    | _ -> failwith "other")
    |> Seq.map (fun (x, y) -> counterToAllString x (Statistic.month_classify y))

let dockFilter jnum date =
    let (occd:string), payd = date in
    if String.length jnum = 11
    then
        match dockToShibu jnum, Util.Date.nendo2 payd with
        | "85", _ -> false
        | _, 2013 -> true
        | _ -> false
    else false

////////////////////////////////////////////////////////////////////////////////////////////////////
let dockKaikei =
    dockMap
    |> Map.filter dockFilter
    |> Map.toSeq
////////////////////////////////////////////////////////////////////////////////////////////////////

let scFilter jnum payd =
    match dockToShibu jnum, Util.Date.nendo2 payd with
    | "85", _ -> false
    | _, 2013 -> true
    | _ -> false

////////////////////////////////////////////////////////////////////////////////////////////////////
let scKaikei =
    scMap
    |> Map.filter scFilter
    |> Map.toSeq
////////////////////////////////////////////////////////////////////////////////////////////////////
