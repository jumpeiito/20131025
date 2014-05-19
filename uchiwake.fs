module Uch

module F172 = FKCA.R172
open Zenken

type counter =
    { all: int []
      h:   int []
      k:   int []}

let initCounter() =
    let init() = Array.init 3 (fun x -> 0) in
    { all = init(); h = init(); k = init() }

let shibu = function
    | On x | Off x -> x.shibu
    | _ -> failwith "Uch.shibu"

let groupCount f data =
    data
    |> Seq.groupBy f
    |> Seq.map (function (x, y) -> x, Seq.length y)

let right_on = function
    | On x | Off x -> match x.right with Cont -> true | _ -> false
    | _ -> false
    
let over20 = function
    | On x | Off x -> if x.year >= 20 && x.year < 75 then true else false
    | _ -> failwith "Uch.over20"

let initial zdata =
    zdata
    |> Seq.filter right_on

let groupByShibu zdata =
    initial zdata
    |> Seq.filter over20
    |> Seq.groupBy shibu

let hk = function
    | Other -> failwith "Uch.groupByHK"
    | On x | Off x ->
        match x.jnum with
        | H _ -> 0
        | K _ -> 1

let groupByHK zdata =
    zdata
    |> Seq.groupBy hk
    |> Seq.map (fun (x, y) -> x, Seq.length y)

let data_all zdata =
    groupByShibu zdata
    |> Seq.map (fun (x, y) -> x, Seq.length y)

let data zdata =
    groupByShibu zdata
    |> Seq.map (fun (x, y) -> x, groupByHK y)

let _run counter dmap smap = function
    | F172.OFF _ | F172.Ignore _ -> ignore()
    | F172.ON x as b ->
        let has map = Map.containsKey (F172.getJnum b) map in
        let hplus n = counter.all.[n] <- counter.all.[n] + 1; counter.h.[n] <- counter.h.[n] + 1 in
        let kplus n = counter.all.[n] <- counter.all.[n] + 1; counter.k.[n] <- counter.k.[n] + 1 in
        match x.jnumber, has dmap, has smap with
        | FKCA.H _, true, _ -> hplus 2
        | FKCA.K _, true, _ -> kplus 2
        | FKCA.H _, _, true -> hplus 1
        | FKCA.K _, _, true -> kplus 1
        | FKCA.H _, _, _ -> hplus 0
        | FKCA.K _, _, _ -> kplus 0
        

let run rdata dmap smap =
    // let dmap = Zenken.Dock.map() in
    // let smap = Zenken.SC.map() in
    let counter = initCounter() in
    rdata
    |> Seq.iter (_run counter dmap smap);
    counter

let calc() =
    let dmap = Zenken.Dock.map() in
    let smap = Zenken.SC.map() in
    F172.shibuMap()
    |> Map.map (fun k v -> run v dmap smap)
    |> Map.toSeq
