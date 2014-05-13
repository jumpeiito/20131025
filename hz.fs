module Util

module HankakuZenkaku =
    let zal   = [ for d in 'Ａ'..'ｚ' -> d ]
    let hal   = [ for d in 'A'..'z' -> d ]
    let znum  = [ for d in '０'..'９' -> d ]
    let hnum  = [ for d in '0'..'9' -> d ]
    let zkat  = "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲンッャュョァィゥェォ" |> Seq.toList
    let hkat  = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝｯｬｭｮｧｨｩｪｫ" |> Seq.toList
    let zkigo = "！”＃＄％＆’（）＝～｜＠｛｝＊＋；：＜＞、。？・／＿￥－　" |> Seq.toList
    let hkigo = "!”#$%&’()=~｜@｛｝＊+;:<>､。?･/_￥－ " |> Seq.toList
    let zdict = zal @ znum @ zkat @ zkigo
    let hdict = hal @ hnum @ hkat @ hkigo
    // let group (n : int) (l : 'a list) =
    //     let rec inner subl (counter : int) small r =
    //         match subl with
    //         | [] -> small :: r
    //         | x :: xs ->
    //             match counter with
    //             | y when y - 1 = n -> inner xs 0 [] ((x :: small) :: r)
    //             | _ -> inner xs (x :: small) r
    //             inner xs 0 [] (x :: r)
    //     in
    //     inner l 0 [] []
    let group (n : int) (l : 'a list) =
        let rec inner subl r =
            match List.length subl with
            | len when len < n -> (List.rev (subl :: r))
            | _ -> inner (Seq.skip n subl |> Seq.toList) ((Seq.))
            
