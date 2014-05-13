let rec hoge n =
    let rec inner r = function
    | 0 -> r
    | n' -> inner (r*n') (n'-1)
    in
    inner 1 n
        
