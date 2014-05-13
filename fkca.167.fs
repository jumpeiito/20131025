module F167

// | 腹囲       | 男性(85cm) 女性(90cm) |     |   |   |
// |------------+-----------------------+-----+---+---|
// | 血糖値     |                   100 | 126 |   |   |
// |------------+-----------------------+-----+---+---|
// | 収縮期血圧 |                   130 | 140 |   |   |
// |------------+-----------------------+-----+---+---|
// | 中性脂肪   |                   150 | 300 |   |   |

module UQ = Util.CSVseq

let file = "f:/FKAC167_2012.csv"

type fkacFile =
    {
        filename: string;
        body:     seq<string list>;
        title:    string list;
        _sex:     int;
        _stomach: int;
        _ketto:   int;
        _bp:      int;
        _tg:      int;
        sex:      string * float;
        stomatch: float;
        ketto:    int;
        bp:       int;
        tg:       int
    }

let searchIndex id title =
   List.findIndex (fun x -> x = id) title

let initFilter = function
    | _::"FKAC167"::_
    | "保険者番号"::_
    | [_] -> false
    | _ -> true

let makeFkacFile file =
    let init = UQ.read file "sjis" in
    let _title = Seq.nth 1 init in
    {
        filename  = file;
        body      = init |> Seq.filter initFilter;
        title     = _title;
        _sex      = searchIndex "性別" _title;
        _stomach  = searchIndex "腹囲" _title;
        _ketto    = searchIndex "空腹時血糖（電位差法）" _title;
        _bp       = searchIndex "収縮期血圧" _title;
        _tg       = searchIndex "中性脂肪（トリグリセリド）" _title;
    }

