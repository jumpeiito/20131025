module Sikaku

open System.Collections.Generic

module Str  = Util.Str
module CSV  = Util.CSV
module Date = Util.Date

let title = [|"被保険者証番号";
             "保険番号（年）";
             "保険番号（支部）";
             "保険番号（連番）";
             "届出日";
             "異動日";
             "世帯番号";
             "支部コード";
             "支部名";
             "分会コード";
             "分会名";
             "職種コード";
             "職種";
             "受付支部コード";
             "受付支部名";
             "氏名（漢字）";
             "氏名（カナ）";
             "郵便番号";
             "住所コード";
             "回収日";
             "住所１";
             "住所２";
             "住所３";
             "旧保険番号（年）";
             "旧国保番号（支部）";
             "旧国保番号（連番）";
             "移行日";
             "電話番号";
             "呼出";
             "事業所番号";
             "事業所名１";
             "事業所名２";
             "限度額認定証発行";
             "行番号";
             "被保険者氏名";
             "続柄コード";
             "続柄";
             "性別コード";
             "性別";
             "生年月日";
             "得喪日";
             "取喪事由コード";
             "取喪事由";
             "保険料区分";
             "受付日";
             "乳・老区分";
             "取喪";
             "証発行区分コード";
             "証発行区分";
             "老人区分コード";
             "老人区分";
             "公費負担番号１";
             "公費負担番号２";
             "備考";
             "介護適用除外区分";
             "保険料（銀行コード）";
             "保険料（銀行名）";
             "保険料（支店コード）";
             "保険料（支店名）";
             "保険料（口座科目コード）";
             "保険料（口座科目）";
             "保険料（口座番号）";
             "保険料（入力日付）";
             "保険料（口座名義）";
             "給付（銀行コード）";
             "給付（銀行名）";
             "給付（支店コード）";
             "給付（支店名）";
             "給付（口座科目コード）";
             "給付（口座科目）";
             "給付（口座番号）";
             "給付（入力日付）";
             "給付（口座名義）";
             "京建労組合員番号";
             "京建労支部コード";
             "京建労分会コード";
             "京建労班コード"|]

let file = "//192.168.1.3/csv2f/HIHOKEN.csv"

let extract (line:string []) =
    [0; 4; 5; 6; 15; 26; 33; 34; 38; 39; 40; 41; 42]
    |> List.map (fun n -> line.[n])

let print_line (line:string []) =
    let num = (Str.String_Take 2 line.[0]) in
    // let year = (Str.String_Take 3 line.[40]) in
    match num, line.[41] with
    | "13", "14" -> Printf.printf "%s\n" (Str.join (extract line) ",")
    | "13", "24" -> Printf.printf "%s\n" (Str.join (extract line) ",")
    | _ -> ignore()
        
let ktime (str:string) =
    let [g; year; month; day] = ([string str.[0]; str.[1..2]; str.[3..4]; str.[5..6]] |> List.map int) in
    match g with
    | 2 -> System.DateTime(1910 + year, month, day)
    | 3 -> System.DateTime(1925 + year, month, day)
    | 4 -> System.DateTime(1988 + year, month, day)
    | _ -> System.DateTime(2099, month, day)

let knendo (str:string) =
    Date.nendo (ktime str)

type gender = | H | K

type person = { name:   string;
                number: string;
                date:   System.DateTime;
                id:     string;
                sex:    string;
                year:   int;
                iotype: string * string;
            }

type category =
| IN of person
| OUT of person
| Other

let judge (p:person) =
    match p.iotype with
    | "14", _ -> IN p
    | "24", _ -> OUT p
    | _ -> Other

let make_line (line:string list) nendo =
    try
        { name      = line.[34];
          number    = line.[0];
          date      = ktime line.[5];
          id        = Printf.sprintf "%s%02d" line.[6] (int line.[33]);
          sex       = line.[38];
          year      = Date.how_old (ktime line.[39]) (Date.nendo_end nendo);
          iotype    = (line.[41], line.[42])
          } |> judge
    with
    | :? System.ArgumentOutOfRangeException -> Other
    | :? System.IndexOutOfRangeException -> Other

// let push_hash line (dictionary:Dictionary<string, category list>) nendo =
//     let c = make_line line nendo in
//     match c with
//     | IN p | OUT p ->
//         ( match dictionary.ContainsKey p.id with
//           | false -> dictionary.[p.id] <- [c]
//           | true  -> dictionary.[p.id] <- (c :: dictionary.[p.id]))
//     | Other -> ignore()
    
let data() =
    CSV.read_map file "shift-jis" (fun x -> make_line x 2013);

