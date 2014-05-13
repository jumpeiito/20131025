module DockSet

module UD = Util.Date

let dockFile = "f:/util2/kserv/.dock"

type dock =
    { appointed:        string;
      year:             int;
      hp:               string;
      shibu:            string * string;
      name:             string;
      birth:            string;
      jnumber:          string;
      flag:             string * string * string;
      occured:          string;
      amount:           string;
      paid:             string;
    }

type board =
    | On of dock
    | Off of dock
    | JustAppoint of dock
    | Brain of dock

let calcYear (app:string) (b:string) =
    let nend = (UD.nendo >> UD.nendo_end) (UD.strdt app).Value in
    UD.how_old (UD.strdt b).Value nend

let dockShibu (kg:string) =
    (Util.Str.String_Take 2 kg, Util.Str.take_right 5 kg)

// let judge (d:dock) =
//     match d.year, d.flag, d.occured, 

let dockLine (line:string list) =
    let birth' = line.[4] in
    let jnumber' = line.[5] in
    let app' = line.[0] in
    { appointed = app';
      year      = calcYear app' birth';
      hp        = line.[1];
      shibu     = dockShibu line.[2];
      name      = line.[3];
      birth     = birth';
      jnumber   = jnumber';
      flag      = (line.[6], line.[7], line.[8]);
      occured   = line.[10];
      amount    = line.[11];
      paid      = line.[12]
    }
