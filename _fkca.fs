module FKCA
type flag =
  | FLAG_OFF
  | FLAG_ON of int
type level =
  | Level1
  | Level2
  | LevelNone
  | LevelOther
type message =
  | Blank
  | Message of string * string
type hk =
  | H
  | K
  | OtherHK
type gender =
  | Male
  | Female
  | OtherSex
val make_hk : string -> hk
val make_sex : string -> gender
val make_flag : string -> flag
val make_level : string -> level
val make_message : string -> string -> message
module R172 = begin
  val file : string
  type person =
    {number: int * string * string;
     id: string;
     sex: gender;
     hk: hk;
     year: int;
     name: string;
     right: flag;
     except: flag;
     jnumber: string;
     hlv: level;
     kmes: message;
     hmes: message;}
  type board =
    | ON of person
    | OFF of person
    | Ignore
  val make_number : string list -> int * string * string
  val calc_year_old : string -> string -> int
  val judge : person -> board
  val make_line : string list -> board
  val data : unit -> board list
  val data_on : unit -> board list
  val investigate : unit -> board list
end
val testline1 : string list
val testline2 : string list

