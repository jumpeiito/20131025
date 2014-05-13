module Util

open System;;
open System.Text.RegularExpressions;;

module Strdt =
    let (|Regex|_|) pattern str =
        let r = Regex.Match(str, pattern)
        if r.Success then Some (List.tail [ for g in r.Groups -> g.Value ])
        else None

    let (|Regex0|_|) pattern str =
        let r = Regex.Match(str, pattern)
        if r.Success then Some r.Value
        else None

    let english_month (month : string) :int =
        match month with
        | "Jan" | "January"   -> 1
        | "Feb" | "February"  -> 2
        | "Mar" | "March"     -> 3
        | "Apr" | "April"     -> 4
        | "May"               -> 5
        | "Jun" | "June"      -> 6
        | "Jul" | "July"      -> 7
        | "Aug" | "August"    -> 8
        | "Sep" | "September" -> 9
        | "Oct" | "October"   -> 10
        | "Nov" | "November"  -> 11
        | "Dev" | "December"  -> 12
        | _ -> 0

    let gengou (g : string) (year : string) =
        match g with
        | "m" | "M" | "明" | "明治" -> 1867 + (int year)
        | "t" | "T" | "大" | "大正" -> 1911 + (int year)
        | "s" | "S" | "昭" | "昭和" -> 1925 + (int year)
        | "h" | "H" | "平" | "平成" -> 1988 + (int year)
        | _ -> 0

    let strdt (str : string) =
        match str with
        | Regex0 "\\d{4}[-/\. 年]\\d{1,2}[-/\. 月]\\d{1,2}" s -> Some (DateTime.Parse s)
        | Regex0 "\\d{4}\\d{2}\\d{2}" s -> Some (DateTime.ParseExact(s, "yyyyMMdd", null))
        | Regex "([MTSHmtsh明大昭平])[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\. 月][ ]*(\\d{1,2})[日]*" [g; year; month; day] ->
            Some (DateTime((gengou g year), (int month), (int day)))
        | Regex "^(明治|大正|昭和|平成)[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\. 月][ ]*(\\d{1,2})[-/\. 日]$" [g; year; month; day] ->
            Some (DateTime((gengou g year), (int month), (int day)))
        | Regex "^(\\d{4})[-/\. 年][ ]*(\\d{1,2})[-/\. 月]*[ ]*$" [year; month] ->
            Some (DateTime((int year), (int month), 1))
        | Regex "^([MTSHmtsh明大昭平])[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\.月][ ]*" [g; year; month] ->
            Some (DateTime((gengou g year), (int month), 1))
        | Regex "^(明治|大正|昭和|平成)[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\. 月][ ]*" [g; year; month] ->
            Some (DateTime((gengou g year), (int month), 1))
    // | Regex "[A-Za-z]+ ([A-Za-z]+) ([0-9]+) 00:00:00 ........ ([0-9]+)" [month; day; year] ->
    //     Some (DateTime(year, (english_month month), day))
        | Regex "^(\\d{4})(\\d{2})" [year; month] ->
            Some (DateTime((int year), (int month), 1))
        | Regex "^(\\d{4})-(\\d{2})-(\\d{2}) 00:00:00 .+" [year; month; day] ->
            Some (DateTime((int year), (int month), (int day)))
        | _ -> None
