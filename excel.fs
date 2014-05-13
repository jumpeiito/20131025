module Excel

open System
open System.IO
open System.Reflection
open Microsoft.Office.Interop.Excel

// let excel =
//     ApplicationClass(Visible = true)

let app = ApplicationClass(Visible = true)

let sheet = app.Workbooks
               .Add()
               .Worksheets.[1] :?> _Worksheet;;

// let 
sheet.Range("A1:A12").Value2;;
// sheet.Name <- "hoge";;
// sheet.[1,1].Value2 <- "hoge";;
