// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.Drawing
open System.Windows.Forms
open System.IO

let convertDataRow(csvLine:string) =
    let cells = List.ofSeq(csvLine.Split(','))
    match cells with
    | title::number::_ -> //co najmniej dwa elelmenty
        let parsedNumber = Int32.Parse(number)
        (title, parsedNumber)
    | _ -> failwith "Incorrect data format!"
;;

let rec calculateSum(rows) =
    match rows with
    | [] -> 0
    | (_, value)::tail ->
        let remainingSum = calculateSum(tail)
        value + remainingSum
;;

let rec processLines(lines) =
    match lines with
    | [] -> []
    | currentLine::remaining ->
        let parsedLine = convertDataRow(currentLine)
        let parsedRest = processLines(remaining)
        parsedLine :: parsedRest;;
let testData = processLines ["Test1,123"; "Test2,456"];;
let lines = List.ofSeq(File.ReadAllLines(@"c:\Users\Giin\Desktop\Studies\Funkcyjne laborki\Lab06\dane.csv"));;
let data = processLines(lines);;
let sum = float(calculateSum(data));;

for (title, value) in data do
    let percentage = int((float(value)) / sum * 100.0)
    Console.WriteLine("{0,-18} - {1,8} ({2}%)",title, value, percentage)
;;


[<EntryPoint>]
let main argv =
    Array.iter (fun x -> printfn "%s" x) argv
    ignore <| Console.ReadKey()
    0 // return an integer exit code
