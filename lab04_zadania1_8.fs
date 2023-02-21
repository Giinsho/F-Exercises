// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.


open System

//zadanie 1
let avg  aList = 
    let rec sum aList suma n= 
        match aList with
        | [] -> (float suma) / (float n)
        | head:: tail -> sum tail (suma + head) (n+1)
    sum aList 0 0

//zadanie 2
let find (items:int list) = 
    let rec findMaxMin (items:int list) (minValue:int,maxValue:int) =
        match items with
        | [] -> (minValue,maxValue) 
        | head::tail -> match head with
        | head when head < minValue && head > maxValue -> findMaxMin tail (head,head)
        | head when head < minValue -> findMaxMin tail (head,maxValue)
        | head when head > maxValue -> findMaxMin tail (minValue,head) 
        | _ -> minValue,maxValue
    findMaxMin items ( System.Int32.MaxValue , System.Int32.MinValue) 

//zadanie 3
let mniejszaNizSrednia (items:int list) = 
    let srednia = avg items
    let rec remove (srednia:float) (items:int list) =
       match items with
       | head::tail when (float)head > srednia -> remove srednia tail
       | head::tail -> head :: (remove srednia tail)
       | [] -> []
    remove  srednia items

//zadanie 4
let rec para lista =
    match lista with
    | [] -> ([],[]) // (%2, !%2)
    | head::tail when head%2=0 && head%3=0 -> ([head]@fst(para tail),[head]@snd(para tail))
    | head::tail when head%2=0 -> ([head]@fst(para tail), snd(para tail))
    | head::tail when head%3=0 -> (fst(para tail), [head]@snd(para tail))
    | head::tail -> (fst(para tail), snd(para tail))

//zadanie 5
let iloscCal(items:int list) = 
    let rec ile (items:int list) (parzyste:int,podzielne3:int) =
        match items with
        | [] -> (parzyste,podzielne3)
        | head::tail when head%2=0 && head%3=0 -> ile tail (parzyste+1,podzielne3+1)
        | head::tail when head%3=0 -> ile tail (parzyste,podzielne3+1)
        | head::tail when head%2=0 -> ile tail (parzyste+1,podzielne3) 
        | head::tail -> (ile tail (parzyste,podzielne3) )
    ile items (0,0) 

//zadanie 6
let listaLiczb (items:int list) = 
    let rec najLista items (pomNeg:list<int>) (pom:list<int>)=
        match items with
        | [] when pom.Length > pomNeg.Length -> List.rev pom // rev Zwraca nową listę z elementami w odwrotnej kolejności.
        | [] -> List.rev pomNeg
        | head::tail when head < 0 -> najLista tail pomNeg (head::pom)
        | _ ::tail when pom.Length > pomNeg.Length -> najLista tail pom []
        | _ ::tail -> najLista tail pomNeg []
    najLista items [] []

//zadanie 7 -brak-
//let dzielListe (items: int list) = 
//    let rec ujNieuj items  dodatnie

//zadanie 8 -źle-
let zlozenie items wyr = 
    let rec zlozenieRec items pom =
        match items with
        | [] -> pom
        | head::tail -> zlozenieRec tail (pom >> head)
    zlozenieRec items wyr

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code

