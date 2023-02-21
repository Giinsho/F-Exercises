// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.


open System

//zadanie 1 
let avg = List.averageBy float

//zadanie 2
let MaxMin (list:int list)  =  (List.min list, List.max list)

//zadanie 3 
let wiekszeAvg (list: int list) = List.filter (fun x -> (float) x > avg list) list

//zadanie 4
let ilePaTrzy (list :int list) = (List.filter (fun x -> x%2 = 0) list |> List.length,
                                  List.filter (fun x -> x%3 = 0) list |> List.length)

//zadanie 5
let LilePaTrzy (list :int list) =(List.filter (fun x -> x%2 = 0) list,
                                  List.filter (fun x -> x%3 = 0) list)

//zadanie 6 
let scan a pom = if a < 0 then a::pom else []
let najUjem (list: int list ) = List.scanBack scan list [] |> List.maxBy (fun x-> x.Length)


//zajecia
(*
let najdluzszeUjemne (lista:int list) = 
    lista
    |> List.foldBack
        (fun el ((szyja:int),głowa,ciało) -> 
            if el < 0
            then
                if System.Math.Sign(el)=System.Math.Sign(szyja)
                then el, el ::głowa, ciało
                else el, [el], głowa::ciało
            else el, głowa, ciało        
        )
        <| (lista.Head, [] , [])
    |> fun (_,głowa,ciało) -> głowa::ciało
    |> List.map (fun m -> (m, m.Length))
*)

//zadanie 7
let ujemneNieujemne (lista:int list) = 
    lista
    |> List.foldBack
        (fun el ((szyja:int),głowa,ciało) -> 
            if el < 0
            then
                if System.Math.Sign(el)=System.Math.Sign(szyja)
                then el, el ::głowa, ciało
                else el, [el], głowa::ciało
            else 
                if System.Math.Sign(el)=System.Math.Sign(szyja)
                then el, el ::głowa, ciało
                else el, [el], głowa::ciało
        )
        <| (lista.Head, [] , [])
    |> fun (_,głowa,ciało) -> głowa::ciało


//zadanie 8
let obliczZlozenie list f = List.reduce(>>) list

//zadanie 9
let obliczListe list f = List.map f list


[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code