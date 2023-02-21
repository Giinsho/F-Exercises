// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.


open System

// zadanie 1
let Rok = seq[1..2020]
let Miesiac = seq[1..12]
let Dzien = seq[1..30]
type RecDate = {
    Dzien:int
    Miesiac:int;
    Rok:int;
}

let RecDateNaKrotke data = (data.Dzien, data.Miesiac, data.Rok)

let KrotkeNaRecDate (d,m,r) :RecDate = {
    Dzien=d;
    Miesiac=m;
    Rok=r
}

//zadanie 2

let porownaj (X1,Y1,Z1) (X2,Y2,Z2) =
    match (X1,Y1,Z1), (X2,Y2,Z2) with
    |   (X1,_,_),(X2,_,_) when X1>X2 -> -1
    |   (X1,_,_),(X2,_,_) when X1<X2 -> 1
    |   (_,Y1,_),(_,Y2,_) when Y1>Y2 -> -1
    |   (_,Y1,_),(_,Y2,_) when Y1<Y2 -> 1
    |   (_,_,Z1),(_,_,Z2) when Z1>Z2 -> -1
    |   (_,_,Z1),(_,_,Z2) when Z1<Z2 -> 1
    |   (_,_,_),(_,_,_) -> 0
//zadanie 3
let rokPrzestepny rok = 
    match rok with
    |   rok when rok%100 <> 0 && rok%4=0 -> true
    |   rok when rok%400 = 0 -> true
    |   _ -> false
//zadanie 4
let wczytajDate :data option = 
    let d = KrotkeNaRecDate(10,11,2020)
    match rokPrzestepny d with
    | 0 -> None
    | 1 -> Some d
[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
