// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
// Łukasz Graczyk Lab3

open System

type Color =
    | Red
    | White
    | Blue

 type VehicleType = 
    | Bicycle // brak cech
    | Car of int * int // drzwi, moc KM
    | Motorcycyle of int // pojemnosc silnika cm3
 
type Vehicle = Color * string * VehicleType

// zadanie 1 - // zadanie 2

let porownaj (veh1 : Vehicle) (veh2 : Vehicle) =
    let kolor1, nazwa1, typ1 = veh1
    let kolor2, nazwa2, typ2 = veh2

    match veh1, veh2 with
    | (_,_,Motorcycyle(_)), (_,_,Bicycle) -> printf"%A" veh1
    | (_,_,Bicycle), (_,_,Motorcycyle(_)) -> printf"%A" veh2
    | (_,_,Bicycle), (_,_,Bicycle) -> 
        if(kolor1 = White ) then printfn "%A" veh1
        else if(kolor2 = White ) then printfn "%A" veh2
        else printfn "Są równe"
    | (_,"Trabant",Car(_,_)), (_,_,Motorcycyle(x1)) when x1 > 100 -> printfn "%A" veh2
    | (_,_,Motorcycyle(x1)), (_,"Trabant",Car(_,_)) when x1 > 100 -> printfn "%A" veh1
    | (_,_,Car(_,_)), (_,_,Motorcycyle(_)) -> printfn "%A" veh1
    | (_,_,Motorcycyle(_)), (_,_,Car(_,_)) -> printfn "%A" veh2
    | (_,_,Motorcycyle(x1)), (_,_,Motorcycyle(x2)) when x1 > x2 -> printfn "%A" veh1
    | (_,_,Motorcycyle(x1)), (_,_,Motorcycyle(x2)) when x1 < x2 -> printfn "%A" veh2
    | (_,_,Car(_,_)), (_,_,Car(_,_)) when (nazwa1 = "Ferrari" || nazwa1 = "Prosche") -> printfn "%A" veh1
    | (_,_,Car(_,_)), (_,_,Car(_,_)) when (nazwa2 = "Ferrari" || nazwa2 = "Prosche") -> printfn "%A" veh2
    | (_,_,Car(d1,m1)), (_,_,Car(d2,m2)) when m1 > m2 -> printfn "%A" veh1
    | (_,_,Car(d1,m1)), (_,_,Car(d2,m2)) when m1 < m2 -> printfn "%A" veh2
    | (_,_,Car(d1,m1)), (_,_,Car(d2,m2)) when m1 = m2 && d1 > d2 -> printfn "%A" veh1
    | (_,_,Car(d1,m1)), (_,_,Car(d2,m2)) when m1 = m2 && d1 < d2 -> printfn "%A" veh2
    | (_,_,Car(d1,m1)), (_,_,Car(d2,m2)) when m1 = m2 && d1 = d2 -> printfn "Równe"
    | (_,_,_), (_,_,_) -> printfn "Błędne dane"
//zadanie 2
let inline (/>=) (v1:Vehicle) (v2:Vehicle) = (porownaj v1 v2)  
let inline (/<) (v1:Vehicle) (v2:Vehicle) = (porownaj v1 v2)  
let inline (/<=) (v1:Vehicle) (v2:Vehicle) = (porownaj v1 v2)  
let inline (/>) (v1:Vehicle) (v2:Vehicle) = (porownaj v1 v2)  
let inline (/=) (v1:Vehicle) (v2:Vehicle) = (porownaj v1 v2)  
// zadanie 3

let rec sprawdzWyrazyCiagu(lista: int list) n =
    match lista with
    |[] -> true
    |x::[] -> true
    |x::y::t -> 
       if y - x = n then sprawdzWyrazyCiagu t n
       else false

// zadanie 4

let zwrocListe (lista : int list) n =
    let l1 = lista.[0]
    let l2 = lista.[1]
    let x = l2 - l1

    [for i in 1 .. n -> l1 + (i-1) * x]

//zadanie 5a
let zLF funkcja n =
    [for i in 1 .. n do yield (int)(funkcja**(float)i)]

   

//zadanie 5b 

//let listaFunkcji funkcja n = 


let v1 = (Red,"Trabant",Car(20,20))
let v2 = (White,"Gorski",Bicycle)
[<EntryPoint>]
let main argv =
    porownaj v1 v2
    printfn "%A" argv
    0 // return an integer exit code
