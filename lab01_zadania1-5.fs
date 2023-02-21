// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.


open System

// zadanie 1
let pole_prostokata (a:float) (b:float) = a*b
let obwod_prostokata (a:float) (b:float) = (float)2*a+(float)2*b

let pole_kwadratu (a:float) = a*a
let obwod_kwadratu (a:float) = (float)4*a;

let pole_kola (r:float) = Math.PI * pown r 2
let obwod_kola (r:float) = 2.0 * Math.PI * r

//zadanie 2
let oblicz_wielomian x y = -3.0 * pown y 4+ 0.5 * pown x 2 + 2.0 * y-7.0

//zadanie 3
let rec ciag (a1:int) (q:int) n=
    if n = 1 then a1
    else q * ciag a1 q (n-1)

let rec NWD (a:int) (b:int) =
    if b = 0 then a
    else
    if a = b then a 
    else NWD b (a % b)

let rec sumaCyfr (a:int) =
    if a < 10 then a
    else a%10 + sumaCyfr(a/10) 
    
//zadanie 4
let liczbaPierwsza a=
    let rec brakDzielnikow dz =
        if dz = 1 then true
        elif a%dz = 0 then false
        else brakDzielnikow (dz - 1)
    a > 1 && brakDzielnikow (a/2)

// zdanie 5
let rec liczby a b =
    printf("%i ") a
    if (a<b) then liczby (a+1) b

let rec contatenation (a:string) (x:int) (n:int) =
     printf("%s") a 
     if (x<n) then contatenation a (x+1) n

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    liczby 4 10
    printfn("\nZadanie 5:")
    contatenation "ELO" 1 10
    0 // return an integer exit code
