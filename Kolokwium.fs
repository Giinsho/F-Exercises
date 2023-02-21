open System

let funkcja x y z = x + y + z
let result_function = funkcja 10 20 30


// funkcja nieczysta
let mutable value = 2
let mnozenieByValue x = x * value
//printfn  - powoduje wywołanie bufora co sprawia, że funkcja przestaje być funckją czystą

// FUNKCJA CZYSTA - wywolanie funkcji tworzy wartość i wywołanie tego dowolną ilość 
//razy daje ten sam wynik
let mnozenieCzyste x = x * 2

// funkcja wyższego rzędu - przyjmuje funkcje jako argumenty lub zwraca inne funkcje jako wynik
let funkcjaWyzszego x y = x * y
let result_funkcjaWyzszego = funkcjaWyzszego 10 20

//powiazanie wartości 
let z = 3

//funkcje które nie zwracają wyników tak narpawde zwaracają typ unit.
let unitValue =();;


//praca domowa WYKŁAD 1
let zad1 n =
    n <= 10 || (printfn "TAK"; true)
    n <= 10 && (printfn "NIE"; true)

let potega2 n =
    pown n 2
 
let potega4 n =
    potega2 (potega2 n)
//praca domowa WYKLAD 2
//konkatenacja napisu z samym sobą
let zad4a s:string =
    s + s
//średnia arytmetyczna dwóch liczb
let zad4b (x:float) (y:float) =
    (x + y) / 2.0
// ile razy można podzielić daną liczbę przez dwa
let zad4c n =
    let mutable pom = n
    let mutable licznik = 0
    while pom > 1 do
         pom <- (pom / 2)
         licznik <- licznik+1
    licznik
 // ile razy można podzielić daną liczbę przez dwa sposób 2 
let zad4c2 x =
    let rec f x n =
        match x with
        | x when x <= 1.0 -> n
        | _               -> f(x / 2.0) (n + 1)
    f (float x) 0
// konkatenacja N takich samych napisów
let zad4d2 s n =
    let rec f n acc =
        match n with
        | 0 -> acc
        | _ -> f (n - 1) (acc + s)
    f n ""
 // konkatenacja N takich samych napisów sposób 2
let zad4d (n:int) (s:string) =
    let mutable str = s
    for i in 1 .. n do
        str <- str + s
    str

 // WYKALD 3 
 // Definicja rekordu opisującego wykład (przedmiot, prowadzący, semestr, liczba godzin)
type Wyklad =
    { Przedmiot : string
      Prowadzacy : string
      Semestr : int
      Liczba_godzin : int
    }
// Definicja typu do reprezentacji dni tygodnia
type DniTygodnia =
    | Poniedzialek
    | Wtorek
    | Sroda
    | Czwartek
    | Piatek
    | Sobota
    | Niedziela
 
let funkcjaop n = if n >= 0 then Some(n) else None
 
let wypisz n =
    match funkcjaop n with
    | Some(x) -> printfn "%A" x
    | None -> printfn "Blad"

//WYKLAD 4
//Definicja typu generycznego dla drzewa binarnego
type BinTree<'a> =
   | Nil
   | Leaf of 'a
   | Node of 'a * BinTree<'a> * BinTree<'a>
//Funkcja wypisująca zawartość drzewa
let rec iter f binTree =
   match binTree with
   | Nil -> ()
   | Leaf(x) -> f x
   | Node (x, left, right) ->
       f x
       iter f left
       iter f right
let drzewo = Node(5, Leaf(1), Node(6, Leaf(3), Nil))

//Funkcja zwracająca listę kwadratów liczb nieparzystych od 7 do 19
let kwadrat x = x*x
let lista =
   [7.. 2 .. 19]
   |> List.map (fun n -> kwadrat n)
//Funkcja usuwająca z listy wszystkie liczby pierwsze
let czyPierwsza n =
   let rec f acc =
       match n with
       | n when acc >= n     -> true
       | n when n % acc = 0 -> false
       | _ -> f (acc + 1)
   f 2
let usunPierwsze list = List.filter (fun n -> not(czyPierwsza n)) list

// WYKALD 5 

//Napisz funkcję, która sprawdza, czy lista jest palindromem
let palindrom list =
   list = (List.rev list)
// /Dana jest lista list L = [L1; L2; …;Lk]. Napisz funkcję, która zwraca listę pierwszych elementów Li.
let pierwszyElement list = List.map (fun (x::xs) -> x) list
//Oblicz sumę elementów listy liczb całkowitych korzystając z funkcji List.fold 
let suma list = List.fold (fun state n -> state + n) 0 list
 
let usunzlisty list = (List.filter (fun n -> not (n % 3 = 0 || n % 5 = 0)) list)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code


