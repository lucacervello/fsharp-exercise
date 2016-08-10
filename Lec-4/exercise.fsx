(* Capitolo 4 *)

let rec rmEven = function 
    | [] -> []
    | x :: xs when x % 2 = 0 -> rmEven xs
    | x :: xs -> x :: rmEven xs 

let rm1 = rmEven [-10 .. 10 ] ;; 
// val rm1 : int list = [-9; -7; -5; -3; -1; 1; 3; 5; 7; 9]

let rm2 = rmEven [2; 5; 5; 6; 6; 87; 6; 100; 2] ;; 
// val rm2 : int list =  [5; 5; 87]

let rmOddPos lst =
    let rec rmOddPosIntern lst count =
        match lst, count with
        | [], _ -> []
        | x :: xs, count when count % 2 = 0 -> x :: rmOddPosIntern xs (count + 1)
        | x :: xs, count -> rmOddPosIntern xs (count + 1)
    rmOddPosIntern lst 0

let rmp1 = rmOddPos ['a'  .. 'z'] ;; 
// val rmp1 : char list = ['a'; 'c'; 'e'; 'g'; 'i'; 'k'; 'm'; 'o'; 'q'; 's'; 'u'; 'w'; 'y']

let rmp2 = rmOddPos ["zero" ;  "uno" ; "due" ; "tre" ; "quattro"] ;; 
// val rmp2 : string list = ["zero"; "due" ; "quattro"]

let split lst =
    let rec rmOddPosIntern lst count =
        match lst, count with
        | [], _ -> []
        | x :: xs, count when count % 2 = 0 -> x :: rmOddPosIntern xs (count + 1)
        | x :: xs, count -> rmOddPosIntern xs (count + 1)
    let rec rmEvenPosIntern lst count =
        match lst, count with
        | [], _ -> []
        | x :: xs, count when count % 2 <> 0 -> x :: rmEvenPosIntern xs (count + 1)
        | x :: xs, count -> rmEvenPosIntern xs (count + 1)
    (rmOddPosIntern lst 0 , rmEvenPosIntern lst 0)

let s1 = split [0 .. 9]
// val s1 : int list * int list = ([0; 2; 4; 6; 8], [1; 3; 5; 7; 9])

let s2 = split ["ciao"] ;;
// val s2 : string list * string list = (["ciao"], [])

let s3 = split ["ciao" ; "ciao!!!" ] ;; 
//val s3 : string list * string list = (["ciao"], ["ciao!!!"])

let s4 = split [ 'a' .. 'k'] 
// val s4 : char list * char list = (['a'; 'c'; 'e'; 'g'; 'i'; 'k'], ['b'; 'd'; 'f'; 'h'; 'j'])

let rec cmpLength (first, second) = 
    match first, second with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys -> cmpLength(xs, ys)

let c1 = cmpLength ( [1 .. 10] , ['a' .. 'z'] ) ;; // -1
let c2 = cmpLength ( [1 .. 26] , ['a' .. 'z'] ) ;; // 0
let c3 = cmpLength ( ['a'; 'b';'c'] ,["e" ; "f"]) ; // 1 

let rec remove (elem, lst) = 
    match lst with
    | [] -> []
    | x :: xs when x = elem -> remove (elem, xs)
    | x :: xs -> x :: remove (elem, xs)

let ls1 = remove (2 , [0 ..10] );;
//val ls1 : int list = [0; 1; 3; 4; 5; 6; 7; 8; 9; 10]

let ls2 = remove ( "uva" , [ "mele" ; "uva" ; "pere" ; "uva" ; "banane" ; "uva" ] ) ;;
// val ls2 : string list = ["mele"; "pere"; "banane"]

let ls3 = remove (11 , [0 ..10] );;
// val ls3 : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

let rec removeDup lst =
    match lst with
    | [] -> []
    | x :: xs -> x :: removeDup(remove(x, xs))

let ls4 = removeDup [1; 2; 1; 2; 3] ;;
// val ls4 : int list = [1; 2; 3]

let ls5 = removeDup  [ "mele" ; "uva" ; "mele" ; "pere" ; "uva" ; "banane" ; "uva" ; "pere" ; "pere" ; "banane"] ;;
// val ls5 : string list = ["mele"; "uva"; "pere"; "banane"]

let rec downto0 num =
    match num with
    | 0 -> [0]
    | x -> x :: downto0 (num - 1)

let rec upto num =
    let n = 0
    match num with
    | 0 -> [0]
    | x -> upto (num - 1) @ [x]