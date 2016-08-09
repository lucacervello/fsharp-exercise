(* Ricorsione *)

let rec MCD (m : int, n : int) = 
    match m, n with
    | 0, n -> n
    | m, n when m > 0 -> MCD(n % m, m)
    | _ -> failwith "errore"

let simplify (num : int, den : int) =
    let mcd = MCD(num, den)
    (num / mcd, den / mcd)

simplify (15,9) ;; 
// val it : int * int = (5, 3)

simplify (7,5) ;; 
// val it : int * int = (7, 5)

let rec sum1 = function
    | 1 -> 1
    | n -> n + sum1(n - 1)

let rec sum2 = function
    | m, n when m = n -> m
    | m, n -> n + sum2(m, n - 1)

sum1 4 ;;   
// val it : int = 10   // 10 = 0 + 1 + 2 + 3 + 4

sum2 (2,5) ;;
// val it : int = 14   // 14 = 2 + 3 + 4 + 5

let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n - 1) + fib(n - 2)