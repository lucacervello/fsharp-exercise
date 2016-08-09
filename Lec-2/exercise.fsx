let costo (cod : string, prezzoDi : string->float, scontoDi : string -> int) = 
    let prezzo = prezzoDi(cod)
    let sconto = prezzo * float(scontoDi(cod)) / 100.0
    prezzo - sconto

let prA = function
    | "cod1" -> 20.0
    | "cod2" -> 50.50
    | _ -> failwith "Errore" 

let prB = function
    | "cod1" -> 40.0
    | "cod2" -> 100.50 
    | _ -> failwith "Errore" 

let scA = function
    | "cod1" -> 10
    | "cod2" -> 0 
    | _ -> failwith "Errore" 

let scB = function
    | "cod1" -> 5
    | "cod2" -> 25 
    | _ -> failwith "Errore" 

let c1AA = costo("cod1", prA, scA)
// val c1AA : float = 18.0

let c1BA = costo( "cod1" , prB, scA )
// val c1BA : float = 36.0 

let c2BB = costo( "cod2" , prB, scB )
// val c2BB : float =  75.375