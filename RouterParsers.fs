module Giraffe.RouterParsers

open System
open System.Threading.Tasks
open OptimizedClosures 

type Parser = FSharpFunc<string,int,int,struct(bool*obj)>

let inline between x l u = (x - l) * (u - x) >= LanguagePrimitives.GenericZero

let rtrn (o:obj) = struct (true ,o)
let failure      = struct (false,Unchecked.defaultof<obj>)

/// Private Range Parsers that quickly try parse over matched range (all fpos checked before running in preceeding functions)

let private stringParse (path:string) ipos fpos = path.Substring(ipos,fpos - ipos + 1) |> box |> rtrn

let private  charParse (path:string) ipos _ = path.[ipos] |> box |> rtrn // this is not ideal method (but uncommonly used)

let private boolParse (path:string) ipos fpos =
    if between (fpos - ipos) 4 5 then 
        match path.[ipos] with
        | 't' | 'T' -> true  |> box |> rtrn // todo: Laxy matching, i'll complete later
        | 'f' | 'F' -> false |> box |> rtrn
        | _ -> failure
    else failure

let private intParse (path:string) ipos fpos =

    let mutable result = 0
    let mutable negNumber = false
    let rec go pos =
        let charDiff = int path.[pos] - int '0'
        if between charDiff 0 9 then
            result <- (result * 10) + charDiff
            if pos = fpos then
                if negNumber then - result else result 
                |> box |> rtrn 
            else go (pos + 1)       // continue iter
        else failure
    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)
    
let private int64Parse (path:string) ipos fpos =

    let mutable result = 0L
    let mutable negNumber = false
    let rec go pos =
        let charDiff = int64 path.[pos] - int64 '0'
        if between charDiff 0L 9L then
            result <- (result * 10L) + charDiff
            if pos = fpos then
                if negNumber then - result else result 
                |> box |> rtrn 
            else go (pos + 1)       // continue iter
        else failure
    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)


let decPower = [|1.;10.;100.;1000.;10000.;100000.;1000000.;10000000.;100000000.;100000000.|] 
let decDivide = [|1.;10.;100.;1000.;10000.;100000.;1000000.;10000000.;100000000.;100000000.|] |> Array.map (fun d -> 1. / d) // precompute inverse once at compile time
    
let floatParse (path:string) ipos fpos =
    let mutable result = 0.
    let mutable decPlaces = 0
    let mutable negNumber = false
    
    let rec go pos =
        if path.[pos] = '.' then
            decPlaces <- 1
            if pos < fpos then go (pos + 1) else failure
        else
            let charDiff = float path.[pos] - float '0'
            if between charDiff 0. 9. then
                if decPlaces = 0 then 
                    result <- (result * 10.) + charDiff
                else
                    //result <- result + charDiff 
                    result <- result + ( charDiff * decDivide.[decPlaces]) // char is divided using multiplication of pre-computed divisors
                    decPlaces <- decPlaces + 1
                if pos = fpos || decPlaces > 9 then
                    if negNumber then - result else result 
                    |> box |> rtrn
                else go (pos + 1)   // continue iter
            else failure   // Invalid Character in path

    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)

let floatParse2 (path:string) ipos fpos =
    let mutable result = 0.
    let mutable nominator = 0L
    let mutable demoninator = 0L
    let mutable decPlaces = 0
    let mutable negNumber = false
    
    let rec go pos =
        if path.[pos] = '.' then
            decPlaces <- 1
            if pos < fpos then go (pos + 1) else failure
        else
            let charDiff = int path.[pos] - int '0'
            if between charDiff 0 9 then
                if decPlaces = 0 then 
                    nominator <- (nominator * 10L) + int64 charDiff
                else
                    //result <- result + charDiff 
                    demoninator <- (demoninator * 10L) + int64 charDiff 
                    //result <- result + ( charDiff * decPower.[decPlaces]) // char is divided using multiplication of pre-computed divisors
                    decPlaces <- decPlaces + 1
                if pos = fpos || decPlaces > 9 then
                    (float nominator) + (float demoninator) * (decPower.[decPlaces]) * if negNumber then - 1. else 1. 
                    |> box |> rtrn 
                else go (pos + 1)   // continue iter
            else failure   // Invalid Character in path

    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)


let formatMap =
    dict [
    // Char    Range Parser
    // ---------------  -------------------------------------------
        'b', (FSharpFunc<_,_,_,_>.Adapt boolParse  )  // bool
        'c', (FSharpFunc<_,_,_,_>.Adapt charParse  )  // char
        's', (FSharpFunc<_,_,_,_>.Adapt stringParse)  // string
        'i', (FSharpFunc<_,_,_,_>.Adapt intParse   )  // int
        'd', (FSharpFunc<_,_,_,_>.Adapt int64Parse )  // int64
        'f', (FSharpFunc<_,_,_,_>.Adapt floatParse )  // float
    ]

// match floatParse2 "123.0123456789" 0 11 with
// | Some f -> printfn ">>> %.10f" (f :?> float)
// | None -> ()
