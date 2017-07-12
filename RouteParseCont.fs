module Giraffe.RouterParseCont

open System
open System.Threading.Tasks
open OptimizedClosures 

type Parser<'T> = (string*int*int*(obj->'T)*(unit->'T)) -> 'T


module ParseFactory =
    let FormatMap<'T> () =

        let inline between x l u = (x - l) * (u - x) >= LanguagePrimitives.GenericZero

        /// Private Range Parsers that quickly try parse over matched range (all fpos checked before running in preceeding functions)

        let stringParse (path:string,ipos,fpos,cont,_) = 
            path.Substring(ipos,fpos - ipos + 1) |> box |> cont

        let charParse (path:string,ipos,_,cont,_) = path.[ipos] |> box |> cont // this is not ideal method (but uncommonly used)

        let boolParse (path:string,ipos,fpos,cont:obj->'T,fail:unit->'T) =
            if between (fpos - ipos) 4 5 then 
                match path.[ipos] with
                | 't' | 'T' -> true  |> box |> cont // todo: Laxy matching, i'll complete later
                | 'f' | 'F' -> false |> box |> cont
                | _ -> fail ()
            else fail ()

        let intParse (path:string,ipos,fpos,cont:obj->'T,fail:unit->'T) =

            let mutable result = 0
            let mutable negNumber = false
            let rec go pos =
                let charDiff = int path.[pos] - int '0'
                if between charDiff 0 9 then
                    result <- (result * 10) + charDiff
                    if pos = fpos then
                        if negNumber then - result else result 
                        |> box |> cont 
                    else go (pos + 1)       // continue iter
                else fail ()
            //Start Parse taking into account sign operator
            match path.[ipos] with
            | '-' -> negNumber <- true ; go (ipos + 1)
            | '+' -> go (ipos + 1)
            | _ -> go (ipos)
            
        let int64Parse (path:string,ipos,fpos,cont:obj->'T,fail:unit->'T) =

            let mutable result = 0L
            let mutable negNumber = false
            let rec go pos =
                let charDiff = int64 path.[pos] - int64 '0'
                if between charDiff 0L 9L then
                    result <- (result * 10L) + charDiff
                    if pos = fpos then
                        if negNumber then - result else result 
                        |> box |> cont
                    else go (pos + 1)       // continue iter
                else fail ()
            //Start Parse taking into account sign operator
            match path.[ipos] with
            | '-' -> negNumber <- true ; go (ipos + 1)
            | '+' -> go (ipos + 1)
            | _ -> go (ipos)


        let decPower = [|1.;10.;100.;1000.;10000.;100000.;1000000.;10000000.;100000000.;100000000.;1000000000.|] 
        let decDivide = [|1.;10.;100.;1000.;10000.;100000.;1000000.;10000000.;100000000.;100000000.;1000000000.|] |> Array.map (fun d -> 1. / d) // precompute inverse once at compile time
            
        let floatParse (path:string,ipos,fpos,cont:obj->'T,fail:unit->'T) =
            let mutable result = 0.
            let mutable decPlaces = 0
            let mutable negNumber = false
            
            let rec go pos =
                if path.[pos] = '.' then
                    decPlaces <- 1
                    if pos < fpos then go (pos + 1) else fail ()
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
                            |> cont
                        else go (pos + 1)   // continue iter
                    else fail ()   // Invalid Character in path

            //Start Parse taking into account sign operator
            match path.[ipos] with
            | '-' -> negNumber <- true ; go (ipos + 1)
            | '+' -> go (ipos + 1)
            | _ -> go (ipos)

        let floatParse2 (path:string,ipos,fpos,cont:obj->'T,fail:unit->'T) : 'T =
            
            let rec godec(pos,n,d,dp,nn) = 
                let charDiff = int path.[pos] - int '0'
                if between charDiff 0 9 then
                    let d' = (d * 10L) + (int64 charDiff)
                    let dp' = dp + 1
                    if pos = fpos || dp > 9 then
                        (float n) + ((float d') / (decPower.[dp + 1 ])) * if nn then - 1. else 1. 
                        |> box |> cont 
                    else
                        godec (pos + 1,n,d',dp',nn)
                else fail ()

            let rec go(pos,n,nn) =
                if path.[pos] = '.' then
                    if pos < fpos 
                    then godec(pos + 1,n,0L,1,nn) 
                    else fail ()
                else
                    let charDiff = int path.[pos] - int '0'
                    if between charDiff 0 9 then
                        let n' = (n * 10L) + (int64 charDiff)
                        if pos = fpos then
                            (float n') * if nn then - 1. else 1. 
                            |> box |> cont 
                        else go (pos + 1,n',nn)   // continue iter
                    else fail()  // Invalid Character in path

            // //Start Parse taking into account sign operator
            match path.[ipos] with
            | '-' -> go (ipos + 1,0L,true)
            | '+' -> go (ipos + 1,0L,false)
            | _ -> go (ipos,0L,false)

    // Char    Range Parser
    // ---------------  -------------------------------------------
        dict [
            ('b',  boolParse  )  // bool
            ('c',  charParse  )  // char
            ('s',  stringParse)  // string
            ('i',  intParse   )  // int
            ('d',  int64Parse )  // int64
            ('f',  floatParse )  // float
            ('z',  floatParse2 )  // float
        ]

    // match floatParse2 "123.0123456789" 0 11 with
    // | Some f -> printfn ">>> %.10f" (f :?> float)
   // | None -> ()

// let parseDict = ParseFactory.FormatMap<unit> ()
// let parse1 = parseDict.['f']
// let parse2 = parseDict.['z']

//parse2 ("start4188.088825874end",5,18,(fun v -> printf "%.9f," (v :?> float)),(fun () -> ()))
