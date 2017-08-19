
module Giraffe.CurryTest

let v = 24
let ipchar = 's'

let casty (pchar:char) (a:string) (fn:'T -> unit ) = 
    match pchar with
    | 's' -> fn (a :> 'T)
    | 'i' -> fn (int a :> 'T)

let castInt = casty 's' "ted" (fun str -> printfn "%s" str)