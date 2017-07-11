module Giraffe.HttpTokenRouter

open System.Threading.Tasks
open FSharp.Core.Printf
open Microsoft.AspNetCore.Http
open System.Collections.Generic
open Microsoft.FSharp.Reflection
open Giraffe.HttpHandlers
open Giraffe.RouterParsers

// implimenation of (router) Trie Node
// assumptions: memory and compile time not relevant, all about execution speed, initially testing with Dictionary edges

let routerKey = "router_pos"

type RouteState(path:string) =
    member val path = path with get
    member val pos = 0 with get , set

////////////////////////////////////////////////////
// Node Trie using node mapping functions
////////////////////////////////////////////////////

/// Tail Clip: clip end of 'str' string staring from int pos -> end
let inline (-|) (str:string) (from:int) = str.Substring(from,str.Length - from)

let commonPathIndex (str1:string) (idx1:int) (str2:string) =
    let rec go i j =
        if i < str1.Length && j < str2.Length then
            if str1.[i] = str2.[j] 
            then go (i + 1) (j + 1)
            else j                
        else j
    go idx1 0 

let commonPath (str1:string) (str2:string) =
    let rec go i =
        if i < str1.Length && i < str2.Length then
            if str1.[i] = str2.[i] 
            then go (i + 1)
            else i                               
        else i
    go 0

type PathMatch =
| SubMatch of int
| PathInToken
| TokenInPath
| ZeroToken
| ZeroMatch
| FullMatch

let getPathMatch (path:string) (token:string) =
    if token.Length = 0 then ZeroToken
    else
        let cp = commonPath path token
        let tokenMatch = cp = token.Length
        let pathMatch = cp = path.Length
        if cp = 0 then ZeroMatch
        elif tokenMatch && pathMatch then FullMatch
        elif tokenMatch then TokenInPath
        elif pathMatch  then PathInToken
        else SubMatch cp

type Node(token:string) = 
    
    let mutable midFns = []
    let mutable endFns = []
    
    let addMidFn (mfn:MidCont) = midFns <- mfn :: midFns |> List.sortBy (fun f -> f.Precedence)
    let addEndFn (efn:EndCont) = endFns <- efn :: endFns |> List.sortBy (fun f -> f.Precedence) 
    
    let mutable edges = Dictionary<char,Node>()
    
    member __.Edges 
        with get() = edges
        and set v = edges <- v        
    member val Token = token with get,set
    
    member __.MidFns
        with get() = midFns 
        and set v = midFns <- v
    member __.AddMidFn = addMidFn
    member __.EndFns 
        with get()  = endFns 
        and set v = endFns <- v 
    member __.AddEndFn = addEndFn
    member x.EdgeCount 
        with get () = edges.Count
    member x.GetEdgeKeys = edges.Keys
    member x.TryGetValue v = edges.TryGetValue v

    static member AddFn (node:Node) fn =
        match fn with
        | Empty -> ()
        | Mid mfn -> node.MidFns <- mfn :: node.MidFns |> List.sortBy (fun f -> f.Precedence)
        | End efn -> node.EndFns <- efn :: node.EndFns |> List.sortBy (fun f -> f.Precedence)

    static member Split (node:Node) (pos:int) =
        // need to split existing node out
        let sedges = node.Edges //get ref to pass to split node
        let baseToken = node.Token.Substring(0,pos) //new start base token
        let childToken = (node.Token -| pos)
        let snode = Node(childToken)
        node.Edges <- Dictionary<_,_>() //wipe edges from node before adding new edge
        node.Edges.Add(childToken.[0],snode)
        //node.Add childToken Empty // create split node
        node.Token <- baseToken
        snode.Edges <- sedges //pass old edges dictionary to split node

        //copy over existing functions
        snode.MidFns <- node.MidFns
        snode.EndFns <- node.EndFns
        //clear functions from existing node 
        node.MidFns <- List.empty
        node.EndFns <- List.empty 

    static member AddPath (node:Node) (path:string) (rc:ContType) =
        match getPathMatch path node.Token with
        | ZeroToken ->
            // if node empty/root
            node.Token <- path
            Node.AddFn node rc
            node
        | ZeroMatch ->
            // HACK: failwith <| sprintf "path passed to node with non-matching start in error:%s" path
            node
        | FullMatch -> 
            Node.AddFn node rc
            node
        | PathInToken ->
            Node.Split node (path.Length)
            Node.AddFn node rc
            node 
        | TokenInPath ->
            //path extends beyond this node
            let rem = path -| (node.Token.Length)
            match node.TryGetValue rem.[0] with
            | true, cnode ->
                Node.AddPath cnode rem rc // recursive path scan
            | fales, _    ->
                let nnode = Node(rem)
                node.Edges.Add(rem.[0], nnode)
                Node.AddFn nnode rc
                nnode
        | SubMatch (i) ->
            Node.Split node (i)
            let rem = path -| i
            let nnode = Node(rem)
            node.Edges.Add(rem.[0],nnode)
            Node.AddFn nnode rc
            nnode 
                        
// Route Continuation Functions    
and MidCont =
| SubRouteMap of HttpHandler
| ApplyMatch of (char * (char []) * (Node)) // (parser , nextChar , contNode) list
| ApplyMatchAndComplete of ( char * int * (obj -> HttpHandler)) // (lastParser, No# Parsers, Cont Fn)
    member x.Precedence
        with get () =
            match x with
            | SubRouteMap _ -> 1
            | ApplyMatch (c,_,_) -> (int c)
            | ApplyMatchAndComplete (c,_,_) -> 256 + (int c) 
and EndCont = 
| HandlerMap of HttpHandler
| MatchComplete of ( (int) * (obj -> HttpHandler) ) // ( No# Parsers, Cont Fn) 
    member x.Precedence
        with get () =
            match x with
            | HandlerMap _ -> 1
            | MatchComplete _ -> 2 
and ContType =
| Empty
| Mid of MidCont
| End of EndCont   


////////////////////////////////////////////////////
// Helper Functions
////////////////////////////////////////////////////

// Bindy is a hack to encapsulate type inferance application in node trie of multiple types, partially applied functions fail
// type Bindy() =
//     member x.EatMe<'U,'T> (sf:StringFormat<'U,'T>) (fn : 'T -> HttpHandler) (v2:obj) = v2 :?> 'T |> fn

// let inline bindMe (sf:StringFormat<'U,'T>) (fn : 'T -> HttpHandler) = 
//     let b = Bindy()
//     b.EatMe<'U,'T> sf fn

// temporary compose out handler to allow composition out of route functions, same as wraping in () or using <|
let inline (==>) (a:HttpHandler -> Node -> Node) (b:HttpHandler) = a b

let addCharArray (c:char) (ary:char []) =
    if ary |> Array.exists (fun v -> v = c) then
        ary
    else 
        let nAry = Array.zeroCreate<_>(ary.Length + 1)
        Array.blit ary 0 nAry 0 ary.Length
        nAry.[ary.Length] <- c
        nAry

// helper to get child node of same match format (slow for now, needs optimisation)
let getPostMatchNode fmt (nxt:char) (ils:MidCont list) = 
    let rec go (ls:MidCont list) (acc:MidCont list) (no:Node option) =
        match ls with
        | [] -> 
            match no with 
            | None -> 
                let n = Node("")
                n ,(ApplyMatch(fmt,[|nxt|],n)) :: acc |> List.sortBy (fun fn -> fn.Precedence)
            | Some n -> n, acc |> List.sortBy (fun fn -> fn.Precedence)
        | hfn :: tfns ->
            match hfn with
            | ApplyMatch (f,ncl,n) ->
                if f = fmt then
                    let nncl = addCharArray nxt ncl
                    go tfns (ApplyMatch(f,nncl,n)::acc) (Some(n))
                    // finished as found matched format but need to complete acc list
                else go tfns (hfn::acc) no
            | _ -> go tfns (hfn::acc) no
    go ils [] None
////////////////////////////////////////////////////
// Routing Node Map Functions used to build trie
////////////////////////////////////////////////////

// Simple route that iterates down nodes and if function found, execute as normal
let routeT (path:string) (fn:HttpHandler) (root:Node) = 
    Node.AddPath root path (fn |> HandlerMap |> End)

let subRouteT (path:string) (fn:HttpHandler) (root:Node) =
    Node.AddPath root path (fn |> SubRouteMap |> Mid)  

// parsing route that iterates down nodes, parses, and then continues down further notes if needed
let routeTf (path : StringFormat<_,'T>) (fn:'T -> HttpHandler) (root:Node)=
    let last = path.Value.Length - 1

    let rec go i ts pcount (node:Node) =
        let pl = path.Value.IndexOf('%',i)
        if pl < 0 || pl = last then
            //Match Complete (no futher parse '%' chars
            if pcount = 0 then
                failwith "'routef' (route Parse) used with no arguments? please add % format args or change to simple 'route' for non-parse routes"
            else
                Node.AddPath node (path.Value -| ts) (MatchComplete( pcount , fun(o:obj) -> (o :?> 'T) |> fn ) |> End)              
        else 
            let fmtChar = path.Value.[pl + 1]
            // overrided %% -> % case
            if fmtChar = '%' then
                //keep token start (+1 just one %), skip 
                go (pl + 2) (ts + 1) pcount node
            // formater with valid key
            else if formatMap.ContainsKey fmtChar then

                if pl + 1 = last then // if finishes in a parse
                    if node.MidFns |> List.exists (function | ApplyMatchAndComplete(c,_,_) -> fmtChar = c | _ -> false )
                    then sprintf "duplicate paths detected '%s', Trie Build skipping..." path.Value |> failwith
                    else
                        Node.AddPath node (path.Value.Substring(ts,pl - ts)) (ApplyMatchAndComplete( fmtChar , pcount + 1 , (fun (o:obj) -> o :?> 'T |> fn )) |> Mid)
                else //otherwise add mid pattern parse apply
                    //get node this parser will be on
                    let nnode = Node.AddPath node (path.Value.Substring(ts,pl - ts)) Empty
                    let cnode,midFns = getPostMatchNode fmtChar path.Value.[pl+2] nnode.MidFns                                                    
                    nnode.MidFns <- midFns //update adjusted functions
                    go (pl + 2) (pl + 2) (pcount + 1) cnode
            // badly formated format string that has unknown char after %
            else
                failwith (sprintf "Routef parsing error, invalid format char identifier '%c' , should be: b | c | s | i | d | f" fmtChar)
                go (pl + 1) ts pcount node

    go 0 0 0 root 

// choose root will apply its root node to all route mapping functions to generate Trie at compile time, function produced will take routeState (path) and execute appropriate function


// process path fn that returns httpHandler
let private processPath (rs:RouteState) (root:Node) : HttpHandler =
    fun (ctx:HttpContext) -> //(succ:Continuation) (fail:Continuation)
    
    let path : string = rs.path
    let ipos = rs.pos
    let last = path.Length - 1

    let rec checkCompletionPath (pos:int) (node:Node) = // this funciton is only used by parser paths
        //this function doesn't test array bounds as all callers do so before
        let success(pos,node) = struct (true,pos,node)
        let failure(pos)      = struct (false,pos,Unchecked.defaultof<Node>)

        if commonPathIndex path pos node.Token = node.Token.Length then
            let nxtChar = pos + node.Token.Length
            if (nxtChar - 1) = last then //if this pattern match shares node chain as substring of another
                if node.EndFns.IsEmpty
                then failure pos //pos, None
                else success(nxtChar,node) //nxtChar, Some node
            else
                match node.TryGetValue path.[nxtChar] with
                | true, cnode ->
                    checkCompletionPath nxtChar cnode
                | false, _ ->
                    // no further nodes, either a static url didnt match or there is a pattern match required            
                    if node.MidFns.IsEmpty
                    then failure pos
                    else success(nxtChar,node)
        else failure pos
    
    /// (next match chars,pos,match completion node) -> (parse end,pos skip completed node,skip completed node) option
    let rec getNodeCompletion (cs:char []) pos (node:Node) =
        let success(prend,nxtpos,nxtnode) = struct (true,prend,nxtpos,nxtnode)
        let failure                       = struct (false,0,0,Unchecked.defaultof<Node>)

        match path.IndexOfAny(cs,pos) with // jump to next char ending (possible instr optimize vs node +1 crawl) 
        | -1 -> failure
        | x1 -> //x1 represents position of match close char but rest of chain must be confirmed 
            match checkCompletionPath x1 node with
            | struct(true,x2,cn) -> success(x1 - 1,x2,cn)                 // from where char found to end of node chain complete
            | struct(false,x2,_) -> getNodeCompletion cs (x1 + 1) node // char foundpart of match, not completion string

    let createResult (args:obj list) (argCount:int) (fn:obj -> HttpHandler) =
        let input =  
            match argCount with
            | 0 -> Unchecked.defaultof<obj> //HACK: need routeF to throw error on zero args
            | 1 -> args.Head // HACK: look into performant way to safely extract
            | _ ->
                let values = Array.zeroCreate<obj>(argCount) //<<< this can be pooled?
                let valuesTypes = Array.zeroCreate<System.Type>(argCount) //<<< this should be cached with handler
                let rec revmap ls i = // List.rev |> List.toArray not used to minimise list traversal
                    if i < 0 then ()
                    else
                        match ls with
                        | [] -> ()
                        | h :: t -> 
                            values.[i] <- h
                            valuesTypes.[i] <- h.GetType()
                            revmap t (i - 1)
                revmap args (argCount - 1)
                
                let tupleType = FSharpType.MakeTupleType valuesTypes
                FSharpValue.MakeTuple(values, tupleType)
        fn input ctx

    let saveRouteState pos = 
        rs.pos <- pos
        ctx.Items.[routerKey] <- rs 

    let rec processEnd (fns:EndCont list) pos args =
        match fns with
        | [] -> Task.FromResult None
        | h :: t ->
            match h with                    
            | HandlerMap fn -> fn ctx // run function with all parameters
            | MatchComplete (i,fn) -> createResult args i fn 

    let rec processMid (fns:MidCont list) pos args =
        
        let applyMatchAndComplete pos acc ( f,i,fn ) tail =
            match formatMap.[f].Invoke(path,pos,last) with
            | struct(true, o) -> createResult (o :: acc) i fn
            | struct(false,_) -> processMid tail pos acc // ??????????????????
        
        let rec applyMatch (f:char,ca:char[],n) pos acc tail  =
            match getNodeCompletion ca pos n with
            | struct(true,fpos,npos,cnode) ->
                match formatMap.[f].Invoke(path, pos, fpos) with
                | struct(true, o) -> 
                    if npos - 1 = last then //if have reached end of path through nodes, run HandlerFn
                        processEnd cnode.EndFns npos (o::acc)
                    else
                        processMid cnode.MidFns npos (o::acc)
                | struct(false,_) -> processMid tail pos acc // keep trying    
            | struct(false,_,_,_) -> processMid tail pos acc // subsequent match could not complete so fail
        
        match fns with
        | [] -> Task.FromResult None
        | h :: t ->
            match h with
            | ApplyMatch x -> applyMatch x pos args t
            | ApplyMatchAndComplete x -> applyMatchAndComplete pos args x t
            | SubRouteMap (fn) ->
                saveRouteState pos
                fn ctx

    let rec crawl (pos:int) (node:Node) =
        let cp = commonPathIndex path pos node.Token 
        if cp = node.Token.Length then
            let nxtChar = pos + node.Token.Length 
            if (nxtChar - 1 ) = last then //if have reached end of path through nodes, run HandlerFn
                processEnd node.EndFns pos []
            else
                match node.TryGetValue path.[nxtChar] with
                | true, cnode ->
                    if (pos + cnode.Token.Length ) = last then //if have reached end of path through nodes, run HandlerFn
                        processEnd cnode.EndFns (pos + node.Token.Length) []
                    else                //need to continue down chain till get to end of path
                        crawl (nxtChar) cnode
                | false, _ ->
                    // no further nodes, either a static url didnt match or there is a pattern match required            
                    processMid node.MidFns nxtChar []
        else 
            printfn ">> failed to match %s path with %s token, commonPath=%i" (path.Substring(pos)) (node.Token) (commonPathIndex path pos node.Token)
            Task.FromResult None            

    crawl ipos root

let routeTrie (fns:(Node->Node) list) : HttpHandler =
    let root = Node("/")
    // precompile the route functions into node trie
    let rec go ls =
        match ls with
        | [] -> ()
        | h :: t ->
            h root |> ignore
            go t
    go fns

    fun ctx ->
        //get path progress (if any so far)
        let routeState =
            match ctx.Items.TryGetValue routerKey with
            | true, (v:obj) -> v :?> RouteState  
            | false,_-> RouteState(ctx.Request.Path.Value)
        processPath routeState root ctx

