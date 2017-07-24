// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open NSubstitute
open Giraffe.Task
open Giraffe.RouterParseCont
open Giraffe.HttpHandlers
open Giraffe.HttpTokenRouter
open Giraffe.HttpRouteArray

type RouterTest () =
    
    let routeArray = [|
        "/"
        "/test"
        "/about"
        "/wheretofindus"
        "/ourstory"
        "/products"
        "/delivery"
        "/data/sunnydrycold/weather"
        "/value/thisisavaluethatisbeingpassed"
        "/auth/dashboard"
        "/auth/inbox"
        "/auth/helpdesk"
        "/auth/parsefirstlong987654321stringssecondandthirdIntegers"
        "/authtoken/as8d7f098adsf897asdf7a09dfasd7f9as7df987"
        "/auth/manager/payroll"
        "/auth/manager/timesheets"
        "/auth/manager/teamview"
        "/auth/manager/teambravosales5345545.34544"
        "/auth/manager/accesscode/57646878"
        "/auth/manager/executive/finance"
        "/auth/manager/executive/operations"
        "/auth/manager/executive/mis"
        "/auth/manager/executive/area/globaloperationcentral"
        "/auth/manager/executive/area/london/district/east/costcode8087"
    |] 

    let tokenApi =
        routeTrie [
            routeT "/" ==> text "Hello world, from Giraffe!"
            routeT "/test" ==> text "Giraffe test working"
            routeT "/about" ==> text "Giraffe about page!"
            routeT "/wheretofindus" ==> text "our location page"
            routeT "/ourstory" ==> text "our story page"
            routeT "/products" ==> text "product page"
            routeT "/delivery" ==> text "delivery page"
            routeTf "/data/%s/weather" (fun v -> sprintf "json (weatherForecasts (%s))" v |> text)
            routeTf "/value/%s" (fun v -> text v) 
            subRouteT "/auth" ==> routeTrie [
                routeT "/dashboard" ==> text "Auth Dashboard"
                routeT "/inbox" ==> text "Auth Inbox"
                routeT "/helpdesk" ==> text "Auth Helpdesk"
                routeTf "/parse%slong%istrings%sand%sIntegers" (fun (a,b,c,d) -> sprintf "%s | %i | %s | %s" a b c d |> text)
                routeTf "token/%s" (fun v -> text ("following token recieved:" + v))                                    
                subRouteT "/manager" ==> routeTrie [
                    routeT "/payroll" ==> text "Manager Payroll"
                    routeT "/timesheets" ==> text "Manager Timesheets"
                    routeT "/teamview" ==> text "Manager Teamview"
                    routeTf "/team%ssales%f" (fun (t,s) -> sprintf "team %s had sales of %f" t s |> text)
                    routeTf "/accesscode/%i" (fun i -> sprintf "manager access close is %i" i |> text)
                    subRouteT "/executive" ==> routeTrie [
                        routeT "/finance" ==> text "executive finance"
                        routeT "/operations" ==> text "executive operations"
                        routeT "/mis" ==> text "executive mis"
                        routeTf "/area/%s" (sprintf "executive area %s" >> text)
                        routeTf "/area/%s/district/%s/costcode%i" (fun (a,d,c) -> sprintf "executive area %s district %s costcode %i"  a d c |> text)
                     ]
                ]
            ]
        ]

    let araryApi =
        router [
            route "/" => text "Hello world, from Giraffe!"
            route "/test" => text "Giraffe test working"
            route "/about" => text "Giraffe about page!"
            route "/wheretofindus" => text "our location page"
            route "/ourstory" => text "our story page"
            route "/products" => text "product page"
            route "/delivery" => text "delivery page"
            routef "/data/%s/weather" (fun v -> sprintf "json (weatherForecasts (%s))" v |> text)
            routef "/value/%s" text 
            route "/auth" => [
                route "/dashboard" => text "Auth Dashboard"
                route "/inbox" => text "Auth Inbox"
                route "/helpdesk" => text "Auth Helpdesk"
                routef "/parse%slong%istrings%sand%sIntegers" (fun (a,b,c,d) -> sprintf "%s | %i | %s | %s" a b c d |> text)
                routef "token/%s" (fun v -> text ("following token recieved:" + v))                                    
                route "/manager" => [
                    route "/payroll" => text "Manager Payroll"
                    route "/timesheets" => text "Manager Timesheets"
                    route "/teamview" => text "Manager Teamview"
                    routef "/team%ssales%f" (fun (t,s) -> sprintf "team %s had sales of %f" t s |> text)
                    routef "/accesscode/%i" (fun i -> sprintf "manager access close is %i" i |> text)
                    route "/executive" => [
                        route "/finance" => text "executive finance"
                        route "/operations" => text "executive operations"
                        route "/mis" => text "executive mis"
                        routef "/area/%s" (sprintf "executive area %s" >> text)
                        routef "/area/%s/district/%s/costcode%i" (fun (a,d,c) -> sprintf "executive area %s district %s costcode %i"  a d c |> text)
                    ]
                ]
            ]
        ]

    [<Benchmark>]
    member  x.RouteArray() =
        routeArray 
        |> Array.iter (fun route -> 
            let ctx = Substitute.For<HttpContext>()
            ctx.Request.Path.ReturnsForAnyArgs (PathString(route)) |> ignore
            ctx.Response.Body <- new MemoryStream()
            
            Task.Factory.StartNew(fun () -> araryApi ctx).Wait() 
            )

    [<Benchmark>]
    member  x.RouteTrie() =
        routeArray 
        |> Array.iter ( fun route ->
            let ctx = Substitute.For<HttpContext>()
            ctx.Request.Path.ReturnsForAnyArgs (PathString(route)) |> ignore
            ctx.Response.Body <- new MemoryStream()
            Task.Factory.StartNew(fun () -> tokenApi ctx).Wait() 
            )

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let summary = BenchmarkRunner.Run<RouterTest>()
    printfn "%A" summary
    0 // return an integer exit code
