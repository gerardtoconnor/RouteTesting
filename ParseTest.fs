module ParseTest


type ParseTest() =

    let parseText = [|
        ("start-168.599813578end",5,18,(fun _ -> ()),(fun () -> ()))
        ("start1316.721639001end",5,18,(fun _ -> ()),(fun () -> ()))
        ("start230.403534506end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start303.006177823end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start616.793788421end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start655.960456083end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start-99.401072435end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start147.560806278end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start890.817104294end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start1266.386647938end",5,18,(fun _ -> ()),(fun () -> ()))
        ("start368.8810159end",5,15,(fun _ -> ()),(fun () -> ()))
        ("start329.908783784end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start328.145796772end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start915.924245139end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start15034.956228428end",5,19,(fun _ -> ()),(fun () -> ()))
        ("start2170.404971104end",5,18,(fun _ -> ()),(fun () -> ()))
        ("start1001.824773653end",5,18,(fun _ -> ()),(fun () -> ()))
        ("start1363.003407471end",5,18,(fun _ -> ()),(fun () -> ()))
        ("start205.679093658end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start57.745504903end",5,16,(fun _ -> ()),(fun () -> ()))
        ("start97.998604275end",5,16,(fun _ -> ()),(fun () -> ()))
        ("start1269.830376697end",5,18,(fun _ -> ()),(fun () -> ()))
        ("start-13.789008528end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start5615.029936306end",5,18,(fun _ -> ()),(fun () -> ()))
        ("start-66.224978482end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start5479.740277866end",5,18,(fun _ -> ()),(fun () -> ()))
        ("start929.599745036end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start201.217753121end",5,17,(fun _ -> ()),(fun () -> ()))
        ("start-480.434318039end",5,18,(fun _ -> ()),(fun () -> ()))
        ("start2272.546694673end",5,18,(fun _ -> ()),(fun () -> ()))

    |]

    
    let parseDict = ParseFactory.FormatMap<unit> ()
    let parse1 = parseDict.['f']
    let parse2 = parseDict.['z']

    [<Benchmark>]
    member x.ParseFloatsAlt() =
        for txt in parseText do
            parse2 txt

    [<Benchmark>]
    member x.ParseFloastOG() =
        for txt in parseText do
            parse1 txt    


