namespace DependentTypes

module console1 =

    [<EntryPoint>]
    let main _ = 

        printfn  ""
        printfn  "LimitedValue"
        DemoLimitedValue.demo1()
        printfn  ""
        printfn  "DependentType"
        DemoDependentType.demo1()

        printfn  ""
        printfn  "LimitedValue"
        DemoLimitedValue.demo2()
        printfn  ""
        printfn  "DependentType"
        DemoDependentType.demo2()

        DemoDependentType.demo4()

        DemoDependentType.demo5()

        DemoDependentType.demo6()

        DemoDependentType.demo6_1()

        DemoDependentType.demo7()

        printfn  ""
        printfn "Hit any key to exit."
        System.Console.ReadKey() |> ignore
        0
