namespace DependentTypes.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args DependentType.dependentTypes |> ignore
        

        0
