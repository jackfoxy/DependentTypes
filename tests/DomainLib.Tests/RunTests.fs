namespace DomainLib.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args DomainTypes.trimNonEmptyString |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.digits |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.digits2 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.digits3 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.digits4 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.genericSet |> ignore

        0

