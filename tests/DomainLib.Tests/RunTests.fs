namespace DomainLib.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        [
            Tests.runTestsWithArgs defaultConfig args DomainTypes.trimNonEmptyString
            Tests.runTestsWithArgs defaultConfig args DomainTypes.digits
            Tests.runTestsWithArgs defaultConfig args DomainTypes.digits2
            Tests.runTestsWithArgs defaultConfig args DomainTypes.genericSet
        ]
        |> List.sum

