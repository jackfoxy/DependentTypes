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

            Tests.runTestsWithArgs defaultConfig args Domain2Types.trimNonEmptyString
            Tests.runTestsWithArgs defaultConfig args Domain2Types.digits
            Tests.runTestsWithArgs defaultConfig args Domain2Types.digits2
            Tests.runTestsWithArgs defaultConfig args Domain2Types.genericSet
        ]
        |> List.sum

