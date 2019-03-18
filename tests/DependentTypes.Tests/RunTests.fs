namespace DependentTypes.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        [
            Tests.runTestsWithArgs defaultConfig args DependentType.optionDependentType
            Tests.runTestsWithArgs defaultConfig args SomeDependentType.someDependentType

            Tests.runTestsWithArgs defaultConfig args DependentType.optionDependentTypeHelpers
            Tests.runTestsWithArgs defaultConfig args DependentType.singleDependentType
            Tests.runTestsWithArgs defaultConfig args DependentType.sumDependentType
            Tests.runTestsWithArgs defaultConfig args DependentType.convertsToDependentType
            Tests.runTestsWithArgs defaultConfig args DependentType.dependentPairs
        ] 
        |> List.sum
