namespace TypeShape.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args TypeShape.Tests.GenericTests.reflexivity |> ignore

        0

