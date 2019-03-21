namespace Benchmark.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =
        [
            Tests.runTestsWithArgs defaultConfig args Benchmarks.benchmarkDependentTypeLiftedOption
            Tests.runTestsWithArgs defaultConfig args Benchmarks.benchmarkDependentTypeOption
            Tests.runTestsWithArgs defaultConfig args Benchmarks.benchmarkSomeDependentType
            Tests.runTestsWithArgs defaultConfig args Benchmarks.benchmarkDependentPair
            Tests.runTestsWithArgs defaultConfig args Benchmarks.benchmarkUtcDateTime
        ] 
        |> List.sum
