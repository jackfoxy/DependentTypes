namespace Benchmark.Tests

open DependentTypes
open Expecto

module PercentType =
    let validatePercent _ (n : float) = 
        match n >= 0. && n <= 1. with
        | true -> Some n
        | false -> None

    type PercentValidator() = 
        inherit PiType<unit, float, float option>((), validatePercent)

    type PairPercentValidator() = 
        inherit SigmaType<unit, float, float option>((), validatePercent)

type Percent = DependentType<PercentType.PercentValidator, unit, float, float option>
type PercentPair = DependentPair<PercentType.PairPercentValidator, unit, float, float option>

type FooOption =
    {
        Pct : float option
    }

type FooDependentType =
    {
        Pct : Percent
    }

module Benchmarks =
    let runVanillaOption() =
        [|
            {FooOption.Pct = PercentType.validatePercent () 0.5}
            {FooOption.Pct = PercentType.validatePercent () 2.5}
        |] 

    let runDependentType() =
        [|
            {FooDependentType.Pct = Percent.Create 0.5}
            {FooDependentType.Pct = Percent.Create 2.5}
        |] 

    let inline repeat10 f a =
        let mutable v = f a
        v <- f a
        v <- f a
        v <- f a
        v <- f a
        v <- f a
        v <- f a
        v <- f a
        v <- f a
        v <- f a
        v

    let inline repeat100 f a = repeat10 (repeat10 f) a
    let inline repeat1000 f a = repeat10 (repeat100 f) a
    let inline repeat10000 f a = repeat10 (repeat1000 f) a
    let inline repeat100000 f a = repeat10 (repeat10000 f) a
    let inline repeat1000000 f a = repeat10 (repeat100000 f) a

    [<Tests>]
    let benchmarkDependentType =
        testList "Option DependentTypes vs option" [
            testCase "Option DependentType vs vanilla option" <| fun _ ->
                Expect.isFasterThan
                    (runVanillaOption >> ignore |> repeat1000000)
                    (runDependentType >> ignore |> repeat1000000)
                    "vanilla option is faster than Option DependentType"
        ]
