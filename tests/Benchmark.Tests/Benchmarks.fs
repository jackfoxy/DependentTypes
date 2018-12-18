namespace Benchmark.Tests

open DependentTypes
open DependentTypes.Helpers
open DomainLib
open Expecto
open System

module PercentType =
    let validatePercent _ (n : float) = 
        match n >= 0. && n <= 1. with
        | true -> Some n
        | false -> None

    type PercentValidator() = 
        inherit Pi<unit, float, float option>((), validatePercent)

    type PairPercentValidator() = 
        inherit Sigma<unit, float, float option>((), validatePercent)

type Percent = DependentType<PercentType.PercentValidator, unit, float, float option>
type PercentPair = DependentPair<PercentType.PairPercentValidator, unit, float, float option>

module Benchmarks =
    let runNakedOption() =
        [|
            Some 0.5
            Some 2.5
        |] 

    let runPctOption() =
        [|
            PercentType.validatePercent () 0.5
            PercentType.validatePercent () 2.5
        |] 

    let runLiftedPctDependentType() =
        [|
            Percent.TryCreate 0.5
            Percent.TryCreate 2.5
        |] 
    let runPctDependentType() =
        [|
            Percent.Create 0.5
            Percent.Create 2.5
        |] 

    let verifyUtcDateTime _ (value : DateTime) =
        value.ToUniversalTime() 

    let runDateTimeUtc() =
        [|
            verifyUtcDateTime () <| DateTime(2018, 11, 11, 11, 11, 11,DateTimeKind.Local)
        |] 

    let runDependentTypeDateTimeUtc() =
        [|
            UtcDateTime.Create <| DateTime(2018, 11, 11, 11, 11, 11,DateTimeKind.Local)
        |] 

    let runPctPair() =
        [|
            (0.5, PercentType.validatePercent () 0.5)
            (2.5, PercentType.validatePercent () 2.5)
        |] 

    let runPctDependentPair() =
        [|
            PercentPair.Create 0.5
            PercentPair.Create 2.5
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

    let vanillaOption = runPctOption()
    let readVanillaOption (xs : float option[]) =
        xs
        |> Array.map ( fun x ->
            match x with
            | Some pct -> Some pct
            | None -> None )
    let readVanilla() = readVanillaOption vanillaOption

    let liftedDependentType = runLiftedPctDependentType()

    let vanillaPair = runPctPair()
    let readVanillaPair (xs : (float * float option)[]) =
        xs
        |> Array.map ( fun x -> fst x, snd x )
    let readPair() = readVanillaPair vanillaPair

    let dateTime = runDateTimeUtc()
    let readDatetime (xs : DateTime[]) = 
        xs
        |> Array.map ( fun x -> x)
    let readDateTime()  = readDatetime dateTime

    [<Tests>]
    let benchmarkDependentTypeLiftedOption =
        testList "option vs DependentType option" [
            test "naked option vs DependentType option: TryCreate" {
                Expect.isFasterThan
                    (runNakedOption >> ignore |> repeat1000000)
                    (runLiftedPctDependentType >> ignore |> repeat1000000)
                    "Naked option is faster than TryCreate DependentType option" }

            test "validated option vs DependentType option: TryCreate" {
                Expect.isFasterThan
                    (runPctOption >> ignore |> repeat1000000)
                    (runLiftedPctDependentType >> ignore |> repeat1000000)
                    "Validated option is faster than TryCreate DependentType option" }

            test "validated option vs DependentType option: TryCreate 10X" {
                Expect.isFasterThan
                    (runPctOption >> ignore |> repeat10)
                    (runLiftedPctDependentType >> ignore |> repeat10)
                    "(10X) validated option is faster than TryCreate DependentType option" }

            test "option vs DependentType option: read" {
                let dtValues = runLiftedPctDependentType()
                let readDependentType (xs : Percent option []) =
                    xs
                    |> Array.map ( fun x ->
                        match x with
                        | Some _ -> Some (someValue x)
                        | None -> None )
                let readDependentType() = readDependentType dtValues

                Expect.isFasterThan
                    (readVanilla >> ignore |> repeat1000000)
                    (readDependentType >> ignore |> repeat1000000)
                    "Read Option is faster than Option DependentType" }

            test "Option vs Option DependentType: read value" {
                let dtValues = runLiftedPctDependentType()
                let readDependentType (xs : Percent option []) =
                    xs
                    |> Array.map ( fun x ->
                    match x with
                    | Some pct -> Some pct.Value.Value
                    | None -> None )
                let readDependentType() = readDependentType dtValues

                Expect.isFasterThan
                    (readVanilla >> ignore |> repeat1000000)
                    (readDependentType >> ignore |> repeat1000000)
                    "Read Option is faster than Option DependentType Value.Value" }
        ]

    [<Tests>]
    let benchmarkDependentTypeOption =
        testList "option vs DependentType" [
            test "naked option vs DependentType: Create" {
                Expect.isFasterThan
                    (runNakedOption >> ignore |> repeat1000000)
                    (runPctDependentType >> ignore |> repeat1000000)
                    "Naked option is faster than Create DependentType" }

            test "validated option vs DependentType option: Create" {
                Expect.isFasterThan
                    (runPctOption >> ignore |> repeat1000000)
                    (runPctDependentType >> ignore |> repeat1000000)
                    "Validated option is faster than Create DependentType" }

            test "validated option vs DependentType option: Create 10X" {
                Expect.isFasterThan
                    (runPctOption >> ignore |> repeat10)
                    (runPctDependentType >> ignore |> repeat10)
                    "(10X) validated option is faster than Create DependentType" }

            test "DependentType option Create vs DependentType option TryCreate" {
               Expect.isFasterThan
                   (runPctDependentType >> ignore |> repeat1000000)
                   (runLiftedPctDependentType >> ignore |> repeat1000000)
                   "Create DependentType is faster than TryCreate DependentType" }
        ]

    [<Tests>]
    let benchmarkDependentPair =
        testList "pair vs DependentPair" [
            test "pair vs DependentPair: Create" {
                Expect.isFasterThan
                    (runPctPair >> ignore |> repeat1000000)
                    (runPctDependentPair >> ignore |> repeat1000000)
                    "Pair is faster than Create DependentPair" }

            // This benchmark is always close and sometimes fails.
            // Expecto still does not have a good test option for such cases.
            ptest "pair vs DependentPair: read value" {
                let dpValues = runPctDependentPair()
                let readDependentPair (xs : PercentPair[]) =
                    xs
                    |> Array.map ( fun x -> fst x.Value, snd x.Value )
                let readDependentPair() = readDependentPair dpValues

                Expect.isFasterThan
                    (readPair >> ignore |> repeat1000000)
                    (readDependentPair >> ignore |> repeat1000000)
                    "Read pair is faster than DependentPair" }
        ]

    [<Tests>]
    let benchmarkUtcDateTime =
        testList "DateTime vs DependentType DateTime" [
            test "DateTime vs DependentType DateTime: Create" {
                Expect.isFasterThan
                    (runDateTimeUtc >> ignore |> repeat1000000)
                    (runDependentTypeDateTimeUtc >> ignore |> repeat1000000)
                    "Validated DateTime is faster than Create DependentType DateTime" }

            test "DateTime vs DependentType DateTime: read" {
                let dateTime = runDependentTypeDateTimeUtc()
                let readDependentType (xs : UtcDateTime[]) =
                    xs
                    |> Array.map ( fun x -> x.Value )
                let readDependentTypeDateTime() = readDependentType dateTime

                Expect.isFasterThan
                    (readDateTime >> ignore |> repeat1000000)
                    (readDependentTypeDateTime >> ignore |> repeat1000000)
                    "Read DateTime is faster than DependentType DateTime" }
        ]
