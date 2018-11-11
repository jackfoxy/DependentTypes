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
        inherit PiType<unit, float, float option>((), validatePercent)

    type PairPercentValidator() = 
        inherit SigmaType<unit, float, float option>((), validatePercent)

type Percent = DependentType<PercentType.PercentValidator, unit, float, float option>
type PercentPair = DependentPair<PercentType.PairPercentValidator, unit, float, float option>

type PctOption =
    {
        Pct : float option
    }

type DependentTypePctOption =
    {
        Pct : Percent option
    }

type DependentTypePct =
    {
        Pct : Percent
    }

type DateTimeUtc =
    {
        TimeDate : DateTime
    }

type DependentTypeDateTimeUtc =
    {
        TimeDate : UtcDateTime
    }

module Benchmarks =
    let runNakedOption() =
        [|
            {PctOption.Pct = Some 0.5}
            {PctOption.Pct = Some 2.5}
        |] 

    let runPctOption() =
        [|
            {PctOption.Pct = PercentType.validatePercent () 0.5}
            {PctOption.Pct = PercentType.validatePercent () 2.5}
        |] 

    let runLiftedPctDependentType() =
        [|
            {DependentTypePctOption.Pct = Percent.TryCreate 0.5}
            {DependentTypePctOption.Pct = Percent.TryCreate 2.5}
        |] 
    let runPctDependentType() =
        [|
            {DependentTypePct.Pct = Percent.Create 0.5}
            {DependentTypePct.Pct = Percent.Create 2.5}
        |] 

    let verifyUtcDateTime _ (value : DateTime) =
        value.ToUniversalTime() 

    let runDateTimeUtc() =
        [|
            {DateTimeUtc.TimeDate = verifyUtcDateTime () <| DateTime(2018, 11, 11, 11, 11, 11,DateTimeKind.Local)  }
        |] 

    let runDependentTypeDateTimeUtc() =
        [|
            {DependentTypeDateTimeUtc.TimeDate = UtcDateTime.Create <| DateTime(2018, 11, 11, 11, 11, 11,DateTimeKind.Local)}
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

    //let vanillaOption = runPctOption()
    let readVanillaOption (xs : PctOption[]) =
        xs
        |> Array.map ( fun x ->
        match x.Pct with
        | Some pct -> Some pct
        | None -> None )
    let readVanilla() = readVanillaOption <| runPctOption()

    let liftedDependentType = runLiftedPctDependentType()

    //let dateTime = runDateTimeUtc()
    let readDatetime (xs : DateTimeUtc[]) = 
        xs
        |> Array.map ( fun x -> x.TimeDate)
    let readDateTime()  = readDatetime <| runDateTimeUtc()

    [<Tests>]
    let benchmarkDependentTypeLiftedOption =
        testList "option vs DependentType option" [
            test "naked option vs DependentType option: TryCreate" {
                Expect.isFasterThan
                    (runNakedOption >> ignore |> repeat1000000)
                    (runLiftedPctDependentType >> ignore |> repeat1000000)
                    "naked option is faster than TryCreate DependentType option" }

            test "validated option vs DependentType option: TryCreate" {
                Expect.isFasterThan
                    (runPctOption >> ignore |> repeat1000000)
                    (runLiftedPctDependentType >> ignore |> repeat1000000)
                    "validated option is faster than TryCreate DependentType option" }

            test "validated option vs DependentType option: TryCreate 10X" {
                Expect.isFasterThan
                    (runPctOption >> ignore |> repeat10)
                    (runLiftedPctDependentType >> ignore |> repeat10)
                    "(10X) validated option is faster than TryCreate DependentType option" }

            test "option vs DependentType option: read" {
                let readDependentType (xs : DependentTypePctOption[]) =
                    xs
                    |> Array.map ( fun x ->
                    match x.Pct with
                    | Some _ -> Some (someValue x.Pct)
                    | None -> None )
                let readDependentType() = readDependentType <| runLiftedPctDependentType()

                Expect.isFasterThan
                    (readVanilla >> ignore |> repeat1000000)
                    (readDependentType >> ignore |> repeat1000000)
                    "read Option is faster than Option DependentType" }

            test "Option DependentType vs Option: read value" {
                let readDependentType (xs : DependentTypePctOption[]) =
                    xs
                    |> Array.map ( fun x ->
                    match x.Pct with
                    | Some pct -> Some pct.Value.Value
                    | None -> None )
                let readDependentType() = readDependentType <| runLiftedPctDependentType()

                Expect.isFasterThan
                    (readVanilla >> ignore |> repeat1000000)
                    (readDependentType >> ignore |> repeat1000000)
                    "read Option is faster than Option DependentType Value.Value" }
        ]

    [<Tests>]
    let benchmarkDependentTypeOption =
        testList "option vs DependentType" [
            test "naked option vs DependentType: Create" {
                Expect.isFasterThan
                    (runNakedOption >> ignore |> repeat1000000)
                    (runPctDependentType >> ignore |> repeat1000000)
                    "naked option is faster than Create DependentType" }

            test "validated option vs DependentType option: Create" {
                Expect.isFasterThan
                    (runPctOption >> ignore |> repeat1000000)
                    (runPctDependentType >> ignore |> repeat1000000)
                    "validated option is faster than Create DependentType" }

            test "validated option vs DependentType option: Create 10X" {
                Expect.isFasterThan
                    (runPctOption >> ignore |> repeat10)
                    (runPctDependentType >> ignore |> repeat10)
                    "(10X) validated option is faster than Create DependentType" }
        ]

    [<Tests>]
    let benchmarkUtcDateTime =
        testList "DateTime vs DependentType DateTime" [
            test "DateTime vs DependentType DateTime: Create" {
                Expect.isFasterThan
                    (runDateTimeUtc >> ignore |> repeat1000000)
                    (runDependentTypeDateTimeUtc >> ignore |> repeat1000000)
                    "validated DateTime is faster than Create DependentType DateTime" }

            test "DateTime vs DependentType DateTime: read" {
                let readDependentType (xs : DependentTypeDateTimeUtc[]) =
                    xs
                    |> Array.map ( fun x -> x.TimeDate.Value )
                let readDependentTypeDateTime() = readDependentType <| runDependentTypeDateTimeUtc()

                Expect.isFasterThan
                    (readDateTime >> ignore |> repeat1000000)
                    (readDependentTypeDateTime >> ignore |> repeat1000000)
                    "read DateTime is faster than DependentType DateTime" }
        ]
