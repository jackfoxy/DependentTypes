namespace DependentTypes.Tests

open DependentTypes
open Expecto
open FsCheck
open System

module DependentType =

    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    let configReplay = { FsCheckConfig.defaultConfig with maxTest = 10000 ; replay = Some <| (1940624926, 296296394) } // ; arbitrary = [typeof<DomainGenerators>] }  //see Tips & Tricks for FsCheck

    module OptionType =
        let validate normalize fn v =
            if fn (normalize v) then Some (normalize v) else None

        let validateLen len s = 
            validate id (fun (s:string) -> s.Length <= len) s

        type LenValidator(config) = 
            inherit PiType<int, string, string option>(config, validateLen)

        type PairLenValidator(config) = 
            inherit SigmaType<int, string, string option>(config, validateLen)

        type Size5 () = inherit LenValidator(5) 

        type Size5Pair () = inherit PairLenValidator(5)

    type String5 = DependentType<OptionType.Size5, int, string, string option>
    type String5Pair = DependentPair<OptionType.Size5Pair, int, string, string option>

    module SingleType =
        let transformToUtcDateTime _ (value : DateTime) =
            value.ToUniversalTime()     

        type UtcDateTimeTransformer() = 
            inherit PiType<unit, DateTime, DateTime>((), transformToUtcDateTime)

        type DateTimeUtc () = inherit UtcDateTimeTransformer()
    
    type UtcDateTime = DependentType<SingleType.DateTimeUtc, unit, DateTime, DateTime>

    [<CustomComparison; CustomEquality>]
    type IntegerOfSign =
        | PositiveInt of int
        | Zero of int
        | NegativeInt of int
        override __.Equals(yobj) = 
            match yobj with
                |  :? IntegerOfSign as y -> 
                    match y with
                    | PositiveInt yy ->
                        match __ with
                        | IntegerOfSign.PositiveInt x ->
                            yy = x
                        | _ ->
                            false

                    | Zero _ -> 
                        match __ with
                        | IntegerOfSign.Zero _ ->
                            true
                        | _ ->
                            false

                    | NegativeInt yy -> 
                        match __ with
                        | IntegerOfSign.NegativeInt x ->
                            yy = x
                        | _ ->
                            false

                | _ -> false
        override __.GetHashCode() = hash __

        interface IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? IntegerOfSign as y -> 
                    match y with
                    | PositiveInt yy ->
                        match __ with
                        | IntegerOfSign.PositiveInt x ->
                            if x > yy then 1
                            elif - x = yy then 0
                            else -1
                        | _ ->
                            -1

                    | Zero _ -> 
                        match __ with
                        | IntegerOfSign.PositiveInt x ->
                            1
                        | IntegerOfSign.Zero _ ->
                            0
                        | _ ->
                            -1

                    | NegativeInt yy -> 
                        match __ with
                        | IntegerOfSign.NegativeInt x ->
                            if x > yy then 1
                            elif - x = yy then 0
                            else -1
                        | _ ->
                            1

                | _ -> invalidArg "Source" "cannot compare values of different types"

    module SumType =
        let intType _ (value : int) =
            match value with
            | v when v > 0 ->
                IntegerOfSign.PositiveInt v
            | v when v = 0 ->
                IntegerOfSign.Zero v
            | v ->
                IntegerOfSign.NegativeInt v

        type IntSumTypeDiscriminator() = 
            inherit PiType<unit, int, IntegerOfSign>((), intType)
    
    type IntegerType = DependentType<SumType.IntSumTypeDiscriminator, unit, int, IntegerOfSign>

    type IntegerNegNonNeg =
        | NonNegativeInt of int
        | NegativeInt of int

    module NonNegativSumType =
        let intType _ (value : IntegerOfSign) =
            match value with
            | IntegerOfSign.PositiveInt v ->
                IntegerNegNonNeg.NonNegativeInt v
            | IntegerOfSign.Zero v ->
                IntegerNegNonNeg.NonNegativeInt v
            | IntegerOfSign.NegativeInt v ->
                IntegerNegNonNeg.NegativeInt v

        type IntSumTypeDiscriminator() = 
            inherit PiType<unit, IntegerOfSign, IntegerNegNonNeg>((), intType)
    
    type IntegerSignType = DependentType<NonNegativSumType.IntSumTypeDiscriminator, unit, IntegerOfSign, IntegerNegNonNeg>

    let reflexivity x =
        Expect.equal x x "reflexivity"

    [<Tests>]
    let optionDependentType =
        testList "Option DependentTypes.Equality and Comparison" [

            testCase "Equality" <| fun () ->
                let s100_1 = String5.Create "100"
                let s100_2 = String5.Create "100"

                Expect.equal s100_1 s100_2 "Expected equal"
                reflexivity s100_1
                reflexivity s100_2

            testCase "Equality option" <| fun () ->
                let s100_1 = String5.TryCreate "100"
                let s100_2 = String5.TryCreate "100"

                Expect.equal s100_1 s100_2 "Expected equal"
                reflexivity s100_1
                reflexivity s100_2

            testCase "Inequality" <| fun () ->
                let s100 = String5.Create "100"
                let s200 = String5.Create "200"

                Expect.notEqual s100 s200 "Expected not equal"
                reflexivity s100
                reflexivity s200

            testCase "Inequality option" <| fun () ->
                let s100 = String5.TryCreate "100"
                let s200 = String5.TryCreate "200"

                Expect.notEqual s100 s200 "Expected not equal"
                reflexivity s100
                reflexivity s200

            testCase "Comparison" <| fun () ->
                let n1 = String5.Create "100"
                let n2 = String5.Create "200"
                let n3 = String5.Create "300"
                let n4 = String5.Create "400"
                let n5 = String5.Create "500"

                let l1 = [n1; n2; n3; n4; n5]
                let l2 = [n5; n4; n1; n2; n3]

                Expect.equal l1 (l2 |> List.sort) "Expected equal"
                reflexivity n1
                reflexivity n2
                reflexivity n3
                reflexivity n4
                reflexivity n5

            testCase "Comparison option" <| fun () ->
                let n1 = String5.TryCreate "100"
                let n2 = String5.TryCreate "200"
                let n3 = String5.TryCreate "300"
                let n4 = String5.TryCreate "400"
                let n5 = String5.TryCreate "500"

                let l1 = [n1; n2; n3; n4; n5]
                let l2 = [n5; n4; n1; n2; n3]

                Expect.equal l1 (l2 |> List.sort) "Expected equal"
                reflexivity n1
                reflexivity n2
                reflexivity n3
                reflexivity n4
                reflexivity n5
        ]

    let singleDependentType =
        testList "Single DependentTypes.Equality and Comparison" [

            testCase "Equality" <| fun () ->
                let d1 = UtcDateTime.Create <| DateTime(2001, 1, 1, 1, 1, 1, DateTimeKind.Local)
                let d2 = UtcDateTime.Create <| DateTime(2001, 1, 1, 1, 1, 1, DateTimeKind.Local)

                Expect.equal d1 d2 "Expected equal"
                reflexivity d1
                reflexivity d2

            testCase "Equality option" <| fun () ->
                let d1 =  
                    let x = DateTime(2001, 1,1, 1, 1, 1, DateTimeKind.Local)
                    UtcDateTime.TryCreate x
                let d2 =  
                    UtcDateTime.TryCreate (DateTime(2001, 1, 1, 1, 1, 1, DateTimeKind.Local))

                Expect.equal d1 d2 "Expected equal"
                reflexivity d1
                reflexivity d2

            testCase "Inequality" <| fun () ->
                let d1 = UtcDateTime.Create <| DateTime(2001, 1, 1, 1, 1, 1, DateTimeKind.Local)
                let d2 = UtcDateTime.Create <| DateTime(2001, 1, 1, 1, 1, 2, DateTimeKind.Local)

                Expect.notEqual d1 d2 "Expected not equal"
                reflexivity d1
                reflexivity d2

            testCase "Inequality option" <| fun () ->
                let d1 = UtcDateTime.TryCreate (DateTime(2001, 1, 1, 1, 1, 1, DateTimeKind.Local))
                let d2 = UtcDateTime.TryCreate (DateTime(2001, 1, 1, 1, 1, 2, DateTimeKind.Local))

                Expect.notEqual d1 d2 "Expected not equal"
                reflexivity d1
                reflexivity d2

            testCase "Comparison" <| fun () ->
                let d1 = UtcDateTime.Create <| DateTime(2001, 1, 1, 1, 1, 1, DateTimeKind.Local)
                let d2 = UtcDateTime.Create <| DateTime(2001, 1, 3, 1, 1, 1, DateTimeKind.Utc)
                let d3 = UtcDateTime.Create <| DateTime(2001,2, 2, 1, 1, 1, DateTimeKind.Unspecified)
                let d4 = UtcDateTime.Create <| DateTime(2001, 2, 3, 1, 1, 1, DateTimeKind.Local)
                let d5 = UtcDateTime.Create <| DateTime(2001, 3, 2, 1, 1, 1, DateTimeKind.Utc)

                let l1 = [d1; d2; d3; d4; d5]
                let l2 = [d5; d4; d1; d2; d3]

                Expect.equal l1 (l2 |> List.sort) "Expected equal"
                reflexivity d1
                reflexivity d2
                reflexivity d3
                reflexivity d4
                reflexivity d5

            testCase "Comparison option" <| fun () ->
                let d1 = UtcDateTime.TryCreate (DateTime(2001, 1, 1, 1, 1, 1, DateTimeKind.Local))
                let d2 = UtcDateTime.TryCreate (DateTime(2001, 1, 3, 1, 1, 1, DateTimeKind.Utc))
                let d3 = UtcDateTime.TryCreate (DateTime(2001,2, 2, 1, 1, 1, DateTimeKind.Unspecified))
                let d4 = UtcDateTime.TryCreate (DateTime(2001, 2, 3, 1, 1, 1, DateTimeKind.Local))
                let d5 = UtcDateTime.TryCreate (DateTime(2001, 3, 2, 1, 1, 1, DateTimeKind.Utc))

                let l1 = [d1; d2; d3; d4; d5]
                let l2 = [d5; d4; d1; d2; d3]

                Expect.equal l1 (l2 |> List.sort) "Expected equal"
                reflexivity d1
                reflexivity d2
                reflexivity d3
                reflexivity d4
                reflexivity d5
        ]

    let sumDependentType =
        testList "Sum DependentTypes.Equality and Comparison" [

            testCase "Equality" <| fun () ->
                let s1 = IntegerType.Create 42
                let s2 = IntegerType.Create 42

                Expect.equal s1 s2 "Expected equal"
                reflexivity s1
                reflexivity s2

            testCase "Equality option" <| fun () ->
                let s1 = IntegerType.TryCreate 42
                let s2 = IntegerType.TryCreate 42

                Expect.equal s1 s2 "Expected equal"
                reflexivity s1
                reflexivity s2

            testCase "Inequality" <| fun () ->
                let s1 = IntegerType.Create 42
                let s2 = IntegerType.Create -42

                Expect.notEqual s1 s2 "Expected not equal"
                reflexivity s1
                reflexivity s2

            testCase "Inequality option" <| fun () ->
                let s1 = 42
                let s2 = -42

                Expect.notEqual s1 s2 "Expected not equal"
                reflexivity s1
                reflexivity s2

            testCase "Comparison" <| fun () ->
                let s1 = IntegerType.Create -42
                let s2 = IntegerType.Create -21
                let s3 = IntegerType.Create 0
                let s4 = IntegerType.Create 21
                let s5 = IntegerType.Create 42

                let l1 = [s1; s2; s3; s4; s5]
                let l2 = [s5; s4; s1; s2; s3]

                Expect.equal l1 (l2 |> List.sort) "Expected equal"
                reflexivity s1
                reflexivity s2
                reflexivity s3
                reflexivity s4
                reflexivity s5

            testCase "Comparison option" <| fun () ->
                let s1 = IntegerType.TryCreate -42
                let s2 = IntegerType.TryCreate -21
                let s3 = IntegerType.TryCreate 0
                let s4 = IntegerType.TryCreate 21
                let s5 = IntegerType.TryCreate 42

                let l1 = [s1; s2; s3; s4; s5]
                let l2 = [s5; s4; s1; s2; s3]

                Expect.equal l1 (l2 |> List.sort) "Expected equal"
                reflexivity s1
                reflexivity s2
                reflexivity s3
                reflexivity s4
                reflexivity s5
        ]

    let convertsToDependentType =

        testList "ConvertTo" [
            testCase "Equality" <| fun () ->
                let s42 = IntegerType.Create 42
                let s42A = IntegerOfSign.PositiveInt 42 |> IntegerSignType.Create

                let s0  = IntegerType.Create 0
                let s0A = IntegerOfSign.Zero 0 |> IntegerSignType.Create

                let s_42 = IntegerType.Create -42
                let s_42A = IntegerOfSign.NegativeInt -42 |> IntegerSignType.Create

                let s42B = IntegerSignType.ConvertTo s42
                let s0B = IntegerSignType.ConvertTo s0
                let s_42B : IntegerSignType = IntegerSignType.ConvertTo s_42

                Expect.equal s42A s42B "Expected positive equal"
                Expect.equal s0A s0B "Expected zero equal"
                Expect.equal s_42A s_42B "Expected negative equal"

                reflexivity s42B
                reflexivity s0B
                reflexivity s_42B
        ]

    let dependentPairs =

        testList "DependentPairs.Equality and Comparison" [

            testCase "Equality" <| fun () ->
                let s100_1 = String5Pair.Create "100"
                let s100_2 = String5Pair.Create "100"

                Expect.equal s100_1 s100_2 "Expected equal"
                reflexivity s100_1
                reflexivity s100_2

            testCase "Inequality" <| fun () ->
                let s100 = String5Pair.Create "100"
                let s200 = String5Pair.Create "200"

                Expect.notEqual s100 s200 "Expected not equal"
                reflexivity s100
                reflexivity s200

            testCase "Comparison" <| fun () ->
                let n1 = String5Pair.Create "100"
                let n2 = String5Pair.Create "200"
                let n3 = String5Pair.Create "300"
                let n4 = String5Pair.Create "400"
                let n5 = String5Pair.Create "500"

                let l1 = [n1; n2; n3; n4; n5]
                let l2 = [n5; n4; n1; n2; n3]

                Expect.equal l1 (l2 |> List.sort) "Expected equal"
                reflexivity n1
                reflexivity n2
                reflexivity n3
                reflexivity n4
                reflexivity n5
        ]