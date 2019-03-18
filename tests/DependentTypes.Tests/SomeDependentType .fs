namespace DependentTypes.Tests

open DependentTypes
open Expecto
open FsCheck
open System

module SomeDependentType =

    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    let configReplay = { FsCheckConfig.defaultConfig with maxTest = 10000 ; replay = Some <| (1940624926, 296296394) } // ; arbitrary = [typeof<DomainGenerators>] }  //see Tips & Tricks for FsCheck

    module OptionType =
        let validate normalize fn v =
            if fn (normalize v) then Some (normalize v) else None

        let validateLen len s = 
            validate id (fun (s:string) -> s.Length <= len) s

        type LenValidator(config) = 
            inherit Pi<int, string, string option>(config, validateLen)

        type PairLenValidator(config) = 
            inherit Sigma<int, string, string option>(config, validateLen)

        type Size5 () = inherit LenValidator(5) 

        type Size5Pair () = inherit PairLenValidator(5)

    type String5 = SomeDependentType<OptionType.Size5, int, string, string>
    //type String5Pair = DependentPair<OptionType.Size5Pair, int, string, string option>

    
    let reflexivity x =
        Expect.equal x x "reflexivity"

    [<Tests>]
    let someDependentType =
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

    //[<Tests>]
    //let dependentPairs =

    //    testList "DependentPairs.Equality and Comparison" [

    //        testCase "Equality" <| fun () ->
    //            let s100_1 = String5Pair.Create "100"
    //            let s100_2 = String5Pair.Create "100"

    //            Expect.equal s100_1 s100_2 "Expected equal"
    //            reflexivity s100_1
    //            reflexivity s100_2

    //        testCase "Inequality" <| fun () ->
    //            let s100 = String5Pair.Create "100"
    //            let s200 = String5Pair.Create "200"

    //            Expect.notEqual s100 s200 "Expected not equal"
    //            reflexivity s100
    //            reflexivity s200

    //        testCase "Comparison" <| fun () ->
    //            let n1 = String5Pair.Create "100"
    //            let n2 = String5Pair.Create "200"
    //            let n3 = String5Pair.Create "300"
    //            let n4 = String5Pair.Create "400"
    //            let n5 = String5Pair.Create "500"

    //            let l1 = [n1; n2; n3; n4; n5]
    //            let l2 = [n5; n4; n1; n2; n3]

    //            Expect.equal l1 (l2 |> List.sort) "Expected equal"
    //            reflexivity n1
    //            reflexivity n2
    //            reflexivity n3
    //            reflexivity n4
    //            reflexivity n5
    //    ]