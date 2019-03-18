namespace DomainLib.Tests

open DependentTypes
open DomainLib2
open DomainGeneratorsCode
open Expecto
open FsCheck

module Domain2Types =

    let config10kArb = { FsCheckConfig.defaultConfig with maxTest = 10000; arbitrary = [typeof<DomainGenerators>] } //FullName/equality
    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    let configReplay = { FsCheckConfig.defaultConfig with maxTest = 10000 ; replay = Some <| (1940624926, 296296394) } // ; arbitrary = [typeof<DomainGenerators>] }  //see Tips & Tricks for FsCheck

    let reflexivity x =
        Expect.equal x x "reflexivity"

    [<Tests>]
    let trimNonEmptyString =
        testList "Domain2Types.TrimNonEmptyString" [

            testCase "Create None on empty string" <| fun () ->
                let f = fun () -> TrimNonEmptyString.Create System.String.Empty |> ignore
                Expect.throws f "Expected None"
                
            testCase "TryCreate None on empty string" <| fun () ->
                Expect.isNone (TrimNonEmptyString.TryCreate System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryCreate None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| whitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.TryCreate x
                            t.IsNone)

            testPropertyWithConfig config10k "Create" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.Create x
                            reflexivity t.Value
                            x.Trim() = t.Value )

            testPropertyWithConfig config10k "equality" <|
                fun  (x : NonEmptyString) ->

                    let t = 
                        x.ToString()
                        |> TrimNonEmptyString.TryCreate 

                    reflexivity t

                    match t with
                    | Some s ->
                        reflexivity t.Value
                        t = TrimNonEmptyString.TryCreate s.Value
                    | None ->
                        let t2 = 
                            x.ToString()
                            |> TrimNonEmptyString.TryCreate
                        t = t2

            testPropertyWithConfig config10k "is trim" <|
                fun  (x : NonEmptyString) ->

                    let t = 
                        x.ToString()
                        |> TrimNonEmptyString.TryCreate

                    reflexivity t

                    match t with
                    | Some s ->
                        reflexivity s
                        x.ToString().Trim() = s.Value
                    | None ->
                        t = t

            testCase "Supports comparison" <| fun () ->
                let a = TrimNonEmptyString.Create "AAA"
                let a' = TrimNonEmptyString.Create "AAA"
                let b = TrimNonEmptyString.Create "BBB"
                Expect.isTrue (b > a) "Expected true"
                Expect.isFalse (a > b) "Expected false"
                Expect.equal a a' "expected equal"
        ]

    [<Tests>]
    let digits =
        testList "Domain2Types.Digits" [

            testPropertyWithConfig config10k "TryCreate None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits.TryCreate x
                           t.IsNone)

            testPropertyWithConfig config10k "TryCreate" <|
                fun  (digits : NonNegativeInt) ->
                    let t = digits.ToString() |> Digits.TryCreate
                    reflexivity t.Value
                    (digits.ToString()) = t.Value.Value

                    //to do: digit strings wrapped in whitespace
            testPropertyWithConfig config10k "TryCreate trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsInWhiteSpace())
                        (fun (x : string) -> 
                           let t = Digits.TryCreate x
                           reflexivity t.Value
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let t = digits.ToString() |> Digits.TryCreate
                    let t2 = Digits.Create t.Value.Value
                    reflexivity t.Value
                    reflexivity t2.Value
                    t2 = t.Value

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits.Create "555"; Digits.Create "111"; Digits.Create "33"; Digits.Create "2"; ]
                    |> List.sort
                Expect.equal ordered [Digits.Create "111"; Digits.Create "2"; Digits.Create "33"; Digits.Create "555"; ]
                    "expected equality"
        ]

    [<Tests>]
    let digits2 =
        testList "Domain2Types.Digits2" [

            testCase "TryCreate None on empty string" <| fun () ->
                let f = fun () -> Digits2.Create System.String.Empty |> ignore
                Expect.throws f "Expected throw"

            testPropertyWithConfig config10k "TryCreate None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits2.TryCreate x
                           t.IsNone)

            testPropertyWithConfig config10k "TryCreate None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = invalidDigits digits 2 |> Digits2.TryCreate
                    t.IsNone

            testPropertyWithConfig config10k "TryCreate" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 2
                    let t = Digits2.TryCreate validDigit
                    reflexivity t.Value
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "TryCreate trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsOfLengthInWhiteSpace 2)
                        (fun (x : string) -> 
                           let t = Digits2.TryCreate x
                           reflexivity t.Value
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 2
                    let t = Digits2.TryCreate validDigit
                    let t2 = Digits2.Create t.Value.Value
                    reflexivity t.Value
                    reflexivity t2.Value
                    t2 = t.Value 

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits2.Create "55"; Digits2.Create "11"; Digits2.Create "33"; Digits2.Create "22"; ]
                    |> List.sort
                Expect.equal ordered [Digits2.Create "11"; Digits2.Create "22"; Digits2.Create "33"; Digits2.Create "55"; ]
                    "expected equality"
        ]

    [<Tests>]
    let genericSet =
        testList "Domain2Types.GenericSet" [
            testCase "int" <| fun () ->
                let nonEmptyIntSet = [1;2;3] |> Set.ofList |> NonEmptySet.TryCreate
                Expect.equal nonEmptyIntSet.Value.Value ([1;2;3] |> Set.ofList)
                    "expected equality"

            testCase "string" <| fun () ->
                let nonEmptyIntSet = ["Rob";"Jack";"Don"] |> Set.ofList |> NonEmptySet.TryCreate
                Expect.equal nonEmptyIntSet.Value.Value (["Rob";"Jack";"Don"] |> Set.ofList)
                    "expected equality"
        ]
