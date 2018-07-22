namespace DomainLib.Tests

open DependentTypes
open DomainLib
open DomainGeneratorsCode
open Expecto
open FsCheck

module DomainTypes =

    let config10kArb = { FsCheckConfig.defaultConfig with maxTest = 10000; arbitrary = [typeof<DomainGenerators>] } //FullName/equality
    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    let configReplay = { FsCheckConfig.defaultConfig with maxTest = 10000 ; replay = Some <| (1940624926, 296296394) } // ; arbitrary = [typeof<DomainGenerators>] }  //see Tips & Tricks for FsCheck

    let reflexivity x =
        Expect.equal x x "reflexivity"

    [<Tests>]
    let trimNonEmptyString =
        testList "DomainTypes.TrimNonEmptyString" [

            testCase "Create None on empty string" <| fun () ->
                Expect.isNone (TrimNonEmptyString.Create System.String.Empty).Value "Expected None"

            testPropertyWithConfig config10k "TryCreate None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| whitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.Create x
                            t.Value.IsNone)

            testPropertyWithConfig config10k "TryCreate" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.Create x
                            reflexivity t.Value
                            x.Trim() = t.Value.Value )

            testPropertyWithConfig config10k "equality" <|
                fun  (x : NonEmptyString) ->

                    let t = 
                        x.ToString()
                        |> TrimNonEmptyString.Create 

                    match t.Value with
                    | Some s ->
                        reflexivity t.Value
                        t = TrimNonEmptyString.Create s
                    | None ->
                        let t2 = 
                            x.ToString()
                            |> TrimNonEmptyString.Create
                        t = t2

            testPropertyWithConfig config10k "is trim" <|
                fun  (x : NonEmptyString) ->

                    let t = 
                        x.ToString()
                        |> TrimNonEmptyString.Create

                    match t.Value with
                    | Some s ->
                        reflexivity s
                        x.ToString().Trim() = s
                    | None ->
                        t = t

            testPropertyWithConfig config10k "Create on string list" <|
                fun  (xs : list<string>) ->

                    let listNonEmptyStringSorted = 
                        TrimNonEmptyString.Create xs
                        |> Seq.sort
                        |> Seq.map (fun x -> x.Value)

                    let filteredListSorted =
                        xs
                        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
                        |> List.map (fun x -> x.Trim())
                        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
                        |> List.sort
                        |> Seq.ofList
                        |> Seq.map (fun x -> Some x)
                   
                    filteredListSorted = listNonEmptyStringSorted

            testPropertyWithConfig config10k "list equality" <|
                fun  (xs : list<string>) ->

                    let listTrimNonEmptyStringSorted = 
                        TrimNonEmptyString.Create (xs |> List.filter (fun x -> System.String.IsNullOrEmpty x |> not) )

                    let list2 =
                        listTrimNonEmptyStringSorted
                        |> Seq.choose (fun x -> x.Value)
                        |> TrimNonEmptyString.Create 

                    list2 = listTrimNonEmptyStringSorted
        ]

    [<Tests>]
    let digits =
        testList "DomainTypes.Digits" [


            testPropertyWithConfig config10k "TryCreate None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits.Create x
                           t.Value.IsNone)

            testPropertyWithConfig config10k "TryCreate" <|
                fun  (digits : NonNegativeInt) ->
                    let t = digits.ToString() |> Digits.Create
                    reflexivity t.Value
                    (digits.ToString()) = t.Value.Value

                    //to do: digit strings wrapped in whitespace
            testPropertyWithConfig config10k "TryCreate trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsInWhiteSpace())
                        (fun (x : string) -> 
                           let t = Digits.Create x
                           reflexivity t.Value
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let t = digits.ToString() |> Digits.Create
                    let t2 = Digits.Create t.Value.Value
                    reflexivity t.Value
                    reflexivity t2.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits.Create "555"; Digits.Create "111"; Digits.Create "33"; Digits.Create "2"; ]
                    |> List.sort
                Expect.equal ordered [Digits.Create "111"; Digits.Create "2"; Digits.Create "33"; Digits.Create "555"; ]
                    "expected equality"
        ]

    [<Tests>]
    let digits2 =
        testList "DomainTypes.Digits2" [

            testCase "TryCreate None on empty string" <| fun () ->
                Expect.isNone (Digits2.Create System.String.Empty).Value "Expected None"

            testPropertyWithConfig config10k "TryCreate None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits2.Create x
                           t.Value.IsNone)

            testPropertyWithConfig config10k "TryCreate None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = invalidDigits digits 2 |> Digits2.Create
                    t.Value.IsNone

            testPropertyWithConfig config10k "TryCreate" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 2
                    let t = Digits2.Create validDigit
                    reflexivity t.Value
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "TryCreate trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsOfLengthInWhiteSpace 2)
                        (fun (x : string) -> 
                           let t = Digits2.Create x
                           reflexivity t.Value
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 2
                    let t = Digits2.Create validDigit
                    let t2 = Digits2.Create t.Value.Value
                    reflexivity t.Value
                    reflexivity t2.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits2.Create "55"; Digits2.Create "11"; Digits2.Create "33"; Digits2.Create "22"; ]
                    |> List.sort
                Expect.equal ordered [Digits2.Create "11"; Digits2.Create "22"; Digits2.Create "33"; Digits2.Create "55"; ]
                    "expected equality"
        ]

    [<Tests>]
    let genericSet =
        testList "DomainTypes.GenericSet" [
            testCase "int" <| fun () ->
                let nonEmptyIntSet = [1;2;3] |> Set.ofList |> NonEmptySet.Create
                Expect.equal nonEmptyIntSet.Value.Value ([1;2;3] |> Set.ofList)
                    "expected equality"

            testCase "string" <| fun () ->
                let nonEmptyIntSet = ["Rob";"Jack";"Don"] |> Set.ofList |> NonEmptySet.Create
                Expect.equal nonEmptyIntSet.Value.Value (["Rob";"Jack";"Don"] |> Set.ofList)
                    "expected equality"
        ]
