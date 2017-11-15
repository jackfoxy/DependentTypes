namespace DomainLib.Tests

open robkuz.DependentTypes
open Jackfoxy.DomainLib
open DomainGeneratorsCode
open Expecto
open FsCheck

module DomainTypes =

    let config10kArb = { FsCheckConfig.defaultConfig with maxTest = 10000; arbitrary = [typeof<DomainGenerators>] } //FullName/equality
    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    let configReplay = { FsCheckConfig.defaultConfig with maxTest = 10000 ; replay = Some <| (1940624926, 296296394) } // ; arbitrary = [typeof<DomainGenerators>] }  //see Tips & Tricks for FsCheck

    [<Tests>]
    let trimNonEmptyString =
        testList "DomainTypes.TrimNonEmptyString" [

            testCase "TryCreate None on empty string" <| fun () ->
                Expect.isNone (TrimNonEmptyString.TryCreate System.String.Empty) "Expected None"

            testCase "TryCreate None on null string" <| fun () ->
                Expect.isNone (TrimNonEmptyString.TryCreate null) "Expected None"

            testPropertyWithConfig config10k "TryCreate None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| whitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.TryCreate x
                            t.IsNone)

            testPropertyWithConfig config10k "TryCreate" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.TryCreate x
                            x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (x : NonEmptyString) ->

                    let t = 
                        x.ToString()
                        |> TrimNonEmptyString.TryCreate 
                    match t with
                    | Some s ->
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
                    match t with
                    | Some s ->
                        x.ToString().Trim() = s.Value
                    | None ->
                        t = t

            testCase "TryCreate None on None" <| fun () ->
                Expect.isNone (TrimNonEmptyString.TryCreate None) "Expected None"

            testPropertyWithConfig config10k "TryCreate on Some x" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.TryCreate (Some x)
                            x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "Create on string list" <|
                fun  (xs : list<string>) ->

                    let listNonEmptyStringSorted = 
                        TrimNonEmptyString.Create xs
                        |> List.sort
                        |> List.map (fun x -> x.Value)

                    let filteredListSorted =
                        xs
                        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
                        |> List.map (fun x -> x.Trim())
                        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
                        |> List.sort
                   
                    filteredListSorted = listNonEmptyStringSorted

            testPropertyWithConfig config10k "list equality" <|
                fun  (xs : list<string>) ->

                    let listTrimNonEmptyStringSorted = 
                        TrimNonEmptyString.Create xs

                    let list2 =
                        listTrimNonEmptyStringSorted
                        |> List.map (fun x -> x.Value)
                        |> TrimNonEmptyString.Create 

                    list2 = listTrimNonEmptyStringSorted
        ]

    [<Tests>]
    let digits =
        testList "DomainTypes.Digits" [

            testCase "TryCreate None on empty string" <| fun () ->
                Expect.isNone (Digits.TryCreate System.String.Empty) "Expected None"

            testCase "TryCreate None on null string" <| fun () ->
                Expect.isNone (Digits.TryCreate null) "Expected None"

            testPropertyWithConfig config10k "TryCreate None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits.TryCreate x
                           t.IsNone)

            testPropertyWithConfig config10k "TryCreate" <|
                fun  (digits : NonNegativeInt) ->
                    let t = digits.ToString() |> Digits.TryCreate
                    (digits.ToString()) = t.Value.Value

                    //to do: digit strings wrapped in whitespace
            testPropertyWithConfig config10k "TryCreate trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsInWhiteSpace())
                        (fun (x : string) -> 
                           let t = Digits.TryCreate x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let t = digits.ToString() |> Digits.TryCreate
                    let t2 = Digits.TryCreate t.Value.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits.TryCreate "555"; Digits.TryCreate "111"; Digits.TryCreate "33"; Digits.TryCreate "2"; ]
                    |> List.sort
                Expect.equal ordered [Digits.TryCreate "111"; Digits.TryCreate "2"; Digits.TryCreate "33"; Digits.TryCreate "555"; ]
                    "expected equality"
        ]

    [<Tests>]
    let digits2 =
        testList "DomainTypes.Digits2" [

            testCase "TryCreate None on empty string" <| fun () ->
                Expect.isNone (Digits2.TryCreate System.String.Empty) "Expected None"

            testCase "TryCreate None on null string" <| fun () ->
                Expect.isNone (Digits2.TryCreate null) "Expected None"

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
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "TryCreate trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsOfLengthInWhiteSpace 2)
                        (fun (x : string) -> 
                           let t = Digits2.TryCreate x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 2
                    let t = Digits2.TryCreate validDigit
                    let t2 = Digits2.TryCreate t.Value.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits2.TryCreate "55"; Digits2.TryCreate "11"; Digits2.TryCreate "33"; Digits2.TryCreate "22"; ]
                    |> List.sort
                Expect.equal ordered [Digits2.TryCreate "11"; Digits2.TryCreate "22"; Digits2.TryCreate "33"; Digits2.TryCreate "55"; ]
                    "expected equality"
        ]

    [<Tests>]
    let genericSet =
        testList "DomainTypes.GenericSet" [
            testCase "int" <| fun () ->
                let nonEmptyIntSet = [1;2;3] |> Set.ofList |> NonEmptySet.Create
                Expect.equal nonEmptyIntSet.Value ([1;2;3] |> Set.ofList)
                    "expected equality"

            testCase "string" <| fun () ->
                let nonEmptyIntSet = ["Rob";"Jack";"Don"] |> Set.ofList |> NonEmptySet.Create
                Expect.equal nonEmptyIntSet.Value (["Rob";"Jack";"Don"] |> Set.ofList)
                    "expected equality"
        ]
