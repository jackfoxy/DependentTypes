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

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (TrimNonEmptyString.TryParse System.String.Empty) "Expected None"

            testCase "TryParse None on null string" <| fun () ->
                Expect.isNone (TrimNonEmptyString.TryParse null) "Expected None"

            testPropertyWithConfig config10k "TryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| whitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.TryParse x
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.TryParse x
                            x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (x : NonEmptyString) ->

                    let t = 
                        x.ToString()
                        |> TrimNonEmptyString.TryParse 
                    match t with
                    | Some s ->
                        t = TrimNonEmptyString.TryParse s.Value
                    | None ->
                        let t2 = 
                            x.ToString()
                            |> TrimNonEmptyString.TryParse
                        t = t2

            testPropertyWithConfig config10k "is trim" <|
                fun  (x : NonEmptyString) ->

                    let t = 
                        x.ToString()
                        |> TrimNonEmptyString.TryParse
                    match t with
                    | Some s ->
                        x.ToString().Trim() = s.Value
                    | None ->
                        t = t

            testCase "TryParse None on None" <| fun () ->
                Expect.isNone (TrimNonEmptyString.TryParse None) "Expected None"

            testPropertyWithConfig config10k "TryParse on Some x" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.TryParse (Some x)
                            x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "Parse on string list" <|
                fun  (xs : list<string>) ->

                    let listNonEmptyStringSorted = 
                        TrimNonEmptyString.Parse xs
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
                        TrimNonEmptyString.Parse xs

                    let list2 =
                        listTrimNonEmptyStringSorted
                        |> List.map (fun x -> x.Value)
                        |> TrimNonEmptyString.Parse 

                    list2 = listTrimNonEmptyStringSorted
        ]

    [<Tests>]
    let digits =
        testList "DomainTypes.Digits" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (Digits.TryParse System.String.Empty) "Expected None"

            testCase "TryParse None on null string" <| fun () ->
                Expect.isNone (Digits.TryParse null) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits.TryParse x
                           t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let t = digits.ToString() |> Digits.TryParse
                    (digits.ToString()) = t.Value.Value

                    //to do: digit strings wrapped in whitespace
            testPropertyWithConfig config10k "TryParse trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsInWhiteSpace())
                        (fun (x : string) -> 
                           let t = Digits.TryParse x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let t = digits.ToString() |> Digits.TryParse
                    let t2 = Digits.TryParse t.Value.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits.TryParse "555"; Digits.TryParse "111"; Digits.TryParse "33"; Digits.TryParse "2"; ]
                    |> List.sort
                Expect.isTrue (ordered = [Digits.TryParse "111"; Digits.TryParse "2"; Digits.TryParse "33"; Digits.TryParse "555"; ])
                    "expected equality"
        ]

    [<Tests>]
    let digits2 =
        testList "DomainTypes.Digits2" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (Digits2.TryParse System.String.Empty) "Expected None"

            testCase "TryParse None on null string" <| fun () ->
                Expect.isNone (Digits2.TryParse null) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits2.TryParse x
                           t.IsNone)

            testPropertyWithConfig config10k "TryParse None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = invalidDigits digits 2 |> Digits2.TryParse
                    t.IsNone

            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 2
                    let t = Digits2.TryParse validDigit
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "TryParse trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsOfLengthInWhiteSpace 2)
                        (fun (x : string) -> 
                           let t = Digits2.TryParse x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 2
                    let t = Digits2.TryParse validDigit
                    let t2 = Digits2.TryParse t.Value.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits2.TryParse "55"; Digits2.TryParse "11"; Digits2.TryParse "33"; Digits2.TryParse "22"; ]
                    |> List.sort
                Expect.isTrue (ordered = [Digits2.TryParse "11"; Digits2.TryParse "22"; Digits2.TryParse "33"; Digits2.TryParse "55"; ])
                    "expected equality"
        ]

    [<Tests>]
    let digits3 =
        testList "DomainTypes.Digits3" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (Digits3.TryParse System.String.Empty) "Expected None"

            testCase "TryParse None on null string" <| fun () ->
                Expect.isNone (Digits3.TryParse null) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits3.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "TryParse None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = invalidDigits digits 3 |> Digits3.TryParse
                    t.IsNone
            
            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 3
                    let t = Digits3.TryParse validDigit
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "TryParse trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsOfLengthInWhiteSpace 3)
                        (fun (x : string) -> 
                           let t = Digits3.TryParse x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 3
                    let t = Digits3.TryParse validDigit
                    let t2 = Digits3.TryParse t.Value.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits3.TryParse "555"; Digits3.TryParse "111"; Digits3.TryParse "223"; Digits3.TryParse "222"; ]
                    |> List.sort
                Expect.isTrue (ordered = [Digits3.TryParse "111"; Digits3.TryParse "222"; Digits3.TryParse "223"; Digits3.TryParse "555"; ])
                    "expected equality"
        ]

    [<Tests>]
    let digits4 =
        testList "DomainTypes.Digits4" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (Digits4.TryParse System.String.Empty) "Expected None"

            testCase "TryParse None on null string" <| fun () ->
                Expect.isNone (Digits4.TryParse null) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits4.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "TryParse None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = invalidDigits digits 4 |> Digits4.TryParse
                    t.IsNone
            
            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 4
                    let t = Digits4.TryParse validDigit
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "TryParse trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsOfLengthInWhiteSpace 4)
                        (fun (x : string) -> 
                           let t = Digits4.TryParse x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 4
                    let t = Digits4.TryParse validDigit 
                    let t2 = Digits4.TryParse t.Value.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits4.TryParse "5555"; Digits4.TryParse "1111"; Digits4.TryParse "2232"; Digits4.TryParse "2222"; ]
                    |> List.sort
                Expect.isTrue (ordered = [Digits4.TryParse "1111"; Digits4.TryParse "2222"; Digits4.TryParse "2232"; Digits4.TryParse "5555"; ])
                    "expected equality"
        ]
