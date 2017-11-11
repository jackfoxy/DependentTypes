module DemoDependentType

open robkuz.DependentTypes
open DependentTypes
open System

(* 'T -> 'T2 tests existing LimitValue tests where 'T = 'T2 *)

let validate normalize fn v =
    if fn (normalize v) then Some (normalize v) else None

let validateLen len s = 
    validate id (fun (s:string) -> s.Length <= len) s

type LenValidator(config) = 
    inherit Cctor<int, string, string>(config, validateLen)

type Size5 () = inherit LenValidator(5) 

type String5 = DependentType<Size5, int, string, string>

let demo1() =
    let okString = String5.TryParse "short" // Some
    let failString = String5.TryParse "much too long" //None
    let z = okString.Value
    printfn "okString is: %s" z.Value
    printfn "failString is: %A" failString

let validateRange (min,max) v = validate id (fun v -> v >= min && v <= max) v
let validateMin (min) v = validate id (fun v -> v >= min) v
let validateMax (max) v = validate id (fun v -> v <= max) v

type NumRangeValidator(config) = inherit Cctor<int * int, int, int>(config, validateRange)
type MinNumRangeValidator(config) = inherit Cctor<int, int, int>(config, validateMin)
type MaxNumRangeValidator(config) = inherit Cctor<int, int, int>(config, validateMax)

type MaxPos100 () = inherit NumRangeValidator(0, 100)
type MaxPos20000 () = inherit NumRangeValidator(0, 20000)
type RangeMinus100To100 () = inherit NumRangeValidator(-100, 100)
type Min101 () = inherit MinNumRangeValidator(101)
type MaxMinus101 () = inherit MaxNumRangeValidator(-101)

type PositiveInt100 = DependentType<MaxPos100, int * int, int, int>
type PositiveInt20000 = DependentType<MaxPos20000, int * int, int, int>
type Minus100To100 = DependentType<RangeMinus100To100, int * int, int, int>

type GT100 = DependentType<Min101, int, int, int>
type LTminus100 = DependentType<MaxMinus101, int, int, int>

let demo2() =
    let a: Option<PositiveInt100> = mkDependentType 100
    let b = a.Value |> extract
    let c = a.Value
    let c' : Option<PositiveInt20000> = convertTo c
    let d : Option<PositiveInt20000> = PositiveInt20000.ConvertTo c

    printfn "%i" d.Value.Value

/// this is a dependent function
/// the type hint is not necessary, only to enhance the intellisense
let f n : DependentType<_, _, int, int> =
    match n with
    | n' when n' < -100 -> (LTminus100.TryParse n).Value |> box
    | n' when n' > 100 -> (GT100.TryParse n).Value |> box
    | _ -> (Minus100To100.TryParse n ).Value|> box
    |> unbox

let demo3() =
    let lTminus100 : LTminus100 = f -200
    let gT100 : GT100 = f 101
    let minus100To100 : Minus100To100 = f 1

    printfn "dependent function result is: %A of type %A" lTminus100 <| lTminus100.GetType()
    printfn "dependent function result is: %A of type %A" gT100 <| gT100.GetType()
    printfn "dependent function result is: %A of type %A" minus100To100 <|  minus100To100.GetType()

(* 'T -> 'T2 test 1*)

let tryConstruct normalize fn v =
    fn (normalize v)

let tryConstructIndexToString i = 
    tryConstruct id (fun i' -> 
        (match i' with
        | n when n < 0 -> "negative"
        | n when n > 0 -> "positive"
        | _ -> "zero" )
        |> Some ) i

let tryIndexToString _ v = tryConstruct id tryConstructIndexToString v

type IndexToStringCctor() = 
    inherit Cctor<unit, int, string>((), tryIndexToString)

type IndexToString = DependentType<IndexToStringCctor, unit, int, string>

let demo4() =
    printfn ""
    printfn "dependent type that indexes to string (int -> string)"
    let neg =  (IndexToString.TryParse -100).Value

    printfn "%s" neg.Value

    let zero : Option<IndexToString> = mkDependentType 0
    let zeroExtracted  = zero.Value |> extract

    printfn "zero extracted: %s" zeroExtracted

    let zeroVal = zero.Value
    let zeroValToString5 : Option<String5> = convertTo zeroVal

    printfn "zero converted to String5: %A of type %A" zeroValToString5.Value <| zeroValToString5.GetType()

(* 'T -> 'T2 test 2*)

let tryConstructMultiplyToString multiplier i = 
    tryConstruct id (fun i' -> 
        (multiplier * i').ToString()
        |> Some ) i

type Multiply5ToStringCctor() = 
    inherit Cctor<int, int, string>(5, tryConstructMultiplyToString)

type Multiply5ToString = DependentType<Multiply5ToStringCctor, int, int, string>

let demo5() =
    printfn ""
    printfn "dependent type that performs arithmentic on element and converts to string (int -> string)"
    let neg500 =  (Multiply5ToString.TryParse -100).Value

    printfn "%s" neg500.Value

    let neg500' : Option<Multiply5ToString> = mkDependentType -100
    let neg500Extracted  = neg500'.Value |> extract

    printfn "neg500Extracted extracted: %s" neg500Extracted

    let neg500Val = neg500'.Value
    let neg500ValToString5 : Option<String5> = convertTo neg500Val

    printfn "neg500Val converted to String5: %A of type %A" neg500ValToString5.Value <| neg500ValToString5.GetType()

let demo6() =
    let neg500_1 =  (Multiply5ToString.TryParse -100).Value
    let neg500_2 =  (Multiply5ToString.TryParse -100).Value

    printfn "same values equal?: %b" (neg500_1 = neg500_2)

let demo6_1() =
    let neg500 =  (Multiply5ToString.TryParse -100).Value
    let neg1000 =  (Multiply5ToString.TryParse -200).Value

    printfn "unequal values equal?: %b" (neg500 = neg1000)


let demo7() =
    let n1 =  (String5.TryParse "100").Value
    let n2 =  (String5.TryParse "200").Value
    let n3 =  (String5.TryParse "300").Value
    let n4 =  (String5.TryParse "400").Value
    let n5 =  (String5.TryParse "500").Value

    let l1 = [n1; n2; n3; n4; n5]
    let l2 = [n5; n4; n1; n2; n3]

    printfn "supports comparison?: %b" (l1 = (l2 |> List.sort))
