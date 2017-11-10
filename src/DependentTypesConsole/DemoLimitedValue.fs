module DemoLimitedValue

open robkuz.DependentTypes
open DependentTypes
open System

let validate normalize fn v =
    if fn (normalize v) then Some (normalize v) else None

let validateLen len s = 
    //validate trim (fun (s:string) -> s.Length <= len) s
    validate id (fun (s:string) -> s.Length <= len) s

type LenValidator(config) = 
    inherit Validator<int, string>(config, validateLen)

type Size5 () = inherit LenValidator(5) 

type String5 = LimitedValue<Size5, int, string>

let demo1() =
    let okString = String5.TryParse "short" // Some
    let failString = String5.TryParse "much too long" //None
    let z = okString.Value
    printfn "okString is: %s" z.Value
    printfn "failString is: %A" failString

let validateRange (min,max) v = validate id (fun v -> v >= min && v <= max) v
let validateMin (min) v = validate id (fun v -> v >= min) v
let validateMax (max) v = validate id (fun v -> v <= max) v

type NumRangeValidator(config) = inherit Validator<int * int, int>(config, validateRange)
type MinNumRangeValidator(config) = inherit Validator<int, int>(config, validateMin)
type MaxNumRangeValidator(config) = inherit Validator<int, int>(config, validateMax)

type MaxPos100 () = inherit NumRangeValidator(0, 100)
type MaxPos20000 () = inherit NumRangeValidator(0, 20000)
type RangeMinus100To100 () = inherit NumRangeValidator(-100, 100)
type Min101 () = inherit MinNumRangeValidator(101)
type MaxMinus101 () = inherit MaxNumRangeValidator(-101)

type PositiveInt100 = LimitedValue<MaxPos100, int * int, int>
type PositiveInt20000 = LimitedValue<MaxPos20000, int * int, int>
type Minus100To100 = LimitedValue<RangeMinus100To100, int * int, int>

type GT100 = LimitedValue<Min101, int, int>
type LTminus100 = LimitedValue<MaxMinus101, int, int>

let demo2() =
    let a: Option<PositiveInt100> = mkDependentType 100
    let b = a.Value |> extract
    let c = a.Value
    let c' : Option<PositiveInt20000> = convertTo c
    let d : Option<PositiveInt20000> = PositiveInt20000.ConvertTo c

    printfn "%i" d.Value.Value

/// this is a dependent function
/// the type hint is not necessary, only to enhance the intellisense
let f n : LimitedValue<_, _, int> =
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

