#I __SOURCE_DIRECTORY__
#load "load-project-debug.fsx"

open DependentTypes

module SetUp =
    let validate normalize fn v =
        if fn (normalize v) then Some (normalize v) else None

    let validateMax (max) v = 
        validate id (fun v -> v <= max) v
    let validateMin (min) v = 
        validate id (fun v -> v >= min) v
    let validateRange (min,max) v = 
        validate id (fun v -> v >= min && v <= max) v

    type MaxNumRangeValidator(config) = inherit Cctor<int, int, int>(config, validateMax)
    type MinNumRangeValidator(config) = inherit Cctor<int, int, int>(config, validateMin)
    type NumRangeValidator(config) = inherit Cctor<int * int, int, int>(config, validateRange)

    type MaxMinus101 () = inherit MaxNumRangeValidator(-101)
    type Min101 () = inherit MinNumRangeValidator(101)
    type RangeMinus100To100 () = inherit NumRangeValidator(-100, 100)

type LTminus100 = DependentType<SetUp.MaxMinus101, int, int, int>
type GT100 = DependentType<SetUp.Min101, int, int, int>
type Minus100To100 = DependentType<SetUp.RangeMinus100To100, int * int, int, int>

/// this is a dependent function
/// the type hint is not necessary, only to enhance the intellisense
let f n : DependentType<_, _, int, int> =
    match n with
    | n' when n' < -100 -> (LTminus100.TryCreate n).Value |> box
    | n' when n' > 100 -> (GT100.TryCreate n).Value |> box
    | _ -> (Minus100To100.TryCreate n ).Value|> box
    |> unbox

let demo() =
    let lTminus100 : LTminus100 = f -200
    let gT100 : GT100 = f 101
    let minus100To100 : Minus100To100 = f 1

    printfn "dependent function result is: %A of type %A" lTminus100 <| lTminus100.GetType()
    printfn "dependent function result is: %A of type %A" gT100 <| gT100.GetType()
    printfn "dependent function result is: %A of type %A" minus100To100 <|  minus100To100.GetType()

demo()