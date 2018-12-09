module DemoDependentFunction

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

    type MaxNumRangeValidator(config) = inherit Pi<int, int, int option>(config, validateMax)
    type MinNumRangeValidator(config) = inherit Pi<int, int, int option>(config, validateMin)
    type NumRangeValidator(config) = inherit Pi<int * int, int, int option>(config, validateRange)

    type MaxMinus101 () = inherit MaxNumRangeValidator(-101)
    type Min101 () = inherit MinNumRangeValidator(101)
    type RangeMinus100To100 () = inherit NumRangeValidator(-100, 100)
    
// dependent types aliased
type LTminus100 = DependentType<SetUp.MaxMinus101, int, int, int option>
type GT100 = DependentType<SetUp.Min101, int, int, int option>
type Minus100To100 = DependentType<SetUp.RangeMinus100To100, int * int, int, int option>

///// this is a dependent function
///// the type hint is not necessary, only to enhance the intellisense
//let f (n : int) : DependentType<_, _, int, int> =
//    match n with
//    | n' when n' < -100 -> 
//        LTminus100.Create n |> box
//    | n' when n' > 100 -> 
//        GT100.Create n |> box
//    | _ -> 
//        Minus100To100.Create n |> box
//    |> unbox

//let demo() =
//    let lTminus100 : LTminus100 = f -200
//    let gT100 : GT100 = f 101
//    let minus100To100 : Minus100To100 = f 1

//    printfn "dependent function result is: %A of type %A" lTminus100 <| lTminus100.GetType()
//    printfn "dependent function result is: %A of type %A" gT100 <| gT100.GetType()
//    printfn "dependent function result is: %A of type %A" minus100To100 <|  minus100To100.GetType()

//demo()