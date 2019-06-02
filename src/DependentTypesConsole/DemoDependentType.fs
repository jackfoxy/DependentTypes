module DemoDependentType

open DependentTypes
open DependentTypes.Helpers

let validate normalize fn v =
    if fn (normalize v) then Some (normalize v) else None

let validateLen len s = 
    validate id (fun (s:string) -> s.Length <= len) s

type LenValidator(config) = 
    inherit Pi<int, string, string option>(config, validateLen)

type Size4 () = inherit LenValidator(4) 

type String4 = SomeDependentType<Size4, int, string, string>

let demo1() =
    let okString = String4.TryCreate "good" // Some
    let failString = String4.TryCreate "much too long" //None
    let z = okString.Value
    printfn "okString is: %s" z.Value
    printfn "failString is: %A" failString

let validateRange (min,max) v = validate id (fun v -> v >= min && v <= max) v
let validateMin (min) v = validate id (fun v -> v >= min) v
let validateMax (max) v = validate id (fun v -> v <= max) v

type NumRangeValidator(config) = inherit Pi<int * int, int, int option>(config, validateRange)
type MinNumRangeValidator(config) = inherit Pi<int, int, int option>(config, validateMin)
type MaxNumRangeValidator(config) = inherit Pi<int, int, int option>(config, validateMax)

type MaxPos100 () = inherit NumRangeValidator(0, 100)
type MaxPos20000 () = inherit NumRangeValidator(0, 20000)
type RangeMinus100To100 () = inherit NumRangeValidator(-100, 100)
type Min101 () = inherit MinNumRangeValidator(101)
type MaxMinus101 () = inherit MaxNumRangeValidator(-101)

type PositiveInt100 = SomeDependentType<MaxPos100, int * int, int, int>
type PositiveInt20000 = SomeDependentType<MaxPos20000, int * int, int, int>
//type Minus100To100 = DependentType<RangeMinus100To100, int * int, int, int option>

type GT100 = DependentType<Min101, int, int, int option>
//type LTminus100 = DependentType<MaxMinus101, int, int, int option>

let demo2() =
    let a : PositiveInt100 = mkDependentType 100
    let b = a |> extract
    let c : PositiveInt20000 = convertTo a
    let d : PositiveInt20000 = PositiveInt20000.ConvertTo a

    printfn "%i" d.Value
    printfn "%b" ((b = c.Value) = (c.Value = d.Value))

(* 'T -> 'T2 test 1*)

let tryConstruct normalize fn v =
    fn (normalize v)

let tryConstructIndexToString i = 
    tryConstruct id (fun i' -> 
        (match i' with
        | n when n < 0 -> "negative"
        | n when n > 0 -> "positive"
        | _ -> "zero" ) ) i

let tryIndexToString _ v = tryConstruct id tryConstructIndexToString v

type IndexToStringPi() = 
    inherit Pi<unit, int, string>((), tryIndexToString)

type IndexToString = DependentType<IndexToStringPi, unit, int, string>

let demo4() =
    printfn ""
    printfn "dependent type that indexes to string (int -> string)"
    let neg =  (IndexToString.Create -100).Value

    printfn "%s" neg

    let zero : IndexToString = mkDependentType 0
    let zeroExtracted  = zero |> extract

    printfn "zero extracted: %s" zeroExtracted

    //let zeroValToString4 : String4 =  zero |> convertTo 

    //printfn "zero converted to String5: %A of type %A" zeroValToString4.Value <| zeroValToString4.GetType()

(* 'T -> 'T2 test 2*)

let tryConstructMultiplyToString multiplier i = 
    tryConstruct id (fun i' -> 
        (multiplier * i').ToString()
        |> Some ) i

type Multiply5ToStringPi() = 
    inherit Pi<int, int, string option>(5, tryConstructMultiplyToString)

type Multiply5ToString = SomeDependentType<Multiply5ToStringPi, int, int, string>

let demo5() =
    printfn ""
    printfn "dependent type that performs arithmentic on element and converts to string (int -> string)"
    let neg500 =  Multiply5ToString.Create -100

    printfn "%s" neg500.Value

    let neg500' : Multiply5ToString = mkDependentType -100
    let neg500Extracted  = neg500' |> extract

    printfn "neg500Extracted extracted: %A" neg500Extracted

    let neg500Val = neg500'.Value
    let neg500ValToString4 : String4 = convertTo neg500'

    printfn "neg500Val converted to String5: %A of type %A" neg500ValToString4.Value <| neg500ValToString4.GetType()

let demo6() =
    let neg500_1 =  (Multiply5ToString.Create -100).Value
    let neg500_2 =  (Multiply5ToString.Create -100).Value

    printfn "same values equal?: %b" (neg500_1 = neg500_2)

let demo6_1() =
    let neg500 =  (Multiply5ToString.Create -100).Value
    let neg1000 =  (Multiply5ToString.Create -200).Value

    printfn "unequal values equal?: %b" (neg500 = neg1000)


let demo7() =
    let n1 =  (String4.Create "100").Value
    let n2 =  (String4.Create "200").Value
    let n3 =  (String4.Create "300").Value
    let n4 =  (String4.Create "400").Value
    let n5 =  (String4.Create "500").Value

    let l1 = [n1; n2; n3; n4; n5]
    let l2 = [n5; n4; n1; n2; n3]

    printfn "supports comparison?: %b" (l1 = (l2 |> List.sort))
