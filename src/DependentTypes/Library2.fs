namespace robkuz.DependentTypes2

module DependentTypes2 =
    let inline mkDependentType (x: ^S) : Option< ^T> = 
        (^T: (static member TryParse: ^S -> Option< ^T>) x)

    let inline extract (x:^S) = 
        (^S: (static member Extract: ^S -> ^T) x)

    let inline convertTo (x: ^S) : Option< ^T> = 
        (^T: (static member ConvertTo: ^S -> Option< ^T>) x)

open DependentTypes2
open System

type Cctor<'Config, 'T, 'T2> (config: 'Config, vfn: 'Config -> 'T -> Option<'T2>) =
    member __.TryCreate(x:'T) : Option<'T2> = vfn config x

type DependentType<'Cctor, 'Config, 'T, 'T2 when 'Cctor :> Cctor<'Config, 'T, 'T2>
                                            and  'Cctor : (new: unit -> 'Cctor)> =
    DependentType of 'T2 
    
    with
        member __.Value = 
            let (DependentType s) = __
            s
        static member Extract (x : DependentType<'Cctor, 'Config, 'T, 'T2> ) = 
            let (DependentType s) = x
            s
        static member TryParse(x:'T) : Option<DependentType<'Cctor, 'Config, 'T, 'T2>> =
            (new 'Cctor()).TryCreate x
            |> Option.map DependentType

        static member inline ConvertTo(x : DependentType<'x, 'y, 'q, 'r> ) : Option<DependentType<'a, 'b, 'r, 's>> = 
            let (DependentType v) = x
            mkDependentType v     

module TestTypes =
    (* 'T -> 'T2 tests existing LimitValue tests where 'T = 'T2 *)

    let validate normalize fn v =
        if fn (normalize v) then Some (normalize v) else None

    let validateLen len s = 
        validate id (fun (s:string) -> s.Length <= len) s

    type LenValidator(config) = 
        inherit Cctor<int, string, string>(config, validateLen)

    type Size5 () = inherit LenValidator(5) 

    type String5 = DependentType<Size5, int, string, string>
    let okString = String5.TryParse "short" // Some
    let failString = String5.TryParse "much too long" //None

    let z = okString.Value
    printfn "%s" z.Value

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

    let lTminus100 : LTminus100 = f -200
    let gT100 : GT100 = f 101
    let minus100To100 : Minus100To100 = f 1

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

    let neg =  (IndexToString.TryParse -100).Value

    printfn "%s" neg.Value

    let zero : Option<IndexToString> = mkDependentType 0
    let zeroExtracted  = zero.Value |> extract
    let zeroVal = zero.Value
    let zeroValToString5 : Option<String5> = convertTo zeroVal

    (* 'T -> 'T2 test 2*)

    let tryConstructMultiplyToString multiplier i = 
        tryConstruct id (fun i' -> 
            (multiplier * i').ToString()
            |> Some ) i

    type Multiply5ToStringCctor() = 
        inherit Cctor<int, int, string>(5, tryConstructMultiplyToString)

    type Multiply5ToString = DependentType<Multiply5ToStringCctor, int, int, string>

    let neg500 =  (Multiply5ToString.TryParse -100).Value

    printfn "%s" neg500.Value

    let neg500' : Option<Multiply5ToString> = mkDependentType -100
    let neg500Extracted  = neg500'.Value |> extract
    let neg500Val = neg500'.Value
    let neg500ValToString5 : Option<String5> = convertTo neg500Val