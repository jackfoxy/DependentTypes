namespace robkuz.DependentTypes

module DependentTypes =
    let inline mkLimitedValue (x: ^S) : Option< ^T> = 
        (^T: (static member TryParse: ^S -> Option< ^T>) x)

    let inline extract (x:^S) = 
        (^S: (static member Extract: ^S -> ^T) x)

    let inline convertTo (x: ^S) : Option< ^T> = 
        (^T: (static member ConvertTo: ^S -> Option< ^T>) x)

open DependentTypes
open System

type Validator<'Config, 'T> (config: 'Config, vfn: 'Config -> 'T -> Option<'T>) =
    member __.Validate(x:'T) : Option<'T> = vfn config x

type DependentType<'Validator, 'Config, 'T when 'Validator :> Validator<'Config, 'T>
                                           and  'Validator : (new: unit -> 'Validator)> =
    DependentType of 'T 
    
    with
        member __.Value = 
            let (DependentType s) = __
            s
        static member Extract (x : DependentType<'Validator, 'Config, 'T> ) = 
            let (DependentType s) = x
            s
        static member TryParse(x:'T) : Option<DependentType<'Validator, 'Config, 'T>> =
            (new 'Validator()).Validate x
            |> Option.map DependentType

        static member inline ConvertTo(x : DependentType<'x, 'y, 'q> ) : Option<DependentType<'a, 'b, 'q>> = 
            let (DependentType v) = x
            mkLimitedValue v

module TestTypes =
    //let private validate normalize fn v = 
    let validate normalize fn v =
        if fn (normalize v) then Some (normalize v) else None

    let validateLen len s = 
        //validate trim (fun (s:string) -> s.Length <= len) s
        validate id (fun (s:string) -> s.Length <= len) s

    type LenValidator(config) = 
        inherit Validator<int, string>(config, validateLen)

    type Size5 () = inherit LenValidator(5) 

    type String5 = DependentType<Size5, int, string>
    let okString = String5.TryParse "short" // Some
    let failString = String5.TryParse "much too long" //None

    let z = okString.Value
    printfn "%s" z.Value

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

    type PositiveInt100 = DependentType<MaxPos100, int * int, int>
    type PositiveInt20000 = DependentType<MaxPos20000, int * int, int>
    type Minus100To100 = DependentType<RangeMinus100To100, int * int, int>

    type GT100 = DependentType<Min101, int, int>
    type LTminus100 = DependentType<MaxMinus101, int, int>

    let a: Option<PositiveInt100> = mkLimitedValue 100
    //let b = a.Value |> extract
    let c = a.Value
    let c' : Option<PositiveInt20000> = convertTo c
    let d : Option<PositiveInt20000> = PositiveInt20000.ConvertTo c

    printfn "%i" d.Value.Value

    /// this is a dependent function
    /// the type hint is not necessary, only to enhance the intellisense
    let f n : DependentType<_, _, int> =
        match n with
        | n' when n' < -100 -> (LTminus100.TryParse n).Value |> box
        | n' when n' > 100 -> (GT100.TryParse n).Value |> box
        | _ -> (Minus100To100.TryParse n ).Value|> box
        |> unbox

    let lTminus100 : LTminus100 = f -200
    let gT100 : GT100 = f 101
    let minus100To100 : Minus100To100 = f 1

    type Foo =
        | LTminus100 of LTminus100
        | GT100 of GT100
        | Minus100To100 of Minus100To100

            //because dependent functions in a strongly typed language are useless
    //let foo (x: DependentType<_, _, int>) =
    //    match x with
    //    | :? LTminus100 -> Foo.LTminus100

    //let f1 = f -200 |> Foo


