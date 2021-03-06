﻿module DemoDependentType

open DependentTypes
open DependentTypes
open System

let validate normalize fn v =
    if fn (normalize v) then Some (normalize v) else None

let validateLen len s = 
    //validate trim (fun (s:string) -> s.Length <= len) s
    validate id (fun (s:string) -> s.Length <= len) s

type LenValidator(config) = 
    inherit PiType<int, string, string option>(config, validateLen)

type Size5 () = inherit LenValidator(5) 

type String5 = DependentType<Size5, int, string, string option>

let demo1() =
    let okString = String5.TryCreate "short" // Some
    let failString = String5.TryCreate "much too long" //None
    let z = okString.Value
    printfn "okString is: %s" z.Value
    printfn "failString is: %A" failString

let validateRange (min,max) v = validate id (fun v -> v >= min && v <= max) v
let validateMin (min) v = validate id (fun v -> v >= min) v
let validateMax (max) v = validate id (fun v -> v <= max) v

type NumRangeValidator(config) = inherit PiType<int * int, int>(config, validateRange)
type MinNumRangeValidator(config) = inherit PiType<int, int>(config, validateMin)
type MaxNumRangeValidator(config) = inherit PiType<int, int>(config, validateMax)

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

let demo2() =
    let a: Option<PositiveInt100> = mkDependentType 100
    let b = a.Value |> extract
    let c = a.Value
    let c' : Option<PositiveInt20000> = convertTo c
    let d : Option<PositiveInt20000> = PositiveInt20000.ConvertTo c

    printfn "%i" d.Value.Value
