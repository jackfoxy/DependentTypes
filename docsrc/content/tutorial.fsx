(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/DependentTypes/net45"
#r "DependentTypes.dll"

(**
Dependent types tutorial
========================

### Trimmed, non-empty, non-null strings

This is an example of passing ````unit```` as the ````config````.
We will see later that passing a configuration other than unit, (), requires a second level of type inheritance.

Note that the ````module TrimNonEmptyStringDef```` helps format the code for readability. Otherwise it serves no functional purpose. 
*)
open DependentTypes
open DependentTypes.DependentTypes
open System

module TrimNonEmptyStringDef =
    let verifyTrimNonEmptyString _ (value : string) =
        if String.IsNullOrWhiteSpace value then
            None        
        else 
            Some <| value.Trim()

    type NonEmptyValidator() = 
        inherit PiType<unit, string, string option>((), verifyTrimNonEmptyString)

type TrimNonEmptyString = DependentType<TrimNonEmptyStringDef.NonEmptyValidator, unit, string, string option>  
(**
### Generalized and specific type creation

Use the ````config```` input to make more specific types over a generalized validator function. 

Construct the ````DependentType option```` one of three ways

* ````mkDependentType```` function, requires type hint in let value
* ````TryCreate```` for option base types this will lift option to the DependentType
* ````Create```` is always safe (will not throw), but does not lift option to the DependentType

Note that passing a configuration other than unit, (), requires a second level of inheritance.
*)
module RangeValidation =
    let validate normalize fn v =
        if fn (normalize v) then Some (normalize v) else None

    let validateRange (min,max) v = validate id (fun v -> v >= min && v <= max) v

    type NumRangeValidator(config) = inherit PiType<int * int, int, int option>(config, validateRange)

    type MaxPos100 () = inherit NumRangeValidator(0, 100)

type PositiveInt100 = DependentType<RangeValidation.MaxPos100, int * int, int, int option>

let a : PositiveInt100 = mkDependentType 100

let b = PositiveInt100.TryCreate 100

let c = PositiveInt100.Create 100
(**
### TryCreate method

If the base type is an option type, and it was created with ````TryCreate````, option is lifted to the DependentType itself. If the value is known to be 
````Some````, the unsafe function ````someValue```` may be used to access the value.

````DependentType.Value```` returns the base type value.
*)
let myGoodString = (TrimNonEmptyString.TryCreate "good string   ")

let notTrimNonEmptyString = TrimNonEmptyString.TryCreate "    "

// DependentType "good string"
printfn "%A" myGoodString

// "good string"
printfn "%s" <|
    match myGoodString with
    | Some goodString -> goodString.Value.Value
    | None -> "this won't print"

// "good string"
printfn "%s" myGoodString.Value.Value.Value

// "good string" (unsafe)
printfn "%s" <| someValue myGoodString

// true
printfn "%b" notTrimNonEmptyString.IsNone
(**
### Create method

For all ````'T2```` base types the ````Create```` method is safe (meaning it will not throw), but for option types if will not lift the option).

Here is an example of a dependent type with a simple ````'T2```` base type.
*)
module UtcDateTimeDef =
    let verifyUtcDateTime _ (value : DateTime) =
        value.ToUniversalTime()     

    type UtcDateTimeValidator() = 
        inherit PiType<unit, DateTime, DateTime>((), verifyUtcDateTime)

    type ValidUtcDateTime () = inherit UtcDateTimeValidator()
    
type UtcDateTime = DependentType<UtcDateTimeDef.ValidUtcDateTime, unit, DateTime, DateTime> 

let utcTime = DateTime.Now |> UtcDateTime.Create
(** 
### Base type of discriminated Union
*)
type IntegerOfSign =
        | PositiveInt of int
        | Zero of int
        | NegativeInt of int

module SumType =
    let intType _ (value : int) =
        match value with
        | v when v > 0 ->
            IntegerOfSign.PositiveInt v
        | v when v = 0 ->
            IntegerOfSign.Zero v
        | v ->
            IntegerOfSign.NegativeInt v

    type IntSumTypeDiscriminator() = 
        inherit PiType<unit, int, IntegerOfSign>((), intType)

    type IntSumType () = inherit IntSumTypeDiscriminator()
    
type IntegerType = DependentType<SumType.IntSumType, unit, int, IntegerOfSign>

let s2 = IntegerType.Create -21
let s3 = IntegerType.Create 0
let s4 = IntegerType.Create 21

// DependentType (NegativeInt -21)
printfn "%A"s2

// DependentType (Zero 0)
printfn "%A"s3

// DependentType (PositiveInt 21)
printfn "%A"s4
(**
### Generic dependent types

You can also create generic dependent types.
*)
module NonEmptySetDef =
    let verifyNonEmptySet _ (value : Set<'T>) =
        if value.Count > 0 then
            Some value  
        else
            None

    type NonEmptySetValidator<'T when 'T : comparison>() = 
        inherit PiType<unit, Set<'T>, Set<'T> option>((), verifyNonEmptySet)
  
type NonEmptySet<'T  when 'T : comparison> = DependentType<NonEmptySetDef.NonEmptySetValidator<'T>, unit, Set<'T>, Set<'T> option> 

let myNonEmptyIntSet = [1;2;3] |> Set.ofList |> NonEmptySet.Create
let myNonEmptyStringSet = ["Rob";"Jack";"Don"] |> Set.ofList |> NonEmptySet.Create
(**
### Limit values to ranges

Note that passing a configuration other than unit, (), requires a second level of inheritance.
*)
module IntRange =
    let validate fn v =
        if fn v then Some v else None
    let validateRange (min,max) v = validate (fun v -> v >= min && v <= max) v
    let validateMin (min) v = validate (fun v -> v >= min) v
    let validateMax (max) v = validate (fun v -> v <= max) v

    type NumRangeValidator(config) = inherit PiType<int * int, int, int option>(config, validateRange)
    type MinNumRangeValidator(config) = inherit PiType<int, int, int option>(config, validateMin)
    type MaxNumRangeValidator(config) = inherit PiType<int, int, int option>(config, validateMax)

    type MaxPos100 () = inherit NumRangeValidator(0, 100)
    type MaxPos20000 () = inherit NumRangeValidator(0, 20000)
    type RangeMinus100To100 () = inherit NumRangeValidator(-100, 100)
    type Min101 () = inherit MinNumRangeValidator(101)
    type MaxMinus101 () = inherit MaxNumRangeValidator(-101)

type PositiveInt100' = DependentType<IntRange.MaxPos100, int * int, int, int option>
type PositiveInt20000 = DependentType<IntRange.MaxPos20000, int * int, int, int option>
type Minus100To100 = DependentType<IntRange.RangeMinus100To100, int * int, int, int option>

type GT100 = DependentType<IntRange.Min101, int, int, int option>
type LTminus100 = DependentType<IntRange.MaxMinus101, int, int, int option>
(**
### Working with the underlying element

Return the underlying typed element with the ````extract```` function or the ````Value```` property. 

````DependentType.ToString()```` implements the underlying ````'T2```` element's type ````ToString()````.
*)
let a' = extract a
let b' = someValue b
let c' = c.Value.Value

// Some 100
printfn "%A" a'

// "100"
printfn "%i" b'

// "100"
printfn "%i" c'

// Some 100
printfn "%A" <| a.ToString()
(**
### DependentPair
*)
module SizeMax5Type =
    let validate normalize fn v =
        if fn (normalize v) then Some (normalize v) else None

    let validateLen len s = 
        validate id (fun (s:string) -> s.Length <= len) s

    type LenValidator(config) = 
        inherit PiType<int, string, string option>(config, validateLen)

    type PairLenValidator(config) = 
        inherit SigmaType<int, string, string option>(config, validateLen)

    type SizeMax5 () = inherit LenValidator(5) 

    type SizeMax5Pair () = inherit PairLenValidator(5)

type StringMax5Pair = DependentPair<SizeMax5Type.SizeMax5Pair, int, string, string option>

let s100 = StringMax5Pair.Create "100"
let s100000 = StringMax5Pair.Create "100000"

// DependentPair ("100",Some "100")
printfn "%A" s100

// DependentPair ("100000",None)
printfn "%A" s100000
