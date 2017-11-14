(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/DependentTypes"
#r "DependentTypes.dll"
(**
Dependent types tutorial
========================

Most of the examples support both the ````'T1 -> 'T2```` (named DependentType) and ````'T -> 'T```` (named LimitedValue) style
dependent types. 

### Trimmed, non-empty, non-null strings

The ````module```` is for code formatting purposes. Otherwise it serves no functional purpose. This is an example of passing ````unit````
as the ````config````.
*)
open robkuz.DependentTypes
open DependentTypes
open System

module TrimNonEmptyStringDef =
    let verifyTrimNonEmptyString _ (value : string) =
        if String.IsNullOrWhiteSpace value then
            None        
        else 
            Some <| value.Trim()

    type NonEmptyValidator() = 
        inherit Cctor<unit, string, string>((), verifyTrimNonEmptyString)

    type NonEmpty () = inherit NonEmptyValidator()

type TrimNonEmptyString = DependentType<TrimNonEmptyStringDef.NonEmpty, unit, string, string> 
(**
The ````DependentType.Value```` returns the element in its base type.
*)
let myGoodString = (TrimNonEmptyString.TryParse "good string   ").Value

let notTrimNonEmptyString = TrimNonEmptyString.TryParse "    "

// DependentType "good string"
printfn "%A" myGoodString

// "good string"
printfn "%s" myGoodString.Value

// true
printfn "%b" notTrimNonEmptyString.IsNone
(**
### Overloaded try and parse (create) methods

If the dependent type construction is guaranteed to return ````Some````, you can safely use the ````Parse```` method.

If ````DependentType```` supported method extensions, only 1 ````TryParse```` static member would be required, and users
could overload ````TryParse```` and ````Parse```` to meet their needs. For now we must provide all necessary overloads in
the ````DependentTypes```` library.
*)
module UtcDateTimeDef =
    let verifyUtcDateTime _ (value : DateTime) =
        Some <| value.ToUniversalTime()     

    type UtcDateTimeValidator() = 
        inherit Cctor<unit, DateTime, DateTime>((), verifyUtcDateTime)

    type ValidUtcDateTime () = inherit UtcDateTimeValidator()
    
type UtcDateTime = DependentType<UtcDateTimeDef.ValidUtcDateTime, unit, DateTime, DateTime> 

let utcTime = DateTime.Now |> UtcDateTime.Parse
(**
### Generalized and specific type creation

Use the ````config```` input to make more specific types over a generalized validator function. 

Construct the ````DependentType option```` one of two ways
*)
let validate normalize fn v =
    if fn (normalize v) then Some (normalize v) else None

//let validateLen len s = 
//    validate id (fun (s:string) -> s.Length <= len) s

//type LenValidator(config) = 
//    inherit Cctor<int, string, string>(config, validateLen)
let validateRange (min,max) v = validate id (fun v -> v >= min && v <= max) v

type NumRangeValidator(config) = inherit Cctor<int * int, int, int>(config, validateRange)

type MaxPos100 () = inherit NumRangeValidator(0, 100)

type PositiveInt100 = DependentType<MaxPos100, int * int, int, int>

let a : Option<PositiveInt100> = mkDependentType 100

let b = PositiveInt100.TryParse 100
(**
### Working with the underlying element

Return the underlying typed element with the ````extract```` function or the ````Value```` property. 

````DependentType.ToString()```` implements the underlying element's type ````ToString()````.
*)
let a' = a.Value |> extract
let b' = b.Value.Value

// 100
printfn "%i" a'

// "100"
printfn "%s" <| b.ToString()
(**
### Converting between dependent types

````convertTo```` supports trying to convert between dependent types. F# type inference cannot infer the resulting option type, therefore a hint is needed.
*)
type MaxPos200 () = inherit NumRangeValidator(0, 200)

type PositiveInt200 = DependentType<MaxPos100, int * int, int, int>

let valToString5 : Option<PositiveInt200> = convertTo a.Value

printfn "PositiveInt100 converted to PositiveInt200: %A of type %A" valToString5.Value <| valToString5.GetType()
