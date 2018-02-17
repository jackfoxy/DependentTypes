(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/DependentTypes/net47"
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
        inherit PiType<unit, string, string>((), verifyTrimNonEmptyString)

    type NonEmpty () = inherit NonEmptyValidator()

type TrimNonEmptyString = DependentType<TrimNonEmptyStringDef.NonEmpty, unit, string, string> 
(**
The ````DependentType.Value```` returns the element in its base type.
*)
let myGoodString = (TrimNonEmptyString.TryCreate "good string   ").Value

let notTrimNonEmptyString = TrimNonEmptyString.TryCreate "    "

// DependentType "good string"
printfn "%A" myGoodString

// "good string"
printfn "%s" myGoodString.Value

// true
printfn "%b" notTrimNonEmptyString.IsNone
(**
### TryCreate and Create methods

If the dependent type construction is guaranteed to return ````Some````, you can safely use the ````Create```` method.
*)
module UtcDateTimeDef =
    let verifyUtcDateTime _ (value : DateTime) =
        Some <| value.ToUniversalTime()     

    type UtcDateTimeValidator() = 
        inherit PiType<unit, DateTime, DateTime>((), verifyUtcDateTime)

    type ValidUtcDateTime () = inherit UtcDateTimeValidator()
    
type UtcDateTime = DependentType<UtcDateTimeDef.ValidUtcDateTime, unit, DateTime, DateTime> 

let utcTime = DateTime.Now |> UtcDateTime.Create
(**
### Generalized and specific type creation

Use the ````config```` input to make more specific types over a generalized validator function. 

Construct the ````DependentType option```` one of two ways
*)
let validate normalize fn v =
    if fn (normalize v) then Some (normalize v) else None

let validateRange (min,max) v = validate id (fun v -> v >= min && v <= max) v

type NumRangeValidator(config) = inherit PiType<int * int, int, int>(config, validateRange)

type MaxPos100 () = inherit NumRangeValidator(0, 100)

type PositiveInt100 = DependentType<MaxPos100, int * int, int, int>

let a : PositiveInt100 option = mkDependentType 100

let b = PositiveInt100.TryCreate 100
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
        inherit PiType<unit, Set<'T>, Set<'T>>((), verifyNonEmptySet)

    type ValidNonEmptySet<'T when 'T : comparison>() = inherit NonEmptySetValidator<'T>()
    
type NonEmptySet<'T  when 'T : comparison> = DependentType<NonEmptySetDef.ValidNonEmptySet<'T>, unit, Set<'T>, Set<'T>>

let myNonEmptyIntSet = [1;2;3] |> Set.ofList |> NonEmptySet.Create
let myNonEmptyStringSet = ["Rob";"Jack";"Don"] |> Set.ofList |> NonEmptySet.Create
(**
### Limit values to ranges

Exercise for the student: make the following dependent types for generic numbers.
*)
module IntRange =
    let validate fn v =
        if fn v then Some v else None
    let validateRange (min,max) v = validate (fun v -> v >= min && v <= max) v
    let validateMin (min) v = validate (fun v -> v >= min) v
    let validateMax (max) v = validate (fun v -> v <= max) v

    type NumRangeValidator(config) = inherit Validator<int * int, int>(config, validateRange)
    type MinNumRangeValidator(config) = inherit Validator<int, int>(config, validateMin)
    type MaxNumRangeValidator(config) = inherit Validator<int, int>(config, validateMax)

    type MaxPos150 () = inherit NumRangeValidator(0, 150)
    type MaxPos20000 () = inherit NumRangeValidator(0, 20000)
    type RangeMinus100To100 () = inherit NumRangeValidator(-100, 100)
    type Min101 () = inherit MinNumRangeValidator(101)
    type MaxMinus101 () = inherit MaxNumRangeValidator(-101)

type PositiveInt150 = LimitedValue<IntRange.MaxPos150, int * int, int>
type PositiveInt20000 = LimitedValue<IntRange.MaxPos20000, int * int, int>
type Minus100To100 = LimitedValue<IntRange.RangeMinus100To100, int * int, int>

type GT100 = LimitedValue<IntRange.Min101, int, int>
type LTminus100 = LimitedValue<IntRange.MaxMinus101, int, int>
(**
### Create and TryCreate overloads

If ````DependentType```` and ````LimitedValue```` supported method extensions, only a single ````TryCreate```` static member would be required, and users
could overload ````TryCreate```` and ````Create```` to meet their needs. For now we must provide all necessary overloads in
the ````DependentTypes```` library.
*)
let aa = Some " this string " |> TrimNonEmptyString.TryCreate

let bb = "this string " |> TrimNonEmptyString.TryCreate

let cc = "this string " |> TrimNonEmptyString.Create

let dd = [" this string "; "that string"; "the other string "] |> TrimNonEmptyString.Create

let ee = [|" this string "; "that string"; "the other string "|] |> TrimNonEmptyString.Create
(**
### Why 'T1 -> 'T2?

So far we have not shown any compelling examples exploiting ````DependentType```` taking ````'T1```` input to a ````'T2```` base type.
That's because we have not thought of any! But we do not want to deny you the opportunity to come up with your own use case.

Here is a trivial example:
*)
module TrivialT1toT2 =
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

    type IndexToStringPiType() = 
        inherit PiType<unit, int, string>((), tryIndexToString)

type IndexToString = DependentType<TrivialT1toT2.IndexToStringPiType, unit, int, string>

let neg =  (IndexToString.TryCreate -100).Value

printfn "%s" neg.Value
