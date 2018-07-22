(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/DependentTypes/net47"
#r "DependentTypes.dll"
open System
open System.Text.RegularExpressions
let regExStringVerify (regex : Regex) config (value : string) =
    if String.IsNullOrWhiteSpace value then
        None
    else
        let s' = value.Trim()
        if regex.IsMatch s' then 
            if config > 0 then
                if String.length(s') = config then 
                        Some s'
                    else 
                        None
            else
                Some s'
        else
            None
(**
DependentTypes
==============

This project is an experiment in bringing [dependent types](https://en.wikipedia.org/wiki/Dependent_type) to F#.
Dependent typing makes supporting a finer level of type granularity easier.

This library presents two alternate generic dependent types. One taking an input element to the same base type within the dependent type ````'T -> 'T````, and the other
taking the input type to a new base type ````'T1 -> 'T2````. Dependent types support the same equality and comparison traits as their base type
(````'T```` or ````'T2````). [Extension methods are not yet supported](https://github.com/jackfoxy/DependentTypes/issues/1).

The core dependent type 
````
type DependentType<'PiType, 'Config, 'T, 'T2 when 'PiType :> PiType<'Config, 'T, 'T2>
                                            and  'PiType : (new: unit -> 'PiType)>
````
relies on a "constructor" type 
````
type PiType<'Config, 'T, 'T2> (config: 'Config, vfn: 'Config -> 'T -> Option<'T2>)
````
which in turn requires a function ````'Config -> 'T1 -> 'T2 option```` that validates the input element. Another type handles consuming the ````'Config```` parameter.

In practice the construction of a family of dependent types looks like this:
*)
open DependentTypes

module DigitsDef =
    let regex = new Regex("^[0-9]+$")
    let verifyDigits config value =
        regExStringVerify regex config value

    type DigitsValidator(config) = 
        inherit PiType<int, string, string>(config, verifyDigits)

    type ValidDigits () = inherit DigitsValidator(0)
    type ValidDigits2 () = inherit DigitsValidator(2)
    type ValidDigits3 () = inherit DigitsValidator(3)
    type ValidDigits4 () = inherit DigitsValidator(4)
    
type Digits = DependentType<DigitsDef.ValidDigits, int, string, string>
type Digits2 = DependentType<DigitsDef.ValidDigits2, int, string, string>
type Digits3 = DependentType<DigitsDef.ValidDigits3, int, string, string>
type Digits4 = DependentType<DigitsDef.ValidDigits4, int, string, string>

let digits = Digits.Create "093884765"
let digitsofLength3 = Digits3.Create "007"
(**
### Notes: 

1. The full validation function ````regExStringVerify```` is not shown.

2. The presence of ````module DigitsDef```` is strictly for readability purposes, segregating the "helper" functions and types.

3. All the helper types must have the same access level as the dependent type.

4. Aliasing is optional, but obviously provides better readability.

5. Yes, ````PiType```` is not really a constructor.

6. With [possible changes to the F# language](https://github.com/jackfoxy/DependentTypes/issues/3), the intervening ````'Config```` consuming helper type may be superfluous.

### Alternate form of dependent types

Alternately, a dependent type that restricts the underlying base type to the input element type requires one less type parameter.

See the [Tutorial](tutorial.html) and [sample library of dependent types](https://github.com/jackfoxy/DependentTypes/blob/9281602a7d119920735e1c786df462ca000d4ab2/src/DomainLib/Domain.fs#L32) for an
example of a generic collection type, ````Set<'T>````.
*)
module NonEmptySetDef =
    let verifyNonEmptySet _ (value : Set<int>) =
        if value.Count > 0 then
            Some value  
        else
            None

    type NonEmptySetValidator() = 
        inherit Validator<unit, Set<int>>((), verifyNonEmptySet)

    type ValidNonEmptySet() = inherit NonEmptySetValidator()
    
type NonEmptyIntSet = DependentType<NonEmptySetDef.ValidNonEmptySet, unit, Set<int>>

let myNonEmptyIntSetOpt = [1;2;3] |> Set.ofList |> NonEmptyIntSet.TryCreate
(**

Samples & documentation
-----------------------

 * [Tutorial](tutorial.html) contains a further explanation of this dependent types library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules,
   and functions in the library.

 * The [DomainLib](https://github.com/jackfoxy/DependentTypes/tree/master/src/DomainLib/Domain.fs) project is a sample library of useful dependent types:

 **trimmed, non-empty, non-null string**

 **non-empty generic set**

 **utc datetime**

 **uppercase Latin string of undetermined or static length**

 **digit string of undetermined or static length**

 **integer restricted to a range**

 * The [DependentTypesConsole](https://github.com/jackfoxy/DependentTypes/tree/master/src/DependentTypesConsole) project runs demos on both the ````'T1 -> 'T2```` and ````'T -> 'T```` style
   dependent types.

 * [Expecto](https://github.com/haf/expecto) test projects for both the [DependentTypes](https://github.com/jackfoxy/DependentTypes/tree/master/tests/DependentTypes.Tests) library and the [DomainLib](https://github.com/jackfoxy/DependentTypes/tree/master/tests/DomainLib.Tests) sample dependent types.

Issues
------

Several issues are available for discussion. Among the most interesting

 * [Dependent types do not support extension methods](https://github.com/jackfoxy/DependentTypes/issues/1).
 
 * [Add Expecto BenchmarkDotNet performance tests](https://github.com/jackfoxy/DependentTypes/issues/5)

 * [Future direction: literal type parameters](https://github.com/jackfoxy/DependentTypes/issues/3).
 
Contributing and copyright
--------------------------

This library is based on original experiments by @robkuz with the LimitedValue type,
[Creating Generic Wrappers for Validated Values](https://robkuz.github.io/Limited-Values/).
Further discussion [here](https://github.com/robkuz/robkuz.github.io/issues/6)

You can [report issues][issues], fork 
the project, and submit pull requests. Please also 
add tests and samples that can be turned into [documentation](https://github.com/jackfoxy/DependentTypes/tree/master/docsrc/content).

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/jackfoxy/DependentTypes/tree/master/docs/content
  [gh]: https://github.com/jackfoxy/DependentTypes
  [issues]: https://github.com/jackfoxy/DependentTypes/issues
  [readme]: https://github.com/jackfoxy/DependentTypes/blob/master/README.md
  [license]: https://github.com/jackfoxy/DependentTypes/blob/master/LICENSE.txt
*)
