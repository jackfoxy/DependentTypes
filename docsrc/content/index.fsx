(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/DependentTypes/net45"
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

In computer science and logic, a **dependent type** is a type whose definition depends on a value. A "pair of integers" is a type. A "pair of integers where the second 
is greater than the first" is a dependent type...

 > *from Wikipedia article, Dependent type*

This project is an experiment in bringing [dependent types](https://en.wikipedia.org/wiki/Dependent_type) to F# for supporting finer levels of type granularity
in a consistent manner.

Dependent types in logic and computer science take an element of a specific type and output a typed element in a family of types. F# does not support *a family of types* in a 
generic sense, but we can still use F# and .NET types as output types that have family-like characteristics. This library presents generic dependent types, taking an element 
of a specific input type to a new base type through a typed function ````'T1 -> 'T2````. 

The base ````'T2```` output types can be:

* any F# or .NET type, a family of one type
* an F# option type, the input element does or does not belong to the output underlying type, mimicking a family of two types
* an F# discriminated union type, the input element belongs to some member of the DU, mimicking a family of arbitrarily many types


The dependent type 
````
type DependentType<'PiType, 'Config, 'T, 'T2 when 'PiType :> PiType<'Config, 'T, 'T2>  
                                              and  'PiType : (new: unit -> 'PiType)>
````
has a type parameter that includes a *configuration*, and a typed *pi* function, which maps input elements of the specified type to elements of the output type.
````
type PiType<'Config, 'T, 'T2> (config: 'Config, pi: 'Config -> 'T -> 'T2)
````
The *configuration* is a convenience allowing re-use of the same function code to serve multiple dependent types by passing any desired parameters.

The construction of similar dependent types sharing the same *pi* function looks like this:
*)
open DependentTypes

module DigitsDef =
    let verifyDigits config value =
        regExStringVerify (new Regex("^[0-9]+$")) config value

    type DigitsValidator(config) = 
        inherit PiType<int, string, string option>(config, verifyDigits)

    type ValidDigits () = inherit DigitsValidator(0)
    type ValidDigits2 () = inherit DigitsValidator(2)
    type ValidDigits3 () = inherit DigitsValidator(3)
    type ValidDigits4 () = inherit DigitsValidator(4)
    
type Digits = DependentType<DigitsDef.ValidDigits, int, string, string option>
type Digits2 = DependentType<DigitsDef.ValidDigits2, int, string, string option>
type Digits3 = DependentType<DigitsDef.ValidDigits3, int, string, string option>
type Digits4 = DependentType<DigitsDef.ValidDigits4, int, string, string option>

let digits = Digits.Create "093884765"
let digitsOfLength3 = Digits3.Create "007"
let notDigitsOfLength3 = Digits3.TryCreate "0007"
(**
### Notes: 

1. The full validation function ````regExStringVerify```` is not shown. A config value < 1 accepts digit strings of any length.

2. The presence of ````module DigitsDef```` is strictly for readability purposes, segregating the "helper" functions and types.

3. All the helper types must have the same access level as the dependent type.

4. Aliasing is optional for providing better readability.

5. ````TryCreate```` lifts the ````option```` of ````'T2```` to the ````DependentType````.

Dependent types support the same equality and comparison traits as their base ````'T2```` type. 

[Extension methods are not yet supported](https://github.com/jackfoxy/DependentTypes/issues/1).

See the [Tutorial](tutorial.html) and [sample library of dependent types](https://github.com/jackfoxy/DependentTypes/blob/9281602a7d119920735e1c786df462ca000d4ab2/src/DomainLib/Domain.fs#L32) for an
example of a generic collection type, ````Set<'T>````.

DependentPairs
==============

The dependent pair 
````
type DependentPair<'SigmaType, 'Config, 'T, 'T2 when 'SigmaType :> SigmaType<'Config, 'T, 'T2>  
                                                 and 'SigmaType : (new: unit -> 'SigmaType)>
````
is a pair of the input element and resulting dependent type. Usage is similar to that of dependent types.

Samples & documentation
-----------------------

 * [Tutorial](tutorial.html) contains a further explanation of this dependent types library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules,
   and functions in the library.

 * The [DomainLib](https://github.com/jackfoxy/DependentTypes/tree/master/src/DomainLib/Domain.fs) project is a sample library of useful dependent types:

     1. trimmed, non-empty, non-null string

     2. non-empty generic set

     3. UTC datetime

     4. uppercase Latin string of undetermined or specific length

     5. digit string of undetermined or specific length

     6. integer restricted to a range

 * The [DependentTypesConsole](https://github.com/jackfoxy/DependentTypes/tree/master/src/DependentTypesConsole) project runs demos of dependent types.

 * [Expecto](https://github.com/haf/expecto) test projects for both the [DependentTypes](https://github.com/jackfoxy/DependentTypes/tree/master/tests/DependentTypes.Tests) library and the [DomainLib](https://github.com/jackfoxy/DependentTypes/tree/master/tests/DomainLib.Tests) sample dependent types.
 
Contributing and copyright
--------------------------

This library is based on original experiments by @robkuz with the LimitedValue type:
[Creating Generic Wrappers for Validated Values](https://robkuz.github.io/Limited-Values/).
Further discussion can be found [here](https://github.com/robkuz/robkuz.github.io/issues/6).

You can [report issues][issues], fork the project, and submit pull requests. Please also 
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
