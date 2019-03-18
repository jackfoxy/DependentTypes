namespace DomainLib2

open DependentTypes
open System
open System.Text.RegularExpressions

module TrimNonEmptyStringDef =
    let verifyTrimNonEmptyString _ (value : string) =
        if String.IsNullOrWhiteSpace value then
            None        
        else 
            Some <| value.Trim()

    type NonEmptyValidator() = 
        inherit Pi<unit, string, string option>((), verifyTrimNonEmptyString)

type TrimNonEmptyString = SomeDependentType<TrimNonEmptyStringDef.NonEmptyValidator, unit, string, string> 

module UtcDateTimeDef =
    let verifyUtcDateTime _ (value : DateTime) =
        value.ToUniversalTime()     

    type UtcDateTimeValidator() = 
        inherit Pi<unit, DateTime, DateTime>((), verifyUtcDateTime)
 
type UtcDateTime = DependentType<UtcDateTimeDef.UtcDateTimeValidator, unit, DateTime, DateTime> 

module NonEmptySetDef =
    let verifyNonEmptySet _ (value : Set<'T>) =
        if value.Count > 0 then
            Some value  
        else
            None

    type NonEmptySetValidator<'T when 'T : comparison>() = 
        inherit Pi<unit, Set<'T>, Set<'T> option>((), verifyNonEmptySet)
 
type NonEmptySet<'T  when 'T : comparison> = SomeDependentType<NonEmptySetDef.NonEmptySetValidator<'T>, unit, Set<'T>, Set<'T>>   

module RegExStringVerify =
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

module UpperLatinDef =
    let regex = new Regex("^[A-Z]+$")
    let verifyUpperLatin config value =
        RegExStringVerify.regExStringVerify regex config value

    type UpperLatinValidator(config) = 
        inherit Pi<int, string, string option>(config, verifyUpperLatin)

    type ValidUpperLatin2 () = inherit UpperLatinValidator(2)
    type ValidUpperLatin3 () = inherit UpperLatinValidator(3)
    
type UpperLatin2 = SomeDependentType<UpperLatinDef.ValidUpperLatin2, int, string, string>
type UpperLatin3 = SomeDependentType<UpperLatinDef.ValidUpperLatin3, int, string, string>

module DigitsDef =
    let regex = new Regex("^[0-9]+$")
    let verifyDigits config value =
        RegExStringVerify.regExStringVerify regex config value

    type DigitsValidator(config) = 
        inherit Pi<int, string, string option>(config, verifyDigits)

    type ValidDigits () = inherit DigitsValidator(0)
    type ValidDigits2 () = inherit DigitsValidator(2)
    type ValidDigits3 () = inherit DigitsValidator(3)
    type ValidDigits4 () = inherit DigitsValidator(4)
    
type Digits = SomeDependentType<DigitsDef.ValidDigits, int, string, string>
type Digits2 = SomeDependentType<DigitsDef.ValidDigits2, int, string, string>
type Digits3 = SomeDependentType<DigitsDef.ValidDigits3, int, string, string>
type Digits4 = SomeDependentType<DigitsDef.ValidDigits4, int, string, string>

module IntRange =
    let validate fn v =
        if fn v then Some v else None
    let validateRange (min,max) v = validate (fun v -> v >= min && v <= max) v
    let validateMin (min) v = validate (fun v -> v >= min) v
    let validateMax (max) v = validate (fun v -> v <= max) v

    type NumRangeValidator(config) = inherit Pi<int * int, int, int option>(config, validateRange)
    type MinNumRangeValidator(config) = inherit Pi<int, int, int option>(config, validateMin)
    type MaxNumRangeValidator(config) = inherit Pi<int, int, int option>(config, validateMax)

    type MaxPos100 () = inherit NumRangeValidator(0, 100)
    type MaxPos20000 () = inherit NumRangeValidator(0, 20000)
    type RangeMinus100To100 () = inherit NumRangeValidator(-100, 100)
    type Min101 () = inherit MinNumRangeValidator(101)
    type MaxMinus101 () = inherit MaxNumRangeValidator(-101)

type PositiveInt100 = SomeDependentType<IntRange.MaxPos100, int * int, int, int>
type PositiveInt20000 = SomeDependentType<IntRange.MaxPos20000, int * int, int, int>
type Minus100To100 = SomeDependentType<IntRange.RangeMinus100To100, int * int, int, int>

type GT100 = SomeDependentType<IntRange.Min101, int, int, int>
type LTminus100 = SomeDependentType<IntRange.MaxMinus101, int, int, int>