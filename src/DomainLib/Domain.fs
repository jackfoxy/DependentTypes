namespace DomainLib

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
        inherit PiType<unit, string, string>((), verifyTrimNonEmptyString)

    type NonEmpty () = inherit NonEmptyValidator()

type TrimNonEmptyString = DependentType<TrimNonEmptyStringDef.NonEmpty, unit, string, string> 

module UtcDateTimeDef =
    let verifyUtcDateTime _ (value : DateTime) =
        Some <| value.ToUniversalTime()     

    type UtcDateTimeValidator() = 
        inherit PiType<unit, DateTime, DateTime>((), verifyUtcDateTime)

    type ValidUtcDateTime () = inherit UtcDateTimeValidator()
    
type UtcDateTime = DependentType<UtcDateTimeDef.ValidUtcDateTime, unit, DateTime, DateTime> 

module NonEmptySetDef =
    let verifyNonEmptySet _ (value : Set<'T>) =
        if value.Count > 0 then
            Some value  
        else
            None

    type NonEmptySetValidator<'T when 'T : comparison>() = 
        inherit Validator<unit, Set<'T>>((), verifyNonEmptySet)

    type ValidNonEmptySet<'T when 'T : comparison>() = inherit NonEmptySetValidator<'T>()
    
type NonEmptySet<'T  when 'T : comparison> = LimitedValue<NonEmptySetDef.ValidNonEmptySet<'T>, unit, Set<'T>>   

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
        inherit PiType<int, string, string>(config, verifyUpperLatin)

    type ValidUpperLatin2 () = inherit UpperLatinValidator(2)
    type ValidUpperLatin3 () = inherit UpperLatinValidator(3)
    
type UpperLatin2 = DependentType<UpperLatinDef.ValidUpperLatin2, int, string, string>
type UpperLatin3 = DependentType<UpperLatinDef.ValidUpperLatin3, int, string, string>

module DigitsDef =
    let regex = new Regex("^[0-9]+$")
    let verifyDigits config value =
        RegExStringVerify.regExStringVerify regex config value

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

module IntRange =
    let validate fn v =
        if fn v then Some v else None
    let validateRange (min,max) v = validate (fun v -> v >= min && v <= max) v
    let validateMin (min) v = validate (fun v -> v >= min) v
    let validateMax (max) v = validate (fun v -> v <= max) v

    type NumRangeValidator(config) = inherit Validator<int * int, int>(config, validateRange)
    type MinNumRangeValidator(config) = inherit Validator<int, int>(config, validateMin)
    type MaxNumRangeValidator(config) = inherit Validator<int, int>(config, validateMax)

    type MaxPos100 () = inherit NumRangeValidator(0, 100)
    type MaxPos20000 () = inherit NumRangeValidator(0, 20000)
    type RangeMinus100To100 () = inherit NumRangeValidator(-100, 100)
    type Min101 () = inherit MinNumRangeValidator(101)
    type MaxMinus101 () = inherit MaxNumRangeValidator(-101)

type PositiveInt100 = LimitedValue<IntRange.MaxPos100, int * int, int>
type PositiveInt20000 = LimitedValue<IntRange.MaxPos20000, int * int, int>
type Minus100To100 = LimitedValue<IntRange.RangeMinus100To100, int * int, int>

type GT100 = LimitedValue<IntRange.Min101, int, int>
type LTminus100 = LimitedValue<IntRange.MaxMinus101, int, int>