namespace DomainLib

open DependentTypes
open System 
open System.Collections.Generic

module TrimNonEmptyStringDef =
    type NonEmptyValidator =
        inherit Cctor<unit, string, string>
        new : config : unit -> NonEmptyValidator

    type NonEmpty =
        inherit NonEmptyValidator
        new : unit -> NonEmpty

/// Trimmed, non-empty, non-null string
type TrimNonEmptyString = DependentType<TrimNonEmptyStringDef.NonEmpty, unit, string, string>

module UtcDateTimeDef =
    type UtcDateTimeValidator =
        inherit Cctor<unit, DateTime, DateTime>
        new : config : unit -> UtcDateTimeValidator

    type ValidUtcDateTime =
        inherit UtcDateTimeValidator
        new : unit -> ValidUtcDateTime

/// DateTime set to UTC.
type UtcDateTime = DependentType<UtcDateTimeDef.ValidUtcDateTime, unit, DateTime, DateTime> 

module NonEmptySetDef =
    type NonEmptySetValidator<'T when 'T : comparison> =
        inherit Validator<unit,Set<'T>>
        new : config : unit -> NonEmptySetValidator<'T>

    type ValidNonEmptySet<'T when 'T : comparison> =
        inherit NonEmptySetValidator<'T>
        new : unit -> ValidNonEmptySet<'T>

/// Generic non-empty Set<'T>
type NonEmptySet<'T when 'T : comparison> = LimitedValue<NonEmptySetDef.ValidNonEmptySet<'T>, unit, Set<'T>>

module UpperLatinDef =
    type UpperLatinValidator =
        inherit Cctor<int, string, string>
        new : config : int -> UpperLatinValidator

    type ValidUpperLatin2 =
        inherit UpperLatinValidator
        new : unit -> ValidUpperLatin2

    type ValidUpperLatin3 =
        inherit UpperLatinValidator
        new : unit -> ValidUpperLatin3

/// Uppercase latin string of length 2.
type UpperLatin2 = DependentType<UpperLatinDef.ValidUpperLatin2, int, string, string>
/// Uppercase latin string of length 3.
type UpperLatin3 = DependentType<UpperLatinDef.ValidUpperLatin3, int, string, string>

module DigitsDef =
    type DigitsValidator = 
        inherit Cctor<int, string, string>
        new : config : int ->  DigitsValidator

    type ValidDigits = 
        inherit DigitsValidator
        new : unit -> ValidDigits
    type ValidDigits2 = 
        inherit DigitsValidator
        new : unit -> ValidDigits2
    type ValidDigits3 = 
        inherit DigitsValidator
        new : unit -> ValidDigits3
    type ValidDigits4 = 
        inherit DigitsValidator
        new : unit -> ValidDigits4
  
/// String of digit characters [0-9].
type Digits = DependentType<DigitsDef.ValidDigits, int, string, string>
/// String of digit characters [0-9] and length 2.
type Digits2 = DependentType<DigitsDef.ValidDigits2, int, string, string>
/// String of digit characters [0-9] and length 3.
type Digits3 = DependentType<DigitsDef.ValidDigits3, int, string, string>
/// String of digit characters [0-9] and length 4.
type Digits4 = DependentType<DigitsDef.ValidDigits4, int, string, string>

module IntRange =
    type NumRangeValidator =
        inherit Validator<(int * int),int>
        new : config:(int * int) -> NumRangeValidator
    type MinNumRangeValidator =
        inherit Validator<int,int>
        new : config:int -> MinNumRangeValidator
    type MaxNumRangeValidator =
        inherit Validator<int,int>
        new : config:int -> MaxNumRangeValidator

    type MaxPos100 =
        inherit NumRangeValidator
        new : unit -> MaxPos100
    type MaxPos20000 =
        inherit NumRangeValidator
        new : unit -> MaxPos20000
    type RangeMinus100To100 =
        inherit NumRangeValidator
        new : unit -> RangeMinus100To100
    type Min101 =
        inherit MinNumRangeValidator
        new : unit -> Min101
    type MaxMinus101 =
        inherit MaxNumRangeValidator
        new : unit -> MaxMinus101

/// Integer in range 0 to 100.
type PositiveInt100 = LimitedValue<IntRange.MaxPos100,(int * int),int>
/// Integer in range 0 to 20000.
type PositiveInt20000 = LimitedValue<IntRange.MaxPos20000,(int * int),int>
/// Integer in range -100 to 100.
type Minus100To100 = LimitedValue<IntRange.RangeMinus100To100,(int * int),int>
/// Integer greater than 100.
type GT100 = LimitedValue<IntRange.Min101,int,int>
/// Integer less than -100.
type LTminus100 = LimitedValue<IntRange.MaxMinus101,int,int>