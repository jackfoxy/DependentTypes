namespace DomainLib

open DependentTypes
open System 
open System.Collections.Generic

module TrimNonEmptyStringDef =
    type NonEmptyValidator =
        inherit Pi<unit, string, string option>
        new : config : unit -> NonEmptyValidator

/// Trimmed, non-empty, non-null string
type TrimNonEmptyString = DependentType<TrimNonEmptyStringDef.NonEmptyValidator, unit, string, string option>

module UtcDateTimeDef =
    type UtcDateTimeValidator =
        inherit Pi<unit, DateTime, DateTime>
        new : config : unit -> UtcDateTimeValidator

/// DateTime set to UTC.
type UtcDateTime = DependentType<UtcDateTimeDef.UtcDateTimeValidator, unit, DateTime, DateTime> 

module NonEmptySetDef =
    type NonEmptySetValidator<'T when 'T : comparison> =
        inherit Pi<unit, Set<'T>, Set<'T> option>
        new : config : unit -> NonEmptySetValidator<'T>

/// Generic non-empty Set<'T>
type NonEmptySet<'T when 'T : comparison> = DependentType<NonEmptySetDef.NonEmptySetValidator<'T>, unit, Set<'T>, Set<'T> option>

module UpperLatinDef =
    type UpperLatinValidator =
        inherit Pi<int, string, string option>
        new : config : int -> UpperLatinValidator

    type ValidUpperLatin2 =
        inherit UpperLatinValidator
        new : unit -> ValidUpperLatin2

    type ValidUpperLatin3 =
        inherit UpperLatinValidator
        new : unit -> ValidUpperLatin3

/// Uppercase latin string of length 2.
type UpperLatin2 = DependentType<UpperLatinDef.ValidUpperLatin2, int, string, string option>
/// Uppercase latin string of length 3.
type UpperLatin3 = DependentType<UpperLatinDef.ValidUpperLatin3, int, string, string option>

module DigitsDef =
    type DigitsValidator = 
        inherit Pi<int, string, string option>
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
type Digits = DependentType<DigitsDef.ValidDigits, int, string, string option>
/// String of digit characters [0-9] and length 2.
type Digits2 = DependentType<DigitsDef.ValidDigits2, int, string, string option>
/// String of digit characters [0-9] and length 3.
type Digits3 = DependentType<DigitsDef.ValidDigits3, int, string, string option>
/// String of digit characters [0-9] and length 4.
type Digits4 = DependentType<DigitsDef.ValidDigits4, int, string, string option>

module IntRange =
    type NumRangeValidator =
        inherit Pi<(int * int), int, int option>
        new : config:(int * int) -> NumRangeValidator
    type MinNumRangeValidator =
        inherit Pi<int, int, int option>
        new : config:int -> MinNumRangeValidator
    type MaxNumRangeValidator =
        inherit Pi<int, int, int option>
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
type PositiveInt100 = DependentType<IntRange.MaxPos100,(int * int), int, int option>
/// Integer in range 0 to 20000.
type PositiveInt20000 = DependentType<IntRange.MaxPos20000,(int * int), int, int option>
/// Integer in range -100 to 100.
type Minus100To100 = DependentType<IntRange.RangeMinus100To100,(int * int), int, int option>
/// Integer greater than 100.
type GT100 = DependentType<IntRange.Min101, int, int, int option>
/// Integer less than -100.
type LTminus100 = DependentType<IntRange.MaxMinus101, int, int, int option>