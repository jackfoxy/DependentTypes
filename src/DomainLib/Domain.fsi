namespace Jackfoxy.DomainLib

open robkuz.DependentTypes
open System 
open System.Collections.Generic

module TrimNonEmptyStringDef =
    type NonEmptyValidator =
        inherit Cctor<unit, string, string>
        new : config : unit -> NonEmptyValidator

    type NonEmpty =
        inherit NonEmptyValidator
        new : unit -> NonEmpty

type TrimNonEmptyString = DependentType<TrimNonEmptyStringDef.NonEmpty, unit, string, string>

module UtcDateTimeDef =
    type UtcDateTimeValidator =
        inherit Cctor<unit, DateTime, DateTime>
        new : config : unit -> UtcDateTimeValidator

    type ValidUtcDateTime =
        inherit UtcDateTimeValidator
        new : unit -> ValidUtcDateTime

type UtcDateTime = DependentType<UtcDateTimeDef.ValidUtcDateTime, unit, DateTime, DateTime> 

module NonEmptySetDef =
    type NonEmptySetValidator =
      class
        inherit Validator<unit,Set<int>>
        new : unit -> NonEmptySetValidator
        new : config:unit -> NonEmptySetValidator
      end

    type ValidNonEmptySet =
      class
        inherit NonEmptySetValidator
        new : unit -> ValidNonEmptySet
      end

/// note using LimitedValue<'Validator, 'Config, 'T> type, not DependentType<'Cctor, 'Config, 'T, 'T2>
type NonEmptyIntSet = LimitedValue<NonEmptySetDef.ValidNonEmptySet, unit, Set<int>>

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

type UpperLatin2 = DependentType<UpperLatinDef.ValidUpperLatin2, int, string, string>
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
    
type Digits = DependentType<DigitsDef.ValidDigits, int, string, string>
type Digits2 = DependentType<DigitsDef.ValidDigits2, int, string, string>
type Digits3 = DependentType<DigitsDef.ValidDigits3, int, string, string>
type Digits4 = DependentType<DigitsDef.ValidDigits4, int, string, string>

