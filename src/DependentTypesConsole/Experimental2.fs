﻿module Experimental2

open System 
open DependentTypes

module TrimNonEmptyStringDef =
    let verifyTrimNonEmptyString _ (value : string) =
        if String.IsNullOrWhiteSpace value then
            None        
        else 
            Some <| value.Trim()

    type NonEmptyValidator() = 
        inherit Pi<unit, string, string option>((), verifyTrimNonEmptyString)

    type NonEmpty () = inherit NonEmptyValidator()

type TrimNonEmptyString = DependentType<TrimNonEmptyStringDef.NonEmpty, unit, string, string option> 

let t = TrimNonEmptyString.Create "asdf"
let t2 = TrimNonEmptyString.TryCreate " "

let t3 = TrimNonEmptyString.TryCreate (Some "sdfg  ")

let t2Value = Helpers.someValue t2

let t3Value = Helpers.someValue t3
