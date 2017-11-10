(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/DependentTypes"
#r "DependentTypes.dll"
(**
Introducing your project
========================

Say more

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

    type NonEmptyValidator(config) = 
        inherit Cctor<unit, string, string>(config, verifyTrimNonEmptyString)
        new() = NonEmptyValidator(())

    type NonEmpty () = inherit NonEmptyValidator()

type TrimNonEmptyString = DependentType<TrimNonEmptyStringDef.NonEmpty, unit, string, string> 
(**
Some more info
*)
let myGoodString = (TrimNonEmptyString.TryParse "good string   ").Value

let notTrimNonEmptyString = TrimNonEmptyString.TryParse "    "

// DependentType "good string"
printfn "%A" myGoodString


// true
printfn "%b" notTrimNonEmptyString.IsNone
(**
Some more info
*)
(**
Some more info
*)
