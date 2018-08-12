module Experimental2


#if INTERACTIVE
//    #r "file.dll";;        Reference (dynamically load) the given DLL
//    #I "<folder-list>;;    Add the given search folder(s) for referenced DLLs
//    #load "file.fs" ...;;  Load the given file(s) as if compiled and referenced
//    #time ["on"|"off""];;  Toggle timing on/off
//    #help;;                Display help
//    #quit;;                Exit
//    fsi path by default C:\Users\Jack\AppData\Local\Temp\
#I __SOURCE_DIRECTORY__, "C:\Packages", E:\Dropbox\VisualStudio\Scripts\packages"
//#load @"C:\FsEye\FsEye.fsx"
//#I "C:\Packages" 
//#I "E:\Dropbox\VisualStudio\Scripts\packages" 
#r @"FSharpx.Core.1.8.41\lib\40\FSharpx.Core.dll"
#r "System.Xml.Linq.dll"
#r @"FSharp.Data.1.1.10\lib\net40\FSharp.Data.dll"
#endif

open System
open System.Reflection
//open FSharpx
//open FSharpx.Collections
open FSharp.Data
    
open DependentTypes
open System

module TrimNonEmptyStringDef =
    let verifyTrimNonEmptyString _ (value : string) =
        if String.IsNullOrWhiteSpace value then
            None        
        else 
            Some <| value.Trim()

    type NonEmptyValidator() = 
        inherit PiType<unit, string, string option>((), verifyTrimNonEmptyString)

    type NonEmpty () = inherit NonEmptyValidator()

type TrimNonEmptyString = DependentType<TrimNonEmptyStringDef.NonEmpty, unit, string, string option> 

let t = TrimNonEmptyString.Create "asdf"
let t2 = TrimNonEmptyString.TryCreate "  "

let t2Value = t2.Value.Value.Value
