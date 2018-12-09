module Experimental

open DependentTypes
open DependentTypes.Helpers
open System
open System.Collections.Generic

open System.Runtime.CompilerServices

//@@@@@@@@@@@@@@@@@@
[<Extension>]
type ExtraCSharpStyleExtensionMethodsInFSharp () =
    [<Extension>]
    static member inline Sum(xs: IEnumerable<'T>) = Seq.sum 
//@@@@@@@@@@@@@@@@@@

let validate normalize fn v =
    if fn (normalize v) then Some (normalize v) else None

let validateLen len s = 
    validate id (fun (s:string) -> s.Length <= len) s

type LenValidator(config) = 
    inherit Pi<int, string, string option>(config, validateLen)

type Size5 () = inherit LenValidator(5) 

type String5 = DependentType<Size5, int, string, string option>

[<Extension>]
type String5Length () =
    [<Extension>]
    static member inline Length(x: String5) = x.Value.Value.Length

let okString = String5.Create "short" // Some
let z = okString.Value
printfn "okString is: %s" z.Value

printfn "z length %i" <| z.Value.Length

