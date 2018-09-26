namespace DependentTypes

#if INTERACTIVE
#load @"..\..\paket-files\eiriktsarpalis\TypeShape\src\TypeShape\TypeShape.fs"
#endif

/// Inline helper functions.
module DependentTypes =
    /// Create dependent type
    let inline mkDependentType (x: ^S) : ^T = 
        (^T: (static member Create: ^S -> ^T) x)

    /// Retrieves base type value
    let inline extract (x:^S) = 
        (^S: (static member Extract: ^S -> ^T) x)

    /// Conversion of base type value to compatible dependent type
    let inline convertTo (x: ^S) : ^T = 
        (^T: (static member ConvertTo: ^S -> ^T) x)

    /// Retrieves the 'T2 value from an option DependentType
    let inline someValue (x : ^S Option) =
        (x |> Option.map (fun x' ->
            (^S: (static member Extract: ^S -> ^T option ) x') )
            |> Option.flatten).Value
            
open DependentTypes
open TypeShape

[<Class>]
/// Constructor / validator type for DependentType 'T -> 'T2 
type PiType<'Config, 'T, 'T2> (config: 'Config, pi: 'Config -> 'T -> 'T2) =
    member __.Create x  = pi config x


[<Class>]
/// Constructor / validator type for DependentPair 'T -> 'T * 'T2
type SigmaType<'Config, 'T, 'T2> (config: 'Config, pi: 'Config -> 'T -> 'T2) =
    member __.Create(x:'T) : 'T * 'T2 = x, (pi config x)

/// 'T -> 'T2 dependent type
type DependentType<'PiType, 'Config, 'T, 'T2 when 'PiType :> PiType<'Config, 'T, 'T2>  
                                              and  'PiType : (new: unit -> 'PiType)> =
    DependentType of 'T2
    with 
        member __.Value = 
            let (DependentType t2) = __
            t2

        override __.ToString() = __.Value.ToString()     
        
        static member Extract  (x : DependentType<'PiType, 'Config, 'T, 'T2> ) = 
            let (DependentType t2) = x
            t2

        static member Create x : DependentType<'PiType, 'Config, 'T, 'T2> =
            (new 'PiType()).Create x
            |> DependentType

        static member TryCreate x : DependentType<'PiType, 'Config, 'T, 'T2> Option =
            let piResult = (new 'PiType()).Create x

            match shapeof<'T2> with
            | Shape.FSharpOption _ ->
                if isNull (piResult :> obj) then
                    None
                else
                    Some (DependentType piResult)
            | _ -> 
                Some (DependentType piResult)

        static member TryCreate (x : 'T Option) : DependentType<'PiType, 'Config, 'T, 'T2> Option =
            match x with
            | Some t -> 
                DependentType.TryCreate t

            | None -> 
                None

        static member inline ConvertTo(x : DependentType<'x, 'y, 'q, 'r> ) : DependentType<'a, 'b, 'r, 's> = 
            let (DependentType t2) = x
            mkDependentType t2  

/// 'T -> 'T * 'T2 dependent pair
type DependentPair<'SigmaType, 'Config, 'T, 'T2 when 'SigmaType :> SigmaType<'Config, 'T, 'T2>  
                                                 and 'SigmaType : (new: unit -> 'SigmaType)> =
     DependentPair of 'T * 'T2
     with 
        member __.Value = 
            let (DependentPair (s, s2)) = __
            s, s2

        static member Create(x:'T) : DependentPair<'SigmaType, 'Config, 'T, 'T2> =
            (new 'SigmaType()).Create x
            |> DependentPair
