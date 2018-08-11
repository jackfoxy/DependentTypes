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
            
open DependentTypes
open TypeShape
    
[<Class>]
/// Constructor / validator type for DependentType 'T -> 'T2 
type PiType<'Config, 'T, 'T2> (config: 'Config, pi: 'Config -> 'T -> _) =
    member __.Create x  = pi config x
    member __.TryCreate x : _ Option = 
        let wrap (f : 'a -> 'T2)  = unbox<'T -> _ Option> f

        match shapeof<'T2> with
        | Shape.FSharpOption _ ->
            wrap (pi config) x 
        | _ ->
            pi config x |> Some

[<Class>]
/// Constructor / validator type for DependentPair 'T -> 'T * 'T2
type SigmaType<'Config, 'T, 'T2> (config: 'Config, vfn: 'Config -> 'T -> 'T2) =
    member __.Create(x:'T) : 'T * 'T2 = x, (vfn config x)

/// 'T1 -> 'T2 dependent type
type DependentType<'PiType, 'Config, 'T, 'T2 when 'PiType :> PiType<'Config, 'T, 'T2>  
                                              and  'PiType : (new: unit -> 'PiType)> =
    DependentType of 'T2 
    
    with 
        member __.Value = 
            let (DependentType s) = __
            s

        override __.ToString() = __.Value.ToString()     
        
        static member Extract (x : DependentType<'PiType, 'Config, 'T, 'T2> ) = 
            let (DependentType s) = x
            s   
        static member Create(x:'T) : DependentType<'PiType, 'Config, 'T, 'T2> =
            (new 'PiType()).Create x
            |> DependentType

        static member TryCreate(x:'T) : DependentType<'PiType, 'Config, 'T, 'T2> Option =
            let pi = (new 'PiType()).TryCreate x

            match pi with
            | Some x -> DependentType x |> Some
            | None -> None
            
        static member Create (xs : 'T seq) : DependentType<'PiType, 'Config, 'T, 'T2> seq =
            xs
            |> Seq.map DependentType.Create 

        static member inline ConvertTo(x : DependentType<'x, 'y, 'q, 'r> ) : DependentType<'a, 'b, 'r, 's> = 
            let (DependentType v) = x
            mkDependentType v   

/// 'T -> 'T * 'T2 dependent pair
type DependentPair<'SigmaType, 'Config, 'T, 'T2 when 'SigmaType :> SigmaType<'Config, 'T, 'T2>  
                                                 and  'SigmaType : (new: unit -> 'SigmaType)> =
     DependentPair of 'T * 'T2
     with 
        member __.Value = 
            let (DependentPair (s, s2)) = __
            s, s2

        static member Create(x:'T) : DependentPair<'PiType, 'Config, 'T, 'T2> =
            (new 'PiType()).Create x
            |> DependentPair
