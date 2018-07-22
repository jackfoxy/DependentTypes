namespace DependentTypes

//open DependentTypes
//open System

[<Class>]
/// Constructor / validator type for DependentType 'T1 -> 'T2 style dependent types
type PiType<'Config, 'T, 'T2> (config: 'Config, vfn: 'Config -> 'T -> 'T2) =
    member __.Create(x:'T) : 'T2 = vfn config x

///'T1 -> 'T2 style dependent type
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
        //static member TryCreate(x:'T option) : Option<DependentType<'PiType, 'Config, 'T, 'T2>> =
        //    match x with
        //    | Some x' -> DependentType.TryCreate x'
        //    | None -> None
        //static member Create (x : 'T) : DependentType<'PiType, 'Config, 'T, 'T2> =
        //        (DependentType.TryCreate x).Value
        static member Create (xs : 'T seq) : DependentType<'PiType, 'Config, 'T, 'T2> seq =
            xs
            |> Seq.map DependentType.Create 
        //static member Create (xs : 'T list) : DependentType<'PiType, 'Config, 'T, 'T2> list =
        //    xs
        //    |> List.choose DependentType.TryCreate 
        //static member inline ConvertTo(x : DependentType<'x, 'y, 'q, 'r> ) : Option<DependentType<'a, 'b, 'r, 's>> = 
        //    let (DependentType v) = x
        //    mkDependentType v   


/// Inline helper functions.
module DependentTypes =
    /// Try create dependent type
    let inline mkDependentType (x: ^S) : ^T = 
        (^T: (static member Create: ^S -> ^T) x)

    /// Retrieves base type value
    let inline extract (x:^S) = 
        (^S: (static member Extract: ^S -> ^T) x)



//        /// Try conversion of base type value to compatible dependent type
//    //let inline convertTo (x: ^S) : Option< ^T> = 
//    //    (^T: (static member ConvertTo: ^S -> Option< ^T>) x)

    let inline convertTo (x:  DependentType<'PiType, 'Config, 'T, 'T2> ) : DependentType<'PiType2, 'Config2, 'T2, 'T3>  = 
        (^T : (static member ConvertTo: DependentType<'PiType, 'Config, 'T, 'T2> -> DependentType<'PiType2, 'Config2, 'T2, 'T3> ) x)