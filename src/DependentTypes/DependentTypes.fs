namespace DependentTypes
    
/// Inline helper functions for dependent types.
module Helpers =
    /// Create instance of dependent type.
    let inline mkDependentType (x: ^S) : ^T = 
        (^T: (static member Create: ^S -> ^T) x)

    ///// Create instance of dependent type.
    //let inline mkSomeDependentType (x: ^S) : ^T option = 
    //    (^T: (static member Create: ^S -> ^T option) x)

    /// Retrieves 'T2 (base type) element value.
    let inline extract (x:^S) = 
        (^S: (static member Extract: ^S -> ^T) x)

    /// Create instance of compatible dependent type from 'T2 (base type) element value.
    let inline convertTo (x: ^S) : ^T = 
        (^T: (static member ConvertTo: ^S -> ^T) x)

    /// <summary>
    /// DEPRECATED, use SomeDependentType. Retrieves the 'T2 (base type) element value from a DependentType option.
    /// </summary>
    /// <exception cref="System.NullReferenceException">Thrown when None.</exception>
    let inline someValue (x : ^S option) =
        (x |> Option.map (fun x' ->
            (^S: (static member Extract: ^S -> ^T option ) x') )
            |> Option.flatten).Value

    /// <summary>
    /// DEPRECATED, use SomeDependentType. Retrieves the 'T2 (base type) element value from a DependentType when 'T2 is option.
    /// </summary>
    /// <exception cref="System.NullReferenceException">Thrown when None.</exception>
    let inline forceValue (x : ^S) =
        (^S  : (member Value: ^T option) x).Value

    /// DEPRECATED, use SomeDependentType. Queries the 'T2 (base type) element for IsSome from a DependentType when 'T2 is option.
    let inline isSome (x : ^S) =
        (^S  : (member Value: ^T option) x).IsSome

    /// Flatten a type with Value which in turn has Value
    let inline flatten (x : ^S) =
        (^T'  : (member Value: ^U) (^S  : (member Value: ^T') x))

open Helpers

[<Class>]
/// Construction / validation type for DependentType 
type Pi<'Config, 'T, 'T2> (config: 'Config, pi: 'Config -> 'T -> 'T2) =
    member __.Create x  = pi config x


[<Class>]
/// Construction / validation type for DependentPair 'T -> 'T * 'T2
type Sigma<'Config, 'T, 'T2> (config: 'Config, pi: 'Config -> 'T -> 'T2) =
    member __.Create(x:'T) : 'T * 'T2 = x, (pi config x)

/// 'T -> 'T2 dependent type
type DependentType<'Pi, 'Config, 'T, 'T2 when 'Pi :> Pi<'Config, 'T, 'T2>  
                                         and  'Pi : (new: unit -> 'Pi)> =
    DependentType of 'T2
    with 
        /// 'T2 (base type) element value.
        member __.Value = 
            let (DependentType t2) = __
            t2

        /// 'T2 (base type) ToString(). 
        override __.ToString() = __.Value.ToString()     
        
        /// Retrieve 'T2 (base type) element value.
        static member Extract  (x : DependentType<'Pi, 'Config, 'T, 'T2> ) = 
            let (DependentType t2) = x
            t2

        /// Create instance of dependent type.
        static member Create x : DependentType<'Pi, 'Config, 'T, 'T2> =
            (new 'Pi()).Create x
            |> DependentType

        /// Create instance of DependentType option.
        /// If the 'T2 (base type) is option, lifts Some/None of base type element to DependentType option.
        static member TryCreate x : DependentType<'Pi, 'Config, 'T, 'T2> option =
            let piResult = (new 'Pi()).Create x

            if typedefof<'T2> = typedefof<_ option> then
                if isNull (piResult :> obj) then
                    None
                else
                    Some (DependentType piResult)
            else
                Some (DependentType piResult)

        /// Create instance of DependentType option.
        /// If the 'T2 (base type) is option, lifts Some/None of base type element to DependentType option.
        static member TryCreate (x : 'T Option) : DependentType<'Pi, 'Config, 'T, 'T2> option =
            match x with
            | Some t -> 
                DependentType.TryCreate t

            | None -> 
                None

        /// Create compatible dependent type from 'T2 (base type) element value.
        static member inline ConvertTo(x : DependentType<'x, 'y, 'q, 'r> ) : DependentType<'a, 'b, 'r, 's> = 
            let (DependentType t2) = x
            mkDependentType t2  

/// 'T -> 'T2 option dependent type
type SomeDependentType<'Pi, 'Config, 'T, 'T2 when 'Pi :> Pi<'Config, 'T, 'T2 option>  
                                             and  'Pi : (new: unit -> 'Pi)> =
    SomeDependentType of 'T2
    with 
        /// 'T2 (base type) element value.
        member __.Value = 
            let (SomeDependentType t2) = __
            t2

        /// 'T2 (base type) ToString(). 
        override __.ToString() = __.Value.ToString()     
        
        ///// Retrieve 'T2 (base type) element value.
        static member Extract  (x : SomeDependentType<'Pi, 'Config, 'T, 'T2> ) = 
            let (SomeDependentType t2) = x
            t2

        /// <summary>
        /// Create instance of dependent type.
        /// </summary>
        /// <exception cref="System.NullReferenceException">Thrown when None.</exception>
        static member Create x : SomeDependentType<'Pi, 'Config, 'T, 'T2> =
            ((new 'Pi()).Create x).Value
            |> SomeDependentType

        /// Create instance of SomeDependentType option.
        /// If the 'T2 (base type) is option, lifts Some/None of base type element to DependentType option.
        static member TryCreate x : SomeDependentType<'Pi, 'Config, 'T, 'T2> option =
            match (new 'Pi()).Create x with
            | Some value ->
                Some (SomeDependentType value)
            | None ->
                 None

        ///// Create compatible dependent type from 'T2 (base type) element value.
        static member inline ConvertTo(x : SomeDependentType<'x, 'y, 'q, 'r> ) : SomeDependentType<'a, 'b, 'r, 's> = 
            let (SomeDependentType t2) = x
            mkDependentType t2  

/// 'T -> 'T * 'T2 dependent pair
type DependentPair<'Sigma, 'Config, 'T, 'T2 when 'Sigma :> Sigma<'Config, 'T, 'T2>  
                                            and 'Sigma : (new: unit -> 'Sigma)> =
     DependentPair of 'T * 'T2
     with 
        /// Pair of 'T1 element and 'T2 (base type) element value.
        member __.Value = 
            let (DependentPair (s, s2)) = __
            s, s2

        /// Create instance of dependent pair type.
        static member Create(x:'T) : DependentPair<'Sigma, 'Config, 'T, 'T2> =
            (new 'Sigma()).Create x
            |> DependentPair
