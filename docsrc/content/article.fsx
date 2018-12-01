(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/DependentTypes/net45"
#r "DependentTypes.dll"
open DependentTypes.Helpers
(**
Benchmarking <sup>1</sup>
============

Let's benchmark... 

DependentType option
--------------------

*)
open DependentTypes

module PercentType =
    let validatePercent _ (n : float) = 
        match n >= 0. && n <= 1. with
        | true -> Some n
        | false -> None

    type PercentValidator() = 
        inherit PiType<unit, float, float option>((), validatePercent)

    type PairPercentValidator() = 
        inherit SigmaType<unit, float, float option>((), validatePercent)

type Percent = DependentType<PercentType.PercentValidator, unit, float, float option>
type PercentPair = DependentPair<PercentType.PairPercentValidator, unit, float, float option>

let runPctOption() =
    [|
        PercentType.validatePercent () 0.5
        PercentType.validatePercent () 2.5
    |] 

let runLiftedPctDependentType() =
    [|
        Percent.TryCreate 0.5
        Percent.TryCreate 2.5
    |] 
(**
- Create a percent DependentType option compared to creating a simple float option using the same validation logic. 
- Each benchmark instance creates 1 Some option and 1 None option in both cases. 
- Benchmark 1,000,000 instances, hence 2M option instances. 

````
validated option is faster than TryCreate DependentType option. 
f1 (21.9484 00B1 2.5478 ms) is ~96% faster than f2 (607.0490 00B1 15.8352 ms).
````

Not surprisingly validation and creation of a simple option is faster, 28X faster. 

And it scales nearly linearly, as we see when executing the benchmark 10X instead of 1M.

````
(10X) validated option is faster than TryCreate DependentType option. 
f1 (0.0006 00B1 0.0000 ms) is ~94% faster than f2 (0.0091 00B1 0.0002 ms).
````

Considering in our first benchmark Dependent type created 2M option instances in less than 700 ms, and creates 10 in a fraction of a millisecond, this is probably acceptable performance for all but the most
demanding network applications.

The validation logic adds little overhead. Even comparing creating DependentType to "naked" options (not run through the validation logic) makes little difference in the performance ratio.

````
naked option is faster than TryCreate DependentType option. 
f1 (18.3340 00B1 0.1073 ms) is ~97% faster than f2 (601.7315 00B1 4.2582 ms).
````

Can we squeeze even more performance from DependentType creation? Let's use ````Create```` instead of ````TryCreate```` so we eliminate the overhead of "lifting" the ````'T2```` base type element option result
to the DependentType element.

````
validated option is faster than Create DependentType. 
f1 (17.0064 00B1 0.3517 ms) is ~93% faster than f2 (256.9292 00B1 2.3419 ms).
````

Validate option is now only 15X faster, so the "lift" overhead of DependentType is noticeable at large scales (2M creations).

### Consuming (reading) DependentType. ###
*)
let readDependentType (xs : Percent option []) =
    xs
    |> Array.map ( fun x ->
        match x with
        | Some _ -> Some (someValue x)
        | None -> None )

let readVanillaOption (xs : float option[]) =
    xs
    |> Array.map ( fun x ->
        match x with
        | Some pct -> Some pct
        | None -> None )

(**
````
read Option is faster than Option DependentType. 
f1 (17.9776 00B1 0.2142 ms) is ~33% faster than f2 (27.0092 00B1 0.3793 ms).
````

- Reading 2M float options is only 33% faster than read/extract value operation on DependentTypes.

Substituting the verbose ````Some pct.Value.Value```` for the helper function ````someValue```` almost erases any advantage of float option.

````
read Option is faster than Option DependentType Value.Value. 
f1 (19.5531 00B1 0.1250 ms) is ~4% faster than f2 (20.2828 00B1 0.1295 ms).
````

UtcDateTime DependentType
-------------------------

Benchmarking a type that is not an ````option````, we compare UtcDateTime in the DomainLib to an implementation validating a DateTime. 

This time 1M benchmark runs is also 1M instances.

````
validated DateTime is faster than Create DependentType DateTime. 
f1 (278.7760 00B1 3.3103 ms) is ~28% faster than f2 (385.8573 00B1 0.6970 ms).
````

````
read DateTime is faster than DependentType DateTime. 
f1 (7.3626 00B1 0.0496 ms) is ~7% faster than f2 (7.9543 00B1 0.0946 ms).
````

In this case the create and read performance differences are barely meainingful.


DependentPair
-------------

Banchmarks comparing a validated pair to DependentPair yields similar performance ratios to option DependentType.

1M runs again creates 2M instances.

````
pair is faster than Create DependentPair. 
f1 (30.2823 00B1 0.0807 ms) is ~86% faster than f2 (223.3536 00B1 0.9640 ms).
````

````
read pair is faster than DependentPair. 
f1 (28.5499 00B1 0.0862 ms) is ~2% faster than f2 (29.0964 00B1 0.1033 ms).
````

Creation of a simple validated pair is 7X faster than creating a DependentPair, but read/consume performance is so similar we sometimes see the benchmark test failing because DependentPair performs faster.
*)


(**
<sup>1</sup> FThe usual caveats about benchmarking apply. You should benchmark
your own situation on your own system, etc. There is variance in running the benchmarks multiple times. The variance I saw was typically
in absolute run time for each scenario, and not so much in the DependentType / control run time ratios. By and large these results are representative of typical benchmark runs on my system, 
FSharp.Core 4.5.3, net45 DependentTypes.dll
*)