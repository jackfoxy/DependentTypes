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

type PctOption = { Pct : float option }

type DependentTypePctOption = { Pct : Percent option }

let runPctOption() =
    [|
        {PctOption.Pct = PercentType.validatePercent () 0.5}
        {PctOption.Pct = PercentType.validatePercent () 2.5}
    |] 

let runLiftedPctDependentType() =
    [|
        {DependentTypePctOption.Pct = Percent.TryCreate 0.5}
        {DependentTypePctOption.Pct = Percent.TryCreate 2.5}
    |] 
(**
We benchmark creating a percent DependentType option against creating a simple float option using the same validation logic. Each benchmark instance creates 1 Some option and 1 None option in both cases. 
We benchmark 1,000,000 instances, hence 2M option instances. 

````
validated option is faster than TryCreate DependentType option. 
f1 (36.2342 00B1 2.7402 ms) is ~95% faster than f2 (689.8306 00B1 30.1484 ms)
````

Not surprisingly validation and creation of a simple option is faster, 19X faster. 

And it scales linearly, as we see when executing the benchmark 10X.

````
(10X) validated option is faster than TryCreate DependentType option. 
f1 (0.0007 00B1 0.0000 ms) is ~95% faster than f2 (0.0128 00B1 0.0015 ms)
````

Considering in our first benchmark Dependent type created 2M option instances in less than 700 ms, and creates 10 in a fraction of a millisecond, this is probably acceptable performance for all but the most
demanding network applications.

The validation logic adds little overhead. Even comparing creating DependentType to "naked" options (not run through the validation logic) makes little difference in the performance ratio.

````
naked option is faster than TryCreate DependentType option. 
f1 (35.9181 00B1 1.4791 ms) is ~95% faster than f2 (697.0493 00B1 36.7884 ms)
````

Can we squeeze even more performance from DependentType creation? Let's use ````Create```` instead of ````TryCreate```` so we eliminate the overhead of "lifting" the ````'T2```` base type element option result
to the DependentType element.

````
validated option is faster than Create DependentType. 
f1 (36.6164 00B1 3.0847 ms) is ~87% faster than f2 (283.1149 00B1 7.6188 ms)
````

Now validate option is less than 8X faster, so the "lift" overhead is noticeable at large scales (2M creations).

Consuming (reading) benchmarks reveals far less performance difference.
*)
let readDependentType (xs : DependentTypePctOption[]) =
        xs
        |> Array.map ( fun x ->
        match x.Pct with
        | Some _ -> Some (someValue x.Pct)
        | None -> None )

let readVanillaOption (xs : PctOption[]) =
        xs
        |> Array.map ( fun x ->
        match x.Pct with
        | Some pct -> Some pct
        | None -> None )

(**
````
read Option is faster than Option DependentType. 
f1 (32.1449 00B1 1.4134 ms) is ~35% faster than f2 (49.8144 00B1 0.7834 ms)
````

Very little practical difference, and even less if we substitute the verbose ````Some pct.Value.Value```` for the helper function ````someValue````.

````
read Option is faster than Option DependentType Value.Value. 
f1 (24.0376 00B1 0.4967 ms) is ~12% faster than f2 (27.3507 00B1 0.6477 ms)
````

UtcDateTime DependentType
-------------------------

Finally, benchmarking a type that is not an ````option````, we compare UtcDateTime in the DomainLib to an implementation validating a DateTime. This time 1M benchmark runs is also 1M instances.

````
validated DateTime is faster than Create DependentType DateTime. 
f1 (298.8211 00B1 2.7119 ms) is ~31% faster than f2 (431.3628 00B1 8.9104 ms)
````

````
read DateTime is faster than DependentType DateTime. 
f1 (312.0462 00B1 3.9815 ms) is ~28% faster than f2 (433.5667 00B1 10.5693 ms)
````
*)

(**
<sup>1</sup> For those following along from home, you must manually run the benchmark tests. They do not run as part of the build process. The usual caveats about benchmarking apply. You should benchmark
your own situation on your own system, etc. There is variance in running the benchmarks multiple times. The variance I saw was typically
in absolute run time for each scenario, and not so much in the DependentType / control run time ratios. By and large these results are representative of typical benchmark runs. 


*)