namespace Terricolor

open System
open System.Diagnostics
open System.Threading.Tasks
open FSharpx.Collections
open Terricolor.Benchmarking
open Terricolor.Concurrency
open Terricolor.Primitives
open Terricolor.Propagation
open Terricolor.Search
open Terricolor.Heuristics
open Terricolor.Reader

module CycleSearch =

    let run (concurrency:int)
            (variableCount:int)
            (clauses:Clause list) =

        let watch = Stopwatch.StartNew()
        let random = new Random()
        
        // construct the initial logic program
        (new Assignment(variableCount), Queue.empty)
        |> Propagation.insertFold clauses
        |> Propagation.propagate
        |>
        function
        | Failure conflict -> Unsatisfiable
        | Success program ->

            let start (state:State) = 
                // define the timeout for this item of work
                let timeout = TimeSpan.FromMilliseconds(watch.Elapsed.TotalMilliseconds * random.NextDouble())
                // update benchmark statistics
                incrementCycles()
                // start this cycle
                Task.Run (fun () -> startSearch random state timeout)

            // define how to restart a state
            let restart (state:State) =
                // for now, simply retain the active set
                let retained = Set.toList state.Active
                Propagation.insertFold retained program
                |> Propagation.propagate
                |>
                function
                | Failure conflict -> Task.FromResult (Success Unsatisfiable)
                | Success propagation ->
                    incrementLearned retained.Length
                    let state = { state with Propagation = propagation;
                                                Active = Set.empty;
                                                Learned = retained }
                    start state

            // define how to initialize with a timeout
            let initialize i =
                let state =
                    { Propagation = program;
                      Active = Set.empty;
                      Learned = [];
                      Heuristic = makeHeuristic random variableCount; }
                start state

            // cycle through completed jobs reissuing as needed
            let rec cycleSearch (tasks : seq<Task<Result<Solution,Timeout>>>) = async {
                let completed, working = splitCompletedAndWorking tasks
                let successes, failures =
                    completed
                    |> List.choose (function Success x -> Some x | _ -> None),
                    completed
                    |> List.choose (function Failure x -> Some x | _ -> None)
                match successes with
                | x :: _ -> return x
                | [] ->
                    let tasks = 
                        failures
                        |> Seq.map (function {Timeout.State=state} -> restart state)
                        |> Seq.append working
                    do! Async.Sleep 1
                    return! cycleSearch tasks }

            // start the initial jobs and wait until they complete
            seq {1.0 .. float concurrency}
            |> Seq.map initialize
            |> cycleSearch
            |> Async.RunSynchronously