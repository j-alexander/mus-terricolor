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
            (clauses:#seq<Clause>) =

        let watch = Stopwatch.StartNew()
        let random = new Random()
        
        // construct the initial logic program
        let program = 
            clauses
            |> Seq.fold Propagation.insert (new Assignment(variableCount), Queue.empty)
            |> Propagation.propagate

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
            let propagation = List.fold Propagation.insert program retained |> Propagation.propagate
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
        let rec cycleSearch (tasks : seq<Task<State>>) = async {
            let completed, working = splitCompletedAndWorking tasks
            let tasks = 
                completed
                |> Seq.map restart
                |> Seq.append working
            do! Async.Sleep 1
            return! cycleSearch tasks }

        // start the initial jobs and wait until they complete
        seq {1.0 .. float concurrency}
        |> Seq.map initialize
        |> cycleSearch
        |> Async.RunSynchronously