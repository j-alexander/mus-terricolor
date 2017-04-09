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
    
    type Message =
        | Report of AsyncReplyChannel<Solution>
        | Update of Result<Solution,Timeout>

    let run (concurrency:int)
            (variables:int)
            (clauses:Clause list) =

        let watch = Stopwatch.StartNew()
        let random = new Random()
        
        // construct the initial logic program
        Propagation.init variables
        |> Propagation.insertFold clauses
        |> Propagation.propagate
        |>
        function
        | Failure conflict -> Unsatisfiable
        | Success program ->

            let processor = MailboxProcessor.Start(fun inbox ->
                async {

                    let timeout _ =
                        random.NextDouble() * watch.Elapsed.TotalMilliseconds
                        |> TimeSpan.FromMilliseconds

                    let start (state:State) =
                        Async.Start <| async {
                            return
                                startSearch random state (timeout())
                                |> Message.Update
                                |> inbox.Post
                        }

                    let rec manage (report:AsyncReplyChannel<Solution>)
                                   (propagation:Propagation)
                                   (clauses:Set<Clause>) =
                        async {
                            let! message = inbox.Receive()
                            match message with
                            | Message.Update (Success solution) ->
                                report.Reply(solution)
                            | Message.Update (Failure {Timeout.State=state }) ->
                                let learned = state.Active - clauses
                                let clauses = Set.union clauses learned
                                let propagation =
                                    propagation
                                    |> Propagation.insertFold (Set.toList learned)
                                    |> Propagation.propagate
                                match propagation with
                                | Failure conflict ->
                                    report.Reply(Unsatisfiable)
                                | Success propagation ->

                                    let threshold = random.NextDouble()
                                    if threshold < 0.25 then
                                        // start knowing nothing
                                        start { Propagation = program
                                                Active = Set.empty
                                                Learned = Set.empty
                                                Heuristic = makeHeuristic random variables }
                                    elif threshold < 0.5 then
                                        // continue
                                        start state
                                    else
                                        // restart knowing everything so far
                                        start { Propagation = propagation
                                                Active = Set.empty
                                                Learned = clauses
                                                Heuristic = makeHeuristic random variables }

                                    return! manage report propagation clauses
                            | _ -> ()
                        }
                    
                    let rec initialize() =
                        async {
                            let! message = inbox.Receive()
                            match message with
                            | Message.Report x -> return x
                            | _ -> return! initialize()
                        }

                    let! report = initialize()

                    for i in [ 1..concurrency] do
                        start { Propagation = program
                                Active = Set.empty
                                Learned = Set.empty
                                Heuristic = makeHeuristic random variables }

                    return! manage report program Set.empty
                })

            processor.PostAndReply(Message.Report)
//
//            let start (state:State) = 
//                // define the timeout for this item of work
//                // update benchmark statistics
//                incrementCycles()
//                // start this cycle
//                Task.Run (fun () -> startSearch random state timeout)
//
//            // define how to restart a state
//            let restart (retained) (state:State) =
//                Propagation.insertFold retained program
//                |> Propagation.propagate
//                |>
//                function
//                | Failure conflict -> Task.FromResult (Success Unsatisfiable)
//                | Success propagation ->
//                    incrementLearned retained.Length
//                    let state = { state with Propagation = propagation;
//                                             Active = [];
//                                             Learned = retained }
//                    start state
//
//            // define how to initialize with a timeout
//            let initialize i =
//                let state =
//                    { Propagation = program;
//                      Active = [];
//                      Learned = [];
//                      Heuristic = makeHeuristic random variables; }
//                start state
//
//            // cycle through completed jobs reissuing as needed
//            let rec cycleSearch (tasks : seq<Task<Result<Solution,Timeout>>>) = async {
//                let completed, working = splitCompletedAndWorking tasks
//                let successes, failures =
//                    completed
//                    |> List.choose (function Success x -> Some x | _ -> None),
//                    completed
//                    |> List.choose (function Failure x -> Some x | _ -> None)
//                match successes with
//                | x :: _ -> return x
//                | [] ->
//                    let tasks = 
//                        failures
//                        |> Seq.map (function {Timeout.State=state} -> restart state.Active state)
//                        |> Seq.append working
//                    do! Async.Sleep 1
//                    return! cycleSearch tasks }
//
//            // start the initial jobs and wait until they complete
//            seq {1.0 .. float concurrency}
//            |> Seq.map initialize
//            |> cycleSearch
//            |> Async.RunSynchronously