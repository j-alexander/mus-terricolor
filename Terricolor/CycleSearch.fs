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
                        random.NextDouble() * random.NextDouble() * watch.Elapsed.TotalMilliseconds
                        |> TimeSpan.FromMilliseconds

                    let startWith (clauses:Clause Set) (state:State) =
                        Async.Start <| async {
                            let propagation =
                                state.Propagation
                                |> Propagation.insertFold (Set.toList clauses)
                                |> Propagation.propagate
                            match propagation with
                            | Failure conflict ->
                                Success Unsatisfiable
                            | Success propagation ->
                                { state with Propagation = propagation }
                                |> startSearch random (timeout())
                            |> Message.Update
                            |> inbox.Post
                        }

                    let rec manage (report:AsyncReplyChannel<Solution>)
                                   (clauses:Set<Clause>) =
                        async {
                            let! message = inbox.Receive()
                            match message with
                            | Message.Update (Success solution) ->
                                report.Reply(solution)
                            | Message.Update (Failure {Timeout.State=state }) ->
                                let learned = state.Active - clauses
                                let clauses = Set.union clauses learned

                                let threshold = random.NextDouble()
                                if threshold < 0.25 then
                                    // continue
                                    startWith Set.empty state
                                elif threshold < 0.5 then
                                    // start knowing nothing
                                    { Propagation = program
                                      Active = Set.empty
                                      Learned = Set.empty
                                      Heuristic = makeHeuristic random variables }
                                    |> startWith Set.empty 
                                elif threshold < 0.7 then
                                    // restart knowing everything so far, fresh heuristics
                                    { Propagation = program
                                      Active = Set.empty
                                      Learned = Set.empty
                                      Heuristic = makeHeuristic random variables }
                                    |> startWith clauses
                                else
                                    // restart knowing everything so far, keeping heuristics
                                    { Propagation = program
                                      Active = Set.empty
                                      Learned = Set.empty
                                      Heuristic = state.Heuristic }
                                    |> startWith clauses

                                return! manage report clauses
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
                        { Propagation = program
                          Active = Set.empty
                          Learned = Set.empty
                          Heuristic = makeHeuristic random variables }
                        |> startWith Set.empty

                    return! manage report Set.empty
                })

            processor.PostAndReply(Message.Report)