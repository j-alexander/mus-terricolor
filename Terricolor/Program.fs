namespace Terricolor

module Program = 

    open System
    open System.ComponentModel
    open System.Diagnostics
    open System.Linq
    open System.IO
    open System.Text
    open System.Threading.Tasks
    open Terricolor.Benchmarking
    open Terricolor.Concurrency
    open Terricolor.Primitives
    open Terricolor.Propagation
    open Terricolor.Search
    open Terricolor.Heuristics
    open Terricolor.Reader
    open FSharpx.Collections

    [<EntryPoint>]
    let main args = 

        Console.OutputEncoding <- Encoding.ASCII
        Console.Out.NewLine <- "\n"

        printfn "c"
        printfn "c Mus Terricolor Satisfiability Solver / (c) 2013 Jonathan A. Leaver"

        if args.Length <= 0 then
            // print help and exit
            printfn "c "
            printfn "c e.g. Terricolor [problem.cnf] [-Parallel]"
            printfn "c "
        else
            // define how to check for command-line options
            let isOption (option : string) =
                let equalIgnoringCase (other : string) =
                    String.Compare(other, option, true) = 0
                args.Where(equalIgnoringCase).Count() > 0

            // read the input DIMACS CNF file
            let filename = args.First()
            printfn "c "
            printfn "c Reading    [%s]" filename
            let contents = File.ReadAllText(filename)
            let variableCount, clauseCount, clauses, time = Reader.read(contents)
            printfn "c            [%d variables, %d clauses (%d ms)]" variableCount clauseCount time
            let watch = Stopwatch.StartNew()

            try
                // construct the initial logic program
                printfn "c Loading program..."
                let program =
                    clauses
                    |> Seq.fold Propagation.insert (new Assignment(variableCount), Queue.empty)
                    |> Propagation.propagate
                printfn "c Searching"

                // increase the priority in the system scheduler
                try
                    Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.High;
                with
                | :? Win32Exception -> printfn "c Unable to boost PriorityClass"
                
                // use the appropriate number of concurrent workers
                let concurrency =
                    if isOption "-Parallel" then Environment.ProcessorCount
                    else 1

                // start the benchmarking
                if isOption "-Benchmark" then startTimer()

                let start state = 
                    // define the timeout for this item of work
                    let timeout = TimeSpan.FromMilliseconds(watch.Elapsed.TotalMilliseconds * random.NextDouble())
                    // update benchmark statistics
                    incrementCycles()
                    // start this cycle
                    Task.Run (fun () -> startSearch state timeout)
                // define how to restart a state
                let restart state =
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
                          Heuristic = makeHeuristic variableCount; }
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

            with
            // a solution has been found, and should be printed
            | Satisfiable(solution) ->
                let decisions =
                    let filter = function (literal, None) -> Some(literal) | _ -> None
                    solution.Trail |> List.choose filter

                printfn "p cnf %d %d" variableCount (clauses.Count() + decisions.Length)
                for clause in clauses do
                    for literal in clause do
                        printf "%d " literal
                    printfn "0"
                for decision in decisions do
                    printfn "%d 0" decision

            // a trivial conflict was encountered in the initial problem (i.e. {{1} {-1}})
            | Conflict (reason, trail) ->
                printfn "Unsatisfiable (Trivial Case)"

            // the problem has no solution
            | Unsatisfiable ->
                printfn "Unsatisfiable"

            watch.Stop()
            printfn "c (%d ms)" watch.ElapsedMilliseconds

        0
