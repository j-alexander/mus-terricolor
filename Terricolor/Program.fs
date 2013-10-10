namespace Terricolor

module Program = 

    open System
    open System.Diagnostics
    open System.Linq
    open System.IO
    open System.Text
    open System.Threading.Tasks
    open Terricolor.Benchmarking
    open Terricolor.Concurrency
    open Terricolor.Primitives
    open Terricolor.Propagation
    open Terricolor.Learning
    open Terricolor.Heuristics
    open Terricolor.Search
    open Terricolor.Reader

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
            let clauses = Set.ofSeq (Seq.map Set.ofSeq clauses)
            printfn "c            [%d duplicates]" (clauseCount - clauses.Count)
            
            // construct the initial logic program
            let watch = Stopwatch.StartNew()
            let program = Seq.fold foldClause (makeEmptyAssignment variableCount) clauses
            printfn "c Searching"

            try
                // increase the priority in the system scheduler
                Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.High;
                
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
                    let isActive clause = Set.contains clause state.Active
                    let learned = List.filter isActive state.Learned
                    let assignment = List.fold foldClause program learned
                    incrementLearned learned.Length
                    let state = { state with Assignment = assignment;
                                             Active = Set.empty;
                                             Learned = learned }
                    start state
                // define how to initialize with a timeout
                let initialize i =
                    let state =
                        { Assignment = program;
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
                printfn "p cnf %d %d" variableCount (clauses.Count + solution.Count)
                for clause in clauses do
                    for literal in clause do
                        printf "%d " literal
                    printfn "0"
                for pair in solution do
                    let literal, variable = pair.Key, pair.Value
                    match variable with
                    | Value(value) ->
                        if value.IsTrue then
                            printfn "%d 0" literal
                    | WatchList(_) -> failwith "An assignment should be complete."

            // a trivial conflict was encountered in the initial problem (i.e. {{1} {-1}})
            | Conflict (trail, reason, assignment) ->
                printfn "Unsatisfiable (Trivial Case)"

            // the problem has no solution
            | Unsatisfiable ->
                printfn "Unsatisfiable"

            watch.Stop()
            printfn "c (%d ms)" watch.ElapsedMilliseconds

        0
