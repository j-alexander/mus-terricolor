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
                
            printfn "c Searching"
            match CycleSearch.run concurrency variableCount clauses with

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

            // the problem has no solution
            | Unsatisfiable ->
                printfn "Unsatisfiable"

            watch.Stop()
            printfn "c (%d ms)" watch.ElapsedMilliseconds

        0
