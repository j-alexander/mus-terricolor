namespace Terricolor

module Benchmarking =

    open System
    open System.Diagnostics
    open System.Timers

    let mutable benchmarking = false
    let mutable stop = false
    let mutable stopwatchTimeout = TimeSpan.FromMinutes(2.0)
    
    let mutable conflicts = 0
    let mutable cycles = 0
    let mutable learned = 0

    let incrementConflicts () =
        Threading.Interlocked.Increment(&conflicts) |> ignore

    let incrementCycles () =
        Threading.Interlocked.Increment(&cycles) |> ignore

    let rec incrementLearned count =
        let current = learned
        let parts = float Environment.ProcessorCount
        let increase = (float count) * (1.0 / parts)
        let retained = (float current) * ((parts - 1.0) / parts)
        let updated = int (increase + retained)
        let result = Threading.Interlocked.CompareExchange (&learned, updated, current)
        if result <> current then
            incrementLearned count

    let printMemory () =
        let current = Process.GetCurrentProcess()
        let counter = new PerformanceCounter()
        counter.CategoryName <- "Process"
        counter.CounterName <- "Working Set - Private"
        counter.InstanceName <- current.ProcessName
        let mem = int (counter.NextValue())
        if benchmarking then
            printf ",%d" mem
            
    let makeTimerEvent () =
        let makePrint (seconds) (name, value) =
            sprintf "%d %s (%.2f/s)" value name (float value / seconds)
        let makeBenchmarkPrint (seconds) (name, value) =
            sprintf ",%d,%f" value (float value / seconds)
        let stopwatch = Stopwatch.StartNew()
        let timerEvent () =
            let print = 
                if benchmarking then
                    makeBenchmarkPrint(stopwatch.Elapsed.TotalSeconds)
                else
                    makePrint(stopwatch.Elapsed.TotalSeconds)

            let conflicts = print("conflicts", conflicts)
            let cycles = print("cycles", cycles)
            let learned = print("retained", learned)
                
            if benchmarking = false then
                printfn "%s, %s, %s" conflicts cycles learned

            if (stopwatch.Elapsed >= stopwatchTimeout) then
                if benchmarking then
                    printMemory()
                    printfn "%s%s%s" conflicts cycles learned
                    Environment.Exit(0)

        timerEvent
        
    let timerEvent = makeTimerEvent()
    let rec startTimer () =
        if not (stop) then
            let timer = new Timer(1000.0)
            timer.AutoReset <- false
            timer.Elapsed.Add(fun x ->
                timer.Stop()
                timerEvent()
                startTimer())
            timer.Start()