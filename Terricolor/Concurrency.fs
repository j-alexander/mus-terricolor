namespace Terricolor

module Concurrency =

    open System
    open System.Diagnostics
    open System.Linq
    open System.Runtime.InteropServices
    open System.Threading
    open System.Threading.Tasks
    
    [<DllImport("kernel32.dll")>]
    extern int GetCurrentThreadId()
    [<DllImport("kernel32.dll")>]
    extern int GetCurrentProcessorNumber()

    let cores = Environment.ProcessorCount

    // define how to obtain the current operating system thread
    let currentThread () =
        let id = GetCurrentThreadId()
        let threads = Process.GetCurrentProcess().Threads
        query {
            for thread in threads.Cast<ProcessThread>() do
            where (thread.Id = id)
            select thread
            head }

    // run an action on a specific CPU core (using OS native calls)
    let runOnCore core action =
        // obtain the target core bitmap
        let core =
            if core < 1  || core > cores then 1
            else 1 <<< (core - 1)
        try
            // lock affinity to the target thread and start the action
            Thread.BeginThreadAffinity()
            currentThread().ProcessorAffinity <- nativeint core 
            action()
        finally
            // release affinity
            currentThread().ProcessorAffinity <- nativeint 0xFFFF
            Thread.EndThreadAffinity()
            
    // split some sequence of tasks into (a list of results, and ongoing work)
    let splitCompletedAndWorking tasks =
        let foldCompleted (completed, working) (task : Task<'TResult>) =
            // when a task has faulted (i.e., Satisfiable or Unsatisfiable) rethrow the fault
            if task.IsFaulted then
                raise task.Exception.InnerException
            // results of completed tasks are added to the result list
            elif task.IsCompleted then
                task.Result :: completed, working
            // running tasks are alternatively added to the list of ongoing work
            else
                completed, task :: working
        // fold the sequence of tasks into the resulting pair of lists
        Seq.fold foldCompleted ([], []) tasks
        

