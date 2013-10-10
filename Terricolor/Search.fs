namespace Terricolor

module Search = 

    open System
    open System.Linq
    open System.Diagnostics

    open Benchmarking
    open Primitives
    open Propagation
    open Learning
    open Heuristics

    // start searching for solutions to the program and stop when timeout is exceeded
    let startSearch (initial : State) timeout =
        
        // capture the high-performance timer's clock value to mark our start time
        let stopwatch = Stopwatch.StartNew()

        // search from an intermediate position in the search tree
        let rec search (decisionLevels : List<State>) =
            match decisionLevels with
            | [] -> raise Unsatisfiable
            | current :: _ ->
                let root = decisionLevels.Last()

                // when the timeout is exceeded, return the current state, dropping intermediate work
                if stopwatch.Elapsed > timeout then
                    root
                else
                    // variables are undecided if no value has been assigned
                    let isUndecided (literal : Literal) (variable : Variable) =
                        match variable with
                        | Value(_) -> false
                        | WatchList(_) -> true
                    // determine the continuing tree configuration
                    let decisionLevels = 
                        match Map.tryFindKey isUndecided current.Assignment with
                        | None ->
                            // all variables have been assigned
                            raise (Satisfiable(current.Assignment))
                        | Some(literal) ->
                            let current, choice = select current literal
                            try
                                // attempt this literal assignment
                                let current = {
                                    current with Assignment = choose choice current.Assignment }

                                let restartInsteadOfBacktrack = true
                                if restartInsteadOfBacktrack then
                                    // just keep the most recent and root decision level
                                    [current; root]
                                else
                                    // keep the existing list of decision levels and continue intermediate search
                                    (current :: decisionLevels)
                            with
                            | Conflict(trail, reason, assignment) ->
                                // analyze conflict
                                let learnedClause, conflictClauses = learnFromConflict(trail, reason, assignment)
                                // define backtracking with assertion and integration
                                let integrate (state : State) = 
                                    try
                                        Some {
                                            Assignment = addClause learnedClause state.Assignment; 
                                            Active = Set.union (Set.ofList conflictClauses) state.Active
                                            Learned = learnedClause :: state.Learned;
                                            Heuristic = bump conflictClauses state.Heuristic }
                                    with Conflict(_) ->
                                        None
                                // integrate this learned clause with the available decision levels
                                List.choose integrate decisionLevels

                    // continue search from these new tree configurations
                    search decisionLevels

        // begin search using the initial state
        search (initial :: [])