namespace Terricolor

module Learning =

    open Benchmarking
    open Primitives

    // learn new clauses using details of the conflict configuration
    let learnFromConflict (trail : Trail, reason : Reason, assignment : Assignment) =

        // update benchmark statistics
        incrementConflicts()

        // define how to resolve two sets of literals
        let resolventFor x y =
            let union = Set.union x y
            let fold partial literal =
                if union.Contains (mirror literal) then partial
                else Set.add literal partial
            Set.fold fold Set.empty union

        // define how to obtain the reason for a literal assignment (if any)
        let reasonFor literal =
            match assignment.[literal] with
            | Value(value) -> value.Reason
            | WatchList(list) -> None
          
        // generate the set of trail literals
        let trailSet = Set.ofList (trail @ (List.map mirror trail))

        // define the 1st UIP resolution procedure
        let rec resolve (trail : List<Literal>) (resolvent : Clause) (conflictClauses : List<Clause>) =
            // obtain the literal and its anticedent
            let literal = trail.Head
            let anticedent = reasonFor literal
            match anticedent with
            | None -> resolvent, conflictClauses
            | Some(reason) ->
                // accumulate conflict clauses
                let conflictClauses = reason :: conflictClauses
                // compute the resolvent
                let result = resolventFor reason resolvent
                let intersection = Set.intersect result trailSet
                // return when the first UIP has been found
                if 1 = Set.count intersection then
                    result, conflictClauses
                else
                    resolve trail.Tail result conflictClauses

        // obtain the boolean constraint in conflict
        let conflict = reason.Value

        // resolve this conflict using the constraint and our search trail
        let learnedClause, conflictClauses = resolve trail conflict []

        learnedClause, conflictClauses

