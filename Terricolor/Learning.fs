namespace Terricolor

module Learning =

    open Benchmarking
    open System

    // obtain the trail from the most recent decision forward
    let headOfTrail (trail : Trail) : Trail =
        let rec fn (acc) = function
        | [] -> []
        | head :: tail ->
            match head with
            | _, None -> head :: acc
            | _, Some _ -> fn (head :: acc) tail
        List.rev (fn [] trail)

    // define how to resolve two arrays of literals
    let resolventFor x y =
        let union = Set.union (Set.ofArray x) (Set.ofArray y)
        let fold partial literal =
            if union.Contains (Literal.mirror literal) then partial
            else Set.add literal partial
        Set.toArray (Set.fold fold Set.empty union)

    // learn new clauses using details of the conflict configuration
    let learnFromConflict (trail : Trail, reason : Reason) : Clause * Clause list =

        // update benchmark statistics
        incrementConflicts()

        // obtain the head of the trail and the set of its literals
        let trail = headOfTrail trail
        let trailSet =
            let literals = List.map fst trail
            Set.ofList (literals @ (List.map Literal.mirror literals))

        // define the 1st UIP resolution procedure
        let rec resolve conflictClauses resolvent = function
        | (literal, Some(reason)) :: tail ->
            // accumulate conflict clauses
            let conflictClauses = reason :: conflictClauses
            // compute the resolvent
            let result = resolventFor reason resolvent
            let intersection = Set.intersect (Set.ofArray result) trailSet
            // return when the first UIP has been found
            if 1 = Set.count intersection then
                result, conflictClauses
            else
                resolve conflictClauses result tail
        | _ -> resolvent, conflictClauses

        // obtain the boolean constraint in conflict
        let conflict = reason.Value

        // resolve this conflict using the constraint and our search trail
        resolve [] conflict trail

