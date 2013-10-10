namespace Terricolor

module Propagation =

    open Benchmarking
    open Primitives
        
    let rec watchAll (assignment : Assignment) (propagations : List<Propagation>) =
        match propagations with
        | [] -> assignment
        | propagation :: propagations ->
            let trail = propagation.Trail
            let residual = propagation.Residual
            let assignment, propagations =
                match residual.Remainder with
                | [] ->
                    // the boolean constraint is unsatisfiable under the current assignment
                    raise (Conflict (trail, Some residual.Clause, assignment))
                | next :: tail ->
                    // consume the next literal of the residual
                    let variable = (Map.find next assignment)
                    match variable with
                    | Value(value) ->
                        if value.IsTrue then
                            // satisfied by the current assignment: drop this clause and continue
                            assignment, propagations
                        else
                            // reduce one more step, since it's unsatisfied here
                            assignment, {propagation with Residual = {residual with Remainder = tail}} :: propagations
                    | WatchList(list) ->
                        // a unit collision occurs if both residuals intersect
                        let collision {Clause = clause} =
                            residual.Clause = clause
                        // check whether such a unit collision exists
                        if List.exists collision list then
                            // a unit collision has occurred
                            let reason = Some residual.Clause
                            let variable = Value {IsTrue = true; Reason = reason}
                            let trail = next :: trail
                            // update the assignment and eliminate the unit's mirror literal
                            eliminate (mirror next, reason, trail) (Map.add next variable assignment) propagations
                        else
                            // otherwise the new residual is added to the watch list
                            let variable =  WatchList(residual :: list)
                            // update the assignment
                            let assignment = Map.add next variable assignment
                            assignment, propagations
            // recurse next unit
            watchAll assignment propagations

    and eliminate (eliminated : Literal, reason : Reason, trail : Trail) (assignment : Assignment) (propagations : List<Propagation>) =
        let variable = Map.find eliminated assignment
        match variable with
        | Value(value) -> 
            // raise a conflict if this literal is true, otherwise we're already done
            if value.IsTrue then
                raise (Conflict ([], reason, assignment))
            else
                assignment, propagations
        | WatchList(list) ->
            // eliminate this variable
            let variable = Value {IsTrue = false; Reason = reason}
            let assignment = Map.add eliminated variable assignment
            // dispatching all watched residuals
            let prepend propagations (residual : Residual) =
                { Trail = trail; Residual = {residual with Remainder = residual.Remainder.Tail}} :: propagations
            assignment, List.fold prepend propagations list

    and watch (residual : Residual) (assignment : Assignment) =
        watchAll assignment ({Trail = []; Residual = residual} :: [])

    and choose (choice : Literal) (assignment : Assignment) =
        // satisfy the choice literal in this assignment
        let variable = Value {IsTrue = true; Reason = None}
        let assignment = Map.add choice variable assignment
        let trail = choice :: []
        // eliminate its mirror
        let assignment, propagations = (eliminate (mirror choice, None, trail) assignment [])
        watchAll assignment propagations
    
    and addClause (clause : seq<int>) (assignment : Assignment) =
        // build the boolean constraint and both remainder lists
        let booleanConstraint = Set.ofSeq clause
        let forward = Set.toList booleanConstraint
        let reverse = List.rev forward
        let forwardResidual = {Clause = booleanConstraint; Remainder = forward}
        let reverseResidual = {forwardResidual with Remainder = reverse}
        // include the residuals in the assignment
        assignment |> watch forwardResidual
                   |> watch reverseResidual

    and foldClause (assignment : Assignment) (clause : seq<int>) =
        addClause clause assignment

