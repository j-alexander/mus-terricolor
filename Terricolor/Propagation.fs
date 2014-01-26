namespace Terricolor

open FSharpx.Collections

type Action = Insert | Update
type Propagation = Assignment * Queue<Implication>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Propagation =

    let watch (action : Action) ((assignment, implications) : Propagation) (clause : Clause) : Propagation =
        if Seq.exists assignment.IsTrue clause then (assignment, implications)  // drop satisfied clauses
        else
            let unit = Seq.tryFind assignment.IsUnassigned clause
            match unit with
            | None -> raise (Conflict(Some clause, assignment.Trail))           // 0 watchable literals -> raise a conflict
            | Some(unit) ->
                let assignment =                                                // watch the first literal (on insert)
                    match action with
                    | Insert -> assignment.Watch unit clause
                    | Update -> assignment
                let second =                                                    // check for a second literal
                    clause
                    |> Seq.filter (fun x -> x <> unit)
                    |> Seq.tryFind assignment.IsUnassigned
                match second with
                | None -> assignment, Queue.conj (unit, clause) implications    // == 1 watch literal -> imply the remaining literal
                | Some(second) -> assignment.Watch second clause, implications  // >= 2 watch literals -> watch the second literal

    let insert : Propagation -> Clause -> Propagation = watch Insert
    let update : Propagation -> Clause -> Propagation = watch Update

    let rec propagate ((assignment, implications) : Propagation) : Propagation =
        match Queue.tryHead implications with
        | None -> (assignment, implications)
        | Some(literal, clause) ->
            let implications = Queue.tail implications
            let assignment, watchedClauses = assignment.Assign literal (Some clause)
            List.fold update (assignment, implications) watchedClauses
            |> propagate

    let choose (literal : Literal) ((assignment, implications) : Propagation) : Propagation =
        let implications = Queue.empty
        let assignment, watchedClauses = assignment.Assign literal None
        List.fold update (assignment, implications) watchedClauses
        |> propagate