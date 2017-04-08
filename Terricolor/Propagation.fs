namespace Terricolor

open FSharpx.Collections

type Action = Insert | Update
type Propagation = Assignment * Queue<Implication>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Propagation =

    let watch (action : Action) (clause : Clause) ((assignment, implications) : Propagation) : Result<Propagation,Conflict> =
        if Seq.exists assignment.IsTrue clause then                             // drop satisfied clauses
            Success (assignment, implications)
        else
            let unit = Seq.tryFind assignment.IsUnassigned clause
            match unit with
            | None ->
                Failure { Conflict.Reason=Some clause; Trail=assignment.Trail } // 0 watchable literals -> raise a conflict
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
                |> Success

    let insert : Clause -> Propagation -> Result<Propagation,Conflict> = watch Insert
    let update : Clause -> Propagation -> Result<Propagation,Conflict> = watch Update
    
    let bindInsert clause = Result.bind (insert clause)
    let bindUpdate clause = Result.bind (update clause)

    let rec fold fn remainder =
        Result.bind (
            match remainder with
            | [] -> Success
            | x :: xs -> fn x >> fold fn xs)

    let updateFold list state = fold update list (Success state)
    let insertFold list state = fold insert list (Success state)

    let rec propagate result : Result<Propagation,Conflict> =
        result
        |> Result.bind(fun (assignment, implications) ->
            match Queue.tryHead implications with
            | None -> Success(assignment, implications)
            | Some(literal, clause) ->
                assignment.Assign literal (Some clause)
                |> Result.bind (fun (assignment, watchedClauses) ->
                    updateFold watchedClauses (assignment, Queue.tail implications)
                    |> propagate))

    let choose (literal : Literal) : Result<Propagation,Conflict> -> Result<Propagation,Conflict> =
        Result.bind (fst >> fun assignment ->
            assignment.Assign literal None
            |> Result.bind (fun (assignment, watchedClauses) ->
                updateFold watchedClauses (assignment,  Queue.empty)
                |> propagate))