namespace Terricolor

open FSharpx.Collections

type Action = Insert | Update
type Propagation = Assignment * Queue<Implication>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Propagation =

    let watch (action : Action) ((assignment, implications) : Propagation) (clause : Clause) : Result<Propagation,Conflict> =
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

    let insert : Propagation -> Clause -> Result<Propagation,Conflict> = watch Insert
    let update : Propagation -> Clause -> Result<Propagation,Conflict> = watch Update
    
    let bindInsert result clause = Result.bind (fun x -> insert x clause) result
    let bindUpdate result clause = Result.bind (fun x -> update x clause) result

    let rec fold fn state remainder =
        match state with
        | Failure conflict -> Failure conflict
        | Success (assignment, watchedClauses) ->
            match remainder with
            | [] -> Success (assignment, watchedClauses)
            | x :: xs -> fold fn (fn (assignment, watchedClauses) x) xs

    let updateFold state list = fold update (Success state) list
    let insertFold state list = fold insert (Success state) list

    let rec propagate : Result<Propagation,Conflict> -> Result<Propagation,Conflict> =
        function
        | Failure conflict -> Failure conflict
        | Success ((assignment, implications) : Propagation) ->
            match Queue.tryHead implications with
            | None -> Success(assignment, implications)
            | Some(literal, clause) ->
                let implications = Queue.tail implications
                match assignment.Assign literal (Some clause) with
                | Success (assignment, watchedClauses) ->
                    updateFold (assignment, implications) watchedClauses
                    |> propagate
                | Failure conflict -> Failure conflict

    let choose (literal : Literal) : Result<Propagation,Conflict> -> Result<Propagation,Conflict> =
        function
        | Failure conflict -> Failure conflict
        | Success ((assignment, implications) : Propagation) ->
            let implications = Queue.empty
            match assignment.Assign literal None with
            | Failure conflict -> Failure conflict
            | Success (assignment, watchedClauses) ->
                updateFold (assignment, implications) watchedClauses
                |> propagate