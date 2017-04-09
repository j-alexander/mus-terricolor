namespace Terricolor

open FSharpx.Collections

type Action = Insert | Update

type Propagation =
    { Assignment:Assignment
      Implications:Queue<Implication> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Propagation =

    let init variableCount =
        { Assignment=Assignment.init variableCount
          Implications=Queue.empty }

    let watch (action : Action)
              (clause : Clause)
              ({ Assignment=assignment
                 Implications=implications } as propagation) : Result<Propagation,Conflict> =
        if Seq.exists (Assignment.isTrue assignment) clause then Success propagation // drop satisfied clauses
        else
            let unit = Seq.tryFind (Assignment.isUnassigned assignment) clause
            match unit with
            | None ->
                Failure { Conflict.Reason=Some clause; Trail=assignment.Trail }      // 0 watchable literals -> raise a conflict
            | Some(unit) ->
                let assignment =                                                     // watch the first literal (on insert)
                    match action with
                    | Insert -> assignment |> Assignment.watch unit clause
                    | Update -> assignment
                let second =                                                         // check for a second literal
                    clause
                    |> Seq.filter (fun x -> x <> unit)
                    |> Seq.tryFind (Assignment.isUnassigned assignment)
                match second with
                | None ->                                                            // == 1 watch literal -> imply the remaining literal
                    { Assignment=assignment
                      Implications=Queue.conj (unit, clause) implications }
                | Some(second) ->                                                    // >= 2 watch literals -> watch the second literal
                    { Assignment=assignment |> Assignment.watch second clause
                      Implications=implications }                                    
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
        |> Result.bind(fun ({ Assignment=assignment
                              Implications=implications } as propagation) ->
            match Queue.tryHead implications with
            | None -> Success propagation
            | Some(literal, clause) ->
                assignment
                |> Assignment.assign literal (Some clause)
                |> Result.bind (fun (assignment, watchedClauses) ->
                    { Assignment=assignment
                      Implications=Queue.tail implications }
                    |> updateFold watchedClauses
                    |> propagate))

    let decide (literal : Literal) { Assignment=assignment } : Result<Propagation,Conflict> =
        assignment
        |> Assignment.assign literal None
        |> Result.bind (fun (assignment, watchedClauses) ->
            { Assignment=assignment
              Implications=Queue.empty }
            |> updateFold watchedClauses
            |> propagate)

    let bindDecide (literal : Literal) : Result<Propagation,Conflict> -> Result<Propagation,Conflict> =
        Result.bind (decide literal)