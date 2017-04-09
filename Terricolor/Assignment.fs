namespace Terricolor

open System
open FSharpx.Collections

type Variable =
    | Value of Literal * Reason
    | WatchList of List<Clause>

type Assignment =
    { Vector : Vector<Variable>
      Trail : Trail }
with
    member x.Variables = x.Vector.Length

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Assignment =

    let empty =
        { Vector=Vector.empty
          Trail=[] }

    let init numberOfVariables = 
        { Vector=Vector.init numberOfVariables (fun x -> WatchList []) 
          Trail=[] }

    let private find (literal:Literal) (vector:Vector<Variable>) =
        vector.[(Math.Abs literal) - 1]

    let private update (literal:Literal) (variable:Variable) (vector:Vector<Variable>) =
        vector.Update((Math.Abs literal) - 1, variable)

    let assign (literal : Literal) (reason : Reason) (assignment:Assignment) : Result<Assignment * List<Clause>, Conflict> =
        match find literal assignment.Vector with
        | Value(assigned, _) ->
            if assigned = literal then
                Success(assignment, [])
            else
                Failure{ Conflict.Reason=reason; Trail=(literal, reason) :: assignment.Trail }
        | WatchList(watches) ->
            let value = (literal, reason)
            let vector = update literal (Value value) assignment.Vector
            let trail = value :: assignment.Trail
            Success ({Assignment.Vector=vector; Trail=trail}, watches)

    let watch (literal:Literal) (clause:Clause) (assignment:Assignment) : Assignment =
        match find literal assignment.Vector with
        | Value(assigned, reason) ->
            raise (Exception "Attaching clause to assigned literal.")
        | WatchList(watches) ->
            let watches = clause :: watches
            let vector = update literal (WatchList watches) assignment.Vector
            { assignment with Vector=vector }

    let isAssigned (assignment:Assignment) (literal : Literal) : bool =
        match find literal assignment.Vector with Value(_) -> true | _ -> false

    let isUnassigned (assignment:Assignment) (literal : Literal) : bool =
        match find literal assignment.Vector with WatchList(_) -> true | _ -> false

    let isTrue (assignment:Assignment) (literal : Literal) : bool =
        match find literal assignment.Vector with Value(x, _) -> x = literal | _ -> false

    let isFalse (assignment:Assignment) (literal : Literal) : bool =
        match find literal assignment.Vector with Value(x, _) -> x <> literal | _ -> false

    let tryFindUnassigned (assignment:Assignment) : Option<Literal> =
       seq { 1 .. assignment.Variables }
       |> Seq.tryFind (isUnassigned assignment)