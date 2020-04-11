namespace Terricolor

open System
open FSharpx.Collections

type Variable =
    | Value of Literal * Reason
    | WatchList of List<Clause>

type Assignment (vector : PersistentVector<Variable>, trail : Trail) =

    let find (literal : Literal) =
        vector.[(Math.Abs literal) - 1]
    let update (literal : Literal) (variable : Variable) =
        vector.Update((Math.Abs literal) - 1, variable)

    new (numberOfVariables : int) =
        Assignment(PersistentVector.init numberOfVariables (fun x -> WatchList []), [])

    member x.Trail = trail
    member x.Variables = vector.Length

    member x.Assign (literal : Literal) (reason : Reason) : Assignment * List<Clause> =
        match find literal with
        | Value(assigned, _) ->
            if assigned = literal then x, []
            else raise (Conflict(reason, (literal, reason) :: trail))
        | WatchList(watches) ->
            let value = (literal, reason)
            let vector = update literal (Value value)
            let trail = value :: trail
            new Assignment(vector, trail), watches

    member x.Watch (literal : Literal) (clause : Clause) : Assignment =
        match find literal with
        | Value(assigned, reason) ->
            raise (Exception "Attaching clause to assigned literal.")
        | WatchList(watches) ->
            let watches = clause :: watches
            let vector = update literal (WatchList watches)
            new Assignment(vector, trail)

    member x.IsAssigned (literal : Literal) : bool =
        match find literal with Value(_) -> true | _ -> false

    member x.IsUnassigned (literal : Literal) : bool =
        match find literal with WatchList(_) -> true | _ -> false

    member x.IsTrue (literal : Literal) : bool =
        match find literal with Value(x, _) -> x = literal | _ -> false

    member x.IsFalse (literal : Literal) : bool =
        match find literal with Value(x, _) -> x <> literal | _ -> false

    member x.TryFindUnassigned() : Option<Literal> =
       seq { 1 .. PersistentVector.length vector }
       |> Seq.tryFind x.IsUnassigned

    static member isTrue (assignment : Assignment) (literal : Literal) =
        assignment.IsTrue(literal)