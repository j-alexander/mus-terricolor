namespace Terricolor

module Primitives =
    
    type Literal = int
    type Weight = int

    type Clause = Set<Literal>

    type Residual = { Clause: Clause; Remainder: List<Literal> }

    type Reason = Option<Clause>

    type Trail = List<Literal>

    type Propagation = { Trail: Trail; Residual: Residual }

    type Value = { IsTrue: bool; Reason: Reason }

    type Variable =
        | Value of Value
        | WatchList of List<Residual>

    type Assignment = Map<Literal, Variable>

    type Occurrences = Map<Literal, Weight>

    type Heuristic = { Steps : int; Occurrences : Occurrences }

    type State = { Assignment : Assignment; Learned : List<Clause>; Heuristic : Heuristic; Active : Set<Clause> }
    
    // define workflow exceptions
    exception Conflict of Trail * Reason * Assignment
    exception Unsatisfiable
    exception Satisfiable of Assignment
    
    // define how to obtain the opposite of a particular literal
    let mirror (literal : Literal) =
        literal * -1
        
    // define how to make an empty variable assignment with no clauses
    let makeEmptyAssignment (variables : int) =
        let empty = WatchList List.empty
        let folder (assignment : Assignment) (x : int) =
            Map.add (mirror x) empty (Map.add x empty assignment)
        Seq.fold folder Map.empty [1..variables]
    
    // select a random number generator
    let random = new System.Random()