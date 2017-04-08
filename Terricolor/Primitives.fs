namespace Terricolor

open FSharpx.Collections
open System

module Primitives =

    type Weight = int
    type Occurrences = Map<Literal, Weight>
    type Heuristic = { Steps : int; Occurrences : Occurrences }

    type State = { Propagation : Propagation ; Learned : List<Clause>; Heuristic : Heuristic; Active : Set<Clause> }
    
    type Solution =
        | Unsatisfiable
        | Satisfiable of Assignment
    type Timeout = { State : State }