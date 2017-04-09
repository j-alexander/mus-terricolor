namespace Terricolor

open FSharpx.Collections
open System

module Primitives =

    type Weight = int
    type Occurrences = Map<Literal, Weight>
    type Heuristic = { Steps : int; Occurrences : Occurrences }

    type State = { Propagation : Propagation ; Learned : Clause Set; Heuristic : Heuristic; Active : Clause Set }
    
    type Solution =
        | Unsatisfiable
        | Satisfiable of Assignment
    type Timeout = { State : State }