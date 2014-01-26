namespace Terricolor

open FSharpx.Collections
open System

module Primitives =

    type Weight = int
    type Occurrences = Map<Literal, Weight>
    type Heuristic = { Steps : int; Occurrences : Occurrences }

    type State = { Propagation : Propagation ; Learned : List<Clause>; Heuristic : Heuristic; Active : Set<Clause> }
    
    // define workflow exceptions
    exception Unsatisfiable
    exception Satisfiable of Assignment
        
    
    // select a random number generator
    let random = new System.Random()