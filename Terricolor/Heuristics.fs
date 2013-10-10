namespace Terricolor

module Heuristics =

    open Primitives
    
    let bump conflictClauses (heuristic : Heuristic)  =
        // weights are reduced using division by 4
        let reduce (literal : Literal) (weight : Weight) =
            weight / 4
        // occurrences are boosted according to literal counts in conflict clauses
        let boost occurrences clauses =
            let boostLiteral occurrences literal =
                let weight =
                    match Map.tryFind literal occurrences with
                    | None -> 1
                    | Some value -> value + 1
                Map.add literal weight occurrences
            Seq.fold boostLiteral occurrences (Seq.concat clauses)
        // reduce every 100 applications of this heuristic (i.e. 100 conflicts) and boost
        if heuristic.Steps < 100 then
            { Steps = heuristic.Steps + 1;
              Occurrences = boost heuristic.Occurrences conflictClauses }
        else
            { Steps = 0;
              Occurrences = boost (Map.map reduce heuristic.Occurrences) conflictClauses }
              
    // random assignment for literal decisions
    let selectRandom (literal : Literal) =
        if random.NextDouble() > 0.5 then literal
        else literal * -1

    // select a better literal, if possible
    let select (state : State)  (defaultLiteral : Literal) =
        let assignment = state.Assignment
        let occurrences = state.Heuristic.Occurrences
    
        // a literal is satisfied it is assigned a true value
        let literalIsSatisfied (literal : Literal) =
            match assignment.[literal] with
            | Value(value) -> value.IsTrue
            | WatchList(_) -> true            
    
        // select the highest value unassigned literal by weight
        let selectTopLiteral (clause : Clause) =
            let unassigned (literal : Literal) =
                match assignment.[literal] with
                | Value(_) -> None
                | WatchList(_) -> Some(literal)
            let weights (literal : Literal) =
                match Map.tryFind literal occurrences with
                | None -> 0
                | Some value -> -1 * value
            clause
            |> Seq.choose unassigned
            |> Seq.sortBy weights
            |> Seq.head

        // reduce the learned clause set and select a literal
        let rec selectFromLearnedClauses learnedClauses =
            match learnedClauses with
            | [] ->
                // no learned clauses remain -> choose a random assignment
                [], selectRandom defaultLiteral
            | clause :: tail ->
                if Set.exists literalIsSatisfied clause then
                    selectFromLearnedClauses tail
                else
                    learnedClauses, selectTopLiteral clause
        
        let learnedClauses, choice = selectFromLearnedClauses state.Learned   

        { state with Learned = learnedClauses }, choice
        
    // define how to make a random set of occurrences
    let makeRandomOccurrences variables low high =
        let fold x map =
            Map.add x (random.Next(low, high)) map
        let bifold map x =
            fold (mirror x) (fold x map)
        Seq.fold bifold Map.empty (seq { 1..variables })

    // define an initial heuristic
    let makeHeuristic variables =
        { Steps = 0;
          Occurrences = makeRandomOccurrences variables 1 10 }