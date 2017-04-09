namespace Terricolor


module Heuristics =

    open System
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
    let threshold (threshold:float) (random:Random) (literal:Literal) =
        if random.NextDouble() > threshold then literal
        else literal * -1
        
    // random assignment for literal decisions with 50:50 odds
    let coinToss (random:Random) (literal:Literal) =
        threshold 0.5 random literal

    // select a better literal, if possible, with custom value function
    let selectBy (fn:Literal->Literal)
                 (defaultLiteral:Literal)
                 ({ Learned=learned
                    Heuristic={ Occurrences=occurrences }
                    Propagation={ Assignment=assignment
                                  Implications=implications } } as state) =
    
        // select the highest value unassigned literal by weight
        let selectTopLiteral (clause : Clause) =
            let weights (literal : Literal) =
                match Map.tryFind literal occurrences with
                | None -> 0
                | Some value -> -1 * value
            clause
            |> Seq.filter (Assignment.isUnassigned assignment)
            |> Seq.sortBy weights
            |> Seq.head

        // reduce the learned clause set and select a literal
        let rec selectFromLearnedClauses (learnedClauses : Set<Clause>) =
            if Set.isEmpty learnedClauses then
                // no learned clauses remain -> choose an assignment
                Set.empty, fn defaultLiteral
            else
                let clause = Set.minElement learnedClauses
                if Array.exists (Assignment.isTrue assignment) clause then
                    let tail = Set.remove clause learnedClauses
                    selectFromLearnedClauses tail
                else
                    learnedClauses, selectTopLiteral clause
        
        let learnedClauses, choice = selectFromLearnedClauses learned

        { state with Learned = learnedClauses }, choice
        
    // select a better literal, if possible
    let select random = selectBy (coinToss random)
        
    // define how to make a random set of occurrences
    let makeOccurrences (random:Random) variables low high =
        let fold x map =
            Map.add x (random.Next(low, high)) map
        let bifold map x =
            fold (Literal.mirror x) (fold x map)
        Seq.fold bifold Map.empty (seq { 1..variables })

    // define an initial heuristic
    let makeHeuristic (random:Random) variables =
        { Steps = 0;
          Occurrences = makeOccurrences random variables 1 10 }