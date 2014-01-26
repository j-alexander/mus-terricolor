namespace Test

module LearningTest =

    open NUnit.Framework
    open Terricolor
    open Terricolor.Primitives
    open Terricolor.Propagation
    open Terricolor.Learning
    open FSharpx.Collections

    [<TestFixture>]
    type LearningTest() =

        [<SetUp>]
        member public x.SetUp() = ()

        [<Test>]
        member public x.TestConflictLearning1() =
            // Ex 4.2.4 - Handbook of Satisfiability (2009)
            // A Biere, M Heule, H van Maaren, T Walsh, Editors
            // J Marques-Silva , I Lynce, S Malik 
            let propagation =
                (new Assignment(100), Queue.empty)
                |> Propagation.insert <| [|1; 31; -2|]   //ω1
                |> Propagation.insert <| [|1; -3|]       //ω2
                |> Propagation.insert <| [|2; 3; 4|]     //ω3
                |> Propagation.insert <| [|-4; -5|]      //ω4
                |> Propagation.insert <| [|21; -4; -6|]  //ω5
                |> Propagation.insert <| [|5; 6|]        //ω6
                |> Propagation.choose -21
                |> Propagation.choose -31
            try
                let propagation =
                    propagation
                    |> Propagation.choose -1 
                    |> Propagation.propagate
                Assert.Fail("Should have reached a conflict.")
            with
            | Conflict(reason, trail) ->
                let learnedClause, conflictClauses = learnFromConflict(trail, reason)
                let learnedClauseSet = Set.ofArray learnedClause
                Assert.AreEqual(2, learnedClause.Length)
                Assert.IsTrue(learnedClauseSet.Contains -4)      //UIP(¬x4)
                Assert.IsTrue(learnedClauseSet.Contains 21)
            ()

        [<Test>]
        member public x.TestUnsatisfiableProblem() =

            let propagation =
                (new Assignment(3), Queue.empty)
                |> Propagation.insert <| [|1; 2; 3|]
                |> Propagation.insert <| [|-1; -2|]
                |> Propagation.insert <| [|-1; -2|]
                |> Propagation.insert <| [|-2; -3|]
                |> Propagation.insert <| [|1; -2|]
                |> Propagation.insert <| [|2; -3|]
                |> Propagation.insert <| [|3; -1|]
            try
                let propagation =
                    propagation
                    |> Propagation.choose 1 
                    |> Propagation.propagate
                Assert.Fail("Should have reached a conflict.")
            with
            | Conflict(reason, trail) ->
                let learnedClause, conflictClauses = learnFromConflict(trail, reason)
                try
                    let propagation =
                        propagation
                        |> Propagation.insert <| learnedClause
                        |> Propagation.propagate
                    Assert.Fail("Should have reached a conflict.")
                with
                | Conflict(reason, trail) ->
                    // conflict encountered just integrating the learned clause
                ()
            ()