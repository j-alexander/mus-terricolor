namespace Test

module LearningTest =

    open NUnit.Framework
    open Terricolor
    open Terricolor.Primitives
    open Terricolor.Propagation
    open Terricolor.Learning

    [<TestFixture>]
    type LearningTest() =

        [<Test>]
        member public x.TestConflictLearning1() =
            // Ex 4.2.4 - Handbook of Satisfiability (2009)
            // A Biere, M Heule, H van Maaren, T Walsh, Editors
            // J Marques-Silva , I Lynce, S Malik 
            let assignment = makeEmptyAssignment 100
                             |> addClause [1; 31; -2]   //ω1
                             |> addClause [1; -3]       //ω2
                             |> addClause [2; 3; 4]     //ω3
                             |> addClause [-4; -5]      //ω4
                             |> addClause [21; -4; -6]  //ω5
                             |> addClause [5; 6]        //ω6
                             |> choose -21
                             |> choose -31
            try
                let result = choose -1 assignment
                Assert.Fail("Should have reached a conflict.")
            with
            | Conflict(trail, reason, assignment) ->
                let learnedClause, conflictClauses = learnFromConflict(trail, reason, assignment)
                Assert.AreEqual(2, learnedClause.Count)
                Assert.IsTrue(learnedClause.Contains -4)      //UIP(¬x4)
                Assert.IsTrue(learnedClause.Contains 21)
            ()

        [<Test>]
        member public x.TestUnsatisfiableProblem() =
            let assignment = makeEmptyAssignment 3
                             |> addClause [1; 2; 3]
                             |> addClause [-1; -2]
                             |> addClause [-2; -3]
                             |> addClause [-1; -3]
                             |> addClause [1; -2]
                             |> addClause [2; -3]
                             |> addClause [3; -1]
            try
                let result = choose 1 assignment
                Assert.Fail("Should have reached a conflict.")
            with
            | Conflict(trail, reason, assignment) ->
                let learnedClause, conflictClauses = learnFromConflict(trail, reason, assignment)
                try
                    let result = addClause learnedClause assignment
                    Assert.Fail("Should have reached a conflict.")
                with
                | Conflict(trail, reason, assignment) ->
                    // conflict encountered just integrating the learned clause
                    Assert.IsTrue(trail.IsEmpty)
                ()
            ()