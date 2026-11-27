namespace Test

module PropagationTest =

    open NUnit.Framework
    open NUnit.Framework.Legacy
    open Terricolor
    open Terricolor.Primitives
    open Terricolor.Propagation
    open FSharpx.Collections

    [<TestFixture>]
    type PropagationTest() =

        let assertUnassigned (assignment : Assignment) (literal : Literal) =
            ClassicAssert.IsTrue(assignment.IsUnassigned literal)
            ClassicAssert.IsFalse(assignment.IsAssigned literal)
            ClassicAssert.IsFalse(assignment.IsTrue literal)
            ClassicAssert.IsFalse(assignment.IsFalse literal)

        let assertTrue (assignment : Assignment) (literal : Literal) =
            ClassicAssert.IsTrue(assignment.IsTrue literal)
            ClassicAssert.IsFalse(assignment.IsFalse literal)
            ClassicAssert.IsFalse(assignment.IsUnassigned literal)
            ClassicAssert.IsTrue(assignment.IsAssigned literal)
            
        let assertFalse (assignment : Assignment) (literal : Literal) =
            ClassicAssert.IsFalse(assignment.IsTrue literal)
            ClassicAssert.IsTrue(assignment.IsFalse literal)
            ClassicAssert.IsFalse(assignment.IsUnassigned literal)
            ClassicAssert.IsTrue(assignment.IsAssigned literal)

        [<Test>]
        member public x.TestEmpty() =

            let numberOfVariables = 3
            let assignment = new Assignment(numberOfVariables)

            ClassicAssert.AreEqual(3, assignment.Variables)
            let literals = [ -3; -2; -1; 1; 2; 3 ]

            List.iter (assertUnassigned assignment) literals
            ()

        [<Test>]
        member public x.TestPositiveInitialUnitClause() =
            let clause = [| 1 |]
            let (assignment, implications) =
                (new Assignment(2), Queue.empty) |> Propagation.insert <| clause
                |> Propagation.propagate

            ClassicAssert.AreEqual(2, assignment.Variables)
            assertUnassigned assignment -2
            assertUnassigned assignment 2

            assertFalse assignment -1
            assertTrue assignment 1

            ClassicAssert.AreEqual(1, assignment.Trail.Length)
            let (unit, reason) = assignment.Trail.Head
            ClassicAssert.AreEqual(1, unit)
            ClassicAssert.True(reason.IsSome)
            ClassicAssert.AreEqual(clause, reason.Value)

            ()

        [<Test>]
        member public x.TestNegativeInitialUnitClause() =
            let clause = [| -2 |]
            let (assignment, implications) =
                (new Assignment(2), Queue.empty) |> Propagation.insert <| clause
                |> Propagation.propagate

            ClassicAssert.AreEqual(2, assignment.Variables)
            assertUnassigned assignment -1
            assertUnassigned assignment 1

            assertFalse assignment 2
            assertTrue assignment -2

            ClassicAssert.AreEqual(1, assignment.Trail.Length)
            let (unit, reason) = assignment.Trail.Head
            ClassicAssert.AreEqual(-2, unit)
            ClassicAssert.True(reason.IsSome)
            ClassicAssert.AreEqual(clause, reason.Value)

            ()

        [<Test>]
        member public x.TestChoiceAndPropagation1() =
            let clause = [| -2; 3 |]
            let choice = -3
            let (assignment, implications) =
                (new Assignment(4), Queue.empty) |> Propagation.insert <| clause
                |> Propagation.choose choice

            ClassicAssert.AreEqual(4, assignment.Variables)
            assertUnassigned assignment -4
            assertUnassigned assignment -1
            assertUnassigned assignment 1
            assertUnassigned assignment 4

            assertTrue assignment -3
            assertFalse assignment 3
            assertTrue assignment -2
            assertFalse assignment 2

            match assignment.Trail with
            | (secondLiteral, Some(secondReason)) :: (firstLiteral, None) :: [] ->
                ClassicAssert.AreEqual(-2, secondLiteral)
                ClassicAssert.AreEqual(clause, secondReason)
                ClassicAssert.AreEqual(-3, firstLiteral)
            | _ -> ClassicAssert.Fail "Incorrect Trail Configuration"

            ()

        [<Test>]
        member public x.TestChoiceAndPropagation2() =
            let clause = [| -2; 3 |]
            let choice = 2
            let (assignment, implications) =
                (new Assignment(4), Queue.empty) |> Propagation.insert <| clause
                |> Propagation.choose choice

            ClassicAssert.AreEqual(4, assignment.Variables)
            assertUnassigned assignment -4
            assertUnassigned assignment -1
            assertUnassigned assignment 1
            assertUnassigned assignment 4

            assertTrue assignment 2
            assertFalse assignment -2
            assertTrue assignment 3
            assertFalse assignment -2

            match assignment.Trail with
            | (secondLiteral, Some(secondReason)) :: (firstLiteral, None) :: [] ->
                ClassicAssert.AreEqual(3, secondLiteral)
                ClassicAssert.AreEqual(clause, secondReason)
                ClassicAssert.AreEqual(2, firstLiteral)
            | _ -> ClassicAssert.Fail "Incorrect Trail Configuration"

            ()

        [<Test>]
        member public x.TestConflictAtConstruction() =
            let clause1 = [| 1; 2 |]
            let clause2 = [| -1; 2 |]
            let clause3 = [| -2 |]

            try
                let result = 
                    (new Assignment(2), Queue.empty)
                    |> Propagation.insert <| clause1
                    |> Propagation.insert <| clause2
                    |> Propagation.insert <| clause3
                    |> Propagation.propagate
                ClassicAssert.Fail("Should have caused a conflict.")
            with
            | Conflict(assertingClause, trail) ->
                match trail with
                | (secondLiteral, Some(secondReason)) :: (firstLiteral, Some(firstReason)) :: [] ->
                    ClassicAssert.AreEqual(-2, firstLiteral)
                    ClassicAssert.AreEqual(clause3, firstReason)
                    ClassicAssert.AreEqual(-1, secondLiteral)
                    ClassicAssert.AreEqual(clause2, secondReason)
                    ClassicAssert.IsTrue(assertingClause.IsSome)
                    ClassicAssert.AreEqual(clause1, assertingClause.Value)
                | _ -> ClassicAssert.Fail "Incorrect Trail Configuration"
            ()

        [<Test>]
        member public x.TestConflictAtChoice() =
            let clause1 = [| 1; 2 |]
            let clause2 = [| -1; 2 |]
            let clause3 = [| -2; 3 |]

            try
                let result = 
                    (new Assignment(3), Queue.empty)
                    |> Propagation.insert <| clause1
                    |> Propagation.insert <| clause2
                    |> Propagation.insert <| clause3
                    |> Propagation.propagate
                    |> Propagation.choose -3
                ClassicAssert.Fail("Should have caused a conflict.")
            with
            | Conflict(assertingClause, trail) ->
                match trail with
                | (thirdLiteral, Some(thirdReason)) ::
                  (secondLiteral, Some(secondReason)) ::
                  (firstLiteral, None) :: [] ->
                    ClassicAssert.AreEqual(-3, firstLiteral)
                    ClassicAssert.AreEqual(-2, secondLiteral)
                    ClassicAssert.AreEqual(clause3, secondReason)
                    ClassicAssert.AreEqual(-1, thirdLiteral)
                    ClassicAssert.AreEqual(clause2, thirdReason)
                    ClassicAssert.IsTrue(assertingClause.IsSome)
                    ClassicAssert.AreEqual(clause1, assertingClause.Value)
                | _ -> ClassicAssert.Fail "Incorrect Trail Configuration"

            ()

        [<Test>]
        member public x.TestMultilevelConflict() =
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
                let result = Propagation.choose -1 propagation
                ClassicAssert.Fail("Should have reached a conflict");
            with
            | Conflict(assertingClause, trail) ->
                ()

            ()