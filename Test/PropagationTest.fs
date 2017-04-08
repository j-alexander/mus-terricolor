namespace Test

module PropagationTest =

    open NUnit.Framework
    open Terricolor
    open Terricolor.Primitives
    open Terricolor.Propagation
    open FSharpx.Collections

    [<TestFixture>]
    type PropagationTest() =

        let assertUnassigned (assignment : Assignment) (literal : Literal) =
            Assert.IsTrue(assignment.IsUnassigned literal)
            Assert.IsFalse(assignment.IsAssigned literal)
            Assert.IsFalse(assignment.IsTrue literal)
            Assert.IsFalse(assignment.IsFalse literal)

        let assertTrue (assignment : Assignment) (literal : Literal) =
            Assert.IsTrue(assignment.IsTrue literal)
            Assert.IsFalse(assignment.IsFalse literal)
            Assert.IsFalse(assignment.IsUnassigned literal)
            Assert.IsTrue(assignment.IsAssigned literal)
            
        let assertFalse (assignment : Assignment) (literal : Literal) =
            Assert.IsFalse(assignment.IsTrue literal)
            Assert.IsTrue(assignment.IsFalse literal)
            Assert.IsFalse(assignment.IsUnassigned literal)
            Assert.IsTrue(assignment.IsAssigned literal)

        let fail x = Assert.Fail(x) ; failwith x

        let assertPropagation : Result<Propagation,Conflict> -> Propagation =
            function
            | Success propagation -> propagation
            | Failure conflict ->
                sprintf "Unexpected conflict: %A" conflict
                |> fail

        [<Test>]
        member public x.TestEmpty() =

            let numberOfVariables = 3
            let assignment = new Assignment(numberOfVariables)

            Assert.AreEqual(3, assignment.Variables)
            let literals = [ -3; -2; -1; 1; 2; 3 ]

            List.iter (assertUnassigned assignment) literals
            ()

        [<Test>]
        member public x.TestPositiveInitialUnitClause() =
            let clause = [| 1 |]
            let (assignment, implications) =
                (new Assignment(2), Queue.empty)
                |> Propagation.insert clause
                |> Propagation.propagate
                |> assertPropagation

            Assert.AreEqual(2, assignment.Variables)
            assertUnassigned assignment -2
            assertUnassigned assignment 2

            assertFalse assignment -1
            assertTrue assignment 1

            Assert.AreEqual(1, assignment.Trail.Length)
            let (unit, reason) = assignment.Trail.Head
            Assert.AreEqual(1, unit)
            Assert.True(reason.IsSome)
            Assert.AreEqual(clause, reason.Value)

            ()

        [<Test>]
        member public x.TestNegativeInitialUnitClause() =
            let clause = [| -2 |]
            let (assignment, implications) =
                (new Assignment(2), Queue.empty)
                |> Propagation.insert clause
                |> Propagation.propagate
                |> assertPropagation

            Assert.AreEqual(2, assignment.Variables)
            assertUnassigned assignment -1
            assertUnassigned assignment 1

            assertFalse assignment 2
            assertTrue assignment -2

            Assert.AreEqual(1, assignment.Trail.Length)
            let (unit, reason) = assignment.Trail.Head
            Assert.AreEqual(-2, unit)
            Assert.True(reason.IsSome)
            Assert.AreEqual(clause, reason.Value)


        [<Test>]
        member public x.TestChoiceAndPropagation1() =
            let clause = [| -2; 3 |]
            let choice = -3
            let (assignment, implications) =
                (new Assignment(4), Queue.empty)
                |> Propagation.insert clause
                |> Propagation.choose choice
                |> assertPropagation

            Assert.AreEqual(4, assignment.Variables)
            assertUnassigned assignment -4
            assertUnassigned assignment -1
            assertUnassigned assignment 1
            assertUnassigned assignment 4

            assertTrue assignment -3
            assertFalse assignment 3
            assertTrue assignment -2
            assertFalse assignment 2

            match assignment.Trail with
            | [ secondLiteral, Some secondReason
                firstLiteral, None ] ->
                Assert.AreEqual(-2, secondLiteral)
                Assert.AreEqual(clause, secondReason)
                Assert.AreEqual(-3, firstLiteral)
            | _ ->
                Assert.Fail "Incorrect Trail Configuration"


        [<Test>]
        member public x.TestChoiceAndPropagation2() =
            let clause = [| -2; 3 |]
            let choice = 2
            let (assignment, implications) =
                (new Assignment(4), Queue.empty)
                |> Propagation.insert clause
                |> Propagation.choose choice
                |> assertPropagation

            Assert.AreEqual(4, assignment.Variables)
            assertUnassigned assignment -4
            assertUnassigned assignment -1
            assertUnassigned assignment 1
            assertUnassigned assignment 4

            assertTrue assignment 2
            assertFalse assignment -2
            assertTrue assignment 3
            assertFalse assignment -2

            match assignment.Trail with
            | [ secondLiteral, Some secondReason
                firstLiteral, None ] ->
                Assert.AreEqual(3, secondLiteral)
                Assert.AreEqual(clause, secondReason)
                Assert.AreEqual(2, firstLiteral)
            | _ ->
                Assert.Fail "Incorrect Trail Configuration"


        [<Test>]
        member public x.TestConflictAtConstruction() =
            let clause1 = [| 1; 2 |]
            let clause2 = [| -1; 2 |]
            let clause3 = [| -2 |]

            let result = 
                (new Assignment(2), Queue.empty)
                |> Propagation.insert clause1
                |> Propagation.bindInsert clause2
                |> Propagation.bindInsert clause3
                |> Propagation.propagate

            match result with
            | Success _ ->
                Assert.Fail("Should have caused a conflict.")
            | Failure {Conflict.Reason=assertingClause; Trail=trail} ->
                match trail with
                | [ secondLiteral, Some secondReason
                    firstLiteral, Some firstReason ] ->
                    Assert.AreEqual(-2, firstLiteral)
                    Assert.AreEqual(clause3, firstReason)
                    Assert.AreEqual(-1, secondLiteral)
                    Assert.AreEqual(clause2, secondReason)
                    Assert.IsTrue(assertingClause.IsSome)
                    Assert.AreEqual(clause1, assertingClause.Value)
                | _ ->  
                    Assert.Fail "Incorrect Trail Configuration"

        [<Test>]
        member public x.TestConflictAtChoice() =
            let clause1 = [| 1; 2 |]
            let clause2 = [| -1; 2 |]
            let clause3 = [| -2; 3 |]

            let result = 
                (new Assignment(3), Queue.empty)
                |> Propagation.insert clause1
                |> Propagation.bindInsert clause2
                |> Propagation.bindInsert clause3
                |> Propagation.propagate
                |> Propagation.choose -3
            
            match result with
            | Success _ ->
                Assert.Fail("Should have caused a conflict.")
            | Failure {Conflict.Reason=assertingClause; Trail=trail } ->
                match trail with
                | [ thirdLiteral, Some thirdReason 
                    secondLiteral, Some secondReason
                    firstLiteral, None ] ->
                    Assert.AreEqual(-3, firstLiteral)
                    Assert.AreEqual(-2, secondLiteral)
                    Assert.AreEqual(clause3, secondReason)
                    Assert.AreEqual(-1, thirdLiteral)
                    Assert.AreEqual(clause2, thirdReason)
                    Assert.IsTrue(assertingClause.IsSome)
                    Assert.AreEqual(clause1, assertingClause.Value)
                | _ ->
                    Assert.Fail "Incorrect Trail Configuration"


        [<Test>]
        member public x.TestMultilevelConflict() =
            // Ex 4.2.4 - Handbook of Satisfiability (2009)
            // A Biere, M Heule, H van Maaren, T Walsh, Editors
            // J Marques-Silva , I Lynce, S Malik 
            let propagation =
                (new Assignment(100), Queue.empty)
                |> Propagation.insert [|1; 31; -2|]      //ω1
                |> Propagation.bindInsert [|1; -3|]      //ω2
                |> Propagation.bindInsert [|2; 3; 4|]    //ω3
                |> Propagation.bindInsert [|-4; -5|]     //ω4
                |> Propagation.bindInsert [|21; -4; -6|] //ω5
                |> Propagation.bindInsert [|5; 6|]       //ω6
                |> Propagation.choose -21
                |> Propagation.choose -31
            
            let result = Propagation.choose -1 propagation

            match result with
            | Success _ ->
                Assert.Fail("Should have reached a conflict");
            | _ -> ()