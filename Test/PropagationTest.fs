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
            Assert.IsTrue(Assignment.isUnassigned assignment literal)
            Assert.IsFalse(Assignment.isAssigned assignment literal)
            Assert.IsFalse(Assignment.isTrue assignment literal)
            Assert.IsFalse(Assignment.isFalse assignment literal)

        let assertTrue (assignment : Assignment) (literal : Literal) =
            Assert.IsTrue(Assignment.isTrue assignment literal)
            Assert.IsFalse(Assignment.isFalse assignment literal)
            Assert.IsFalse(Assignment.isUnassigned assignment literal)
            Assert.IsTrue(Assignment.isAssigned assignment literal)
            
        let assertFalse (assignment : Assignment) (literal : Literal) =
            Assert.IsFalse(Assignment.isTrue assignment literal)
            Assert.IsTrue(Assignment.isFalse assignment literal)
            Assert.IsFalse(Assignment.isUnassigned assignment literal)
            Assert.IsTrue(Assignment.isAssigned assignment literal)

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
            let assignment = Assignment.init numberOfVariables

            Assert.AreEqual(3, assignment.Variables)
            let literals = [ -3; -2; -1; 1; 2; 3 ]

            List.iter (assertUnassigned assignment) literals
            ()

        [<Test>]
        member public x.TestPositiveInitialUnitClause() =
            let clause = [| 1 |]
            let { Assignment=assignment } =
                Propagation.init 2
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
            let { Assignment=assignment } =
                Propagation.init 2
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
            let { Assignment=assignment } =
                Propagation.init 4
                |> Propagation.insert clause
                |> Propagation.bindDecide choice
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
            let { Assignment=assignment } =
                Propagation.init 4
                |> Propagation.insert clause
                |> Propagation.bindDecide choice
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
                Propagation.init 2
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
                Propagation.init 3
                |> Propagation.insert clause1
                |> Propagation.bindInsert clause2
                |> Propagation.bindInsert clause3
                |> Propagation.propagate
                |> Propagation.bindDecide -3
            
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
                Propagation.init 100
                |> Propagation.insert [|1; 31; -2|]      //ω1
                |> Propagation.bindInsert [|1; -3|]      //ω2
                |> Propagation.bindInsert [|2; 3; 4|]    //ω3
                |> Propagation.bindInsert [|-4; -5|]     //ω4
                |> Propagation.bindInsert [|21; -4; -6|] //ω5
                |> Propagation.bindInsert [|5; 6|]       //ω6
                |> Propagation.bindDecide -21
                |> Propagation.bindDecide -31
            
            match propagation with
            | Failure _ ->
                Assert.Fail("Should not have reached a conflict")
            | Success propagation ->
                let result = Propagation.decide -1 propagation

                match result with
                | Success _ ->
                    Assert.Fail("Should have reached a conflict")
                | _ -> ()