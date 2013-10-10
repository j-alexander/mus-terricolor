namespace Test

module PropagationTest =

    open Microsoft.VisualStudio.TestTools.UnitTesting
    open Terricolor
    open Terricolor.Primitives
    open Terricolor.Propagation

    [<TestClass>]
    type PropagationTest() =

        [<TestMethod>]
        member public x.TestEmpty() =

            let numberOfVariables = 3
            let assignment = makeEmptyAssignment numberOfVariables

            Assert.AreEqual(6, assignment.Count)
            Assert.IsTrue(Map.containsKey -3 assignment)
            Assert.IsTrue(Map.containsKey -2 assignment)
            Assert.IsTrue(Map.containsKey -1 assignment)
            Assert.IsTrue(Map.containsKey 1 assignment)
            Assert.IsTrue(Map.containsKey 2 assignment)
            Assert.IsTrue(Map.containsKey 3 assignment)
 
            Assert.AreEqual(WatchList List.empty, assignment.[-3])
            Assert.AreEqual(WatchList List.empty, assignment.[-2])
            Assert.AreEqual(WatchList List.empty, assignment.[-1])
            Assert.AreEqual(WatchList List.empty, assignment.[1])
            Assert.AreEqual(WatchList List.empty, assignment.[2])
            Assert.AreEqual(WatchList List.empty, assignment.[3])
            ()

        [<TestMethod>]
        member public x.TestPositiveInitialUnitClause() =
            let assignment = makeEmptyAssignment 2
                             |> addClause [1]
            
            Assert.AreEqual(4, assignment.Count)
            Assert.AreEqual(WatchList List.empty, assignment.[-2])
            Assert.AreEqual(WatchList List.empty, assignment.[2])

            match assignment.[-1] with
            | WatchList(list) -> Assert.Fail("Should have been unit {1}.")
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsFalse(isTrue)
                Assert.IsTrue(reason.IsSome)
                Assert.IsTrue(reason.Value.Contains(1))
                Assert.AreEqual(1, reason.Value.Count)
            
            match assignment.[1] with
            | WatchList(list) -> Assert.Fail("Should have been unit {1}.")
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsTrue(isTrue)
                Assert.IsTrue(reason.IsSome)
                Assert.IsTrue(reason.Value.Contains(1))
                Assert.AreEqual(1, reason.Value.Count)
            ()

        [<TestMethod>]
        member public x.TestNegativeInitialUnitClause() =
            let assignment = makeEmptyAssignment 2
                             |> addClause [-1]
            
            Assert.AreEqual(4, assignment.Count)
            Assert.AreEqual(WatchList List.empty, assignment.[-2])
            Assert.AreEqual(WatchList List.empty, assignment.[2])

            match assignment.[-1] with
            | WatchList(list) -> Assert.Fail("Should have been unit {-1}.")
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsTrue(isTrue)
                Assert.IsTrue(reason.IsSome)
                Assert.IsTrue(reason.Value.Contains(-1))
                Assert.AreEqual(1, reason.Value.Count)
            
            match assignment.[1] with
            | WatchList(list) -> Assert.Fail("Should have been unit {-1}.")
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsFalse(isTrue)
                Assert.IsTrue(reason.IsSome)
                Assert.IsTrue(reason.Value.Contains(-1))
                Assert.AreEqual(1, reason.Value.Count)
            ()

        [<TestMethod>]
        member public x.TestChoiceAndPropagation1() =
            let assignment = makeEmptyAssignment 4
                             |> addClause [-2; 3]
                             |> choose -3

            Assert.AreEqual(8, assignment.Count)
            Assert.AreEqual(WatchList List.empty, assignment.[-4])
            Assert.AreEqual(WatchList List.empty, assignment.[-1])
            Assert.AreEqual(WatchList List.empty, assignment.[1])
            Assert.AreEqual(WatchList List.empty, assignment.[4])
            
            match assignment.[-3] with
            | WatchList(list) -> Assert.Fail()
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsTrue(isTrue)
                Assert.IsTrue(reason.IsNone)
            match assignment.[-2] with
            | WatchList(list) -> Assert.Fail()
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsTrue(isTrue)
                Assert.IsTrue(reason.IsSome)
                Assert.AreEqual(2, reason.Value.Count)
                Assert.IsTrue(reason.Value.Contains(-2))
                Assert.IsTrue(reason.Value.Contains(3))
            match assignment.[2] with
            | WatchList(list) -> Assert.Fail()
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsFalse(isTrue)
                Assert.IsTrue(reason.IsSome)
                Assert.AreEqual(2, reason.Value.Count)
                Assert.IsTrue(reason.Value.Contains(-2))
                Assert.IsTrue(reason.Value.Contains(3))
            match assignment.[3] with
            | WatchList(list) -> Assert.Fail()
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsFalse(isTrue)
                Assert.IsTrue(reason.IsNone)
            ()

        [<TestMethod>]
        member public x.TestChoiceAndPropagation2() =
            let assignment = makeEmptyAssignment 4
                             |> addClause [-2; 3]
                             |> choose 2

            Assert.AreEqual(8, assignment.Count)
            Assert.AreEqual(WatchList List.empty, assignment.[-4])
            Assert.AreEqual(WatchList List.empty, assignment.[-1])
            Assert.AreEqual(WatchList List.empty, assignment.[1])
            Assert.AreEqual(WatchList List.empty, assignment.[4])
            
            match assignment.[-3] with
            | WatchList(list) -> Assert.Fail()
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsFalse(isTrue)
                Assert.IsTrue(reason.IsSome)
                Assert.AreEqual(2, reason.Value.Count)
                Assert.IsTrue(reason.Value.Contains(-2))
                Assert.IsTrue(reason.Value.Contains(3))
            match assignment.[-2] with
            | WatchList(list) -> Assert.Fail()
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsFalse(isTrue)
                Assert.IsTrue(reason.IsNone)
            match assignment.[2] with
            | WatchList(list) -> Assert.Fail()
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsTrue(isTrue)
                Assert.IsTrue(reason.IsNone)
            match assignment.[3] with
            | WatchList(list) -> Assert.Fail()
            | Value({IsTrue = isTrue; Reason = reason}) ->
                Assert.IsTrue(isTrue)
                Assert.IsTrue(reason.IsSome)
                Assert.AreEqual(2, reason.Value.Count)
                Assert.IsTrue(reason.Value.Contains(-2))
                Assert.IsTrue(reason.Value.Contains(3))
            ()

        [<TestMethod>]
        member public x.TestConflictAtConstruction() =
            let assignment = makeEmptyAssignment 2
                             |> addClause [1; 2]
                             |> addClause [-1; 2]
            try
                let result = addClause [-2] assignment
                Assert.Fail("Should have caused a conflict.")
            with
            | Conflict(trail, reason, assignment) ->
                let trail = List.rev trail
                Assert.AreEqual(2, trail.Length)
                Assert.AreEqual(-2, trail.Head)
                Assert.AreEqual(1, System.Math.Abs trail.Tail.Head)
                Assert.IsTrue(reason.IsSome)
                Assert.AreEqual(2, reason.Value.Count)
                Assert.IsTrue(reason.Value.Contains(2))
                Assert.IsTrue(reason.Value.Contains(-1) || reason.Value.Contains(1))
            ()

        [<TestMethod>]
        member public x.TestConflictAtChoice() =
            let assignment = makeEmptyAssignment 3
                             |> addClause [1; 2]
                             |> addClause [-1; 2]
                             |> addClause [-2; 3]
            try
                let result = choose -3 assignment
                Assert.Fail("Should have caused a conflict.")
            with
            | Conflict(trail, reason, assignment) ->
                let trail = List.rev trail
                Assert.AreEqual(3, trail.Length)
                Assert.AreEqual(-3, trail.Head)
                Assert.AreEqual(-2, trail.Tail.Head)
                Assert.AreEqual(1, System.Math.Abs trail.Tail.Tail.Head)
                Assert.IsTrue(reason.IsSome)
                Assert.AreEqual(2, reason.Value.Count)
                Assert.IsTrue(reason.Value.Contains(2))
                Assert.IsTrue(reason.Value.Contains(-1) || reason.Value.Contains(1))
            ()

        [<TestMethod>]
        member public x.TestMultilevelConflict() =
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
                Assert.Fail("Should have reached a conflict");
            with
            | Conflict(trail, reason, assignment) ->
                ()

            ()