namespace Test

module HeuristicsTest =

    open Microsoft.VisualStudio.TestTools.UnitTesting
    open Terricolor
    open Terricolor.Primitives
    open Terricolor.Heuristics

    [<TestClass>]
    type HeuristicsTest() =

        let conflictClauses =
            [ Set.ofList [-3; 2; 5; 7];
              Set.ofList [5; -2; 11];
              Set.ofList [5; -2; 21] ]
        let occurrences =
            Map.empty
            |> Map.add -3 1 
            |> Map.add 2 1
            |> Map.add 5 3
            |> Map.add 7 1
            |> Map.add -2 2
            |> Map.add 11 1
            |> Map.add 21 1

        [<TestMethod>]
        member public x.TestHeuristicBumpOnce() =
            let result = 
                { Steps = 0; Occurrences = Map.empty }
                |> bump conflictClauses

            Assert.AreEqual(1, result.Steps)
            Assert.AreEqual(occurrences, result.Occurrences)
            ()

        [<TestMethod>]
        member public x.TestHeuristicBumpTwice() =
            let result =
                { Steps = 0; Occurrences = Map.empty }
                |> bump conflictClauses
                |> bump conflictClauses

            let doubleOccurrences =
                Map.map (fun x y -> 2 * y) occurrences

            Assert.AreEqual(2, result.Steps)
            Assert.AreEqual(doubleOccurrences, result.Occurrences)

        [<TestMethod>]
        member public x.TestReduction() =
            let occurrences =
                Map.empty
                |> Map.add 1 4
                |> Map.add 2 8
                |> Map.add 0 3

            let heuristic = 
                { Steps = 100; Occurrences = occurrences }
                |> bump [ Set.ofList [ 1; 2; 0]; Set.ofList [ 3; 1; 2; 0 ] ]
            
            let expected =
                Map.empty
                |> Map.add 1 3
                |> Map.add 2 4
                |> Map.add 0 2
                |> Map.add 3 1

            Assert.AreEqual(0, heuristic.Steps)
            Assert.AreEqual(expected, heuristic.Occurrences)      

