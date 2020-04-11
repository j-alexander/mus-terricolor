namespace Benchmark

open System
open System.Diagnostics
open System.IO
open NUnit.Framework
open Terricolor
open Terricolor.Primitives
open Terricolor.Heuristics
open Terricolor.Search

[<TestFixture>]
type Verification() =

    static let clasp = new DirectoryInfo("Clasp/clasp-3.2.2-win64.exe")
    static let dimacs = new DirectoryInfo("Dimacs")

    let compare (input:string) =
        let startInfo = new ProcessStartInfo()
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardInput <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true
        startInfo.FileName <- clasp.FullName
        startInfo.Arguments <- ""
        let clasp = new Process()
        clasp.StartInfo <- startInfo
        if clasp.Start() then
            clasp.StandardInput.WriteLine(input)
            clasp.StandardInput.Close()
            clasp.WaitForExit()
            clasp.StandardOutput.ReadToEnd()
        else
            failwith "Unable to launch Clasp"

    let verify (file) =
        let contents = File.ReadAllText(file)
        let benchmark = compare contents
        let variableCount, clauseCount, clauses, time = Reader.read(contents)
        try
            CycleSearch.run Environment.ProcessorCount variableCount clauses
            
            Assert.Fail()
        with
        | Satisfiable(solution) ->
            let decisions =
                let filter = function (literal, None) -> Some(literal) | _ -> None
                solution.Trail |> List.choose filter

            let verification =
                let writer = new StringWriter()
                writer.WriteLine(sprintf "p cnf %d %d" variableCount (clauseCount + decisions.Length))
                for clause in clauses do
                    for literal in clause do
                        writer.Write(sprintf "%d " literal)
                    writer.WriteLine("0")
                for decision in decisions do
                    writer.WriteLine(sprintf "%d 0" decision)
                writer.ToString()
                |> compare

            Assert.True(benchmark.Contains "s SATISFIABLE")
            Assert.True(verification.Contains "s SATISFIABLE")
        | Conflict (reason, trail) ->
            Assert.True(benchmark.Contains "s UNSATISFIABLE")
        | Unsatisfiable ->
            Assert.True(benchmark.Contains "s UNSATISFIABLE")

    let verifyRutgers file = 
        verify(Path.Combine(dimacs.FullName, "Rutgers", file))

    [<Test>]
    member x.Aim_50_1_6_yes1_4() =
        verifyRutgers "aim-50-1_6-yes1-4.cnf"

    [<Test>]
    member x.Aim_100_1_6_no_1() =
        verifyRutgers "aim-100-1_6-no-1.cnf"

    [<Test; Ignore("Long compute time.")>]
    member x.Bf0432_007() =
        verifyRutgers "bf0432-007.cnf"

    [<Test>]
    member x.Dubois20() =
        verifyRutgers "dubois20.cnf"

    [<Test>]
    member x.Dubois21() =
        verifyRutgers "dubois21.cnf"

    [<Test>]
    member x.Dubois22() =
        verifyRutgers "dubois22.cnf"

    [<Test; Ignore("Long compute time.")>]
    member x.Hole6() =
        verifyRutgers "hole6.cnf"

    [<Test>]
    member x.Par8_1_c() =
        verifyRutgers "par8-1-c.cnf"

    [<Test>]
    member x.Quinn() =
        verifyRutgers "quinn.cnf"

    [<Test>]
    member x.Simple_v3_c2() =
        verifyRutgers "simple_v3_c2.cnf"

    [<Test; Ignore("Long compute time.")>]
    member x.Zebra_v155_c1135() =
        verifyRutgers "zebra_v155_c1135.cnf"