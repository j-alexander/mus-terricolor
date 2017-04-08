namespace Terricolor

module Reader =
    
    open System
    open System.Diagnostics
    open System.IO
    open System.Linq
    open System.Text.RegularExpressions
    
    open Primitives

    let read (input : string) =
    
        let preambleEx = new Regex("^p[ \t\n\r]+cnf[ \t\n\r]+(\\d)*[ \t\n\r]+(\\d)*")
        let parameterEx = new Regex("[ \t\n\r]+[1-9]+[0-9]*")
        let numberEx = new Regex("[1-9]+[0-9]*")
        let literalEx = new Regex("-?[1-9]+[0-9]*[ \t\n\r]+")
        let clauseEx = new Regex("(-?[1-9]+[0-9]*[ \t\n\r]+)*[ \t\n\r]*0")
        let valueEx = new Regex("-?[1-9]+[0-9]*")
        
        let readPreamble (input : string) =
            let reader = new StringReader(input)
            let rec scanToPreamble (reader : StringReader) =
                let line = reader.ReadLine().ToLower()
                let isPreamble = preambleEx.IsMatch line
                match isPreamble with
                | true -> 
                    let preamble = (preambleEx.Match line).Value
                    let contents = line.Substring(preamble.Length) + reader.ReadToEnd()
                    (preamble, contents)
                | false -> scanToPreamble reader
            let (preamble, contents) = scanToPreamble reader
            let matches = parameterEx.Matches preamble
            let variableCount = Int32.Parse(matches.[0].Value)
            let clauseCount = Int32.Parse(matches.[1].Value)
            (variableCount, clauseCount, contents)

        let readContents (contents : string) =
            let clauseMatches = clauseEx.Matches(contents).AsParallel().Cast<Match>();
            let readLiteral (literalMatch : Match) : Literal =
                literalMatch.Value
                |> valueEx.Match
                |> (fun x -> x.Value)
                |> Int32.Parse
            let readClause (clauseMatch : Match) : Clause = 
                clauseMatch.Value
                |> literalEx.Matches
                |> Seq.cast<Match>
                |> Seq.map readLiteral
                |> Seq.toArray
            clauseMatches.Select(readClause)
            
        let stopwatch = Stopwatch.StartNew()

        let variableCount, clauseCount, contents = readPreamble input
        let clauses = readContents contents

        if clauseCount <> clauses.Count() then
            failwith "The number of clauses read does not match the number of clauses expected."

        stopwatch.Stop()

        (variableCount, clauseCount, List.ofSeq clauses, stopwatch.ElapsedMilliseconds)
