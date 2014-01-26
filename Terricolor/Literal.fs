namespace Terricolor

open System

type Literal = int

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Literal =

    let mirror (literal : Literal) =    // define how to obtain the opposite of a particular literal
        -1 * literal