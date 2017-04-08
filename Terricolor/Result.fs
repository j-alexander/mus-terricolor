namespace Terricolor

type Result<'Success, 'Failure> = 
    | Success of 'Success
    | Failure of 'Failure

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =

    let getSuccess = function
        | Success x -> x
        | Failure x -> failwith "Value is a Failure case."

    let getFailure = function
        | Failure x -> x
        | Success x -> failwith "Value is a Success case."

    let mapSuccess fn = function
        | Success x -> Success(fn x)
        | Failure x -> Failure(x)

    let mapFailure fn = function
        | Success x -> Success(x)
        | Failure x -> Failure(fn x)

    let bind fn = function
        | Success x -> fn(x)
        | Failure x -> Failure(x)