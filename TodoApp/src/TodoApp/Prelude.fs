/// Expresii de calcul (computation expressions) pentru Result și Option.
/// Ele permit înlănțuirea unor pași care pot eșua fără if-uri imbricate:
/// la primul pas eșuat, restul calculului este sărit automat.
[<AutoOpen>]
module TodoApp.Prelude

type ResultBuilder() =
    member _.Bind(r: Result<'a, 'e>, f: 'a -> Result<'b, 'e>) = Result.bind f r
    member _.Return(x: 'a) : Result<'a, 'e> = Ok x
    member _.ReturnFrom(r: Result<'a, 'e>) = r

type MaybeBuilder() =
    member _.Bind(o: 'a option, f: 'a -> 'b option) = Option.bind f o
    member _.Return(x: 'a) = Some x
    member _.ReturnFrom(o: 'a option) = o

/// result { let! x = ...; return ... }
let result = ResultBuilder()

/// maybe { let! x = ...; return ... }
let maybe = MaybeBuilder()
