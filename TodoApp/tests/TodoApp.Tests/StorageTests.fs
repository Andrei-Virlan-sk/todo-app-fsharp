module TodoApp.Tests.StorageTests

open System
open System.IO
open Xunit
open TodoApp.Domain
open TodoApp.Storage

let sample: TodoTask list =
    [ { Id = 1
        Title = "Proiect F#"
        Description = "linia 1\nlinia 2\tcu tab și \\ backslash"
        Priority = High
        DueDate = Some(DateTime(2026, 11, 15))
        CreatedOn = DateTime(2026, 10, 22, 9, 15, 0)
        Status = Pending }
      { Id = 4
        Title = "Cumpărături: pâine, brânză"
        Description = ""
        Priority = Low
        DueDate = None
        CreatedOn = DateTime(2026, 10, 20, 18, 0, 0)
        Status = Done(DateTime(2026, 10, 21, 8, 45, 30)) } ]

[<Theory>]
[<InlineData("")>]
[<InlineData("text simplu")>]
[<InlineData("tab\taici")>]
[<InlineData("rânduri\r\nnoi\n")>]
[<InlineData("\\t nu este tab")>]
[<InlineData("\\")>]
let ``unescape anulează escape`` (text: string) =
    let escaped = escape text
    Assert.DoesNotContain("\t", escaped)
    Assert.DoesNotContain("\n", escaped)
    Assert.Equal(text, unescape escaped)

[<Fact>]
let ``lista salvată și recitită este aceeași`` () =
    match deserialize (serialize sample) with
    | Ok loaded -> Assert.Equal<TodoTask list>(sample, loaded)
    | Error errors -> failwithf "%A" errors

[<Fact>]
let ``fiecare sarcină ocupă o singură linie`` () =
    let lines = (serialize sample).Split '\n'
    Assert.Equal(header, lines[0])
    Assert.Equal(1 + sample.Length, lines.Length)

[<Fact>]
let ``liniile goale și terminațiile Windows sunt acceptate`` () =
    let text = (serialize sample).Replace("\n", "\r\n") + "\r\n\r\n"

    match deserialize text with
    | Ok loaded -> Assert.Equal(sample.Length, loaded.Length)
    | Error errors -> failwithf "%A" errors

[<Fact>]
let ``erorile spun la ce linie apar`` () =
    let text =
        String.concat "\n" [ header; serializeTask sample[0]; "3\tdoar trei câmpuri"; "x\tt\t\tlow\t-\t2026-10-22T10:00:00\tpending" ]

    match deserialize text with
    | Ok _ -> failwith "fișierul stricat a fost acceptat"
    | Error errors ->
        Assert.Equal(2, errors.Length)
        Assert.StartsWith("linia 3:", errors[0])
        Assert.StartsWith("linia 4:", errors[1])

[<Fact>]
let ``numerele duplicate sunt respinse`` () =
    let text = serialize [ sample[0]; sample[0] ]

    match deserialize text with
    | Ok _ -> failwith "duplicatele au fost acceptate"
    | Error errors -> Assert.Contains(errors, fun e -> e.Contains "numărul 1")

[<Fact>]
let ``save și load folosesc fișierul de pe disc`` () =
    let dir = Path.Combine(Path.GetTempPath(), "todoapp-tests-" + Guid.NewGuid().ToString "N")
    let path = Path.Combine(dir, "sarcini.txt")

    try
        Assert.Equal(Ok [], load path)
        Assert.Equal(Ok(), save path sample)
        Assert.Equal(Ok sample, load path)
        Assert.False(File.Exists(path + ".tmp"))
    finally
        if Directory.Exists dir then
            Directory.Delete(dir, true)
