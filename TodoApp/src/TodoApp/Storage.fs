/// Salvarea și încărcarea sarcinilor într-un fișier text UTF-8.
/// Prima linie este un antet, apoi câte o sarcină pe linie, cu câmpurile
/// separate prin tab:
///   id  titlu  descriere  prioritate  termen  creată  stare
module TodoApp.Storage

open System
open System.Globalization
open System.IO
open System.Text
open TodoApp.Domain

let header = "# TodoApp v1"

let private dateFormat = "yyyy-MM-dd"
let private timeFormat = "yyyy-MM-dd'T'HH:mm:ss"
let private inv = CultureInfo.InvariantCulture

// ---------------------------------------------------------------------------
// Scăparea caracterelor speciale din text
// ---------------------------------------------------------------------------

/// Tab-ul și sfârșitul de linie au rol de separator, deci în text se scriu
/// ca \t, \n, \r; backslash-ul se dublează.
let escape (text: string) =
    text
    |> Seq.map (function
        | '\\' -> "\\\\"
        | '\t' -> "\\t"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | c -> string c)
    |> String.concat ""

let unescape (text: string) =
    let rec go acc chars =
        match chars with
        | '\\' :: 't' :: rest -> go ('\t' :: acc) rest
        | '\\' :: 'n' :: rest -> go ('\n' :: acc) rest
        | '\\' :: 'r' :: rest -> go ('\r' :: acc) rest
        | '\\' :: c :: rest -> go (c :: acc) rest
        | c :: rest -> go (c :: acc) rest
        | [] -> List.rev acc

    go [] (List.ofSeq text) |> Array.ofList |> String

// ---------------------------------------------------------------------------
// O sarcină <-> o linie
// ---------------------------------------------------------------------------

let private priorityCode =
    function
    | Low -> "low"
    | Medium -> "medium"
    | High -> "high"

let private parsePriority =
    function
    | "low" -> Ok Low
    | "medium" -> Ok Medium
    | "high" -> Ok High
    | other -> Error $"prioritate necunoscută '{other}'"

let private parseInt (text: string) =
    match Int32.TryParse(text, NumberStyles.None, inv) with
    | true, value when value > 0 -> Ok value
    | _ -> Error $"număr invalid '{text}'"

let private parseDate (format: string) (text: string) =
    match DateTime.TryParseExact(text, format, inv, DateTimeStyles.None) with
    | true, value -> Ok value
    | _ -> Error $"dată invalidă '{text}'"

let private parseStatus (text: string) =
    if text = "pending" then
        Ok Pending
    elif text.StartsWith "done:" then
        parseDate timeFormat (text.Substring 5) |> Result.map Done
    else
        Error $"stare necunoscută '{text}'"

let serializeTask (task: TodoTask) =
    [ string task.Id
      escape task.Title
      escape task.Description
      priorityCode task.Priority
      (match task.DueDate with
       | Some d -> d.ToString(dateFormat, inv)
       | None -> "-")
      task.CreatedOn.ToString(timeFormat, inv)
      (match task.Status with
       | Pending -> "pending"
       | Done at -> "done:" + at.ToString(timeFormat, inv)) ]
    |> String.concat "\t"

let parseTask (line: string) : Result<TodoTask, string> =
    match line.Split '\t' with
    | [| id; title; description; priority; due; created; status |] ->
        result {
            let! id = parseInt id
            let! title = validateTitle (unescape title) |> Result.mapError (fun _ -> "titlu gol sau prea lung")
            let! priority = parsePriority priority

            let! due =
                if due = "-" then
                    Ok None
                else
                    parseDate dateFormat due |> Result.map Some

            let! created = parseDate timeFormat created
            let! status = parseStatus status

            let task: TodoTask =
                { Id = id
                  Title = title
                  Description = unescape description
                  Priority = priority
                  DueDate = due
                  CreatedOn = created
                  Status = status }

            return task
        }
    | fields -> Error $"{fields.Length} câmpuri în loc de 7"

// ---------------------------------------------------------------------------
// Toată lista <-> tot fișierul
// ---------------------------------------------------------------------------

let serialize (tasks: TodoTask list) =
    header :: (tasks |> List.map serializeTask) |> String.concat "\n"

let private duplicateIds (tasks: TodoTask list) =
    tasks
    |> List.countBy (fun t -> t.Id)
    |> List.filter (fun (_, n) -> n > 1)
    |> List.map (fun (id, _) -> $"numărul {id} apare de mai multe ori")

/// Întoarce sarcinile citite sau lista tuturor erorilor găsite,
/// fiecare cu numărul liniei la care apare.
let deserialize (text: string) : Result<TodoTask list, string list> =
    let parsed =
        text.Split '\n'
        |> List.ofArray
        |> List.mapi (fun i line -> i + 1, line.TrimEnd '\r')
        |> List.filter (fun (_, line) -> line.Trim() <> "" && not (line.StartsWith "#"))
        |> List.map (fun (n, line) -> parseTask line |> Result.mapError (fun e -> $"linia {n}: {e}"))

    let tasks = parsed |> List.choose Result.toOption

    let errors =
        (parsed
         |> List.choose (function
             | Error e -> Some e
             | Ok _ -> None))
        @ duplicateIds tasks

    if errors.IsEmpty then Ok tasks else Error errors

/// Un fișier care nu există încă înseamnă o listă goală.
let load (path: string) : Result<TodoTask list, string list> =
    if File.Exists path then
        try
            deserialize (File.ReadAllText(path, Encoding.UTF8))
        with ex ->
            Error [ ex.Message ]
    else
        Ok []

/// Scrie întâi într-un fișier temporar și apoi îl mută peste cel vechi,
/// ca o întrerupere în timpul scrierii să nu lase un fișier pe jumătate.
let save (path: string) (tasks: TodoTask list) : Result<unit, string> =
    try
        let fullPath = Path.GetFullPath path
        Directory.CreateDirectory(Path.GetDirectoryName fullPath) |> ignore
        let tempPath = fullPath + ".tmp"
        File.WriteAllText(tempPath, serialize tasks + "\n", UTF8Encoding false)
        File.Move(tempPath, fullPath, true)
        Ok()
    with ex ->
        Error ex.Message
