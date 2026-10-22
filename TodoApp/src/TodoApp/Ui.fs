/// Tot ce ține de consolă: texte în limba română, culori, citirea și
/// interpretarea datelor introduse și afișarea sarcinilor.
module TodoApp.Ui

open System
open System.Globalization
open TodoApp.Domain

let private inv = CultureInfo.InvariantCulture

// ---------------------------------------------------------------------------
// Texte
// ---------------------------------------------------------------------------

let priorityLabel =
    function
    | High -> "Ridicată"
    | Medium -> "Medie"
    | Low -> "Scăzută"

let filterLabel =
    function
    | All -> "toate"
    | PendingOnly -> "în lucru"
    | DoneOnly -> "finalizate"
    | OverdueOnly -> "întârziate"

let errorMessage =
    function
    | EmptyTitle -> "Titlul nu poate fi gol."
    | TitleTooLong n -> $"Titlul poate avea cel mult {n} de caractere."
    | TaskNotFound id -> $"Nu există nicio sarcină cu numărul {id}."
    | AlreadyDone id -> $"Sarcina {id} este deja finalizată."
    | NotDone id -> $"Sarcina {id} nu este finalizată, deci nu are ce fi redeschis."
    | DueDateInPast -> "Termenul nu poate fi o dată din trecut."

/// „1 sarcină”, „5 sarcini”, „20 de sarcini” (regula lui „de” din română).
let countTasks (n: int) =
    if n = 1 then "1 sarcină"
    elif n <> 0 && (n % 100 = 0 || n % 100 >= 20) then $"{n} de sarcini"
    else $"{n} sarcini"

let formatDate (d: DateTime) = d.ToString("dd.MM.yyyy", inv)

let formatDateTime (d: DateTime) = d.ToString("dd.MM.yyyy HH:mm", inv)

// ---------------------------------------------------------------------------
// Interpretarea textului introdus (funcții pure, testate separat)
// ---------------------------------------------------------------------------

/// Răspuns gol = se păstrează valoarea curentă.
let parsePriorityInput (current: Priority) (text: string) : Result<Priority, string> =
    match text.Trim().ToLowerInvariant() with
    | "" -> Ok current
    | "1" -> Ok Low
    | "2" -> Ok Medium
    | "3" -> Ok High
    | _ -> Error "Alege 1 (scăzută), 2 (medie) sau 3 (ridicată)."

let private parseDaysFromToday (today: DateTime) (text: string) =
    match Int32.TryParse(text, NumberStyles.None, inv) with
    | true, days -> Ok(Some(today.Date.AddDays(float days)))
    | _ -> Error "După „+” scrie numărul de zile, de exemplu +3."

let private parseCalendarDate (text: string) =
    match DateTime.TryParseExact(text, [| "d.M.yyyy"; "dd.MM.yyyy" |], inv, DateTimeStyles.None) with
    | true, d -> Ok(Some d)
    | _ -> Error "Scrie data ca zz.ll.aaaa (de exemplu 15.11.2026), „azi”, „mâine” sau „+N” (peste N zile)."

/// Răspuns gol = se păstrează termenul curent; „-” = fără termen.
let parseDueInput (today: DateTime) (current: DateTime option) (text: string) : Result<DateTime option, string> =
    match text.Trim().ToLowerInvariant() with
    | "" -> Ok current
    | "-" -> Ok None
    | "azi" -> Ok(Some today.Date)
    | "maine"
    | "mâine" -> Ok(Some(today.Date.AddDays 1.0))
    | t when t.StartsWith "+" -> parseDaysFromToday today (t.Substring 1)
    | t -> parseCalendarDate t

let parseId (text: string) : Result<int, string> =
    match Int32.TryParse(text.Trim(), NumberStyles.None, inv) with
    | true, n when n > 0 -> Ok n
    | _ -> Error "Introdu numărul sarcinii (cel din coloana „Nr.”)."

let parseYesNo (text: string) : Result<bool, string> =
    match text.Trim().ToLowerInvariant() with
    | "d"
    | "da" -> Ok true
    | "n"
    | "nu"
    | "" -> Ok false
    | _ -> Error "Răspunde cu d (da) sau n (nu)."

let parseFilter (text: string) : Result<TaskFilter, string> =
    match text.Trim() with
    | "1" -> Ok All
    | "2" -> Ok PendingOnly
    | "3" -> Ok DoneOnly
    | "4" -> Ok OverdueOnly
    | _ -> Error "Alege un număr între 1 și 4."

// ---------------------------------------------------------------------------
// Scriere în consolă
// ---------------------------------------------------------------------------

let private withColor (color: ConsoleColor) (action: unit -> unit) =
    let previous = Console.ForegroundColor
    Console.ForegroundColor <- color

    try
        action ()
    finally
        Console.ForegroundColor <- previous

let printColored (color: ConsoleColor) (text: string) =
    withColor color (fun () -> printfn "%s" text)

let success = printColored ConsoleColor.Green
let failure = printColored ConsoleColor.Red
let warning = printColored ConsoleColor.Yellow
let info = printColored ConsoleColor.Cyan

let heading (text: string) =
    printfn ""
    printColored ConsoleColor.White text
    printfn "%s" (String('=', text.Length))

// ---------------------------------------------------------------------------
// Citire din consolă
// ---------------------------------------------------------------------------

/// None înseamnă că intrarea s-a terminat (Ctrl+Z, Ctrl+D sau sfârșitul
/// unui fișier redirecționat); aplicația se închide atunci normal.
let readLine (prompt: string) : string option =
    printf "%s" prompt

    match Console.ReadLine() with
    | null -> None
    | line -> Some line

/// Repetă întrebarea până când răspunsul poate fi interpretat de `parse`.
let rec readValid (prompt: string) (parse: string -> Result<'a, string>) : 'a option =
    readLine prompt
    |> Option.bind (fun line ->
        match parse line with
        | Ok value -> Some value
        | Error message ->
            failure message
            readValid prompt parse)

/// Citește datele unei sarcini. La editare (`current` = Some) un răspuns gol
/// păstrează valoarea existentă, afișată între paranteze drepte.
let readTaskInput (today: DateTime) (current: TodoTask option) : TaskInput option =
    let hint (show: TodoTask -> string) =
        match current with
        | Some t -> $" [{show t}]"
        | None -> ""

    let parseTitle (line: string) =
        match current, line.Trim() with
        | Some t, "" -> Ok t.Title
        | _ -> validateTitle line |> Result.mapError errorMessage

    let description (line: string) =
        match current, line.Trim() with
        | Some t, "" -> t.Description
        | _, "-" -> ""
        | _, text -> text

    let currentPriority =
        current |> Option.map (fun t -> t.Priority) |> Option.defaultValue Medium

    let currentDue = current |> Option.bind (fun t -> t.DueDate)

    // Termenul existent se poate păstra chiar dacă a trecut; unul nou nu.
    let parseDue (line: string) =
        parseDueInput today currentDue line
        |> Result.bind (fun due ->
            if due = currentDue then
                Ok due
            else
                validateDueDate today due |> Result.mapError errorMessage)

    let dueHint =
        hint (fun t -> t.DueDate |> Option.map formatDate |> Option.defaultValue "fără termen")

    let descriptionPrompt =
        match current with
        | Some t ->
            let shown = if t.Description = "" then "-" else t.Description
            $"Descriere [{shown}] („-” o șterge): "
        | None -> "Descriere (opțional): "

    maybe {
        let! title = readValid $"Titlu{hint (fun t -> t.Title)}: " parseTitle
        let! descriptionLine = readLine descriptionPrompt

        let! priority =
            readValid
                $"Prioritate 1=scăzută, 2=medie, 3=ridicată [{priorityLabel currentPriority}]: "
                (parsePriorityInput currentPriority)

        let! due =
            readValid
                $"Termen (zz.ll.aaaa, azi, mâine, +N; „-” = fără termen){dueHint}: "
                parseDue

        return
            { Title = title
              Description = description descriptionLine
              Priority = priority
              DueDate = due }
    }

// ---------------------------------------------------------------------------
// Afișarea sarcinilor
// ---------------------------------------------------------------------------

let private truncate (width: int) (text: string) =
    let oneLine = text.Replace('\n', ' ').Replace('\r', ' ')

    if oneLine.Length <= width then
        oneLine
    else
        oneLine.Substring(0, width - 1) + "…"

let private taskColor (today: DateTime) (task: TodoTask) =
    if isDone task then ConsoleColor.DarkGray
    elif isOverdue today task then ConsoleColor.Yellow
    elif task.Priority = High then ConsoleColor.Red
    else ConsoleColor.Gray

let printTaskTable (today: DateTime) (tasks: TodoTask list) =
    match tasks with
    | [] -> info "Nu există sarcini de afișat."
    | _ ->
        printfn "%-4s %-4s %-10s %-11s %s" "Nr." "" "Prioritate" "Termen" "Titlu"
        printfn "%s" (String('-', 76))

        tasks
        |> List.iter (fun t ->
            let mark = if isDone t then "[x]" else "[ ]"
            let due = t.DueDate |> Option.map formatDate |> Option.defaultValue "-"
            let late = if isOverdue today t then "  (întârziată)" else ""

            sprintf "%-4d %-4s %-10s %-11s %s%s" t.Id mark (priorityLabel t.Priority) due (truncate 40 t.Title) late
            |> printColored (taskColor today t))

let printTaskDetails (today: DateTime) (task: TodoTask) =
    let status =
        match task.Status with
        | Pending when isOverdue today task -> "în lucru (termen depășit)"
        | Pending -> "în lucru"
        | Done at -> $"finalizată la {formatDateTime at}"

    heading $"Sarcina nr. {task.Id}"
    printfn "  Titlu:      %s" task.Title
    printfn "  Descriere:  %s" (if task.Description = "" then "-" else task.Description)
    printfn "  Prioritate: %s" (priorityLabel task.Priority)
    printfn "  Termen:     %s" (task.DueDate |> Option.map formatDate |> Option.defaultValue "fără termen")
    printfn "  Creată la:  %s" (formatDateTime task.CreatedOn)
    printfn "  Stare:      %s" status

let progressBar (width: int) (percent: float) =
    let filled = int (Math.Round(percent / 100.0 * float width)) |> max 0 |> min width
    "[" + String('#', filled) + String('.', width - filled) + "]"

let printStatistics (stats: Statistics) =
    heading "Statistici"
    printfn "  Total:                     %s" (countTasks stats.Total)
    printfn "  Finalizate:                %d" stats.Completed
    printfn "  În lucru:                  %d" stats.Open
    printfn "  Cu termen depășit:         %d" stats.Overdue
    printfn "  Prioritate ridicată, deschise: %d" stats.HighPriorityOpen
    printfn "  Progres: %s %.0f%%" (progressBar 30 stats.CompletionPercent) stats.CompletionPercent

let printMenu (items: (string * string) list) (filter: TaskFilter) (tasks: TodoTask list) =
    heading $"LISTA DE SARCINI — {countTasks tasks.Length}, filtru: {filterLabel filter}"
    items |> List.iter (fun (key, label) -> printfn "  %2s. %s" key label)
