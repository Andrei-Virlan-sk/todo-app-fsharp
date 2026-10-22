/// Punctul de intrare: bucla meniului și legătura dintre consolă (Ui),
/// reguli (Domain) și fișier (Storage). Starea aplicației nu este o
/// variabilă modificabilă, ci parametrul funcției recursive `loop`.
module TodoApp.Program

open System
open System.Text
open TodoApp.Domain
open TodoApp.Ui

type AppState =
    { Tasks: TodoTask list
      Filter: TaskFilter
      FilePath: string }

let private defaultFile = "sarcini.txt"

let private today () = DateTime.Today

let private reportSave (result: Result<unit, string>) (message: string) =
    match result with
    | Ok() -> success message
    | Error e -> failure $"Modificarea s-a făcut, dar nu a putut fi salvată în fișier: {e}"

/// Salvează lista nouă și o face starea curentă. Dacă salvarea eșuează,
/// modificarea rămâne în memorie și utilizatorul este avertizat.
let private commit (state: AppState) (outcome: Result<TodoTask list * string, DomainError>) =
    match outcome with
    | Error e ->
        failure (errorMessage e)
        state
    | Ok(tasks, message) ->
        reportSave (Storage.save state.FilePath tasks) message
        { state with Tasks = tasks }

let private readId (prompt: string) = readValid prompt parseId

/// Citește un număr de sarcină și o caută; None dacă nu există sau intrarea s-a terminat.
let private readExistingTask (prompt: string) (state: AppState) =
    readId prompt
    |> Option.bind (fun id ->
        match findTask id state.Tasks with
        | Ok task -> Some task
        | Error e ->
            failure (errorMessage e)
            None)

let private confirm (question: string) =
    readValid $"{question} (d/n): " parseYesNo |> Option.defaultValue false

// ---------------------------------------------------------------------------
// Comenzile meniului: fiecare primește starea și întoarce starea următoare
// ---------------------------------------------------------------------------

let private listCmd (state: AppState) =
    let visible = state.Tasks |> applyFilter (today ()) state.Filter |> sortTasks
    heading $"Sarcini ({filterLabel state.Filter}): {visible.Length} din {state.Tasks.Length}"
    printTaskTable (today ()) visible
    state

let private addCmd (state: AppState) =
    heading "Sarcină nouă"

    match readTaskInput (today ()) None with
    | None -> state
    | Some input ->
        addTask DateTime.Now input state.Tasks
        |> Result.map (fun (tasks, t) -> tasks, $"Sarcina „{t.Title}” a fost adăugată cu numărul {t.Id}.")
        |> commit state

let private detailsCmd (state: AppState) =
    readExistingTask "Numărul sarcinii: " state
    |> Option.iter (printTaskDetails (today ()))

    state

let private completeCmd (state: AppState) =
    match readId "Numărul sarcinii finalizate: " with
    | None -> state
    | Some id ->
        completeTask DateTime.Now id state.Tasks
        |> Result.map (fun (tasks, t) -> tasks, $"Sarcina „{t.Title}” a fost marcată ca finalizată.")
        |> commit state

let private reopenCmd (state: AppState) =
    match readId "Numărul sarcinii de redeschis: " with
    | None -> state
    | Some id ->
        reopenTask id state.Tasks
        |> Result.map (fun (tasks, t) -> tasks, $"Sarcina „{t.Title}” este din nou în lucru.")
        |> commit state

let private editCmd (state: AppState) =
    match readExistingTask "Numărul sarcinii de modificat: " state with
    | None -> state
    | Some task ->
        info "Apasă Enter pentru a păstra valoarea dintre paranteze."

        match readTaskInput (today ()) (Some task) with
        | None -> state
        | Some input ->
            editTask (today ()) task.Id input state.Tasks
            |> Result.map (fun (tasks, t) -> tasks, $"Sarcina {t.Id} a fost modificată.")
            |> commit state

let private deleteCmd (state: AppState) =
    match readExistingTask "Numărul sarcinii de șters: " state with
    | None -> state
    | Some task when confirm $"Sigur ștergi sarcina „{task.Title}”?" ->
        removeTask task.Id state.Tasks
        |> Result.map (fun (tasks, t) -> tasks, $"Sarcina „{t.Title}” a fost ștearsă.")
        |> commit state
    | Some _ ->
        info "Nu s-a șters nimic."
        state

let private searchCmd (state: AppState) =
    match readLine "Caută (în titlu și descriere): " with
    | None -> state
    | Some keyword ->
        let found = state.Tasks |> search keyword |> sortTasks
        heading $"Rezultate pentru „{keyword.Trim()}”: {countTasks found.Length}"
        printTaskTable (today ()) found
        state

let private filterCmd (state: AppState) =
    printfn "  1. Toate   2. În lucru   3. Finalizate   4. Întârziate"

    match readValid "Ce sarcini afișez? " parseFilter with
    | None -> state
    | Some filter ->
        success $"Filtrul este acum: {filterLabel filter}."
        listCmd { state with Filter = filter }

let private statsCmd (state: AppState) =
    printStatistics (statistics (today ()) state.Tasks)
    state

let private clearDoneCmd (state: AppState) =
    let kept, removed = removeCompleted state.Tasks

    if removed = 0 then
        info "Nu există sarcini finalizate."
        state
    elif confirm $"Sarcini finalizate care vor fi șterse: {removed}. Continui?" then
        commit state (Ok(kept, $"Sarcini șterse: {removed}."))
    else
        info "Nu s-a șters nimic."
        state

let private commands: (string * string * (AppState -> AppState)) list =
    [ "1", "Afișează sarcinile", listCmd
      "2", "Adaugă o sarcină", addCmd
      "3", "Detaliile unei sarcini", detailsCmd
      "4", "Marchează o sarcină ca finalizată", completeCmd
      "5", "Redeschide o sarcină finalizată", reopenCmd
      "6", "Modifică o sarcină", editCmd
      "7", "Șterge o sarcină", deleteCmd
      "8", "Caută", searchCmd
      "9", "Schimbă filtrul de afișare", filterCmd
      "10", "Statistici", statsCmd
      "11", "Șterge toate sarcinile finalizate", clearDoneCmd ]

let private menuItems =
    (commands |> List.map (fun (key, label, _) -> key, label)) @ [ "0", "Ieșire" ]

/// Bucla principală. Apelul recursiv este ultimul lucru făcut, deci
/// compilatorul îl transformă într-o buclă (fără să crească stiva).
let rec private loop (state: AppState) =
    printMenu menuItems state.Filter state.Tasks

    match readLine "Alege o opțiune: " |> Option.map (fun s -> s.Trim()) with
    | None
    | Some "0" -> ()
    | Some choice ->
        match commands |> List.tryFind (fun (key, _, _) -> key = choice) with
        | Some(_, _, run) -> loop (run state)
        | None ->
            failure "Opțiune necunoscută. Scrie numărul din fața comenzii."
            loop state

let private run (path: string) =
    match Storage.load path with
    | Error errors ->
        failure $"Fișierul „{path}” nu a putut fi citit:"
        errors |> List.iter (fun e -> failure $"  - {e}")
        failure "Corectează sau mută fișierul, apoi pornește din nou aplicația."
        1
    | Ok tasks ->
        info $"Sarcinile se salvează în: {IO.Path.GetFullPath path}"
        let overdue = tasks |> List.filter (isOverdue (today ())) |> List.length

        if overdue > 0 then
            warning $"Atenție: {countTasks overdue} cu termenul depășit."

        loop
            { Tasks = tasks
              Filter = All
              FilePath = path }

        info "La revedere!"
        0

let private usage =
    $"""Aplicație pentru gestionarea sarcinilor (To-Do List)

Utilizare:
  TodoApp                    folosește fișierul {defaultFile} din folderul curent
  TodoApp --fisier <cale>    folosește alt fișier de date
  TodoApp --help             afișează acest mesaj"""

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Encoding.UTF8

    // Diacriticele trebuie citite corect în ambele cazuri: din fișier sau
    // pipe se citește UTF-8 (cu BOM eliminat), iar de la tastatură .NET
    // folosește API-ul Unicode al consolei când codificarea este UTF-16.
    if Console.IsInputRedirected then
        Console.SetIn(new IO.StreamReader(Console.OpenStandardInput(), UTF8Encoding false))
    else
        Console.InputEncoding <- Encoding.Unicode

    match argv with
    | [||] -> run defaultFile
    | [| "--fisier"; path |]
    | [| "-f"; path |] -> run path
    | [| "--help" |]
    | [| "-h" |]
    | [| "/?" |] ->
        printfn "%s" usage
        0
    | _ ->
        failure "Argumente necunoscute."
        printfn "%s" usage
        2
