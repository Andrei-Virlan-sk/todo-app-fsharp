/// Modelul și regulile aplicației. Tot ce este aici e pur: funcțiile primesc
/// lista de sarcini și întorc o listă nouă, fără să modifice nimic și fără
/// să citească ceasul sau consola (data curentă vine ca parametru).
module TodoApp.Domain

open System

// ---------------------------------------------------------------------------
// Tipuri
// ---------------------------------------------------------------------------

type Priority =
    | Low
    | Medium
    | High

type Status =
    | Pending
    | Done of completedOn: DateTime

type TodoTask =
    { Id: int
      Title: string
      Description: string
      Priority: Priority
      DueDate: DateTime option
      CreatedOn: DateTime
      Status: Status }

/// Datele introduse de utilizator la adăugarea sau editarea unei sarcini.
type TaskInput =
    { Title: string
      Description: string
      Priority: Priority
      DueDate: DateTime option }

type TaskFilter =
    | All
    | PendingOnly
    | DoneOnly
    | OverdueOnly

type DomainError =
    | EmptyTitle
    | TitleTooLong of maxLength: int
    | TaskNotFound of id: int
    | AlreadyDone of id: int
    | NotDone of id: int
    | DueDateInPast

type Statistics =
    { Total: int
      Completed: int
      Open: int
      Overdue: int
      HighPriorityOpen: int
      CompletionPercent: float }

// ---------------------------------------------------------------------------
// Validare
// ---------------------------------------------------------------------------

let maxTitleLength = 100

let validateTitle (title: string) : Result<string, DomainError> =
    let trimmed = if isNull title then "" else title.Trim()

    if trimmed = "" then Error EmptyTitle
    elif trimmed.Length > maxTitleLength then Error(TitleTooLong maxTitleLength)
    else Ok trimmed

let validateDueDate (today: DateTime) (due: DateTime option) : Result<DateTime option, DomainError> =
    match due with
    | Some d when d.Date < today.Date -> Error DueDateInPast
    | _ -> Ok(due |> Option.map (fun d -> d.Date))

// ---------------------------------------------------------------------------
// Interogări
// ---------------------------------------------------------------------------

let isDone (task: TodoTask) =
    match task.Status with
    | Done _ -> true
    | Pending -> false

let isOverdue (today: DateTime) (task: TodoTask) =
    match task.Status, task.DueDate with
    | Pending, Some due -> due.Date < today.Date
    | _ -> false

let nextId (tasks: TodoTask list) =
    match tasks with
    | [] -> 1
    | _ -> (tasks |> List.map (fun t -> t.Id) |> List.max) + 1

let findTask (id: int) (tasks: TodoTask list) : Result<TodoTask, DomainError> =
    match tasks |> List.tryFind (fun t -> t.Id = id) with
    | Some task -> Ok task
    | None -> Error(TaskNotFound id)

let matchesFilter (today: DateTime) (filter: TaskFilter) (task: TodoTask) =
    match filter with
    | All -> true
    | PendingOnly -> not (isDone task)
    | DoneOnly -> isDone task
    | OverdueOnly -> isOverdue today task

let applyFilter today filter (tasks: TodoTask list) =
    tasks |> List.filter (matchesFilter today filter)

let private priorityRank =
    function
    | High -> 0
    | Medium -> 1
    | Low -> 2

/// Ordinea de afișare: întâi cele nefinalizate, apoi după prioritate,
/// după termen (cele fără termen la final) și, la egalitate, după număr.
let sortTasks (tasks: TodoTask list) =
    tasks
    |> List.sortBy (fun t ->
        isDone t, priorityRank t.Priority, Option.defaultValue DateTime.MaxValue t.DueDate, t.Id)

/// Caută cuvântul în titlu și descriere, fără să țină cont de litere mari/mici.
let search (keyword: string) (tasks: TodoTask list) =
    let k = keyword.Trim()

    let contains (text: string) =
        text.Contains(k, StringComparison.CurrentCultureIgnoreCase)

    if k = "" then
        tasks
    else
        tasks |> List.filter (fun t -> contains t.Title || contains t.Description)

let statistics (today: DateTime) (tasks: TodoTask list) : Statistics =
    let count predicate = tasks |> List.filter predicate |> List.length
    let total = List.length tasks
    let completed = count isDone

    { Total = total
      Completed = completed
      Open = total - completed
      Overdue = count (isOverdue today)
      HighPriorityOpen = count (fun t -> not (isDone t) && t.Priority = High)
      CompletionPercent =
        if total = 0 then
            0.0
        else
            float completed * 100.0 / float total }

// ---------------------------------------------------------------------------
// Operații care schimbă lista (întorc o listă nouă)
// ---------------------------------------------------------------------------

/// Adaugă o sarcină nouă. Întoarce lista nouă și sarcina creată.
let addTask (now: DateTime) (input: TaskInput) (tasks: TodoTask list) =
    result {
        let! title = validateTitle input.Title
        let! due = validateDueDate now input.DueDate

        let task: TodoTask =
            { Id = nextId tasks
              Title = title
              Description = input.Description.Trim()
              Priority = input.Priority
              DueDate = due
              CreatedOn = now
              Status = Pending }

        return tasks @ [ task ], task
    }

/// Aplică `change` pe sarcina cu numărul dat și o pune la loc în listă.
let private updateTask (id: int) (change: TodoTask -> Result<TodoTask, DomainError>) (tasks: TodoTask list) =
    result {
        let! existing = findTask id tasks
        let! updated = change existing
        let newTasks = tasks |> List.map (fun t -> if t.Id = id then updated else t)
        return newTasks, updated
    }

let completeTask (now: DateTime) (id: int) tasks =
    tasks
    |> updateTask id (fun t ->
        match t.Status with
        | Done _ -> Error(AlreadyDone id)
        | Pending -> Ok { t with Status = Done now })

let reopenTask (id: int) tasks =
    tasks
    |> updateTask id (fun t ->
        match t.Status with
        | Pending -> Error(NotDone id)
        | Done _ -> Ok { t with Status = Pending })

/// Modifică titlul, descrierea, prioritatea și termenul. Un termen deja
/// existent poate fi păstrat chiar dacă a trecut; doar unul nou trebuie
/// să nu fie în trecut.
let editTask (today: DateTime) (id: int) (input: TaskInput) tasks =
    tasks
    |> updateTask id (fun t ->
        result {
            let! title = validateTitle input.Title

            let! due =
                if input.DueDate = t.DueDate then
                    Ok t.DueDate
                else
                    validateDueDate today input.DueDate

            return
                { t with
                    Title = title
                    Description = input.Description.Trim()
                    Priority = input.Priority
                    DueDate = due }
        })

let removeTask (id: int) (tasks: TodoTask list) =
    result {
        let! existing = findTask id tasks
        return tasks |> List.filter (fun t -> t.Id <> id), existing
    }

/// Șterge toate sarcinile finalizate. Întoarce lista rămasă și câte s-au șters.
let removeCompleted (tasks: TodoTask list) =
    let kept, removed = tasks |> List.partition (isDone >> not)
    kept, List.length removed
