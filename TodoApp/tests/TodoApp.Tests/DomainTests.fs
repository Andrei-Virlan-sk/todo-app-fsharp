module TodoApp.Tests.DomainTests

open System
open Xunit
open TodoApp.Domain

let now = DateTime(2026, 10, 22, 10, 30, 0)
let today = now.Date

let input title : TaskInput =
    { Title = title
      Description = ""
      Priority = Medium
      DueDate = None }

/// Adaugă sarcini una după alta; testul pică dacă vreuna e respinsă.
let addAll (inputs: TaskInput list) =
    inputs
    |> List.fold
        (fun tasks i ->
            match addTask now i tasks with
            | Ok(newTasks, _) -> newTasks
            | Error e -> failwithf "adăugare respinsă: %A" e)
        []

let expectError (expected: DomainError) (outcome: Result<'a, DomainError>) =
    match outcome with
    | Error e -> Assert.Equal(expected, e)
    | Ok value -> failwithf "se aștepta eroarea %A, dar a reușit: %A" expected value

[<Fact>]
let ``sarcinile primesc numere consecutive`` () =
    let tasks = addAll [ input "a"; input "b"; input "c" ]
    Assert.Equal<int list>([ 1; 2; 3 ], tasks |> List.map (fun t -> t.Id))

[<Fact>]
let ``titlul este curățat de spații și sarcina nouă e în lucru`` () =
    match addTask now (input "  Cumpără pâine  ") [] with
    | Ok(_, task) ->
        Assert.Equal("Cumpără pâine", task.Title)
        Assert.Equal(Pending, task.Status)
        Assert.Equal(now, task.CreatedOn)
    | Error e -> failwithf "%A" e

[<Theory>]
[<InlineData("")>]
[<InlineData("   ")>]
let ``titlul gol este respins`` (title: string) =
    addTask now (input title) [] |> expectError EmptyTitle

[<Fact>]
let ``titlul prea lung este respins`` () =
    addTask now (input (String('x', maxTitleLength + 1))) []
    |> expectError (TitleTooLong maxTitleLength)

[<Fact>]
let ``termenul din trecut este respins, cel de azi este acceptat`` () =
    addTask now { input "x" with DueDate = Some(today.AddDays -1.0) } []
    |> expectError DueDateInPast

    Assert.True(addTask now { input "x" with DueDate = Some today } [] |> Result.isOk)

[<Fact>]
let ``o sarcină finalizată nu mai poate fi finalizată`` () =
    let tasks = addAll [ input "a" ]

    match completeTask now 1 tasks with
    | Ok(done', task) ->
        Assert.Equal(Done now, task.Status)
        completeTask now 1 done' |> expectError (AlreadyDone 1)
    | Error e -> failwithf "%A" e

[<Fact>]
let ``redeschiderea cere o sarcină finalizată`` () =
    let tasks = addAll [ input "a" ]
    reopenTask 1 tasks |> expectError (NotDone 1)

    let reopened =
        completeTask now 1 tasks |> Result.bind (fun (t, _) -> reopenTask 1 t)

    match reopened with
    | Ok(_, task) -> Assert.Equal(Pending, task.Status)
    | Error e -> failwithf "%A" e

[<Fact>]
let ``operațiile pe un număr inexistent dau TaskNotFound`` () =
    let tasks = addAll [ input "a" ]
    completeTask now 9 tasks |> expectError (TaskNotFound 9)
    reopenTask 9 tasks |> expectError (TaskNotFound 9)
    removeTask 9 tasks |> expectError (TaskNotFound 9)
    editTask today 9 (input "b") tasks |> expectError (TaskNotFound 9)

[<Fact>]
let ``operațiile nu modifică lista inițială`` () =
    let tasks = addAll [ input "a"; input "b" ]
    let _ = removeTask 1 tasks
    let _ = completeTask now 2 tasks
    Assert.Equal(2, tasks.Length)
    Assert.True(tasks |> List.forall (isDone >> not))

[<Fact>]
let ``editarea schimbă câmpurile și păstrează un termen vechi deja depășit`` () =
    let old: TodoTask =
        { Id = 1
          Title = "vechi"
          Description = ""
          Priority = Low
          DueDate = Some(today.AddDays -5.0)
          CreatedOn = now.AddDays -10.0
          Status = Pending }

    let changes =
        { Title = "nou"
          Description = " detalii "
          Priority = High
          DueDate = old.DueDate }

    match editTask today 1 changes [ old ] with
    | Ok(_, task) ->
        Assert.Equal("nou", task.Title)
        Assert.Equal("detalii", task.Description)
        Assert.Equal(High, task.Priority)
        Assert.Equal(old.DueDate, task.DueDate)
        Assert.Equal(old.CreatedOn, task.CreatedOn)
    | Error e -> failwithf "%A" e

    editTask today 1 { changes with DueDate = Some(today.AddDays -1.0) } [ old ]
    |> expectError DueDateInPast

[<Fact>]
let ``ștergerea scoate doar sarcina cerută`` () =
    let tasks = addAll [ input "a"; input "b"; input "c" ]

    match removeTask 2 tasks with
    | Ok(rest, removed) ->
        Assert.Equal("b", removed.Title)
        Assert.Equal<int list>([ 1; 3 ], rest |> List.map (fun t -> t.Id))
    | Error e -> failwithf "%A" e

[<Fact>]
let ``după ștergere numărul următor nu refolosește unul mai mic`` () =
    let tasks = addAll [ input "a"; input "b"; input "c" ]

    let afterRemove =
        match removeTask 1 tasks with
        | Ok(rest, _) -> rest
        | Error e -> failwithf "%A" e

    Assert.Equal(4, nextId afterRemove)

[<Fact>]
let ``removeCompleted păstrează doar sarcinile în lucru`` () =
    let tasks =
        addAll [ input "a"; input "b"; input "c" ]
        |> completeTask now 1
        |> Result.bind (fun (t, _) -> completeTask now 3 t)
        |> Result.map fst
        |> Result.defaultWith (fun e -> failwithf "%A" e)

    let kept, removed = removeCompleted tasks
    Assert.Equal(2, removed)
    Assert.Equal<int list>([ 2 ], kept |> List.map (fun t -> t.Id))

[<Fact>]
let ``o sarcină e întârziată doar dacă e în lucru și termenul a trecut`` () =
    let task: TodoTask =
        { Id = 1
          Title = "x"
          Description = ""
          Priority = Medium
          DueDate = Some(today.AddDays -1.0)
          CreatedOn = now
          Status = Pending }

    Assert.True(isOverdue today task)
    Assert.False(isOverdue today { task with DueDate = Some today })
    Assert.False(isOverdue today { task with DueDate = None })
    Assert.False(isOverdue today { task with Status = Done now })

[<Fact>]
let ``sortarea pune întâi sarcinile în lucru, apoi prioritatea și termenul`` () =
    let make id priority due status : TodoTask =
        { Id = id
          Title = string id
          Description = ""
          Priority = priority
          DueDate = due
          CreatedOn = now
          Status = status }

    let tasks =
        [ make 1 Low None Pending
          make 2 High None (Done now)
          make 3 High (Some(today.AddDays 5.0)) Pending
          make 4 High (Some(today.AddDays 1.0)) Pending
          make 5 Medium None Pending
          make 6 High None Pending ]

    Assert.Equal<int list>([ 4; 3; 6; 5; 1; 2 ], sortTasks tasks |> List.map (fun t -> t.Id))

[<Fact>]
let ``filtrele aleg sarcinile potrivite`` () =
    let tasks =
        addAll [ input "a"; input "b"; { input "c" with DueDate = Some today } ]
        |> completeTask now 1
        |> Result.map fst
        |> Result.defaultWith (fun e -> failwithf "%A" e)

    let ids filter =
        applyFilter (today.AddDays 1.0) filter tasks |> List.map (fun t -> t.Id)

    Assert.Equal<int list>([ 1; 2; 3 ], ids All)
    Assert.Equal<int list>([ 2; 3 ], ids PendingOnly)
    Assert.Equal<int list>([ 1 ], ids DoneOnly)
    Assert.Equal<int list>([ 3 ], ids OverdueOnly)

[<Fact>]
let ``căutarea nu ține cont de litere mari și caută și în descriere`` () =
    let tasks =
        addAll
            [ input "Temă la Haskell"
              { input "Cumpărături" with Description = "lapte, ouă" }
              input "Sală" ]

    Assert.Equal<int list>([ 1 ], search "haskell" tasks |> List.map (fun t -> t.Id))
    Assert.Equal<int list>([ 2 ], search "LAPTE" tasks |> List.map (fun t -> t.Id))
    Assert.Equal(3, (search "  " tasks).Length)

[<Fact>]
let ``statisticile numără corect`` () =
    let tasks =
        addAll
            [ { input "a" with Priority = High }
              { input "b" with DueDate = Some today }
              input "c"
              input "d" ]
        |> completeTask now 3
        |> Result.map fst
        |> Result.defaultWith (fun e -> failwithf "%A" e)

    let stats = statistics (today.AddDays 1.0) tasks
    Assert.Equal(4, stats.Total)
    Assert.Equal(1, stats.Completed)
    Assert.Equal(3, stats.Open)
    Assert.Equal(1, stats.Overdue)
    Assert.Equal(1, stats.HighPriorityOpen)
    Assert.Equal(25.0, stats.CompletionPercent)

[<Fact>]
let ``statisticile unei liste goale nu împart la zero`` () =
    Assert.Equal(0.0, (statistics today []).CompletionPercent)
