module TodoApp.Tests.UiTests

open System
open Xunit
open TodoApp.Domain
open TodoApp.Ui

let today = DateTime(2026, 10, 22)

[<Fact>]
let ``prioritatea: răspuns gol păstrează valoarea curentă`` () =
    Assert.Equal(Ok High, parsePriorityInput High "")
    Assert.Equal(Ok Low, parsePriorityInput High "1")
    Assert.Equal(Ok Medium, parsePriorityInput High " 2 ")
    Assert.Equal(Ok High, parsePriorityInput Low "3")
    Assert.True(parsePriorityInput Low "5" |> Result.isError)

[<Fact>]
let ``termenul se poate scrie în mai multe feluri`` () =
    let current = Some(DateTime(2026, 12, 1))
    Assert.Equal(Ok current, parseDueInput today current "")
    Assert.Equal(Ok None, parseDueInput today current "-")
    Assert.Equal(Ok(Some today), parseDueInput today None "azi")
    Assert.Equal(Ok(Some(today.AddDays 1.0)), parseDueInput today None "mâine")
    Assert.Equal(Ok(Some(today.AddDays 1.0)), parseDueInput today None "Maine")
    Assert.Equal(Ok(Some(today.AddDays 7.0)), parseDueInput today None "+7")
    Assert.Equal(Ok(Some(DateTime(2026, 11, 5))), parseDueInput today None "05.11.2026")
    Assert.Equal(Ok(Some(DateTime(2026, 11, 5))), parseDueInput today None "5.11.2026")

[<Theory>]
[<InlineData("31.02.2026")>]
[<InlineData("2026-11-05")>]
[<InlineData("+")>]
[<InlineData("+-3")>]
[<InlineData("ieri")>]
let ``termenele greșite sunt respinse`` (text: string) =
    Assert.True(parseDueInput today None text |> Result.isError)

[<Fact>]
let ``numărul sarcinii trebuie să fie pozitiv`` () =
    Assert.Equal(Ok 12, parseId " 12 ")
    Assert.True(parseId "0" |> Result.isError)
    Assert.True(parseId "-3" |> Result.isError)
    Assert.True(parseId "abc" |> Result.isError)

[<Fact>]
let ``confirmarea: gol înseamnă nu`` () =
    Assert.Equal(Ok true, parseYesNo "D")
    Assert.Equal(Ok true, parseYesNo "da")
    Assert.Equal(Ok false, parseYesNo "")
    Assert.Equal(Ok false, parseYesNo "nu")
    Assert.True(parseYesNo "poate" |> Result.isError)

[<Theory>]
[<InlineData(0, "0 sarcini")>]
[<InlineData(1, "1 sarcină")>]
[<InlineData(2, "2 sarcini")>]
[<InlineData(19, "19 sarcini")>]
[<InlineData(20, "20 de sarcini")>]
[<InlineData(101, "101 sarcini")>]
[<InlineData(120, "120 de sarcini")>]
let ``pluralul din română`` (n: int, expected: string) = Assert.Equal(expected, countTasks n)

[<Fact>]
let ``bara de progres are mereu aceeași lățime`` () =
    Assert.Equal("[..........]", progressBar 10 0.0)
    Assert.Equal("[#####.....]", progressBar 10 50.0)
    Assert.Equal("[##########]", progressBar 10 100.0)
