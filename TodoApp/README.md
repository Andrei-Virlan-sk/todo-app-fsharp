# TodoApp — listă de sarcini în F#

Aplicație de consolă pentru gestionarea sarcinilor (To-Do List), scrisă în F#
în stil funcțional.

## Cerințe

- .NET SDK 10 (`winget install Microsoft.DotNet.SDK.10`)

## Pornire

```
dotnet run --project src/TodoApp
```

sau dublu-clic pe `porneste.cmd`. Sarcinile se salvează în `sarcini.txt` din
folderul curent; alt fișier se alege cu `--fisier <cale>`.

## Teste

```
dotnet test
```

## Structură

| Fișier | Rol |
|---|---|
| `src/TodoApp/Prelude.fs` | expresii de calcul `result { }` și `maybe { }` |
| `src/TodoApp/Domain.fs` | tipuri și reguli (funcții pure) |
| `src/TodoApp/Storage.fs` | salvare/încărcare în fișier text |
| `src/TodoApp/Ui.fs` | texte, culori, citirea datelor din consolă |
| `src/TodoApp/Program.fs` | meniul și bucla principală |
| `tests/TodoApp.Tests` | 49 de teste xUnit |
