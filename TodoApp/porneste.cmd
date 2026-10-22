@echo off
rem Pornește aplicația din folderul proiectului; datele rămân în sarcini.txt de aici.
chcp 65001 >nul
cd /d "%~dp0"
dotnet run --project src\TodoApp -- %*
pause
