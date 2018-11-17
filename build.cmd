@echo off
cls

.paket\paket.exe restore -s
if errorlevel 1 (
  exit /b %errorlevel%
)
dotnet run --project src\Build\Build.fsproj %*
