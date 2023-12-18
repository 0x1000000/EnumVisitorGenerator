@echo off

SET rootDir=%~dp0
SET PROJ=%rootDir%\EnumVisitorGenerator\EnumVisitorGenerator.csproj
SET OUTDIRPACK=%rootDir%\package

dotnet pack "%PROJ%" -o "%OUTDIRPACK%" -c Release