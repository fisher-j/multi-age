@echo off
setlocal

set "SOURCE=C:\Users\walki\Documents\zotero_library.json"
set "DEST=%~dp0..\zotero_library.json"

if exist "%SOURCE%" (
  copy /Y "%SOURCE%" "%DEST%" >nul
  if errorlevel 1 exit /b 1
  exit /b 0
)

if exist "%DEST%" exit /b 0

echo Zotero library not found at "%SOURCE%" and no repository snapshot exists.
exit /b 1
