@echo off
setlocal

set "PRODUCT=%~1"
set "TARGET=%~2"

if "%PRODUCT%"=="" goto :usage

if "%TARGET%"=="" set "TARGET=%TEMP%\multi-age-legacy-regression-%PRODUCT%-%RANDOM%"

if exist "%TARGET%" (
  echo Regression directory already exists: %TARGET%
  echo Choose a new empty directory or inspect the existing run.
  exit /b 1
)

mkdir "%TARGET%"
if errorlevel 1 exit /b 1

git archive --format=tar HEAD | tar -xf - -C "%TARGET%"
if errorlevel 1 (
  echo Could not create the regression copy.
  exit /b 1
)

rem Reuse the active project's installed renv package-library root for a
rem read-only render. renv appends the R-version and platform directory itself.
rem The archived copy has its own source and output files, but does not carry an
rem installed package library in Git.
if exist "%~dp0analysis\renv\library" set "RENV_PATHS_LIBRARY=%~dp0analysis\renv\library"

:library_found
if "%RENV_PATHS_LIBRARY%"=="" (
  echo No active renv package library was found.
  echo Restore the locked environment before running a full regression render.
  exit /b 1
)

pushd "%TARGET%"
call render-products.bat "%PRODUCT%"
set "RESULT=%ERRORLEVEL%"
popd

if not "%RESULT%"=="0" (
  echo Regression render failed. Inspect: %TARGET%
  exit /b %RESULT%
)

echo Regression render succeeded. Inspect: %TARGET%
exit /b 0

:usage
echo Usage: render-legacy-regression.bat ^<product^> [directory]
echo Example: render-legacy-regression.bat thesis C:\temp\multi-age-thesis-regression
exit /b 1
