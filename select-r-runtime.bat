@echo off
rem Called inside the caller's setlocal scope. Never changes the machine PATH.
set "RENV_CONFIG_USER_PROFILE=FALSE"
set "RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE"
if /I "%~1"=="legacy" goto :legacy
if not defined PUBLICATION_R_HOME set "PUBLICATION_R_HOME=C:\Program Files\R\R-4.6.2"
set "PRODUCT_R_HOME=%PUBLICATION_R_HOME%"
set "RENV_PROJECT=%~dp0publication"
set "RENV_PROFILE="
set "RENV_PATHS_LIBRARY="
set "RENV_PATHS_LIBRARY_ROOT="
set "R_PROFILE_USER=%~dp0publication\.Rprofile"
goto :validate
:legacy
if not defined THESIS_R_HOME set "THESIS_R_HOME=C:\Program Files\R\R-4.3.2"
set "PRODUCT_R_HOME=%THESIS_R_HOME%"
set "RENV_PROJECT=%~dp0analysis"
set "R_PROFILE_USER=%~dp0analysis\.Rprofile"
:validate
if not exist "%PRODUCT_R_HOME%\bin\Rscript.exe" (
  echo Rscript not found under "%PRODUCT_R_HOME%". Set PUBLICATION_R_HOME or THESIS_R_HOME.
  exit /b 1
)
set "QUARTO_R=%PRODUCT_R_HOME%\bin"
exit /b 0
