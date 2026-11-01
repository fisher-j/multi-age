@echo off
setlocal
if "%~1"=="" goto :usage
call "%~dp0select-r-runtime.bat" publication
if errorlevel 1 exit /b 1
pushd "%~dp0publication"
if errorlevel 1 exit /b 1
"%PRODUCT_R_HOME%\bin\Rscript.exe" --vanilla scripts/run-analysis.R %*
set "RESULT=%ERRORLEVEL%"
popd
exit /b %RESULT%
:usage
echo Usage: run-publication.bat ^<restore^|f01^|rd04^|fuel^|regeneration^|all^|check^>
exit /b 1
