@echo off
setlocal

set "PRODUCT=%~1"

if "%PRODUCT%"=="" goto :usage

shift
set "RENDER_ARGS="

:collect_args
if "%~1"=="" goto :select_product
set "RENDER_ARGS=%RENDER_ARGS% %~1"
shift
goto :collect_args

:select_product

if /I "%PRODUCT%"=="lab-book" goto :lab_book
if /I "%PRODUCT%"=="thesis" goto :thesis
if /I "%PRODUCT%"=="committee-slides" goto :committee_slides
if /I "%PRODUCT%"=="thesis-defense-slides" goto :thesis_defense_slides
if /I "%PRODUCT%"=="publication" goto :publication

echo Unknown product: %PRODUCT%
goto :usage

:lab_book
call :render_legacy analysis
exit /b %ERRORLEVEL%

:thesis
call :render_legacy thesis
exit /b %ERRORLEVEL%

:committee_slides
call :render_legacy presentation
exit /b %ERRORLEVEL%

:thesis_defense_slides
call :render_legacy defense
exit /b %ERRORLEVEL%

:publication
call "%~dp0select-r-runtime.bat" publication
if errorlevel 1 exit /b 1
rem Windows sandbox sessions may redirect LOCALAPPDATA away from installed R
rem packages. R expands %%v to its major.minor version at startup. Keep an
rem explicit caller-supplied library, and leave legacy renv products alone.
if not defined R_LIBS_USER if exist "%USERPROFILE%\AppData\Local\R\win-library" set "R_LIBS_USER=%USERPROFILE%\AppData\Local\R\win-library\%%v"
pushd "%~dp0publication"
if errorlevel 1 exit /b 1
quarto render %RENDER_ARGS%
set "RESULT=%ERRORLEVEL%"
popd
exit /b %RESULT%

:render_legacy
call "%~dp0select-r-runtime.bat" legacy
if errorlevel 1 exit /b 1
pushd "%~dp0analysis"
if errorlevel 1 exit /b 1
quarto render --profile %~1 %RENDER_ARGS%
set "RESULT=%ERRORLEVEL%"
popd
exit /b %RESULT%

:usage
echo Usage: render-products.bat ^<product^> [quarto render options]
echo Products: lab-book, thesis, committee-slides, thesis-defense-slides, publication
echo Examples:
echo   render-products.bat thesis --to pdf
echo   render-products.bat thesis --to docx
echo   render-products.bat publication --to html
exit /b 1
