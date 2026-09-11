@echo off
setlocal

set "PRODUCT=%~1"

if "%PRODUCT%"=="" goto :usage

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
pushd publication
quarto render
set "RESULT=%ERRORLEVEL%"
popd
exit /b %RESULT%

:render_legacy
pushd analysis
quarto render --profile %~1
set "RESULT=%ERRORLEVEL%"
popd
exit /b %RESULT%

:usage
echo Usage: render-products.bat ^<product^>
echo Products: lab-book, thesis, committee-slides, thesis-defense-slides, publication
exit /b 1
