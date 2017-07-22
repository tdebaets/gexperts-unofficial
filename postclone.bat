@echo off

setlocal

echo Setting up repository...

git config pull.rebase preserve
if errorlevel 1 goto failed

echo Creating directories...

if not exist Output mkdir Output
if errorlevel 1 goto failed

if not exist Output\DCU mkdir Output\DCU
if errorlevel 1 goto failed

echo Success!
goto exit

:failed
echo *** FAILED ***
:failed2
exit /b 1

:exit
