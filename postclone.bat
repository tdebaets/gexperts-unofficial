@echo off

setlocal

echo Creating directories...

mkdir Output
if errorlevel 1 goto failed

mkdir Output\DCU
if errorlevel 1 goto failed

echo Success!
goto exit

:failed
echo *** FAILED ***
:failed2
exit /b 1

:exit
