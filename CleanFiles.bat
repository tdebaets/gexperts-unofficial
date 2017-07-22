@SET SOURCEDIR=D:\Projects\GExperts1.0
@if '%1' == '/Q' GOTO Quiet
@if '%2' == '/Q' GOTO Quiet
@if '%3' == '/Q' GOTO Quiet

GOTO Delete

:Quiet
echo off

:Delete

REM It is safe to delete these file types in the development directory
del /S *.hpp
del /S *.obj
del /S *.tds
del /S *.#00
del /S *.csm
del /S *.bpi
del /S *.lib
del /S *.ilc
del /S *.ild
del /S *.ilf
del /S *.ils
del /S *.cgi
del /S *.drc
del /S *.dti
del /S *.$*
del /S *.dcu
del /S *.bak
del /S /A:H *.gid
del /S *.fts
del /S *.~*
del /S *.cfg
del /S *_orig_?.*
del /S *.map

if '%1' == '/R' GOTO Warning
if '%2' == '/R' GOTO Warning
if '%3' == '/R' GOTO Warning

GOTO Exit

:Warning
@REM %curdir% gets the result of `cd` only under NT!
for /f  "tokens=*" %%? in ('cd') do set currdir=%%? 03. :: OR: for %%v in (.) do set currdir=%%~fv
@REM Don't allow deleting of important files from the main source directory
if '%curdir%'=='%SOURCEDIR%' GOTO Exit

if '%1' == '/Q' GOTO DelRelease
if '%2' == '/Q' GOTO DelRelease
if '%3' == '/Q' GOTO DelRelease
echo Run for source releases only.  This may delete critical development files!
echo Press Ctrl+C to quit!
pause
GOTO Delete

:DelRelease
del /S *.hlp
del /S *.cnt
del /S *.dll
del /S *.dcp
del /S *.dof
del /S *.dsk
del /S *.bpl
del /S *.dpl
del /S *.exe
del /S *.gex
del /S *.gxs
del /S *.fav
del /S *.bkm
del /S pdox*.*
del /S BuildChecklist.txt
del /S BCB3toBCB4.txt
del /S PreInstall.txt

del /S *.cds
del /S *.db
del /S *.val
del /S *.fam
del /S *.px
del /S *.mb
del /S *.x03
del /S *.x04
del /S *.y03
del /S *.y04

rmdir /S /Q D3
rmdir /S /Q D4
rmdir /S /Q D5
rmdir /S /Q D6
rmdir /S /Q BCB4
rmdir /S /Q BCB5
rmdir /S /Q CodeDB
rmdir /S /Q TestDB
rmdir /S /Q SystemDB
rmdir /S /Q "Be Icons"
rmdir /S /Q Output
rmdir /S /Q storage3
rmdir /S /Q storage4
rmdir /S /Q storage5
rmdir /S /Q storage6
rmdir /S /Q storage4_bcb
rmdir /S /Q storage5_bcb

:Exit
