@ECHO OFF
SETLOCAL
SET TESTRESULT=1
ECHO:
SET STARTINGDIR=%CD%
CD %~dp0\..\WOOD0350v2\
ECHO Translating Adventure from Fortran to C...
%~dp0\..\fortran\x64\Debug\fortran ADVENT.for
SET TESTRESULT=%ERRORLEVEL%
IF %TESTRESULT% NEQ 0 GOTO DONE
ECHO:
ECHO Compiling the C code...
REM TODO:  Add /WX to the compiler options.
REM Debug version
cl /nologo /std:c11 /W4 /Od /ZI /Fo:target\ /Fe:target\ /Fd:target\ target\ADV*.c
REM Release version
REM cl /nologo /std:c11 /W4 /O2 /DNDEBUG /GF /ZI /Fo:target\ /Fe:target\ /Fd:target\ target\ADV*.c
SET TESTRESULT=%ERRORLEVEL%
IF %TESTRESULT% NEQ 0 GOTO DONE
ECHO:
ECHO Running Adventure...
ECHO:
REM The exact executable name depends on the source name, so we'll find it with
REM a directory search.
DIR /B /S target\adv*.exe > target\run.bat
SET TESTRESULT=%ERRORLEVEL%
IF %TESTRESULT% NEQ 0 GOTO DONE
CALL target\run.bat
SET TESTRESULT=%ERRORLEVEL%
ECHO:
ECHO Adventure completed.
:DONE
CD /D %STARTINGDIR%
ECHO:
IF %TESTRESULT% NEQ 0 ECHO Failed (exit code was %TESTRESULT%)
IF %TESTRESULT% EQU 0 ECHO SUCCESS!
ECHO ON
@EXIT /B %TESTRESULT%
