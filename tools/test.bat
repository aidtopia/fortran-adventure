@ECHO OFF
SETLOCAL
SET TESTRESULT=1
SET STARTINGDIR=%CD%
SET SRCDIR=%1
IF "%SRCDIR%"=="" SET SRCDIR=%~dp0\..\WOOD0350v1\
CD /D %SRCDIR%
ECHO:
ECHO Translating Adventure from Fortran to C...
RMDIR /Q/S target > NUL 2> NUL
MKDIR target > NUL 2> NUL
DIR /B *.f* > target\sources.txt
%~dp0..\fortran\x64\Debug\fortran @target\sources.txt
SET TESTRESULT=%ERRORLEVEL%
IF %TESTRESULT% NEQ 0 GOTO DONE
ECHO:
ECHO Compiling the C code...
CD /D target
SET OPTOPT=/Od
REM SET OPTOPT=/O2 /DNDEBUG /GF /GL /Gy
REM TODO:  Add /WX to the compiler options.
FOR %%f IN (ADV*.c) DO ^
cl /nologo /std:c11 /W4 %OPTOPT% /Zi /Fd%%~nf.pdb %%f /link /INCREMENTAL:NO
SET TESTRESULT=%ERRORLEVEL%
CD ..
IF %TESTRESULT% NEQ 0 GOTO DONE
ECHO:
ECHO Running Adventure...
FOR %%d IN (a*.dat) DO SET ADVENDATA=%%d
FOR %%f IN (target\adv*.exe) DO ^
ECHO %%f -t0:00 -d1-JAN-1977 -fTEXT=%ADVENDATA% > target\run.bat
SET TESTRESULT=%ERRORLEVEL%
IF %TESTRESULT% NEQ 0 GOTO DONE
TYPE target\run.bat
ECHO:
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
