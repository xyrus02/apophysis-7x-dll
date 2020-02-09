@echo off

setlocal EnableDelayedExpansion

call "%PROGRAMFILES(X86)%\Embarcadero\Studio\16.0\bin\rsvars.bat"
if errorlevel 1 goto cancel

:build32
msbuild /nologo /t:rebuild /verbosity:quiet /p:Platform=Win32 /p:Configuration=Release src\aporender.dproj
if errorlevel 1 goto cancel

:build64
msbuild /nologo /t:rebuild /verbosity:quiet /p:Platform=Win64 /p:Configuration=Release src\aporender.dproj
if errorlevel 1 goto cancel

goto done

:cancel
echo There have been error building the project 2>&1 

:done