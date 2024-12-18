@echo off
SET progr=example
SET module1=string_utf_8
IF NOT EXIST bin md bin
gfortran -c src\%module1%.f90 -J bin -o bin\%module1%.o
gfortran -o bin\%progr% app\%progr%.f90 bin\%module1%.o -I src -I bin
REM For Visual Studio Code
copy /Y bin\%module1%.mod app
REM Set the active console code page to UTF-8
chcp 65001
bin\%progr%
