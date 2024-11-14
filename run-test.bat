@echo off
SET progr=tests
SET module1=string_utf_8
IF NOT EXIST bin md bin
gfortran -c src\%module1%.f90 -J bin -o bin\%module1%.o
gfortran -o bin\%progr% test\%progr%.f90 bin\%module1%.o -I src -I bin
bin\%progr%
