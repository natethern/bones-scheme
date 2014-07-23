@echo off
set filename=%~pn1
bones %filename%.scm %2 %3 %4 %5 %6 %7 %8 %9 >%filename%.s
nasm -f win64 %filename%.s -o %filename%.obj
link /subsystem:console %filename%.obj libcmt.lib /nologo /out:%filename%.exe
rem with MSVC
%filename%.exe
gcc %filename%.obj -o %filename%.exe
rem with GCC
%filename%.exe

if errorlevel 1 set fail=1
