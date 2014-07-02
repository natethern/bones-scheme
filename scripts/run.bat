@echo off
set filename=%~pn1
bones %filename%.scm >%filename%.s
nasm -f win64 %filename%.s -o %filename%.obj
link /subsystem:console %filename%.obj libcmt.lib /nologo /out:%filename%.exe
%filename%.exe
gcc %filename%.obj -o %filename%.exe
%filename%.exe

if errorlevel 1 set fail=1
