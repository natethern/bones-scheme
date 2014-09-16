@echo off

set fail=0

mkdir tmp

echo tests\r4rstest.scm
call scripts\run tests\r4rstest.scm -case-insensitive
for %%f in (tests\fac.scm tests\tak.scm tests\mandelbrot.scm tests\r5rs_pitfalls.scm tests\dynamic.scm tests\compiler.scm tests\forth.scm) do echo %%f & call scripts\run %%f

call scripts\run tests\null.scm
dir tests\null.exe

echo.

if %fail% neq 0 (
   echo SOME TESTS FAILED.
) else (
   echo all tests succeeded.
)
