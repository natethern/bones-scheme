@echo off

set fail=0

mkdir tmp

for %%f in (tests\fac.scm tests\tak.scm tests\mandelbrot.scm tests\r4rstest.scm tests\r5rs_pitfalls.scm tests\dynamic.scm tests\compiler.scm tests\forth.scm) do echo %%f & call run %%f

echo.

if %fail% neq 0 (
   echo SOME TESTS FAILED.
) else (
   echo all tests succeeded.
)
