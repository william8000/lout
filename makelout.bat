@echo off

rem Make lout for windows

if not exist z01.c goto notloutdir

if exist lout.exe del lout.exe
if exist prg2lout.exe del prg2lout.exe
if exist z01.obj del *.obj

call complout /Felout.exe z*.c

if not exist lout.exe goto fail

call complout /Oity2 prg2lout.c

if not exist prg2lout.exe goto fail

echo Compilation finished.
echo If it worked, run instlout next to install.
goto end

:notloutdir
  echo z01.c not found.
  echo This is not a lout source directory.
  goto end

:fail
  echo The build did not work.
  echo Check for compile errors.
  goto end

:end
