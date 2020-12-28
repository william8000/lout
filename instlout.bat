@echo off

rem Install lout

set INSTALLROOT=\scs
set PREFIX=%INSTALLROOT%\lout
set BINDIR=%PREFIX%\bin
set LOUTLIBDIR=%PREFIX%\lib
set LOUTDOCDIR=%PREFIX%\doc
set MANDIR=%PREFIX%\man

set ALTBINDIR=%INSTALLROOT%\tools\bin

if not exist lout.exe goto missingexe
if not exist prg2lout.exe goto missingexe

if not exist hyph goto missingdir
if not exist maps goto missingdir
if not exist data goto missingdir
if not exist font goto missingdir

rem (a) Installing lout and prg2lout binaries into BINDIR %BINDIR%

if not exist %INSTALLROOT% mkdir %INSTALLROOT%
if not exist %PREFIX% mkdir %PREFIX%
if not exist %BINDIR% mkdir %BINDIR%
copy /Y lout.exe %BINDIR%
copy /Y prg2lout.exe %BINDIR%

if exist %ALTBINDIR% copy /Y lout.exe %ALTBINDIR%
if exist %ALTBINDIR% copy /Y prg2lout.exe %ALTBINDIR%

rem (b) Installing library files into LOUTLIBDIR %LOUTLIBDIR%

if exist %LOUTLIBDIR% echo .. Removing old files in %LOUTLIBDIR%
if exist %LOUTLIBDIR% rmdir /s %LOUTLIBDIR%

if not exist %LOUTLIBDIR% mkdir %LOUTLIBDIR%

if not exist %LOUTLIBDIR%\include mkdir %LOUTLIBDIR%\include
 copy /Y include\*.* %LOUTLIBDIR%\include
if not exist %LOUTLIBDIR%\data mkdir %LOUTLIBDIR%\data
 copy /Y data\*.* %LOUTLIBDIR%\data
if not exist %LOUTLIBDIR%\hyph mkdir %LOUTLIBDIR%\hyph
 copy /Y hyph\*.* %LOUTLIBDIR%\hyph
if not exist %LOUTLIBDIR%\font mkdir %LOUTLIBDIR%\font
 copy /Y font\*.* %LOUTLIBDIR%\font
if not exist %LOUTLIBDIR%\maps mkdir %LOUTLIBDIR%\maps
 copy /Y maps\*.* %LOUTLIBDIR%\maps
if not exist %LOUTLIBDIR%\locale mkdir %LOUTLIBDIR%\locale
 copy /Y locale\*.* %LOUTLIBDIR%\locale

rem (c) Initializing run (should be silent, no errors expected)
%BINDIR%\lout -x -s %LOUTLIBDIR%\include\init

goto end

:missingexe
  echo lout.exe or prg2lout.exe missing.
  echo Check that you compiled lout.
  goto end

:missingdir
  echo hyph, maps, data or font directory missing.
  echo Check that this is a lout source directory.
  goto end

:end
