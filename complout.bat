set LOUTCFLAGS0=/Oity2
set LOUTCFLAGS1=/D OS_UNIX#0 /D OS_DOS#1 /D OS_MAC#0 /D BB_FIX#0 /D USE_STAT#1 /D SAFE_DFT#0 /D COLLATE#1
set LOUTCFLAGS2=/D LIB_DIR#"""/scs/lout/lib""" /D FONT_DIR#"""font""" /D MAPS_DIR#"""maps"""
set LOUTCFLAGS3=/D INCL_DIR#"""include""" /D DATA_DIR#"""data""" /D HYPH_DIR#"""hyph""" /D LOCALE_DIR#"""locale"""
set LOUTCFLAGS4=/D CHAR_IN#1 /D CHAR_OUT#0 /D LOCALE_ON#0 /D ASSERT_ON#1 /D DEBUG_ON#1 /D PDF_COMPRESSION#0
cl %LOUTCFLAGS0% %LOUTCFLAGS1% %LOUTCFLAGS2% %LOUTCFLAGS3% %LOUTCFLAGS4% %1 %2 %3 %4 %5 %6 %7 %8 %9
