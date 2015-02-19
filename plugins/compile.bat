@echo off

:: AMX Mod X
::
:: by the AMX Mod X Development Team
::  originally developed by OLO
::
:: This file is part of AMX Mod X.

echo // AMX Mod X Batch Compiler
echo // by the AMX Mod X Dev Team
echo.

if not exist amxxpc.exe (
	echo // Could not find amxxpc.exe
	pause
	exit
)

if not exist compiled mkdir compiled

for %%i in (*.sma) do (
	echo // Compiling %%i ...

	amxxpc.exe "%%i" -ocompiled/"%%~ni.amxx"

	echo.
)

pause
