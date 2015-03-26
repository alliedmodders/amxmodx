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

if "%1"=="" (
	for %%i in (*.sma) do (
		echo // Compiling %%i ...

		amxxpc.exe "%%i" -ocompiled/"%%~ni.amxx"

		if %ERRORLEVEL% EQU 0 (
			pause
			exit
		)

		echo.
	)
) else (
	for %%i in (%1) do (
		echo // Compiling %%i ...

		amxxpc.exe "%%i" -ocompiled/"%%~ni.amxx"

		if %ERRORLEVEL% EQU 0 (
			pause
			exit
		)

		echo.
	)
)

pause
