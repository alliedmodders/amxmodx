@echo off
rem AMX Mod X
rem
rem (c) 2002-2004, OLO
rem  modified by the AMX Mod X Development Team

if not exist compiled mkdir compiled
if exist temp.txt del temp.txt
for %%i in (*.sma) do sc %%i -e%%i.txt -ocompiled\%%i >> temp.txt
copy compiled\*.sma compiled\*.amx
del compiled\*.sma
cls
type temp.txt
del temp.txt

pause