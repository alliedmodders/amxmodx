@echo off

if not exist compiled mkdir compiled
if exist temp.txt del temp.txt
for %%i in (*.sma) do sc %%i -ocompiled\%%i >> temp.txt
copy compiled\*.sma compiled\*.amx
del compiled\*.sma
cls
type temp.txt
del temp.txt

pause