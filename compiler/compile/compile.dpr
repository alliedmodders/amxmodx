// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

program compile;

{$APPTYPE CONSOLE}
{$R version.res}
{$R icon.res}

uses
  SysUtils, Classes,
  uFunc in 'uFunc.pas';

var
  sr: TSearchRec;
  i: Word;
begin
  WriteLn('//AMXXPC compile.exe');
  WriteLn('// by the AMX Mod X Dev Team');
  WriteLn;

  if not FileExists(ExtractFilePath(ParamStr(0))+COMPILER_EXE) then
  begin
    WriteLn('// Could not find '+COMPILER_EXE);
    AppExit;
  end;

  if not DirectoryExists(ExtractFilePath(ParamStr(0))+'compiled') then
    CreateDir(ExtractFilePath(ParamStr(0))+'compiled');

  if ( ParamCount > 0 ) then
  begin
    for i := 1 to ParamCount do
    begin
      if FileExists(ParamStr(i)) then
        CompilePlugin(ParamStr(i))
      else
      begin
        WriteLn;
        WriteLn('// File not found.');
      end;
    end;
  end
  else
  begin
    if ( FindFirst('*.sma',faAnyFile,sr) = 0 ) then
    begin
      repeat
        CompilePlugin(sr.Name);
      until ( FindNext(sr) <> 0 );
    end
    else
    begin
      WriteLn('// No file found.');
    end;
    FindClose(sr);
  end;

  AppExit;
end.
