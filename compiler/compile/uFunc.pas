// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

unit uFunc;

interface

uses
  Windows, SysUtils, Classes, Math, IniFiles;

resourcestring
  COMPILER_EXE = 'amxxpc.exe';

procedure AppExit;
procedure CompilePlugin(const Name: String);
function GetAgeFromDat(const FileName: String): Integer;
procedure SetAgeToDat(const FileName: String; const Age: Integer);
function GetConsoleOutput(const Command: String; var Output: TStringList): Boolean;

implementation

procedure AppExit;
begin
  WriteLn;
  Write('Press enter to exit ...');
  ReadLn;
  Halt;
end;

procedure CompilePlugin(const Name: String);
var
  Output: TStringList;
  i: Word;
  cStart,cEnd: Longword;
  FileName, FilePath, Compiled: String;
begin
  FileName := ExtractFileName(Name);
  FilePath := ExtractFilePath(Name);
  Compiled := FilePath+'compiled\'+ChangeFileExt(Filename,'.amxx');
  if (FilePath='') then
    FilePath := ExtractFilePath(ParamStr(0));

  WriteLn;
  WriteLn('//// '+ExtractFileName(FileName));

  if FileExists(Compiled) and ( GetAgeFromDat(FileName)=FileAge(Name) ) then
  begin
    WriteLn('// Already compiled.');
    WriteLn('// ----------------------------------------');
    Exit;
  end;

  Output := TStringList.Create;

  try
    cStart := GetTickCount;
    if not GetConsoleOutput(ExtractFilePath(ParamStr(0))+COMPILER_EXE+' "'+FilePath+FileName+'" "-o'+Compiled+'"',Output) then
    begin
      WriteLn('// Internal error.');
      AppExit;
    end;
    cEnd := GetTickCount;

    for i := 3 to (Output.Count-1) do
    begin
      WriteLn('// '+Output.Strings[i]);
    end;

    WriteLn('//');
    WriteLn('// Compilation Time: '+FloatToStr(SimpleRoundTo((cEnd-cStart) / 1000,-2))+' sec');
    WriteLn('// ----------------------------------------');
    Output.Free;
  except
    WriteLn('// Internal error.');
    AppExit;
  end;

  SetAgeToDat(FileName,FileAge(Name));
end;

function GetAgeFromDat(const FileName: String): Integer;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0))+'compile.dat');
  Result := Ini.ReadInteger(FileName,'Age',-1);
  Ini.Free;
end;

procedure SetAgeToDat(const FileName: String; const Age: Integer);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0))+'compile.dat');
  Ini.WriteInteger(FileName,'Age',Age);
  Ini.UpdateFile;
  Ini.Free;
end;

function GetConsoleOutput(const Command: String; var Output: TStringList): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  PipeOutputRead: THandle;
  PipeOutputWrite: THandle;
  PipeErrorsRead: THandle;
  PipeErrorsWrite: THandle;
  Succeed: Boolean;
  Buffer: array [0..255] of Char;
  NumberOfBytesRead: DWORD;
  Stream: TMemoryStream;
begin
  FillChar(ProcessInfo, SizeOf(TProcessInformation), 0);

  FillChar(SecurityAttr, SizeOf(TSecurityAttributes), 0);
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.bInheritHandle := True;
  SecurityAttr.lpSecurityDescriptor := nil;

  CreatePipe(PipeOutputRead, PipeOutputWrite, @SecurityAttr, 0);
  CreatePipe(PipeErrorsRead, PipeErrorsWrite, @SecurityAttr, 0);

  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb:=SizeOf(StartupInfo);
  StartupInfo.hStdInput := 0;
  StartupInfo.hStdOutput := PipeOutputWrite;
  StartupInfo.hStdError := PipeErrorsWrite;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;

  if  CreateProcess(nil, PChar(command), nil, nil, true,
  CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil,
  StartupInfo, ProcessInfo) then begin
    Result := True;
    CloseHandle(PipeOutputWrite);
    CloseHandle(PipeErrorsWrite);

    Stream := TMemoryStream.Create;
    try
      while True do begin
        Succeed := ReadFile(PipeOutputRead, Buffer, 255, NumberOfBytesRead, nil);
        if not Succeed then Break;
        Stream.Write(Buffer, NumberOfBytesRead);
      end;
      Stream.Position := 0;
      Output.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
    CloseHandle(PipeOutputRead);

    try
      while True do
      begin
        Succeed := ReadFile(PipeErrorsRead, Buffer, 255, NumberOfBytesRead, nil);
        if not Succeed then Break;
      end;
    finally
    end;
    CloseHandle(PipeErrorsRead);

    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hProcess);
  end
  else
  begin
    Result := False;
    CloseHandle(PipeOutputRead);
    CloseHandle(PipeOutputWrite);
    CloseHandle(PipeErrorsRead);
    CloseHandle(PipeErrorsWrite);
  end;
end;

end.
