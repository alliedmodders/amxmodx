unit UnitFunctions;

interface

uses SysUtils, Classes, Windows, IdFTPList, Math, Registry;

function CalcSpeed(eOld, eNew: Integer): String;
// local
function GetAllFiles(Mask: String; Attr: Integer; Recursive: Boolean; ShowDirs: Boolean; ShowPath: Boolean = True): TStringList;
function GetSteamAppsDir: String;
function GetSteamAccounts: TStringList;
// ftp
function GetAllDirs: TStringList;

implementation

uses UnitfrmMain;

function CalcSpeed(eOld, eNew: Integer): String;
begin
  Result := frmMain.Caption;
  if (eOld < eNew) and (eOld <> 0) then begin
    eOld := eNew - eOld;
    //eOld := eOld *2; // this is only used for faster updates...
    Result := 'AMX Mod X Installer - Uploading at ' + FloatToStr(RoundTo(eOld / 1024, -2)) + ' kb/s';
  end;
end;

function GetAllFiles(Mask: String; Attr: Integer; Recursive: Boolean; ShowDirs: Boolean; ShowPath: Boolean = True): TStringList;
var eSearch: TSearchRec;
begin
  Result := TStringList.Create;

  // Find all files
  if (FindFirst(Mask, Attr, eSearch) = 0) then begin
    repeat
      if (eSearch.Name[1] <> '.') then begin
        if ShowPath then begin
          if ((eSearch.Attr and Attr) = eSearch.Attr) and ((eSearch.Attr and faDirectory) <> eSearch.Attr) then
            Result.Add(ExtractFilePath(Mask) + eSearch.Name)
          else if (ShowDirs) and ((eSearch.Attr and faDirectory) = eSearch.Attr) then
            Result.Add(ExtractFilePath(Mask) + eSearch.Name);
        end
        else begin
          if ((eSearch.Attr and Attr) = eSearch.Attr) and ((eSearch.Attr and faDirectory) <> eSearch.Attr) then
            Result.Add(eSearch.Name)
          else if (ShowDirs) and ((eSearch.Attr and faDirectory) = eSearch.Attr) then
            Result.Add(eSearch.Name);
        end;

        if ((eSearch.Attr and faDirectory) = eSearch.Attr) and (Recursive) then begin
          with GetAllFiles(ExtractFilePath(Mask) + eSearch.Name + '\' + ExtractFileName(Mask), Attr, True, ShowDirs, ShowPath) do begin
            Result.Text := Result.Text + Text;
            Free;
          end;
        end;
      end;
    until (FindNext(eSearch) <> 0);
  end;

  SysUtils.FindClose(eSearch);
end;

function GetSteamAppsDir: String;
var eRegistry: TRegistry;
begin
  eRegistry := TRegistry.Create(KEY_READ);
  try
    eRegistry.RootKey := HKEY_CURRENT_USER;
    if eRegistry.OpenKey('Software\Valve\Steam', False) then
      Result := IncludeTrailingPathDelimiter(StringReplace(eRegistry.ReadString('SteamPath'), '/', '\', [rfReplaceAll])) + 'SteamApps\'
    else
      Result := '';
  except
    Result := '';
  end;
  eRegistry.Free;
end;

function GetSteamAccounts: TStringList;
var eSearch: TSearchRec;
    ePath: String;
begin
  Result := TStringList.Create;
  ePath := GetSteamAppsDir;
  if DirectoryExists(ePath) then begin
    if FindFirst(ePath + '*.*', faAnyFile, eSearch) = 0 then begin
      repeat
        if (eSearch.Attr and faDirectory = faDirectory) and (eSearch.Name <> '.') and (eSearch.Name <> '..') then
          Result.Add(eSearch.Name)
      until FindNext(eSearch) <> 0;
    end;
    SysUtils.FindClose(eSearch);
  end;
end;

function GetAllDirs: TStringList;
var eList: TStringList;
    i: integer;
begin
  eList := TStringList.Create;
  try
    frmMain.IdFTP.List(eList);
  except
    // nothing, not an exception?!
  end;
  frmMain.IdFTP.DirectoryListing.LoadList(eList);
  eList.Clear;
  for i := 0 to frmMain.IdFTP.DirectoryListing.Count -1 do begin
    if (frmMain.IdFTP.DirectoryListing.Items[i].FileName <> '.') and (frmMain.IdFTP.DirectoryListing.Items[i].FileName <> '..') and (frmMain.IdFTP.DirectoryListing.Items[i].ItemType = ditDirectory) then
      eList.Add(frmMain.IdFTP.DirectoryListing.Items[i].FileName);
  end;
  Result := eList;
end;

{ This is another possibility I wrote because I couldn't find another bug...

function GetAllDirs: TStringList;
var eList: TStringList;
    i, eStart: integer;
begin
  eList := TStringList.Create;
  try
    frmMain.IdFTP.List(eList, '', True);
  except
    // nothing
  end;
  eStart := 0;
  

 //   +----------------------------------------------------------------+
 //   | drwxr-xr-x   5 web3     ftponly      2048 Jun 25 19:43 files   |
 //   | drwxr-xr-x   2 web3     ftponly      2048 Jun 25 19:57 html    |
 //   | drwxr-xr-x   3 root     root         2048 Jun 20 05:03 log     |
 //   | drwxrwxrwx   2 web3     ftponly      2048 Jun 19  2004 phptmp  |
 //   +----------------------------------------------------------------+


  // at first remove all non-directories from the list
  for i := eList.Count -1 downto 0 do begin
    if Pos('d', eList[i]) <> 1 then
      eList.Delete(i);
  end;
  // then we have to find the position where ALL filenames start...
  for i := 0 to eList.Count -1 do begin
    if (eStart = 0) and (Pos(':', eList[i]) <> 0) then
      eStart := Pos(':', eList[i]);
  end;

  if eStart = 0 then
    eList.Clear
  else begin
    // find the position
    for i := 0 to eList.Count -1 do begin
      if Pos(':', eList[i]) <> 0 then begin
        while (eStart <> Length(eList[i])) and (eList[i][eStart] <> #32) do
          Inc(eStart, 1);
      end;
    end;
    // remove the detail stuff...
    for i := 0 to eList.Count -1 do
      eList[i] := Copy(eList[i], eStart +1, Length(eList[i]));
  end;

  Result := eList;
end;  }

end.


