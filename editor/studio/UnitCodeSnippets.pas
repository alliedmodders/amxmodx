unit UnitCodeSnippets;

interface

uses SysUtils, Classes, Windows;

function GetSnippet(Lang, Ident: String): String;
function GetSnippetList(Lang: String): TStringList;
procedure AddSnippet(Lang, Ident, Code: String);
procedure DelSnippet(Lang, Ident: String);
procedure SetSnippet(Lang, Ident, Code: String);

implementation

function GetSnippet(Lang, Ident: String): String;
var eFile: TStringList;
    i: Integer;
begin
  eFile := TStringList.Create;
  eFile.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'config\' + Lang + '.csl');  // ... .csl = CodeSnippetList
  for i := 0 to eFile.Count -1 do begin
    if Pos(Ident + #1, eFile[i]) = 1 then begin
      Result := eFile[i];
      Delete(Result, 1, Length(Ident) +1);
      Result := StringReplace(Result, #2, #13#10, [rfReplaceAll]);
    end;
  end;
  eFile.Destroy;
end;

function GetSnippetList(Lang: String): TStringList;
var i: Integer;
begin
  Result := TStringList.Create;
  if FileExists(ExtractFilePath(ParamStr(0)) + 'config\' + Lang + '.csl') then begin
    Result.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'config\' + Lang + '.csl');  // ... .csl = CodeSnippetList
    for i := 0 to Result.Count -1 do
      Result[i] := Copy(Result[i], 1, Pos(#1, Result[i]) -1);
  end;
end;

procedure AddSnippet(Lang, Ident, Code: String);
var eFile: TStringList;
begin
  Code := StringReplace(Code, #13#10, #2, [rfReplaceAll]);

  eFile := TStringList.Create;
  eFile.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'config\' + Lang + '.csl');  // ... .csl = CodeSnippetList
  eFile.Add(Ident + #1 + Code);
  eFile.SaveToFile(ExtractFilePath(ParamStr(0)) + 'config\' + Lang + '.csl');  // ... .csl = CodeSnippetList
  eFile.Destroy;
end;

procedure DelSnippet(Lang, Ident: String);
var eFile: TStringList;
    i: Integer;
begin
  eFile := TStringList.Create;
  eFile.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'config\' + Lang + '.csl');  // ... .csl = CodeSnippetList
  for i := eFile.Count -1 downto 0 do begin
    if Pos(Ident + #1, eFile[i]) = 1 then
      eFile.Delete(i);
  end;
  eFile.SaveToFile(ExtractFilePath(ParamStr(0)) + 'config\' + Lang + '.csl');  // ... .csl = CodeSnippetList
  eFile.Destroy;
end;

procedure SetSnippet(Lang, Ident, Code: String);
var eFile: TStringList;
    i: Integer;
begin
  Code := StringReplace(Code, #13#10, #2, [rfReplaceAll]);

  eFile := TStringList.Create;
  eFile.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'config\' + Lang + '.csl');  // ... .csl = CodeSnippetList
  { If item exists... }
  for i := 0 to eFile.Count -1 do begin
    if Pos(Ident + #1, eFile[i]) = 1 then begin
      eFile[i] := Ident + #1 + Code;
      eFile.SaveToFile(ExtractFilePath(ParamStr(0)) + 'config\' + Lang + '.csl');  // ... .csl = CodeSnippetList
      eFile.Destroy;

      exit;
    end;
  end;
  { else... }
  eFile.Add(Ident + #1 + Code);
  eFile.SaveToFile(ExtractFilePath(ParamStr(0)) + 'config\' + Lang + '.csl');  // ... .csl = CodeSnippetList
  eFile.Destroy;
end;

end.
