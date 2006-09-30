unit UnitCodeUtils;

interface

uses SysUtils, Classes, Forms, Controls, Windows, ScintillaLanguageManager,
     RichEdit, ComCtrls, JvInspector;

function PosBack(eSubStr, eStr: String): Integer;
function IsAtStart(eSubStr, eStr: String; AllowFunctions: Boolean = True): Boolean;
function GetIndents(Line: Integer = -1): String;
function GetStyleAt(ePos: Integer): TSciStyle;
function LineFromPos(ePos: Integer): Integer;
function RemoveSemicolon(eStr: String): String;
procedure IndentCode;
procedure UnindentCode;
function Between(eText, eFirst, eSecond: String; eSecondBack: Boolean = False): String;
procedure Delay(eTime: Integer);
function CountChars(eIn: String; eChar: Char): Integer;
function RemoveStringsAndComments(eLine: String; eRemoveStrings: Boolean; eRemoveComments: Boolean): String;
function GetMatchingBrace(eString: String): Integer;
function GetColoredLine(eLine: Integer): String;
function GetFunctionPos: Integer;
function GetCurrFunc: String;

function GetRTFText(ARichEdit: TRichedit): string;
procedure SetRTFText(ARichEdit: TRichedit; ARTFText: String);

implementation

uses UnitfrmMain, UnitMainTools, UnitLanguages, UnitfrmIRCPaster;

function PosBack(eSubStr, eStr: String): Integer;
begin
  Result := 0;
  if Pos(eSubStr, eStr) <> 0 then begin
    while Pos(eSubStr, Copy(eStr, Result +1, Length(eStr))) <> 0 do
      Inc(Result, 1);
  end;
end;

function IsAtStart(eSubStr, eStr: String; AllowFunctions: Boolean = True): Boolean;
begin
  eStr := LowerCase(Trim(StringReplace(eStr, #9, #32, [rfReplaceAll])));
  eSubStr := LowerCase(eSubStr);
  if Pos(eSubStr + #32, eStr) = 1 then
    Result := True
  else if (Pos(eSubStr + '(', eStr) = 1) and (AllowFunctions) then
    Result := True
  else
    Result := False;
end;

function GetIndents(Line: Integer = -1): String;
var i: integer;
begin
  Result := '';
  if Line = -1 then
    Line := frmMain.sciEditor.GetCurrentLineNumber;

  if Length(frmMain.sciEditor.Lines[Line]) <> 0 then begin
    for i := 1 to Length(frmMain.sciEditor.Lines[Line]) do begin
      if (frmMain.sciEditor.Lines[Line][i] <> #32) and (frmMain.sciEditor.Lines[Line][i] <> #9) then
        break
      else
        Result := Result + frmMain.sciEditor.Lines[Line][i];
    end;
  end;
end;

function GetStyleAt(ePos: Integer): TSciStyle;
var eBits: Integer;
    eStyleNo: Byte;
    i: integer;
begin
  Result := nil;

  eStyleNo := Byte(frmMain.sciEditor.GetStyleAt(ePos));
  eBits := frmMain.sciEditor.GetStyleBits;

  if eBits = 5 then
    eStyleNo := eStyleNo and $1f //Strip away the indicators (3 bits)
  else if eBits = 7 then
    eStyleNo := eStyleNo and $7f //Strip away the indicators (1 bit)
  else if eBits = 6 then
    eStyleNo := eStyleNo and $3f; //Strip away the indicators (2 bits)

  with frmMain.sciEditor.LanguageManager.LanguageList.Find(ActiveDoc.Highlighter).Styles do begin
    for i := 0 to Count -1 do begin
      if TSciStyle(Items[i]).StyleNumber = eStyleNo then
        Result := TSciStyle(Items[i]);
    end;
  end;
end;

function LineFromPos(ePos: Integer): Integer;
var i: integer;
    eLength: Integer;
begin
  Result := -1;
  eLength := 0;

  for i := 0 to frmMain.sciEditor.Lines.Count -1 do begin
    eLength := eLength + Length(frmMain.sciEditor.Lines[i]) + 2;
    if eLength >= ePos then begin
      Result := i;
      break;
    end;
  end;
end;

function RemoveSemicolon(eStr: String): String;
begin
  if Length(eStr) <> 0 then begin
    if eStr[Length(eStr)] = ';' then
      Result := Copy(eStr, 1, Length(eStr) -1)
    else
      Result := eStr;
  end
  else
    Result := eStr;
end;

procedure IndentCode;
var eStr: TStringList;
    i, k: integer;
    eIndent, eTempIndent: Integer;
    eString: String;
begin
  Screen.Cursor := crHourGlass;
  frmMain.sciEditor.Enabled := False;
  eStr := TStringList.Create;
  eIndent := 0;
  eTempIndent := 0;

  Cancel := False;
  ShowProgress(False);
  frmMain.pbLoading.Max := frmMain.sciEditor.Lines.Count *2 -2;
  for i := 0 to frmMain.sciEditor.Lines.Count -1 do begin
    if Cancel then begin
      Cancel := False;
      exit;
    end;
    eStr.Add(TrimLeft(frmMain.sciEditor.Lines[i]));
    // Remove strings and comments virtually because they could include brackets
    frmMain.pbLoading.Position := i;
    SetProgressStatus('Indenting Code...');
    eStr[i] := RemoveStringsAndComments(eStr[i], True, True);
    eStr[i] := LowerCase(Trim(eStr[i]));
  end;

  for i := 0 to eStr.Count -1 do begin
    if CountChars(eStr[i], '{') <> CountChars(eStr[i], '}') then
      eIndent := eIndent - CountChars(eStr[i], '}');
    frmMain.sciEditor.Lines[i] := TrimLeft(frmMain.sciEditor.Lines[i]);

    for k := 1 to eIndent + eTempIndent do
      frmMain.sciEditor.Lines[i] := '	' + frmMain.sciEditor.Lines[i];
    if eTempIndent > 0 then
      eTempIndent := eTempIndent -1;

    if (IsAtStart('if', eStr[i], True)) and (Pos('{', eStr[i]) = 0) and (Length(eStr[i]) > 3) then begin
      eString := eStr[i];
      Delete(eString, 1, 2);
      if (eString[1] <> Trim(eString)[1]) or (eString[1] = '(') then begin
        eString := Trim(eString);
        if GetMatchingBrace(eString) = Length(eString) then
          eTempIndent := eTempIndent +1;
      end;
    end
    else if (IsAtStart('for', eStr[i], True)) and (Pos('{', eStr[i]) = 0) and (Length(eStr[i]) > 4) then begin
      eString := eStr[i];
      Delete(eString, 1, 3);
      if (eString[1] <> Trim(eString)[1]) or (eString[1] = '(') then begin
        eString := Trim(eString);
        if GetMatchingBrace(eString) = Length(eString) then
          eTempIndent := eTempIndent +1;
      end;
    end
    else if (eStr[i] = 'else') or ((Pos('else if', eStr[i]) = 1) or (Pos('case', eStr[i]) = 1) or (Pos('switch', eStr[i]) = 1)) and (Pos('{', eStr[i]) = 0) then
        eTempIndent := eTempIndent +1
    else if (Pos('{', eStr[i]) = 0) and (Length(eStr[i]) > 6) then begin
      if (IsAtStart('stock', eStr[i], False)) or (IsAtStart('while', eStr[i], True)) then begin
        eString := eStr[i];
        Delete(eString, 1, 5);
        if (eString[1] <> Trim(eString)[1]) or (eString[1] = '(') then begin
          eString := Trim(eString);
          if GetMatchingBrace(eString) = Length(eString) then
            eTempIndent := eTempIndent +1;
        end;
      end;
    end;
    
    if (Pos('{', eStr[i]) = 0) and (Length(eStr[i]) > 7) then begin
      if (Pos('public', eStr[i]) = 1) or (Pos('native', eStr[i]) = 1) then begin
        eString := eStr[i];
        Delete(eString, 1, 6);
        if eString[1] <> Trim(eString)[1] then begin
          eString := Trim(eString);
          if GetMatchingBrace(eString) = Length(eString) then
            eTempIndent := eTempIndent +1;
        end;
      end;
    end
    else if (IsAtStart('forward', eStr[i], False)) and (Pos('{', eStr[i]) = 0) and (Length(eStr[i]) > 8) then begin
      eString := eStr[i];
      Delete(eString, 1, 7);
      if eString[1] <> Trim(eString)[1] then begin
        eString := Trim(eString);
        if GetMatchingBrace(eString) = Length(eString) then
          eTempIndent := eTempIndent +1;
      end;
    end;

    if CountChars(eStr[i], '{') <> CountChars(eStr[i], '}') then
      eIndent := eIndent + CountChars(eStr[i], '{');
    if (i+1 < eStr.Count) and (Trim(RemoveStringsAndComments(eStr[i +1], true, true)) = '{') then
        eTempIndent := eTempIndent -1;

    frmMain.pbLoading.Position := frmMain.sciEditor.Lines.Count + i -1;
    SetProgressStatus('Indenting Code...');
    frmMain.pnlLoading.Repaint;
  end;

  ActiveDoc.Modified := True;
  frmMain.mnuModified.Caption := lModified;
  HideProgress;
  
  frmMain.sciEditor.Enabled := True;
  Screen.Cursor := crDefault;
end;

procedure UnindentCode;
var i: integer;
begin
  Screen.Cursor := crHourGlass;
  frmMain.sciEditor.Enabled := False;
  Cancel := False;
  ShowProgress(False);
  frmMain.pbLoading.Max := frmMain.sciEditor.Lines.Count -1;

  for i := 0 to frmMain.sciEditor.Lines.Count -1 do begin
    if Cancel then begin
      Cancel := False;
      exit;
    end;

    frmMain.sciEditor.Lines[i] := TrimLeft(frmMain.sciEditor.Lines[i]);
    frmMain.pbLoading.Position := i;
    SetProgressStatus('Unintending Code...');
    frmMain.pnlLoading.Repaint;
  end;
  HideProgress;

  frmMain.sciEditor.Enabled := True;
  Screen.Cursor := crDefault;
end;

function RemoveStringsAndComments(eLine: String; eRemoveStrings: Boolean; eRemoveComments: Boolean): String;
begin
  // Remove comments
  if eRemoveComments then begin
    if (Pos(GetCurrLang.CommentBoxStart, eLine) = 1) or (Pos(GetCurrLang.CommentBoxMiddle, eLine) = 1) or (Pos(GetCurrLang.CommentBoxEnd, eLine) = 1) or (Pos(GetCurrLang.CommentBlock, eLine) = 1) then
      eLine := '';
    if Pos(GetCurrLang.CommentBlock, eLine) <> 0 then
      eLine := Copy(eLine, 1, Pos('//', eLine) -2);
    if (Pos(GetCurrLang.CommentStreamStart, eLine) < Pos(GetCurrLang.CommentStreamEnd, eLine)) and (Pos(GetCurrLang.CommentStreamStart, eLine) <> 0) then
      eLine := StringReplace(eLine, GetCurrLang.CommentStreamStart + Between(eLine, GetCurrLang.CommentStreamStart, GetCurrLang.CommentStreamEnd) + GetCurrLang.CommentStreamEnd, '', [rfReplaceAll]); // maybe not the best method, but simple and quite easy
  end;
  // Remove quotes
  if eRemoveStrings then begin
    while Between(eLine, '"', '"') <> '' do
      eLine := StringReplace(eLine, '"' + Between(eLine, '"', '"') + '"', '', [rfReplaceAll]);
  end;
  
  Result := eLine;
end;

procedure Delay(eTime: Integer);
var i: integer;
begin
  for i := 1 to eTime do begin
    Sleep(1);
    Application.ProcessMessages;
    if Application.Terminated then
      exit;
  end;
end;

function CountChars(eIn: String; eChar: Char): Integer;
var i: integer;
begin
  Result := 0;
  if Length(eIn) <> 0 then begin
    for i := 1 to Length(eIn) do begin
      if eIn[i] = eChar then
        Inc(Result, 1);
    end;
  end;
end;

function Between(eText, eFirst, eSecond: String; eSecondBack: Boolean = False): String;
var eTemp: String;
begin
  Result := '';
  
  if Pos(eFirst, eText) = PosBack(eSecond, eText) then exit;
  if Pos(eFirst, eText) = 0 then exit;
  if PosBack(eSecond, eText) < Pos(eFirst, eText) then exit;

  eTemp := eText;
  Delete(eTemp, 1, Pos(eFirst, eText) + Length(eFirst) - 1);
  if eSecondBack then
    Delete(eTemp, PosBack(eSecond, eTemp), Length(eTemp))
  else
    Delete(eTemp, Pos(eSecond, eTemp), Length(eTemp));
  Result := eTemp;
end;

function GetMatchingBrace(eString: String): Integer;
var a, b,c : integer;
begin
  Result := 0;
  if Length(eString) < 1 then exit;

  b := 0;
  c := 0;

  for a := 1 to Length(eString) do begin
    if eString[a] = '(' then begin
      b := b +1;
      c := 1;
    end
    else if eString[a] = ')' then begin
      b := b -1;
      c := 1;
    end;

    if (b = 0) and (c = 1) then begin
      Result := a;
      exit;
    end;
  end;
end;

function GetColoredLine(eLine: Integer): String;
var eCurrStyle: String;
    eChars: Integer;
    i: integer;
begin
  eChars := 0;
  if eLine <> 0 then begin
    for i := 0 to eLine -1 do
      eChars := eChars + Length(frmMain.sciEditor.Lines[i]) + 2; // +2 for #13#10
  end;

  eCurrStyle := '';
  if frmIRCPaster.chkLineNumbers.Checked then
    Result := IntToStr(eLine +1) + '] '
  else
    Result := '';

  if Trim(frmMain.sciEditor.Lines[eLine]) = '' then
    exit;
    
  for i := 0 to Length(frmMain.sciEditor.Lines[eLine]) -1 do begin
    if eCurrStyle <> GetStyleAt(eChars + i).Name then begin
      eCurrStyle := GetStyleAt(eChars + i).Name;
      
      if (eCurrStyle = 'White Space') and (Length(Result) <> Length(IntToStr(eLine +1) + '] ')) then
        Result := Result + '';
      if eCurrStyle = 'Ok Braces' then
        Result := Result + '02';
      if eCurrStyle = 'Bad Braces' then
        Result := Result + '04';
      if eCurrStyle = 'White Space' then
        Result := Result + '12';
      if eCurrStyle = 'Comment' then
        Result := Result + '07';
      if eCurrStyle = 'Line Comment' then
        Result := Result + '07';
      if eCurrStyle = 'Doc Comment' then
        Result := Result + '07';
      if eCurrStyle = 'Number' then
        Result := Result + '12';
      if eCurrStyle = 'Keyword' then
        Result := Result + '03';
      if eCurrStyle = 'Double quoted string' then
        Result := Result + '04';
      if eCurrStyle = 'Single quoted string' then
        Result := Result + '04';
      if eCurrStyle = 'Symbols/UUID' then
        Result := Result + '04';
      if eCurrStyle = 'Preprocessor' then
        Result := Result + '07';
      if eCurrStyle = 'Operators' then
        Result := Result + '12';
      if eCurrStyle = 'Identifier' then
        Result := Result + '12';
      if eCurrStyle = 'Regular expressions' then
        Result := Result + '10';
      if eCurrStyle = 'Doc Comment Line' then
        Result := Result + '07';
      if eCurrStyle = 'User-defined keywords' then
        Result := Result + '04';
    end;
    Result := Result + frmMain.sciEditor.Lines[eLine][i +1];
  end;
  Result := StringReplace(Result, '	', '  ', [rfReplaceAll]);
end;

{ ------------------ NOTES ------------------ }

function GetRTFText(ARichEdit: TRichedit): string;
var
  ss: TStringStream;
  emptystr: string;

  eStr: TStringList;
  i: integer;
begin
  Result := '';
  
  emptystr := '';
  ss := TStringStream.Create(emptystr);
  eStr := TStringList.Create;
  try
    ARichEdit.PlainText := False;
    ARichEdit.Lines.SaveToStream(ss);
    eStr.Text := StringReplace(ss.DataString, '\', '\\ ', [rfReplaceAll]);
    for i := 0 to eStr.Count -1 do
      Result := Result + '\n' + eStr[i];
    Delete(Result, 1, 2);
  finally
    ss.Free;
    eStr.Free;
  end;
end;

procedure SetRTFText(ARichEdit: TRichedit; ARTFText: String);
var
  ss: TStringStream;
begin
  ARTFText := StringReplace(ARTFText, '\n', #13#10, [rfReplaceAll]);
  ARTFText := StringReplace(ARTFText, '\\ ', '\', [rfReplaceAll]);

  ss := TStringStream.Create(ARTFText);
  try
    ARichEdit.PlainText := False;
    ARichEdit.Lines.LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;

function GetFunctionPos: Integer;
var eStr: String;
    i: integer;
begin
  Result := 0;
  eStr := Copy(eStr, 1, frmMain.sciEditor.GetCaretInLine);
  eStr := StringReplace(frmMain.sciEditor.Lines[frmMain.sciEditor.GetCurrentLineNumber], '^"', '', [rfReplaceAll]);
  if (Length(eStr) = 0) then exit;

  while Between(eStr, '"', '"') <> '' do
    eStr := StringReplace(eStr, Between(eStr, '"', '"'), '', [rfReplaceAll]);
  while Between(eStr, '{', '}') <> '' do
    eStr := StringReplace(eStr, Between(eStr, '{', '}'), '', [rfReplaceAll]);
  for i := Length(eStr) -1 downto 1 do begin
    if eStr[i] = ',' then
      Result := Result +1
    else if eStr[i] = '(' then
      exit;
  end;
end;

function GetCurrFunc: String;
var eStr: String;
    i: integer;
    eStart, eEnd: integer;
    eInString, eInArray: Boolean;
begin
  Result := '';
  eStart := 1;
  eEnd := -1;
  eInString := False;
  eInArray := False;
  eStr := frmMain.sciEditor.Lines[frmMain.sciEditor.GetCurrentLineNumber];
  eStr := StringReplace(eStr, '^"', 'AB', [rfReplaceAll]); // we don't need those

  if (Length(eStr) = 0) then exit;

  for i := frmMain.sciEditor.GetCaretInLine downto 1 do begin
    if eStr[i] = '"' then
      eInString := not eInString
    else if (eStr[i] = '{') and (not eInString) then
      eInArray := True
    else if (eStr[i] = '}') and (not eInString) then
      eInArray := False
    else if (not eInArray) and (not eInString) and (eStr[i] = '(') then begin
      eEnd := i-1;
      break;
    end;
  end;
  
  if eEnd <> -1 then begin
    for i := eEnd downto 1 do begin
      if eStr[i] = '"' then
        eInString := not eInString
      else if (eStr[i] = '{') and (not eInString) then
        eInArray := True
      else if (eStr[i] = '}') and (not eInString) then
        eInArray := False
      else if (not eInArray) and (not eInString) and (eStr[i] = '(') then begin
        eStart := i+1;
        break;
      end;
    end;
  end
  else
    exit;

  Result := Trim(Copy(eStr, eStart, eEnd - eStart + 1));
end;

end.
