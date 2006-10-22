unit UnitTextAnalyze;

interface

uses SysUtils, Classes, Windows, Forms;

type TPawnParseResult = class
  public
    Constants: TStringList;
    Defined: TStringList;
    CVars: TStringList;
    Included: TStringList;
    MethodsDefault: TStringList;
    Events: TStringList;
    Stocks: TStringList;
    Natives: TStringList;
    Forwards: TStringList;
    Variables: TStringList;

    CallTips: TStringList;
    AutoComplete: TStringList;
    HighlightKeywords: TStringList;

    constructor Create; reintroduce;
    procedure DestroyResult;
  end;

function ParseCodePawn(eCode: TStringList; FileName: string; IsRecursive: Boolean = False): TPawnParseResult;
function UpdateIncPath(eInput: string): string;

var eCPUSpeed: Integer = 1;

implementation

uses UnitCodeExplorerUpdater, UnitCodeUtils, UnitfrmSettings,
  UnitMainTools, UnitfrmMain;

var eLookedUpIncluded: TStringList;

function UpdateIncPath(eInput: string): string;
begin
  eInput := StringReplace(Trim(eInput), '/', '\', [rfReplaceAll]);
  if ExtractFileExt(eInput) = '' then
    eInput := eInput + '.inc';
  
  if FileExists(ExtractFilePath(frmSettings.txtPawnCompilerPath.Text) + eInput) then
    Result := ExtractFilePath(frmSettings.txtPawnCompilerPath.Text) + eInput
  else if FileExists(ExtractFilePath(frmSettings.txtPawnCompilerPath.Text) + 'include\' + eInput) then
    Result := ExtractFilePath(frmSettings.txtPawnCompilerPath.Text) + 'include\' + eInput
  else if (FileExists(ExtractFilePath(ActiveDoc.FileName) + eInput)) then
    Result := ExtractFilePath(ActiveDoc.FileName) + eInput
  else
    Result := '';
end;

function ParseCodePawn(eCode: TStringList; FileName: string; IsRecursive: Boolean = False): TPawnParseResult;
var i, k: integer;
    eString, eTemp, eBackup: string;
    eStr, ePreEvents: TStringList;
    eStartLine, eBracesOpen: Integer;
    eTimeToSleep: Integer;
    eAddingEnum: Integer;
    eTempResult: TPawnParseResult;
    eProcedureAdded: Boolean;
    eCActive: Boolean;
    eTempBool: Boolean;
begin
  Result := TPawnParseResult.Create;
  if not IsRecursive then
    eLookedUpIncluded.Clear;

  eStr := TStringList.Create;
  ePreEvents := TStringList.Create;
  eBracesOpen := 0;
  eStartLine := -1;
  eTimeToSleep := 0;
  eAddingEnum := 0;
  eCActive := False;

  for i := 0 to eCode.Count - 1 do begin
    if (Application.Terminated) or (not Started) or (frmMain.pnlLoading.Visible) or (not frmMain.trvExplorer.Visible) then exit;

    eBackup := Trim(eCode[i]);
    eString := RemoveStringsAndComments(Trim(eCode[i]), True, True);
    if (Pos('/*', eBackup) = 1) or (Pos('*/', eBackup) <> 0) then begin
      eCActive := (Pos('/*', eBackup) = 1);
      if (eCActive) and (Pos('*/', eBackup) <> 0) then begin
        eCActive := False;
        continue
      end;
    end;
    if (eBackup = '') or (Pos('//', eBackup) = 1) or (eCActive) then
      continue;
      

    eProcedureAdded := False;
    Inc(eTimeToSleep, 1);

    if eTimeToSleep = eCPUSpeed then begin
      Sleep(1);
      eTimeToSleep := 0;
    end;

    { Constants and Variables }
    if (IsAtStart('new', eString, False)) or (IsAtStart('const', eString, False)) or (IsAtStart('stock', eString, False)) then begin // const or variable
      if (eBracesOpen = 0) and (not IsRecursive) and (Pos('(', eString) = Pos(')', eString)) then begin
        // we don't need braces so delete them...
        while (CountChars(eString, '{') <> 0) and (CountChars(eString, '}') <> 0) and (Pos('{', eString) < Pos('}', eString)) do
          eString := StringReplace(eString, '{' + Between(eString, '{', '}') + '}', '', [rfReplaceAll]);
        while (CountChars(eString, '[') <> 0) and (CountChars(eString, ']') <> 0) and (Pos('[', eString) < Pos(']', eString)) do
          eString := StringReplace(eString, '[' + Between(eString, '[', ']') + ']', '', [rfReplaceAll]);
        // done? okay, split all items if there are more than one; and if not, it's okay...
        eStr.Text := StringReplace(Copy(eString, Pos(#32, eString)+1, Length(eString)), ',', #13, [rfReplaceAll]);
        for k := 0 to eStr.Count - 1 do begin
          if (Trim(eStr[k]) <> '') and (eStr[k] <> '}') then begin
            eTemp := Trim(RemoveSemicolon(eStr[k]));

            if (IsAtStart('const', eTemp, False)) then begin
              Delete(eTemp, 1, 5);
              eTemp := Trim(eTemp);
              eTempBool := True;
            end
            else
              eTempBool := (IsAtStart('const', eString, False));
            
            if (Pos(':', eTemp) <> 0) then
              eTemp := Copy(eTemp, Pos(':', eTemp) + 1, Length(eTemp));

            if (Pos('=', eTemp) <> 0) then begin // constant
              Result.Constants.AddObject(Copy(eTemp, 1, Pos('=', eTemp) - 1), TObject(i));
              Result.AutoComplete.Add(Copy(eTemp, 1, Pos('=', eTemp) - 1));
            end
            else begin // variable
              if (eTempBool) then
                Result.Constants.AddObject(eTemp, TObject(i))
              else
                Result.Variables.AddObject(eTemp, TObject(i));
              Result.AutoComplete.Add(eTemp);
            end;
          end;
        end;
        eString := RemoveStringsAndComments(Trim(eCode[i]), True, True);
        continue;
      end;
    end;
    { Included }
    if (IsAtStart('#include', eBackup)) then begin
      eString := StringReplace(eBackup, '/', '\', [rfReplaceAll]);
      if Between(eString, '<', '>') <> '' then
        eString := Between(eString, '<', '>')
      else if Between(eString, '"', '"') <> '' then
        eString := Between(eString, '"', '"');
      eString := Trim(eString);
      Result.Included.AddObject(eString, TObject(i));

      // Recursive update
      if (eLookedUpIncluded.IndexOf(eString) = -1) then begin
        eLookedUpIncluded.Add(eString);
        eTemp := UpdateIncPath(eString);

        if (eString <> '') and (FileExists(eTemp)) then begin
          // Load code and parse
          eTempResult := nil;
          try
            eStr.LoadFromFile(eTemp);
            if Application.Terminated then exit;
            eTempResult := ParseCodePawn(eStr, ExtractFileName(eTemp), True);
            // Assign parsed values
            Result.AutoComplete.AddStrings(eTempResult.AutoComplete);
            Result.CallTips.AddStrings(eTempResult.CallTips);
            Result.HighlightKeywords.AddStrings(eTempResult.HighlightKeywords);
            // free
          except
            // mmmm.. burger
          end;
          if Assigned(eTempResult) then
            eTempResult.DestroyResult;
          // wait
          Sleep(20);
        end;
      end;
      continue;
    end;
    { CVars }
    if (IsAtStart('register_cvar', eString)) and (not IsRecursive) then begin
      if Between(eString, '"', '"') <> '' then
        Result.CVars.AddObject(Between(eBackup, '"', '"'), TObject(i));
      continue;
    end;
    { Defined }
    if (IsAtStart('#define', eString)) then begin
      eString := Copy(eString, 8, Length(eString));
      eString := Trim(eString);
      Result.CallTips.Add(eString + '-> ' + FileName + ', defined constant');
      if Pos(#32, eString) <> 0 then
        eString := Copy(eString, 1, Pos(#32, eString) - 1);
      if Pos('	', eString) <> 0 then
        eString := Copy(eString, 1, Pos('	', eString) - 1);
      Result.Defined.AddObject(eString, TObject(i));
      Result.AutoComplete.Add(eString);
      continue;
    end;
    { Events (Part 1) }
    if (IsAtStart('register_event(', eString)) and (not IsRecursive) then begin
      if CountChars(eBackup, '"') >= 4 then begin
        eTemp := StringReplace(eBackup, '"' + Between(eBackup, '"', '"') + '"', '', []);
        ePreEvents.Add(Between(eBackup, '"', '"'));
      end;
      continue;
    end;

    { Functions (1), this is adapted from AMXX-Edit v2 [see TextAnalyze.pas] }
    eBracesOpen := eBracesOpen + CountChars(eString, '{');
    eBracesOpen := eBracesOpen - CountChars(eString, '}');

    if Pos('{', eString) <> 0 then begin
      { Enums -> }
      if eAddingEnum = 1 then begin
        eAddingEnum := 2;
        Delete(eString, 1, Pos('{', eString) + 1);
      end
      else begin
        if eStartLine = -1 then begin
          eProcedureAdded := True;
          eStartLine := i;
        end;
      end;
      { <- Enums }
    end;
    if (Pos('}', eString) <> 0) then begin
      { Enums -> }
      if eAddingEnum <> 0 then
        eAddingEnum := 0;

      { <- Enums }
      if (eStartLine <> -1) then begin
        if (eBracesOpen = 0) and (not IsAtStart('new', Trim(eCode[eStartLine]))) then begin
          if Trim(RemoveStringsAndComments(eCode[eStartLine], True, True)) = '{' then
            eStartLine := eStartLine - 1;
          eTemp := Trim(RemoveSemicolon(Trim(eCode[eStartLine])));

          // Analyze type
          k := 0;
          if IsAtStart('public', eTemp) then
            k := 1
          else if IsAtStart('stock', eTemp) then
            k := 2
          else if IsAtStart('native', eTemp) then
            k := 3
          else if IsAtStart('forward', eTemp) then
            k := 4
          else if Pos('enum', LowerCase(eTemp)) = 1 then // no method
            k := 5;


          // Remove type
          if Pos('@', eTemp) = 1 then begin
            eTemp := Copy(eTemp, 2, Length(eTemp));
            k := 1;
          end
          else begin
            if (Pos(#32, eTemp) <> 0) and (Pos(#32, eTemp) < Pos('(', eTemp)) then
              eTemp := Copy(eCode[eStartLine], Pos(#32, eCode[eStartLine]) + 1, Length(eCode[eStartLine]))
            else if (Pos(#9, eTemp) <> 0) and (Pos(#9, eTemp) < Pos('(', eTemp)) then
              eTemp := Copy(eTemp, Pos(#9, eTemp) + 1, Length(eTemp));
          end;

          if eTemp[Length(eTemp)] = '{' then
            eTemp := Trim(Copy(eTemp, 1, Length(eTemp) - 1));

          // Remove return-type
          if (Pos(':', eTemp) <> 0) and (Pos(':', eTemp) < Pos('(', eTemp)) then
            Delete(eTemp, 1, Pos(':', eTemp));

          if Pos('operator', eTemp) = 1 then
            k := 6;

          eTemp := RemoveSemicolon(eTemp);
          if k < 5 then begin
            case k of
              0: Result.CallTips.Add(eTemp + '-> ' + FileName + ', function');
              1: Result.CallTips.Add(eTemp + '-> ' + FileName + ', public function');
              2: Result.CallTips.Add(eTemp + '-> ' + FileName + ', stock');
              3: Result.CallTips.Add(eTemp + '-> ' + FileName + ', native');
              4: Result.CallTips.Add(eTemp + '-> ' + FileName + ', forward');
            end;
          end;
          // Copy function-name
          if Pos('(', eTemp) <> 0 then
            eTemp := Copy(eTemp, 1, Pos('(', eTemp) - 1);
          eTemp := Trim(eTemp);
          
          if (Result.AutoComplete.IndexOf(eTemp) <> -1) then begin
            eStartLine := -1;
            eBracesOpen := 0;
            continue;
          end;

          if k < 5 then begin
            Result.AutoComplete.Add(eTemp);
            Result.HighlightKeywords.Add(eTemp);
          end;

          if eTemp <> '' then begin
            case k of
              0: begin
                  if not IsRecursive then
                    Result.MethodsDefault.AddObject(eTemp, TObject(eStartLine)); // Default Method
                end;
              1: begin
                  k := ePreEvents.IndexOf(eTemp);
                  if k <> -1 then begin
                    Result.Events.AddObject(eTemp, ePreEvents.Objects[k]);
                    ePreEvents.Delete(k);
                  end
                  else
                    Result.MethodsDefault.AddObject(eTemp, TObject(eStartLine));
                end;
              2: Result.Stocks.AddObject(eTemp, TObject(eStartLine));
              3: Result.Natives.AddObject(eTemp, TObject(eStartLine));
              4: Result.Forwards.AddObject(eTemp, TObject(eStartLine));
            end;
          end;
          eStartLine := -1;
          eBracesOpen := 0;
        end;
      end;
    end
    else if (eAddingEnum = 2) and (Pos('enum', LowerCase(eString)) <> 1) then begin
      if Pos(' ', eString) <> 0 then
        eString := Copy(eString, 1, Pos(' ', eString) - 1);
      if Pos(',', eString) <> 0 then
        eString := Copy(eString, 1, Pos(',', eString) - 1);
      if Pos('	', eString) <> 0 then
        eString := Copy(eString, 1, Pos('	', eString) - 1);
      if Pos(':', eString) <> 0 then
        eString := Copy(eString, 1, Pos(':', eString) - 1);
      Result.AutoComplete.Add(eString);
    end;

    { Enums }
    if IsAtStart('enum', eString) then begin
      if Pos('{', eString) <> 0 then
        eAddingEnum := 2 // Add values immediately
      else
        eAddingEnum := 1; // Wait for next brace and add then
    end;

    { Functions (2) }
    if (IsAtStart('forward', eString)) or (IsAtStart('public', eString)) or (IsAtStart('native', eString)) or (IsAtStart('stock', eString)) then begin
      if (not eProcedureAdded) and (Pos('(', eString) <> 0) and (Pos(')', eString) <> 0) then begin
        eTemp := StringReplace(Trim(eBackup), #9, #32, [rfReplaceAll]);
        eTemp := Trim(RemoveSemicolon(eTemp));
        if eTemp[Length(eTemp)] = '{' then
          eTemp := Trim(Copy(eTemp, 1, Length(eTemp) - 1));

        // Remove type
        if (Pos(#32, eTemp) <> 0) and (Pos(#32, eTemp) < Pos('(', eTemp)) then
          eTemp := Copy(eTemp, Pos(#32, eTemp) + 1, Length(eTemp));
        if (Pos(#9, eTemp) <> 0) and (Pos(#9, eTemp) < Pos('(', eTemp)) then
          eTemp := Copy(eTemp, Pos(#9, eTemp) + 1, Length(eTemp));
        // Remove return-type
        if (Pos(':', eTemp) <> 0) and (Pos(':', eTemp) < Pos('(', eTemp)) then
          Delete(eTemp, 1, Pos(':', eTemp));

        eTemp := RemoveSemicolon(eTemp);
        if (Pos('enum', eTemp) = Pos('operator', eTemp)) and (Pos('enum', eTemp) = 0) then
          Result.CallTips.Add(eTemp + '-> ' + FileName + ', ' + Trim(Copy(eString, 1, Pos(#32, eString) -1)));

        // Copy function-name
        if Pos('(', eTemp) <> 0 then
          eTemp := Copy(eTemp, 1, Pos('(', eTemp) - 1);
        eTemp := Trim(eTemp);

        if (Pos('enum', eTemp) = Pos('operator', eTemp)) and (Pos('enum', eTemp) = 0) then begin
          Result.AutoComplete.Add(eTemp);
          Result.HighlightKeywords.Add(eTemp);
        end;

        if eTemp <> '' then begin
          if IsAtStart('forward', eString) then
            Result.Forwards.AddObject(eString, TObject(i))
          else if IsAtStart('public', eString) then begin
            k := ePreEvents.IndexOf(eTemp);
            if k <> -1 then begin
              Result.Events.AddObject(eTemp, ePreEvents.Objects[k]);
              ePreEvents.Delete(k);
            end
            else
              Result.MethodsDefault.AddObject(eTemp, TObject(i));
          end
          else if IsAtStart('native', eString) then
            Result.Natives.AddObject(eTemp, TObject(i))
          else if IsAtStart('stock', eString) then
            Result.Stocks.AddObject(eTemp, TObject(i))
          else if (Pos('enum', eTemp) = Pos('operator', eTemp)) and (Pos('enum', eTemp) = 0) then
            Result.MethodsDefault.AddObject(eTemp, TObject(i));
        end;
      end;
    end;
  end;
  ePreEvents.Free;
  eStr.Free;
end;

{ TPawnParseResult }

constructor TPawnParseResult.Create;
begin
  inherited Create;

  Constants := TStringList.Create;
  Defined := TStringList.Create;
  CVars := TStringList.Create;
  Included := TStringList.Create;
  MethodsDefault := TStringList.Create;
  Events := TStringList.Create;
  Stocks := TStringList.Create;
  Natives := TStringList.Create;
  Forwards := TStringList.Create;
  Variables := TStringList.Create;

  CallTips := TStringList.Create;
  AutoComplete := TStringList.Create;
  HighlightKeywords := TStringList.Create;
end;

procedure TPawnParseResult.DestroyResult;
begin
  AutoComplete.Free;
  CallTips.Free;
  Constants.Free;
  CVars.Free;
  Defined.Free;
  Events.Free;
  Forwards.Free;
  HighlightKeywords.Free;
  Included.Free;
  MethodsDefault.Free;
  Natives.Free;
  Stocks.Free;
  Variables.Free;

  Free;
end;

initialization

  eLookedUpIncluded := TStringList.Create;

finalization

  eLookedUpIncluded.Free;


end.

