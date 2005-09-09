unit UnitTextAnalyze;

interface

uses SysUtils, Classes, Windows, Forms;

type TPAWNParseResult = class
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

function ParseCodePAWN(eCode: TStringList; FileName: String; IsRecursive: Boolean = False): TPAWNParseResult;
function UpdateIncPath(eInput: String): String;

var eCPUSpeed: Integer = 1;

implementation

uses UnitCodeExplorerUpdater, UnitCodeUtils, UnitfrmSettings,
  UnitMainTools, UnitfrmMain;

var eLookedUpIncluded: TStringList;

function UpdateIncPath(eInput: String): String;
begin
  eInput := StringReplace(Trim(eInput), '/', '\', [rfReplaceAll]);
  if FileExists(ExtractFilePath(frmSettings.txtPAWNCompilerPath.Text) + eInput + '.inc') then
    Result := ExtractFilePath(frmSettings.txtPAWNCompilerPath.Text) + eInput + '.inc'
  else if FileExists(ExtractFilePath(frmSettings.txtPAWNCompilerPath.Text) + 'include\' + eInput + '.inc') then
    Result := ExtractFilePath(frmSettings.txtPAWNCompilerPath.Text) + 'include\' + eInput + '.inc'
  else if (FileExists(ExtractFilePath(ActiveDoc.FileName) + eInput + '.inc')) and (not ActiveDoc.Modified) then
    Result := ExtractFilePath(ActiveDoc.FileName) + eInput + '.inc'
  else
    Result := '';
end;

function ParseCodePAWN(eCode: TStringList; FileName: String; IsRecursive: Boolean = False): TPAWNParseResult;
var i, k: integer;
    eString, eTemp, eBackup: string;
    eStr, ePreEvents: TStringList;
    eStartLine, eBracesOpen: Integer;
    eTimeToSleep: Integer;
    eAddingEnum: Integer;
    eTempResult: TPawnParseResult;
    eProcedureAdded: Boolean;
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

  for i := 0 to eCode.Count - 1 do begin
    if (Application.Terminated) or (not Started) or (frmMain.pnlLoading.Visible) or (not frmMain.trvExplorer.Visible) then exit;

    eString := RemoveStringsAndComments(Trim(eCode[i]), True);
    eBackup := Trim(eCode[i]);
    
    eProcedureAdded := False;
    Inc(eTimeToSleep, 1);

    if eTimeToSleep = eCPUSpeed then begin
      Sleep(1);
      eTimeToSleep := 0;
    end;

    if Pos('smbans/constants.inl"', eString) <> 0 then
      eString := eString;
    { Constants and Variables }
    if (IsAtStart('new', eString)) and (eBracesOpen = 0) and (not IsRecursive) then begin // const or variable
      Delete(eString, 1, 4);
      eString := Trim(eString);
      // we don't need braces so delete them...
      while (CountChars(eString, '{') <> 0) and (CountChars(eString, '}') <> 0) and (Pos('{', eString) < Pos('}', eString)) do
        eString := StringReplace(eString, '{' + Between(eString, '{', '}') + '}', '', [rfReplaceAll]);
      while (CountChars(eString, '[') <> 0) and (CountChars(eString, ']') <> 0) and (Pos('[', eString) < Pos(']', eString)) do
        eString := StringReplace(eString, '[' + Between(eString, '[', ']') + ']', '', [rfReplaceAll]);
      // done? okay, split all items if there are more than one; and if not, it's okay...
      eStr.Text := StringReplace(eString, ',', #13, [rfReplaceAll]);
      for k := 0 to eStr.Count - 1 do begin
        if (Trim(eStr[k]) <> '') and (eStr[k] <> '}') then begin
          eTemp := Trim(RemoveSemicolon(eStr[k]));

          if Pos(':', eTemp) <> 0 then
            eTemp := Copy(eTemp, Pos(':', eTemp) + 1, Length(eTemp));

          if Pos('=', eTemp) <> 0 then begin // constant
            Result.Constants.AddObject(Copy(eTemp, 1, Pos('=', eTemp) - 1), TObject(i));
            Result.AutoComplete.Add(Copy(eTemp, 1, Pos('=', eTemp) - 1));
          end
          else begin // variable
            Result.Variables.AddObject(eTemp, TObject(i));
            Result.AutoComplete.Add(eTemp);
          end;
        end;
      end;
      eString := RemoveStringsAndComments(Trim(eCode[i]), True);
    end
    { Included }
    else if (IsAtStart('#include', eBackup)) then begin
      eString := StringReplace(eBackup, '/', '\', [rfReplaceAll]);
      if Between(eString, '<', '>') <> '' then begin
        eString := Between(eString, '<', '>');
        if ExtractFileExt(eString) <> '' then
          ChangeFileExt(eString, '');
      end
      else if Between(eString, '"', '"') <> '' then begin
        eString := Between(eString, '"', '"');
        if ExtractFileExt(eString) <> '' then
          ChangeFileExt(eString, '');
      end
      else begin
        eString := Trim(eString);
        if ExtractFileExt(eString) <> '' then
          ChangeFileExt(eString, '');
      end;
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
            eTempResult := ParseCodePAWN(eStr, ExtractFileName(eTemp), True);
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
    end
    { CVars }
    else if (IsAtStart('register_cvar', eString)) and (not IsRecursive) then begin
      if Between(eString, '"', '"') <> '' then
        Result.CVars.AddObject(Between(eBackup, '"', '"'), TObject(i));
    end
    { Defined }
    else if (IsAtStart('#define', eString)) then begin
      eString := Copy(eString, 8, Length(eString));
      eString := Trim(eString);
      Result.CallTips.Add(eString + '-> ' + FileName);
      if Pos(#32, eString) <> 0 then
        eString := Copy(eString, 1, Pos(#32, eString) - 1);
      if Pos('	', eString) <> 0 then
        eString := Copy(eString, 1, Pos('	', eString) - 1);
      Result.Defined.AddObject(eString, TObject(i));
      Result.AutoComplete.Add(eString);
    end
    { Events (Part 1) }
    else if (IsAtStart('register_event(', eString)) and (not IsRecursive) then begin
      if CountChars(eBackup, '"') >= 4 then begin
        eTemp := StringReplace(eBackup, '"' + Between(eBackup, '"', '"') + '"', '', []);
        ePreEvents.Add(Between(eBackup, '"', '"'));
      end;
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
    if (Pos('}', eString) <> 0) and (not IsAtStart('new', Trim(eCode[eStartLine]))) then begin
      { Enums -> }
      if eAddingEnum <> 0 then
        eAddingEnum := 0;
        
      { <- Enums }
      if (eBracesOpen = 0) and (Length(Trim(eCode[eStartLine])) > 1) then begin
        eTemp := Trim(RemoveSemicolon(Trim(eCode[eStartLine])));

        if eTemp[Length(eTemp)] = '{' then
          eTemp := Trim(Copy(eTemp, 1, Length(eTemp) -1));

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
        if (Pos(#32, eTemp) <> 0) and (Pos(#32, eTemp) < Pos('(', eTemp)) then
          eTemp := Copy(eCode[eStartLine], Pos(#32, eCode[eStartLine]) + 1, Length(eCode[eStartLine]))
        else if (Pos(#9, eTemp) <> 0) and (Pos(#9, eTemp) < Pos('(', eTemp)) then
          eTemp := Copy(eTemp, Pos(#9, eTemp) + 1, Length(eTemp));
        // Remove return-type
        if (Pos(':', eTemp) <> 0) and (Pos(':', eTemp) < Pos('(', eTemp)) then
          Delete(eTemp, 1, Pos(':', eTemp));

        if Pos('operator', eTemp) = 1 then
          k := 6;

        if k < 5 then
          Result.CallTips.Add(eTemp + '-> ' + FileName);
        // Copy function-name
        if Pos('(', eTemp) <> 0 then
          eTemp := Copy(eTemp, 1, Pos('(', eTemp) - 1);
        eTemp := Trim(eTemp);

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
      if (not eProcedureAdded) and (Pos('(', eString) <> 0) then begin
        eTemp := Trim(RemoveSemicolon(eString));
        if eTemp[Length(eTemp)] = '{' then
          eTemp := Trim(Copy(eTemp, 1, Length(eTemp) -1));

        // Remove type
        if (Pos(#32, eTemp) <> 0) and (Pos(#32, eTemp) < Pos('(', eTemp)) then
          eTemp := Copy(eTemp, Pos(#32, eTemp) + 1, Length(eTemp));
        if (Pos(#9, eTemp) <> 0) and (Pos(#9, eTemp) < Pos('(', eTemp)) then
          eTemp := Copy(eTemp, Pos(#9, eTemp) + 1, Length(eTemp));
        // Remove return-type
        if (Pos(':', eTemp) <> 0) and (Pos(':', eTemp) < Pos('(', eTemp)) then
          Delete(eTemp, 1, Pos(':', eTemp));

        if (Pos('enum', eTemp) = Pos('operator', eTemp)) and (Pos('enum', eTemp) = 0) then
          Result.CallTips.Add(eTemp + '-> ' + FileName);
          
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

{ TPAWNParseResult }

constructor TPAWNParseResult.Create;
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

procedure TPAWNParseResult.DestroyResult;
begin
  Constants.Free;
  Defined.Free;
  CVars.Free;
  Included.Free;
  MethodsDefault.Free;
  Events.Free;
  Stocks.Free;
  Natives.Free;
  Forwards.Free;
  Variables.Free;

  CallTips.Free;
  AutoComplete.Free;
  HighlightKeywords.Free;

  Free;
end;

initialization

  eLookedUpIncluded := TStringList.Create;

finalization

  eLookedUpIncluded.Free;


end.
