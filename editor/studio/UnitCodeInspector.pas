unit UnitCodeInspector;

interface

uses SysUtils, Classes, Windows, JvInspector, UnitMainTools, Contnrs;

type
  TSelectionTextList = class(TStringList)
  private
    FSelected: Integer;
    function GetSelectedText: string;
    function GetSelected: Integer;
    procedure SetSelectedText(const Value: string);
    procedure SetSelected(const Value: Integer);
  public
    property Selected: Integer read GetSelected write SetSelected;
    property SelectedText: string read GetSelectedText write SetSelectedText;
  end;

  TJvInspectorSelectionTextListItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;

  TStringWrapper = class(TObject)
  public
    Value: string;
    constructor Create(const AValue: string); reintroduce;
  end;

  TSTLWrapper = class(TObject)
  public
    STL: TSelectionTextList;
    Value: String;
    constructor Create(const ASTL: TSelectionTextList; const AValue: String); reintroduce;
    destructor Destroy; reintroduce;
  end;

function AddField(eName, eCategory, eValue: String): TJvCustomInspectorItem;
function AddCombo(eName, eCategory, eValue: String; eValues: array of string): TJvCustomInspectorItem;

procedure UpdateCI;
procedure UpdateCI_Pawn;

var eFormatSettings: String;
    eAllIncluded: TStringArray;
    
    FItems: TObjectList;
    STLItem: TSelectionTextList;

implementation

uses UnitfrmMain, UnitLanguages, UnitCodeUtils, UnitMenuGenerators,
  UnitPlugins;

var eBraceTexts: TStringList;

{ "Combobox"-Item }

function TSelectionTextList.GetSelected: Integer;
begin
  if FSelected < -1 then
    FSelected := -1
  else if FSelected >= Count then
    FSelected := Count - 1;
  Result := FSelected;
end;

function TSelectionTextList.GetSelectedText: string;
begin
  if Selected >= 0 then
    Result := Strings[Selected]
  else
    Result := '';
end;

procedure TSelectionTextList.SetSelected(const Value: Integer);
begin
  FSelected := Value;
  GetSelected; // adjust FSelected
end;

procedure TSelectionTextList.SetSelectedText(const Value: string);
begin
  FSelected := IndexOf(Value);
end;

function TJvInspectorSelectionTextListItem.GetDisplayValue: string;
begin
  Result := TSelectionTextList(Data.AsOrdinal).SelectedText;
end;

procedure TJvInspectorSelectionTextListItem.GetValueList(const Strings: TStrings);
begin
  Strings.Assign(TSelectionTextList(Data.AsOrdinal));
end;

procedure TJvInspectorSelectionTextListItem.SetDisplayValue(const Value: string);
begin
  TSelectionTextList(Data.AsOrdinal).SelectedText := Value;
end;

procedure TJvInspectorSelectionTextListItem.SetFlags(const Value: TInspectorItemFlags);
begin
  inherited SetFlags(Value + [iifValueList]);
end;

constructor TStringWrapper.Create(const AValue: string);
begin
  inherited Create;
  Value := AValue;
end;

{ Codeinspector Add Functions }

function AddCombo(eName, eCategory, eValue: String; eValues: array of string): TJvCustomInspectorItem;
var
  i: integer;
  eParent: TJvCustomInspectorItem;
  Item: TSTLWrapper;
  Found: Boolean;
begin
  eParent := nil;
  for i := 0 to frmMain.jviCode.Root.Count -1 do
  begin
    if (frmMain.jviCode.Root.Items[i].DisplayName = eCategory) and (frmMain.jviCode.Root.Items[i] is TJvInspectorCustomCategoryItem) then
    begin
      eParent := frmMain.jviCode.Root.Items[i];
      Break;
    end;
  end;
  if eParent = nil then
  begin
    eParent := TJvInspectorCustomCategoryItem.Create(frmMain.jviCode.Root, nil);
    eParent.DisplayName := eCategory;
  end;

  if eName <> '' then
  begin
    STLItem := TSelectionTextList.Create;
    Found := False;
    for i := 0 to High(eValues) do begin
      STLItem.Add(eValues[i]);
      if eValues[i] = eValue then
        Found := True;
    end;

    if not Found then begin
      STLItem.Add(eValue);
      STLItem.Sort;
    end;

    Item := TSTLWrapper.Create(STLItem, eValue);
    FItems.Add(Item);
    FItems.Add(STLItem);
    
    STLItem.SelectedText := Item.Value;
    Result := TJvInspectorVarData.New(eParent, eName, TypeInfo(TSelectionTextList), @Item.STL);
    frmMain.jviCode.Root.Sort;
    eParent.Expanded := True;
  end
  else
  begin
    Result := nil;
    frmMain.jviCode.Root.Sort;
  end;
end;

function AddField(eName, eCategory, eValue: String): TJvCustomInspectorItem;
var
  i: integer;
  eParent: TJvCustomInspectorItem;
  Item: TStringWrapper;
begin
  eParent := nil;
  for i := 0 to frmMain.jviCode.Root.Count -1 do
  begin
    if (frmMain.jviCode.Root.Items[i].DisplayName = eCategory) and (frmMain.jviCode.Root.Items[i] is TJvInspectorCustomCategoryItem) then
    begin
      eParent := frmMain.jviCode.Root.Items[i];
      Break;
    end;
  end;
  if eParent = nil then
  begin
    eParent := TJvInspectorCustomCategoryItem.Create(frmMain.jviCode.Root, nil);
    eParent.DisplayName := eCategory;
  end;

  if eName <> '' then
  begin
    Item := TStringWrapper.Create(eValue); // StringWrapper erzeugen, damit der String erhalten bleibt
    FItems.Add(Item); // und das Item in die Liste eintragen, damit kein Speicherleck entsteht
    Result := TJvInspectorVarData.New(eParent, eName, TypeInfo(String), @Item.Value);
    frmMain.jviCode.Root.Sort;
    eParent.Expanded := True;
  end
  else
  begin
    Result := nil;
    frmMain.jviCode.Root.Sort;
  end;
end;

{ Parse Functions }

procedure UpdateCI;
begin
  if not Plugin_UpdateCodeInspector(GetCurrLang.Name, ActiveDoc.FileName, frmMain.tsMain.Items[frmMain.tsMain.ActiveTabIndex].Caption, True) then exit;

  if GetCurrLang.Name = 'Pawn' then begin
    UpdateCI_Pawn;
    Plugin_UpdateCodeInspector(GetCurrLang.Name, ActiveDoc.FileName, frmMain.tsMain.Items[frmMain.tsMain.ActiveTabIndex].Caption, False);
  end;
end;

procedure UpdateCI_Pawn;
procedure HideBracesAndStrings(var eStr: String);
begin
  while Between(eStr, '{', '}') <> '' do begin
    eBraceTexts.Add('{' + Between(eStr, '{', '}') + '}');
    eStr := StringReplace(eStr, '{' + Between(eStr, '{', '}') + '}', #1 + IntToStr(eBraceTexts.Count) + #1, []);
  end;
  while CountChars(eStr, '"') > 1 do begin
    eBraceTexts.Add('"' + StringReplace(Between(eStr, '"', '"'), ':', #3, [rfReplaceAll]) + '"');
    eStr := StringReplace(eStr, '"' + Between(eStr, '"', '"') + '"', #2 + IntToStr(eBraceTexts.Count) + #2, []);
  end;
end;

function ShowBracesAndStrings(eStr: String): String;
var k: integer;
begin
  while Between(eStr, #1, #1) <> '' do begin
     k := StrToInt(Between(eStr, #1, #1));
     eStr := StringReplace(eStr, #1 + IntToStr(k) + #1, eBraceTexts[k -1], []);
  end;
  while Between(eStr, #2, #2) <> '' do begin
    k := StrToInt(Between(eStr, #2, #2));
    eStr := StringReplace(eStr, #2 + IntToStr(k) + #2, eBraceTexts[k -1], []);
  end;
  Result := eStr;
end;

var eCurrLine, eBackupLine: String;
    i, k: integer;
    eStr: TStringList;
    eVars, eConsts: Integer;
    eVarName, eVarType, eVarValue: String;
begin
  eBackupLine := frmMain.sciEditor.Lines[frmMain.sciEditor.GetCurrentLineNumber];
  eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
  eCurrLine := RemoveStringsAndComments(eCurrLine, False);
  eAllIncluded := GetAllIncludeFiles;
  eStr := TStringList.Create;
  eBraceTexts := TStringList.Create;
  eVars := 0;
  eConsts := 0;
  frmMain.jviCode.Clear;
  FItems.Clear;

  eFormatSettings := '';
  { Constants and Variables }
  if (IsAtStart('new', eCurrLine, False)) then begin // const or variable
    Delete(eCurrLine, 1, 4);

    // done? okay, split all items if there are more than one; and if not, it's okay...
    HideBracesAndStrings(eCurrLine);
    eStr.Text := StringReplace(eCurrLine, ',', #13, [rfReplaceAll]);

    for i := 0 to eStr.Count - 1 do begin
      eStr[i] := ShowBracesAndStrings(eStr[i]);
      eVarType := '';
      eVarValue := '';
      
      if (Trim(eStr[i]) <> '') then begin
        eVarName := Trim(RemoveSemicolon(eStr[i]));
        if Pos(':', eVarName) <> 0 then begin
          eVarType := Trim(Copy(eVarName, 1,  Pos(':', eVarName) -1));
          eVarName := Trim(Copy(eVarName, Pos(':', eVarName) +1, Length(eVarName)));
        end;

        if Pos('=', eVarName) <> 0 then begin // constant
          Inc(eConsts, 1);
          eFormatSettings := eFormatSettings + '-Constant ' + IntToStr(eConsts) + '-';

          eVarValue := Trim(Copy(eVarName, Pos('=', eVarName) +1, Length(eVarName)));
          eVarValue := StringReplace(eVarValue, #3, ':', [rfReplaceAll]);
          eVarName := Trim(Copy(eVarName, 1, Pos('=', eVarName) -1));

          AddField(lName, 'Constant ' + IntToStr(eConsts), eVarName);
          if eVarType <> '' then
            AddField(lType, 'Constant ' + IntToStr(eConsts), eVarType);
          if eVarValue <> '' then
            AddField(lValue, 'Constant ' + IntToStr(eConsts), eVarValue);
        end
        else begin // variable
          Inc(eVars, 1);
          eFormatSettings := eFormatSettings + '-Variable ' + IntToStr(eVars) + '-';
          AddField(lName, 'Variable ' + IntToStr(eVars), eVarName);
          if eVarType <> '' then
            AddField(lType, 'Variable ' + IntToStr(eVars), eVarType);            
       end;
      end;
    end;

    if frmMain.jviCode.Root.Count = 1 then
      frmMain.jviCode.Root.Items[0].DisplayName := Copy(frmMain.jviCode.Root.Items[0].DisplayName, 1, Length(frmMain.jviCode.Root.Items[0].DisplayName) -2);
  end
  { Conditions }
  else if (IsAtStart('if', eCurrLine, False)) then begin
    if (CountChars(eCurrLine, '(') = CountChars(eCurrLine, ')')) and (CountChars(eCurrLine, '(') <> 0) then begin
      eCurrLine := Copy(eCurrLine, 1, GetMatchingBrace(eCurrLine) -1);
      eCurrLine := Copy(eCurrLine, Pos('(', eCurrLine) +1, Length(eCurrLine));
      eFormatSettings := StringReplace(eBackupLine, '(' + eCurrLine + ')', #1#3#3#7, []);
      HideBracesAndStrings(eCurrLine);
      eStr.Text := StringReplace(eCurrLine, '||', #13, [rfReplaceAll]);
      k := eStr.Count -1; // from 0 to k -> OR operators
      eStr.Text := StringReplace(eStr.Text, '&&', #13, [rfReplaceAll]);

      for i := 0 to eStr.Count -1 do begin
        eStr[i] := Trim(ShowBracesAndStrings(eStr[i]));
        if (Pos('(', eStr[i]) = 1) and (Pos(')', eStr[i]) = Length(eStr[i])) then
          eStr[i] := Copy(Copy(eStr[i], 2, Length(eStr[i])), 1, Length(eStr[i]) -1);

        if eStr.Count = 1 then
          eCurrLine := 'If-Condition'
        else
          eCurrLine := 'If-Condition ' + IntToStr(frmMain.jviCode.Root.Count +1);

        if i <> eStr.Count -1 then begin
          if i < k then
            AddCombo('Operator', eCurrLine, '||', ['||', '&&'])
          else
            AddCombo('Operator', eCurrLine, '&&', ['||', '&&']);
        end;
        AddField('Condition', eCurrLine, eStr[i]);
      end;
    end
    else
      AddField('', 'Invalid condition', '');
  end
  { Defined }
  else if (IsAtStart('#define', eCurrLine, False)) then begin
    eCurrLine := Trim(Copy(eCurrLine, 8, Length(eCurrLine)));
    HideBracesAndStrings(eCurrLine);
    eCurrLine := StringReplace(eCurrLine, #9, #32, [rfReplaceAll]);
    eCurrLine := ShowBracesAndStrings(eCurrLine);
    AddField('Name', 'Defined', Copy(eCurrLine, 1, Pos(#32, eCurrLine) -1));
    eCurrLine := Trim(Copy(eCurrLine, Pos(#32, eCurrLine) +1, Length(eCurrLine)));
    AddField('Value', 'Defined', eCurrLine);
  end
  { Included }
  else if (IsAtStart('#include', eCurrLine, False)) then begin
    eCurrLine := Trim(StringReplace(eCurrLine, #9, #32, [rfReplaceAll]));
    if Between(eCurrLine, '<', '>') <> '' then begin
      eCurrLine := Between(eCurrLine, '<', '>');
      eFormatSettings := StringReplace(eBackupLine, '<' + eCurrLine + '>', '<-Filename->', []);
    end
    else if Between(eCurrLine, '"', '"') <> '' then begin
      eCurrLine := Between(eCurrLine, '"', '"');
      eFormatSettings := StringReplace(eBackupLine, '"' + eCurrLine + '"', '"-Filename-"', []);
    end
    else begin
      eCurrLine := Copy(eCurrLine, 9, Length(eCurrLine));
      eFormatSettings := '#include -Filename-';
    end;
    eCurrLine := Trim(eCurrLine);
    AddCombo('File', 'Included', eCurrLine, eAllIncluded);
  end
  { Assignments }
  else begin
    if (Pos('=', eCurrLine) <> 0) then begin
      eCurrLine := Trim(eCurrLine);
      while Pos(eCurrLine[1], frmMain.sciEditor.WordChars + '[]') <> 0 do
        Delete(eCurrLine, 1, 1);
      eCurrLine := Trim(eCurrLine);
      if Pos('=', eCurrLine) <= 2 then begin
        while (Pos(Copy(eCurrLine, 1, 1), frmMain.sciEditor.WordChars + #32 + #9) = 0) and (Length(eCurrLine) <> 0) do begin
           eFormatSettings := eFormatSettings + eCurrLine[1];
           Delete(eCurrLine, 1, 1);
        end;
        eCurrLine := RemoveSemicolon(Trim(eBackupLine));
        eCurrLine := StringReplace(eCurrLine, #9, #32, [rfReplaceAll]);
        AddField('a', 'Assignment', Copy(eCurrLine, 1, Pos(eFormatSettings, eCurrLine) - Length(eFormatSettings)));
        AddField('b', 'Assignment', Trim(Copy(eCurrLine, Pos(eFormatSettings, eCurrLine) + Length(eFormatSettings), Length(eCurrLine))));
        AddField('Operator', 'Assignment', eFormatSettings);
      end
      else
        AddField('', 'No information available.', '');
    end
    else
      AddField('', 'No information available.', '');
  end;
  eBraceTexts.Free;
  SetLength(eAllIncluded, 0);
  eStr.Free;
end;

{ TSTLWrapper }

constructor TSTLWrapper.Create(const ASTL: TSelectionTextList;
  const AValue: String);
begin
  STL := ASTL;
  Value := AValue;
  inherited Create;
end;

destructor TSTLWrapper.Destroy;
begin
  STL.Free;
  inherited;
end;

initialization

FItems := TObjectList.Create;

finalization

FItems.Free;

end.
