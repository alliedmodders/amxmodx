unit UnitCodeInspector;

interface

uses SysUtils, Classes, Windows, JvInspector, UnitMainTools, Contnrs, Forms;

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
    destructor Destroy; reintroduce;
  end;

  TSTLWrapper = class(TObject)
  public
    STL: TSelectionTextList;
    Value: string;
    constructor Create(const ASTL: TSelectionTextList; const AValue: string); reintroduce;
    destructor Destroy; reintroduce;
  end;

function AddField(eName, eCategory, eValue: string): TJvCustomInspectorItem;
function AddCombo(eName, eCategory, eValue: string; eValues: array of string): TJvCustomInspectorItem;
function FindItem(eParent: TJvCustomInspectorItem; eName: string): TJvCustomInspectorItem;
function CreateCategory(eCategory: string): TJvInspectorCustomCategoryItem;

procedure UpdateCI(eLine: Integer);
procedure UpdateCI_Pawn(eLine: Integer);

procedure RebuildLine;

var eCILine: Integer;
    FItems: TObjectList;
    STLItem: TSelectionTextList;
    eStock: Boolean;
    eUpdating: Boolean;

implementation

uses UnitfrmMain, UnitLanguages, UnitCodeUtils, UnitMenuGenerators,
  UnitPlugins, UnitCodeExplorerUpdater, UnitfrmSettings;

var eBraceTexts: TStringList;

procedure HideBracesAndStrings(var eStr: string; ReplaceAll: Boolean);
begin
  eStr := StringReplace(eStr, '^"', #5, [rfReplaceAll]);
  while Between(eStr, '"', '"') <> '' do begin
    eBraceTexts.Add('"' + Between(eStr, '"', '"') + '"');
    eStr := StringReplace(eStr, '"' + Between(eStr, '"', '"') + '"', #3 + IntToStr(eBraceTexts.Count) + #3, []);
  end;
  while Between(eStr, '{', '}') <> '' do begin
    eBraceTexts.Add('{' + Between(eStr, '{', '}') + '}');
    eStr := StringReplace(eStr, '{' + Between(eStr, '{', '}') + '}', #1 + IntToStr(eBraceTexts.Count) + #1, []);
  end;
  if ReplaceAll then begin
    while Between(eStr, '(', ')', True) <> '' do begin
      eBraceTexts.Add('(' + Between(eStr, '(', ')', True) + ')');
      eStr := StringReplace(eStr, '(' + Between(eStr, '(', ')', True) + ')', #2 + IntToStr(eBraceTexts.Count) + #2, []);
    end;
  end;
end;

function ShowBracesAndStrings(eStr: string): string;
var k: integer;
begin
  while Between(eStr, #1, #1) <> '' do begin
    k := StrToInt(Between(eStr, #1, #1));
    eStr := StringReplace(eStr, #1 + IntToStr(k) + #1, eBraceTexts[k - 1], [])
  end;
  while Between(eStr, #2, #2) <> '' do begin
    k := StrToInt(Between(eStr, #2, #2));
    eStr := StringReplace(eStr, #2 + IntToStr(k) + #2, eBraceTexts[k - 1], [])
  end;
  while Between(eStr, #3, #3) <> '' do begin
    k := StrToInt(Between(eStr, #3, #3));
    eStr := StringReplace(eStr, #3 + IntToStr(k) + #3, eBraceTexts[k - 1], [])
  end;
  eStr := StringReplace(eStr, #5, '^"', [rfReplaceAll]);
  Result := eStr;
end;

procedure CheckCIComment(eCurrLine: string);
begin
  if (Pos('//', eCurrLine) < Pos('/*', eCurrLine)) and (Pos('//', eCurrLine) <> 0) then begin
    CreateCategory('Comment');
    AddField('Line Comment', 'Comment', Trim(Copy(eCurrLine, Pos('//', eCurrLine) + 2, Length(eCurrLine))));
  end
  else if (Pos('//', eCurrLine) <> 0) and (Pos('/*', eCurrLine) = 0) then begin
    CreateCategory('Comment');
    AddField('Line Comment', 'Comment', Trim(Copy(eCurrLine, Pos('//', eCurrLine) + 2, Length(eCurrLine))));
  end
  else if Pos('/*', eCurrLine) <> 0 then begin
    CreateCategory('Comment');
    if Pos('*/', eCurrLine) = 0 then
      AddField('Doc Comment', 'Comment', Trim(Copy(eCurrLine, Pos('/*', eCurrLine) + 2, Length(eCurrLine))))
    else begin
      eCurrLine := Copy(eCurrLine, Pos('/*', eCurrLine) + 2, Length(eCurrLine));
      AddField('Doc Comment', 'Comment', Trim(Copy(eCurrLine, 1, Pos('*/', eCurrLine) - 2)));
    end;
  end;
end;

function Assignment(eLine: string): string;
begin
  Result := '';
  HideBracesAndStrings(eLine, True);
  eLine := RemoveStringsAndComments(eLine, True, True);

  if Pos('=', eLine) <> 0 then
    Result := '=';
  if Pos('+=', eLine) <> 0 then
    Result := '+=';
  if Pos('-=', eLine) <> 0 then
    Result := '-=';
  if Pos('*=', eLine) <> 0 then
    Result := '*=';
  if Pos('/=', eLine) <> 0 then
    Result := '/=';
  if Pos('%=', eLine) <> 0 then
    Result := '%=';
  if Pos('>>=', eLine) <> 0 then
    Result := '>>=';
  if Pos('>>>=', eLine) <> 0 then
    Result := '>>>=';
  if Pos('<<=', eLine) <> 0 then
    Result := '<<=';
  if Pos('&=', eLine) <> 0 then
    Result := '&=';
  if Pos('|=', eLine) <> 0 then
    Result := '|=';

  if (Pos(Result, Trim(eLine)) < Pos('(', Trim(eLine))) and (Result <> '') then
    Result := '';
end;

procedure AddFunc(eValueName, eFuncCall, eParent: string);
var eStr: TStringList;
  i: integer;
begin
  eFuncCall := Trim(eFuncCall);
  if (CountChars(eFuncCall, '(') = CountChars(eFuncCall, ')')) and (CountChars(eFuncCall, '(') = 0) then
    AddField(eValueName, eParent, eFuncCall) // Simple assignment
  else if (Trim(Copy(eFuncCall, 1, Pos('(', eFuncCall) - 1)) = '') or (Pos('"', eFuncCall) = 1) then
    AddField(eValueName, eParent, eFuncCall) // Simple assignment
  else begin // Function call
    AddField(eValueName, eParent, 'Function Call').ReadOnly := True;
    AddField('Function', eParent + '->' + eValueName, Trim(Copy(eFuncCall, 1, Pos('(', eFuncCall) - 1)));

    eStr := TStringList.Create;
    eFuncCall := Copy(eFuncCall, Pos('(', eFuncCall) + 1, GetMatchingBrace(eFuncCall) - Pos('(', eFuncCall) - 1);
    HideBracesAndStrings(eFuncCall, True);
    eStr.Text := StringReplace(eFuncCall, ',', #13, [rfReplaceAll]);
    for i := 0 to eStr.Count - 1 do
      AddField('Param ' + IntToStr(i + 1), eParent + '->' + eValueName, Trim(ShowBracesAndStrings(eStr[i])));
    eStr.Free;
  end;
end;


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
  if IndexOf(Value) = -1 then
    FSelected := Add(Value)
  else
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
  inherited SetFlags(Value + [iifValueList, iifAllowNonListValues]);
end;

constructor TStringWrapper.Create(const AValue: string);
begin
  inherited Create;
  Value := AValue;
end;

{ Codeinspector Add Functions }

function AddCombo(eName, eCategory, eValue: string; eValues: array of string): TJvCustomInspectorItem;
var i: integer;
  eParent: TJvCustomInspectorItem;
  Item: TSTLWrapper;
  Found: Boolean;
begin
  eName := StringReplace(eName, '&', '&&', [rfReplaceAll]);

  eParent := FindItem(nil, eCategory);
  if eParent = nil then begin
    eParent := CreateCategory(eCategory);
    eCategory := Copy(eCategory, Pos('->', eCategory) + 2, Length(eCategory));
  end;

  if eName <> '' then begin
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
    eParent.Expanded := True;
  end
  else
    Result := nil;
end;

function AddField(eName, eCategory, eValue: string): TJvCustomInspectorItem;
var eParent: TJvCustomInspectorItem;
  Item: TStringWrapper;
begin
  eName := StringReplace(eName, '&', '&&', [rfReplaceAll]);
  
  eParent := FindItem(nil, eCategory);
  if eParent = nil then begin
    eParent := CreateCategory(eCategory);
    eCategory := Copy(eCategory, Pos('->', eCategory) + 2, Length(eCategory));
  end;

  if eName <> '' then begin
    Item := TStringWrapper.Create(eValue);
    FItems.Add(Item);
    Result := TJvInspectorVarData.New(eParent, eName, TypeInfo(string), @Item.Value);
    Result.SortKind := iskNone;
    eParent.Expanded := True;
  end
  else
    Result := nil;
end;

function FindItem(eParent: TJvCustomInspectorItem; eName: string): TJvCustomInspectorItem;
var i: integer;
  eParentText: string;
begin
  if Pos('->', eName) <> 0 then begin
    eParentText := Copy(eName, 1, Pos('->', eName) - 2);
    eName := Copy(eName, Pos('->', eName) + 2, Length(eName));
  end
  else
    eParentText := '';

  Result := nil;
  if Assigned(eParent) then begin
    for i := 0 to eParent.Count - 1 do begin
      if eParent.Items[i].DisplayName = eName then
        Result := eParent.Items[i]
      else if eParent.Items[i].Count <> 0 then
        Result := FindItem(eParent.Items[i], eName);

      if Assigned(Result) then begin
        if (Assigned(Result.Parent)) and (eParentText <> '') then begin
          if eParentText = Result.Parent.DisplayName then
            exit
          else
            Result := nil;
        end
        else
          exit;
      end;
    end;
  end
  else begin
    for i := 0 to frmMain.jviCode.Root.Count - 1 do begin
      if frmMain.jviCode.Root.Items[i].DisplayName = eName then
        Result := frmMain.jviCode.Root.Items[i]
      else if frmMain.jviCode.Root.Items[i].Count <> 0 then
        Result := FindItem(frmMain.jviCode.Root.Items[i], eName);

      if Assigned(Result) then begin
        if (Assigned(Result.Parent)) and (eParentText <> '') then begin
          if eParentText = Result.Parent.DisplayName then
            exit
          else
            Result := nil;
        end
        else
          exit;
      end;
    end;
  end;
end;

function CreateCategory(eCategory: string): TJvInspectorCustomCategoryItem;
begin
  if Pos('->', eCategory) <> 0 then
    eCategory := Copy(eCategory, Pos('->', eCategory) + 2, Length(eCategory));

  Result := TJvInspectorCustomCategoryItem(FindItem(nil, eCategory));
  if Assigned(Result) then exit;

  Result := TJvInspectorCustomCategoryItem.Create(frmMain.jviCode.Root, nil);
  Result.DisplayName := eCategory;
  Result.SortKind := iskNone;
end;

{ Parse Functions }

procedure UpdateCI(eLine: Integer);
begin
  if (eUpdating) or (not Assigned(ActiveDoc)) then exit;
  eUpdating := True;
  if not Plugin_UpdateCodeInspector(GetCurrLang.Name, ActiveDoc.FileName, frmMain.tbDocs.SelectedTab.Caption, True) then exit;

  if GetCurrLang.Name = 'Pawn' then begin
    UpdateCI_Pawn(eLine);
    Plugin_UpdateCodeInspector(GetCurrLang.Name, ActiveDoc.FileName, frmMain.tbDocs.SelectedTab.Caption, False);
  end;
  eUpdating := False;
end;

procedure UpdateCI_Pawn(eLine: Integer);
var eCurrLine, eBackupLine: string;
  i, k: integer;
  eStr, eStr2: TStringList;
  eVarName, eVarType, eVarValue: string;
  eVarCount, eConstCount: Integer;
  eFound: Boolean;
begin
  eCILine := eLine;
  eBackupLine := frmMain.sciEditor.Lines[eLine];
  // Prevent parse errors
  eBackupLine := StringReplace(eBackupLine, #1, '', [rfReplaceAll]);
  eBackupLine := StringReplace(eBackupLine, #2, '', [rfReplaceAll]);
  eBackupLine := StringReplace(eBackupLine, #3, '', [rfReplaceAll]);

  eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
  eCurrLine := RemoveStringsAndComments(eCurrLine, False, True);
  eStr := TStringList.Create;
  eBraceTexts := TStringList.Create;
  frmMain.jviCode.BeginUpdate;
  frmMain.jviCode.Clear;
  frmMain.jviCode.Root.SortKind := iskNone;
  FItems.Clear;
  eVarCount := 0;
  eConstCount := 0;

  { Constants and Variables }
  if (IsAtStart('new', eCurrLine, False)) or ((IsAtStart('stock', eCurrLine, False) and (CountChars(RemoveStringsAndComments(eCurrLine, True, True), '(') = 0) and (CountChars(RemoveStringsAndComments(eCurrLine, True, True), ')') = 0))) then begin
    {--> Get var information }
    eStock := IsAtStart('stock', eCurrLine, False);
    if eStock then
      eCurrLine := Trim(Copy(eCurrLine, 6, Length(eCurrLine)))
    else
      eCurrLine := Trim(Copy(eCurrLine, 4, Length(eCurrLine)));

    HideBracesAndStrings(eCurrLine, True);
    eStr.Text := StringReplace(eCurrLine, ',', #13, [rfReplaceAll]);
    eStr.Text := ShowBracesAndStrings(eStr.Text);
    for i := 0 to eStr.Count - 1 do begin
      if Pos('/*', eStr[i]) <> 0 then
        eStr[i] := Trim(Copy(eStr[i], 1, Pos('/*', eStr[i]) - 2));

      if Pos(':', eStr[i]) <> 0 then begin // Get type
        eVarType := Trim(Copy(eStr[i], 1, Pos(':', eStr[i]) - 1));
        eStr[i] := Trim(Copy(eStr[i], Pos(':', eStr[i]) + 1, Length(eStr[i])));
      end
      else
        eVarType := '';

      if Pos('=', eStr[i]) <> 0 then begin // Is constant?
        eVarName := Trim(Copy(eStr[i], 1, Pos('=', eStr[i]) - 1));
        eVarValue := Trim(Copy(eStr[i], Pos('=', eStr[i]) + 1, Length(eStr[i])));

        Inc(eConstCount, 1);
        with CreateCategory('Constants') do begin
          AddField('Constant ' + IntToStr(eConstCount), 'Constants', '').ReadOnly := True; ;
          AddField('Name', 'Constant ' + IntToStr(eConstCount), eVarName);
          AddField('Type', 'Constant ' + IntToStr(eConstCount), eVarType);

          AddFunc('Value', eVarValue, 'Constant ' + IntToStr(eConstCount));
        end;
      end
      else begin // or variable?
        eVarName := Trim(eStr[i]);
        eVarValue := '';

        Inc(eVarCount, 1);
        with CreateCategory('Variables') do begin
          AddField('Variable ' + IntToStr(eVarCount), 'Variables', '').ReadOnly := True; ;
          AddField('Name', 'Variable ' + IntToStr(eVarCount), eVarName);
          AddField('Type', 'Variable ' + IntToStr(eVarCount), eVarType);
        end;
      end;
    end;
    {--> Analyze comments }
    eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
    eCurrLine := RemoveStringsAndComments(eCurrLine, True, False);
    CheckCIComment(eCurrLine);

    eBraceTexts.Free;
    eStr.Free;
    frmMain.jviCode.EndUpdate;
    if eLine <> eCILine then
      UpdateCI_Pawn(eLine);
    exit;
  end;
  { Conditions }
  if (IsAtStart('if', eCurrLine)) or (IsAtStart('elseif', StringReplace(StringReplace(eCurrLine, #9, '', [rfReplaceAll]), #32, '', [rfReplaceAll]))) then begin
    {--> Analyze condition }
    eCurrLine := Between(eCurrLine, '(', ')', True);
    if eCurrLine = '' then
      AddField('', 'Invalid condition', '')
    else begin
      { Get condition(s) }
      HideBracesAndStrings(eCurrLine, False);
      eStr.Text := StringReplace(eCurrLine, '&&', #13 + #254, [rfReplaceAll]);
      eStr.Text := StringReplace(eStr.Text, '||', #13 + #255, [rfReplaceAll]);
      if eStr.Count > 1 then begin
        for i := 1 to eStr.Count - 1 do begin
          if Pos(#254, eStr[i]) = 1 then
            eStr.Objects[i] := TObject(1)
          else
            eStr.Objects[i] := TObject(2);
          eStr[i] := ShowBracesAndStrings(Copy(eStr[i], 2, Length(eStr[i])));
        end;
      end;
      CreateCategory('If-Condition');
      for i := 0 to eStr.Count - 1 do begin
        eStr[i] := Trim(ShowBracesAndStrings(eStr[i]));
        if Length(eStr[i]) >= 2 then begin
          if (eStr[i] = '(') and (eStr[i][Length(eStr[i])] = ')') then
            eStr[i] := Between(eStr[i], '(', ')', True);
          AddField('Condition ' + IntToStr(i + 1), 'If-Condition', '').ReadOnly := True;
          if i <> 0 then begin
            if eStr.Objects[i] = TObject(1) then
              AddCombo('Operator', 'If-Condition->Condition ' + IntToStr(i + 1), '&&', ['&&', '||'])
            else
              AddCombo('Operator', 'If-Condition->Condition ' + IntToStr(i + 1), '||', ['||', '&&']);
          end;
          AddField('Condition', 'If-Condition->Condition ' + IntToStr(i + 1), eStr[i]);
        end;
      end;
      { Action }
      eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
      eCurrLine := RemoveStringsAndComments(eCurrLine, False, True);
      eCurrLine := Trim(Copy(eCurrLine, GetMatchingBrace(eCurrLine) + 1, Length(eCurrLine)));
     { if eCurrLine = '{' then
        AddField('Action', 'If-Condition', 'New Code-Block').ReadOnly := True
      else
        AddFunc('Action', eCurrLine, 'If-Condition'); }
    end;
    {--> Analyze comments }
    eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
    eCurrLine := RemoveStringsAndComments(eCurrLine, True, False);
    CheckCIComment(eCurrLine);

    eBraceTexts.Free;
    eStr.Free;
    frmMain.jviCode.EndUpdate;
    if eLine <> eCILine then
      UpdateCI_Pawn(eLine);
    exit;
  end;
  { Defined }
  if IsAtStart('#define', eCurrLine, False) then begin
    {--> Get definition }
    Delete(eCurrLine, 1, 7);
    if Pos(#32, eCurrLine) = 0 then
      AddField('', 'Invalid Definition', '')
    else begin
      eCurrLine := Trim(eCurrLine);
      eVarName := Copy(eCurrLine, 1, Pos(#32, eCurrLine) - 1);
      eVarValue := RemoveStringsAndComments(Trim(Copy(eCurrLine, Length(eVarName) + 1, Length(eCurrLine))), False, True);

      CreateCategory('Definition');
      AddField('Name', 'Definition', eVarName);
      AddField('Value', 'Definition', eVarValue);
    end;
    {--> Analyze comments }
    eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
    eCurrLine := RemoveStringsAndComments(eCurrLine, True, False);
    CheckCIComment(eCurrLine);

    eBraceTexts.Free;
    eStr.Free;
    frmMain.jviCode.EndUpdate;
    if eLine <> eCILine then
      UpdateCI_Pawn(eLine);
    exit;
  end;
  { Included }
  if IsAtStart('#include', eCurrLine, False) then begin
    {--> Get included file }
    Delete(eCurrLine, 1, 8);
    eCurrLine := Trim(eCurrLine);
    CreateCategory('Included Header');
    if Between(eCurrLine, '<', '>') <> '' then
      AddCombo('File', 'Included Header', Between(eCurrLine, '<', '>', True), GetAllIncludeFiles(eCurrLine))
    else if Between(eCurrLine, '"', '"') <> '' then
      AddCombo('File', 'Included Header', Between(eCurrLine, '"', '"', True), GetAllIncludeFiles(eCurrLine))
    else
      AddCombo('File', 'Included Header', eCurrLine, GetAllIncludeFiles(eCurrLine));
    {--> Analyze comments }
    eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
    eCurrLine := RemoveStringsAndComments(eCurrLine, True, False);
    CheckCIComment(eCurrLine);

    eBraceTexts.Free;
    eStr.Free;
    frmMain.jviCode.EndUpdate;
    if eLine <> eCILine then
      UpdateCI_Pawn(eLine);
    exit;
  end;
  { Return }
  if Pos('return', LowerCase(Trim(eCurrLine))) = 1 then begin
    {--> Get value }
    eCurrLine := Trim(eCurrLine);
    Delete(eCurrLine, 1, 7);
    eCurrLine := Trim(eCurrLine);
    CreateCategory('Return');
    AddCombo('Value', 'Return', Trim(eCurrLine), ['PLUGIN_HANDLED', 'PLUGIN_CONTINUE', 'PLUGIN_HANDLED_MAIN']);
    {--> Analyze comments }
    eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
    eCurrLine := RemoveStringsAndComments(eCurrLine, True, False);
    CheckCIComment(eCurrLine);

    eBraceTexts.Free;
    eStr.Free;
    frmMain.jviCode.EndUpdate;
    if eLine <> eCILine then
      UpdateCI_Pawn(eLine);
    exit;
  end;
  { For-Loop }
  if IsAtStart('for', eCurrLine) then begin
    {--> Get For-Loop }
    if (Between(eCurrLine, '(', ')') = '') or (CountChars(eCurrLine, ';') < 2) then
      AddField('', 'Invalid For-Loop', '')
    else begin
      eCurrLine := Between(eCurrLine, '(', ')', True);
      HideBracesAndStrings(eCurrLine, True);
      eStr.Text := StringReplace(eCurrLine, ';', #13, [rfReplaceAll]);
      if eStr.Count <> 3 then
        AddField('Invalid For-Loop', '', '')
      else begin
        CreateCategory('For-Loop');
        eStr.Text := ShowBracesAndStrings(eStr.Text);
        AddField('Reset', 'For-Loop', eStr[0]);
        AddField('Condition', 'For-Loop', eStr[1]);
        AddField('Increase', 'For-Loop', eStr[2]);
      end;

      {--> Action }
      eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
      eCurrLine := RemoveStringsAndComments(eCurrLine, False, True);
      eCurrLine := Trim(Copy(eCurrLine, GetMatchingBrace(eCurrLine) + 1, Length(eCurrLine)));
      if eCurrLine = '{' then
        AddField('Action', 'For-Loop', 'New Code-Block').ReadOnly := True
      else
        AddFunc('Action', eCurrLine, 'If-Condition');
      {--> Analyze comments }
      eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
      eCurrLine := RemoveStringsAndComments(eCurrLine, True, False);
      CheckCIComment(eCurrLine);
    end;
    eBraceTexts.Free;
    eStr.Free;
    frmMain.jviCode.EndUpdate;
    if eLine <> eCILine then
      UpdateCI_Pawn(eLine);
    exit;
  end;
  { While-Loops }
  if IsAtStart('while', eCurrLine) then begin
    {--> Analyze condition }
    eCurrLine := Between(eCurrLine, '(', ')', True);
    if eCurrLine = '' then
      AddField('', 'Invalid condition', '')
    else begin
      HideBracesAndStrings(eCurrLine, False);
      eStr.Text := StringReplace(eCurrLine, '&&', #13 + #254, [rfReplaceAll]);
      eStr.Text := StringReplace(eStr.Text, '||', #13 + #255, [rfReplaceAll]);
      if eStr.Count > 1 then begin
        for i := 1 to eStr.Count - 1 do begin
          if Pos(#254, eStr[i]) = 1 then
            eStr.Objects[i] := TObject(1)
          else
            eStr.Objects[i] := TObject(2);
          eStr[i] := ShowBracesAndStrings(Copy(eStr[i], 2, Length(eStr[i])));
        end;
      end;
      CreateCategory('While-Loop');
      for i := 0 to eStr.Count - 1 do begin
        eStr[i] := Trim(eStr[i]);
        if Length(eStr[i]) >= 2 then begin
          if (eStr[i] = '(') and (eStr[i][Length(eStr[i])] = ')') then
            eStr[i] := Between(eStr[i], '(', ')', True);
          AddField('Condition ' + IntToStr(i + 1), 'While-Loop', '').ReadOnly := True;
          if i <> 0 then begin
            if eStr.Objects[i] = TObject(1) then
              AddCombo('Operator', 'While-Loop->Condition ' + IntToStr(i + 1), '&&', ['&&', '||'])
            else
              AddCombo('Operator', 'While-Loop->Condition ' + IntToStr(i + 1), '||', ['||', '&&']);
          end;
          AddField('Condition', 'While-Loop->Condition ' + IntToStr(i + 1), eStr[i]);
        end;
      end;

      {--> Analyze comments }
      eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
      eCurrLine := RemoveStringsAndComments(eCurrLine, True, False);
      CheckCIComment(eCurrLine);    
    end;
    eBraceTexts.Free;
    eStr.Free;
    frmMain.jviCode.EndUpdate;
    if eLine <> eCILine then
      UpdateCI_Pawn(eLine);
    exit;
  end;

  { Function header/call }
  with GetNode('Methods') do begin
    eFound := False;
    for i := 0 to Item[0].Count - 1 do begin
      if Integer(Item[0].Item[i].Data) = eLine then begin
        eFound := True;
        break;
      end;
    end;
    if not eFound then begin
      for i := 0 to Item[1].Count - 1 do begin
        if Integer(Item[1].Item[i].Data) = eLine then begin
          eFound := True;
          break;
        end;
      end;
    end;
    if not eFound then begin
      for i := 0 to Item[2].Count - 1 do begin
        if Integer(Item[2].Item[i].Data) = eLine then begin
          eFound := True;
          break;
        end;
      end;
    end;
  end;

  if eFound then begin // Function
    if Pos('(', eCurrLine) <> Pos(')', eCurrLine) then begin
      CreateCategory('Function');
      eVarName := Trim(Copy(eCurrLine, 1, Pos('(', eCurrLine) - 1));
      if Pos(#32, eVarName) <> 0 then begin
        eVarType := Copy(eVarName, 1, Pos(#32, eVarName) - 1);
        eVarName := Copy(eVarName, Pos(#32, eVarName) + 1, Length(eVarName));
      end
      else if Pos('@', eVarName) = 1 then begin
        eVarType := '@';
        eVarName := Copy(eVarName, 2, Length(eVarName));
      end
      else
        eVarType := '';

      AddField('Name', 'Function', Trim(eVarName));
      AddField('Type', 'Function', Trim(eVarType));

      eCurrLine := Between(eCurrLine, '(', ')');
      HideBracesAndStrings(eCurrLine, True);
      eStr.Text := StringReplace(eCurrLine, ',', #13, [rfReplaceAll]);
      for i := 0 to eStr.Count - 1 do begin
        if Pos(':', eStr[i]) <> 0 then begin
          eVarType := Trim(Copy(eStr[i], 1, Pos(':', eStr[i]) - 1));
          eVarName := Trim(Copy(eStr[i], Pos(':', eStr[i]) + 1, Length(eStr[i])));
        end
        else begin
          eVarType := '';
          eVarName := Trim(eStr[i]);
        end;

        eVarType := ShowBracesAndStrings(eVarType);
        eVarName := ShowBracesAndStrings(eVarName);
        AddField('Param ' + IntToStr(i + 1), 'Function', '').ReadOnly := True;
        AddField('Name', 'Param ' + IntToStr(i + 1), eVarName);
        AddField('Type', 'Param ' + IntToStr(i + 1), eVarType)
      end;
      {--> Action }
      eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
      eCurrLine := RemoveStringsAndComments(eCurrLine, False, True);
      eCurrLine := Trim(Copy(eCurrLine, GetMatchingBrace(eCurrLine) + 1, Length(eCurrLine)));
      if eCurrLine = '{' then
        AddField('Action', 'Function', 'New Code-Block').ReadOnly := True
      else
        AddFunc('Action', eCurrLine, 'Function');
      {--> Analyze comments }
      eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
      eCurrLine := RemoveStringsAndComments(eCurrLine, True, False);
      CheckCIComment(eCurrLine);
    end
    else
      CreateCategory('Invalid Function');
      
    eBraceTexts.Free;
    eStr.Free;
    frmMain.jviCode.EndUpdate;
    if eLine <> eCILine then
      UpdateCI_Pawn(eLine);
    exit;
  end
  else if (Pos('(', eCurrLine) <> Pos(')', eCurrLine)) and (Assignment(eBackupLine) = '') then begin // Function Call
    eVarName := Trim(Copy(eCurrLine, 1, Pos('(', eCurrLine) - 1));
    eVarValue := '';
    with frmMain.sciCallTips.ApiStrings do begin
      for i := 0 to Count - 1 do begin
        if IsAtStart(eVarName, Strings[i]) then begin
          eVarValue := Strings[i];
          break;
        end
        else
          Application.ProcessMessages;
      end;
    end;

    eStr2 := TStringList.Create;
    if eVarValue = '' then
      CreateCategory('Invalid Function Call')
    else begin
      eVarValue := Between(eVarValue, '(', ')', True);
      HideBracesAndStrings(eVarValue, True);
      eStr.Text := StringReplace(eVarValue, ',', #13, [rfReplaceAll]);

      if Between(eCurrLine, '(', ')', True) = '' then
        eCurrLine := Copy(eCurrLine, Pos('(', eCurrLine) + 1, Length(eCurrLine))
      else
        eCurrLine := Between(eCurrLine, '(', ')', True);
      HideBracesAndStrings(eCurrLine, True);
      eStr2.Text := StringReplace(eCurrLine, ',', #13, [rfReplaceAll]);
      if (Trim(eStr2.Text) = ')') then
        eStr2.Clear;

      CreateCategory('Function Call');
      AddField('Function', 'Function Call', eVarName);
      AddField('Params', 'Function Call', '').ReadOnly := True;

      if eStr.Count >= eStr2.Count then
        k := eStr.Count
      else
        k := eStr2.Count;

      for i := 0 to k - 1 do begin
        if (i < eStr2.Count) and (i < eStr.Count) then begin
          eStr[i] := ShowBracesAndStrings(eStr[i]);
          eStr2[i] := ShowBracesAndStrings(eStr2[i]);
          AddField(Trim(eStr[i]), 'Params', Trim(eStr2[i]));
        end
        else if i < eStr.Count then begin
          eStr[i] := ShowBracesAndStrings(eStr[i]);
          AddField(Trim(eStr[i]), 'Params', '');
        end
        else begin
          eStr2[i] := ShowBracesAndStrings(eStr2[i]);
          AddField('...', 'Params', Trim(eStr2[i]));
        end;
      end;
      {--> Language Strings }
      eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
      eCurrLine := RemoveStringsAndComments(eCurrLine, False, True);
      if Pos('%L', eCurrLine) <> 0 then begin
        Delete(eCurrLine, 1, Pos('%L', eCurrLine) + 1);
        Delete(eCurrLine, 1, Pos('"', eCurrLine) + 1);
        Delete(eCurrLine, 1, Pos(',', eCurrLine) + 1);
        eVarName := Between(eCurrLine, '"', '"');
        eVarValue := '';
        eStr.Clear;
        eStr2.Clear;
        for i := 0 to frmMain.sciEditor.Lines.Count - 1 do begin
          if IsAtStart('register_dictionary', frmMain.sciEditor.Lines[i]) then begin
            eVarValue := Between(frmMain.sciEditor.Lines[i], '"', '"');
            if (eVarValue <> '') and (FileExists(frmSettings.txtLangDir.Text + eVarValue)) then begin
              eStr2.LoadFromFile(frmSettings.txtLangDir.Text + eVarValue);
              eStr.AddStrings(eStr2);
            end;
          end;
        end;

        if eVarValue = '' then
          CreateCategory('No dictionary registered')
        else if not FileExists(frmSettings.txtLangDir.Text + eVarValue) then
          CreateCategory('Invalid dictionary')
        else begin
          with CreateCategory('Language Strings') do begin
            SortKind := iskNone;
            Expanded := False;

            eVarType := '';
            eVarCount := 0;
            for i := 0 to eStr.Count - 1 do begin
              if Pos('[', eStr[i]) = 1 then
                eVarType := Between(eStr[i], '[', ']')
              else if (eVarType <> '') and (Pos(eVarName, eStr[i]) = 1) then begin
                Inc(eVarCount, 1);
                AddField('String ' + IntToStr(eVarCount), 'Language Strings', '').ReadOnly := True;
                AddField('Language', 'String ' + IntToStr(eVarCount), eVarType).ReadOnly := True;
                AddField('Value', 'String ' + IntToStr(eVarCount), Trim(Copy(eStr[i], Pos('=', eStr[i]) + 1, Length(eStr[i])))).ReadOnly := True;
              end;
            end;

            if Count = 0 then
              DisplayName := 'No matching string found';
          end;
        end;
      end;
      {--> Analyze comments }
      eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
      eCurrLine := RemoveStringsAndComments(eCurrLine, True, False);
      CheckCIComment(eCurrLine);
    end;

    eBraceTexts.Free;
    eStr.Free;
    eStr2.Free;
    frmMain.jviCode.EndUpdate;
    if eLine <> eCILine then
      UpdateCI_Pawn(eLine);
    exit;
  end
  { Assignment }
  else if Assignment(eBackupLine) <> '' then begin
    {--> Get values }
    eVarName := Copy(eBackupLine, 1, Pos(Assignment(eBackupLine), eBackupLine) - 1);
    eVarType := Copy(eBackupLine, Pos(Assignment(eBackupLine), eBackupLine) + Length(Assignment(eBackupLine)), Length(eBackupLine));
    if (Trim(eVarName) = '') or (Trim(eVarType) = '') then
      CreateCategory('Invalid assignment')
    else begin
      CreateCategory('Assignment');
      AddField('a', 'Assignment', Trim(eVarName));
      AddFunc('b', eVarType, 'Assignment');
      AddCombo('Operator', 'Assignment', Assignment(eBackupLine), ['=', '+=', '-=', '*=', '/=', '%=', '>>=', '>>>=', '<<=', '&=', '|=']);
      {--> Analyze comments }
      eCurrLine := Trim(StringReplace(eBackupLine, #9, #32, [rfReplaceAll]));
      eCurrLine := RemoveStringsAndComments(eCurrLine, True, False);
      CheckCIComment(eCurrLine);
    end;
    eBraceTexts.Free;
    eStr.Free;
    frmMain.jviCode.EndUpdate;
    if eLine <> eCILine then
      UpdateCI_Pawn(eLine);
    exit;
  end;

  CreateCategory('No information available');
  eBraceTexts.Free;
  eStr.Free;
  frmMain.jviCode.EndUpdate;

  if eLine <> eCILine then
    UpdateCI_Pawn(eLine);
end;

{ TSTLWrapper }

constructor TSTLWrapper.Create(const ASTL: TSelectionTextList;
  const AValue: string);
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

destructor TStringWrapper.Destroy;
begin
  Value := '';
  inherited;
end;

procedure RebuildLine;
function GetFunc(eItem: TJvCustomInspectorItem): String;
var i: integer;
begin
  if eItem.Count = 0 then
    Result := eItem.DisplayValue
  else begin
    Result := eItem.Items[0].DisplayValue + '(';
    for i := 1 to eItem.Count -1 do begin
      if i = 1 then
        Result := Result + eItem.Items[i].DisplayValue
      else
        Result := Result + ', ' + eItem.Items[i].DisplayValue;
    end;
    Result := Result + ')';
  end;
end;

function CheckItem(eItem: TJvCustomInspectorItem; eVal: Integer): Boolean;
var i: integer;
begin
  Result := False;
  for i := eVal to eItem.Count -1 do begin
    if eItem.Items[i].DisplayValue <> '' then begin
      Result := True;
      exit;
    end;
  end;
end;

var i, k, l, m: integer;
  eLine: string;
  eVarName, eVarType, eVarValue: string;
begin
  eLine := GetIndents(eCILine);

  eBraceTexts := TStringList.Create;
  with frmMain.jviCode.Root do begin
    for i := 0 to Count - 1 do begin
      { Variables and consts }
      if (Items[i].DisplayName = 'Variables') or (Items[i].DisplayName = 'Constants') then begin

        if Trim(eLine) = '' then begin
          if eStock then
            eLine := eLine + 'stock '
          else
            eLine := eLine + 'new ';
        end;

        for k := 0 to Items[i].Count - 1 do begin
          eVarName := '';
          eVarType := '';
          eVarValue := '';

          for l := 0 to Items[i].Items[k].Count - 1 do begin
            if Items[i].Items[k].Items[l].DisplayName = 'Name' then
              eVarName := Items[i].Items[k].Items[l].DisplayValue
            else if Items[i].Items[k].Items[l].DisplayName = 'Type' then
              eVarType := Items[i].Items[k].Items[l].DisplayValue
            else if Items[i].Items[k].Items[l].DisplayName = 'Value' then begin
              if Items[i].Items[k].Items[l].ReadOnly = True then begin // Function
                for m := 0 to Items[i].Items[k].Items[l].Count - 1 do begin
                  if Items[i].Items[k].Items[l].Items[m].DisplayName = 'Function' then
                    eVarValue := Items[i].Items[k].Items[l].Items[m].DisplayValue + '('
                  else if m = Items[i].Items[k].Items[l].Count - 1 then
                    eVarValue := eVarValue + Items[i].Items[k].Items[l].Items[m].DisplayValue
                  else
                    eVarValue := eVarValue + Items[i].Items[k].Items[l].Items[m].DisplayValue + ', ';
                end;
                if Pos('(', eVarValue) <> 0 then
                  eVarValue := eVarValue + ')';
              end
              else // Other value
                eVarValue := Items[i].Items[k].Items[l].DisplayValue;
            end;
          end;

          if Trim(eLine) <> 'new' then
            eLine := eLine + ', ';

          if eVarType <> '' then
            eLine := eLine + eVarType + ':';
          eLine := eLine + eVarName;
          if eVarValue <> '' then
            eLine := eLine + ' = ' + eVarValue;
        end;
      end;
      { Comments }
      if (Items[i].DisplayName = 'Comment') then begin
        if Items[i].Items[0].DisplayName = 'Line Comment' then
          eLine := eLine + ' // ' + Items[i].Items[0].DisplayValue
        else begin
          eVarName := Copy(frmMain.sciEditor.Lines[eCILine], Pos('/*', frmMain.sciEditor.Lines[eCILine]) + 1, Length(frmMain.sciEditor.Lines[eCILine]));
          if Pos('*/', eVarName) = 0 then
            eLine := eLine + ' /* ' + Items[i].Items[0].DisplayValue
          else
            eLine := eLine + ' /* ' + Items[i].Items[0].DisplayValue + ' */';
        end;
      end;
      { Functions }
      if Items[i].DisplayName = 'Function' then begin
        if Items[i].Items[1].DisplayValue <> '' then
          eLine := eLine + Items[i].Items[1].DisplayValue + #32;
        eLine := eLine + Items[i].Items[0].DisplayValue + '(';
        for k := 2 to Items[i].Count - 2 do begin
          if Items[i].Items[k].Items[1].DisplayValue <> '' then
            eLine := eLine + Items[i].Items[k].Items[1].DisplayValue + ':';
          if k = 2 then
            eLine := eLine + Items[i].Items[k].Items[0].DisplayValue
          else
            eLine := eLine + ', ' + Items[i].Items[k].Items[0].DisplayValue;
        end;
        eLine := eLine + ')';
        if Items[i].Items[Items[i].Count - 1].ReadOnly then
          eLine := eLine + ' {'
        else
          eLine := eLine + GetFunc(Items[i].Items[Items[i].Count - 1]);
      end;
      { Function Call }
      if Items[i].DisplayName = 'Function Call' then begin
        eLine := eLine + Items[i].Items[0].DisplayValue + '(';
        for k := 0 to Items[i].Items[1].Count - 1 do begin
          if k = 0 then
            eLine := eLine + Items[i].Items[1].Items[k].DisplayValue
          else if CheckItem(Items[i].Items[1], k) then
            eLine := eLine + ', ' + Items[i].Items[1].Items[k].DisplayValue;
        end;
        eLine := eLine + ')';
      end;
      { Condition }
      if Items[i].DisplayName = 'If-Condition' then begin
        eLine := eLine + 'if (';
        for k := 0 to Items[i].Count - 2 do begin
          if k = 0 then
            eLine := eLine + Items[i].Items[k].Items[0].DisplayValue
          else
            eLine := eLine + #32 + Items[i].Items[k].Items[0].DisplayValue + #32 + Items[i].Items[k].Items[1].DisplayValue;
        end;

        if Items[i].Items[Items[i].Count - 1].ReadOnly then
          eLine := eLine + ') {'
        else
          eLine := eLine + ') ' + GetFunc(Items[i].Items[Items[i].Count - 1]);
      end;
      { Defined }
      if Items[i].DisplayName = 'Definition' then
        eLine := eLine + '#define ' + Items[i].Items[0].DisplayValue + #32 + Items[i].Items[1].DisplayValue;
      { Included Header }
      if Items[i].DisplayName = 'Included Header' then begin
        if Pos('<', frmMain.sciEditor.Lines[eCILine]) <> Pos('>', frmMain.sciEditor.Lines[eCILine]) then
          eLine := eLine + '#include <' + Items[i].Items[0].DisplayValue + '>'
        else
          eLine := eLine + '#include "' + Items[i].Items[0].DisplayValue + '"';
      end;
      { Return }
      if Items[i].DisplayName = 'Return' then
        eLine := eLine + 'return ' + Items[i].Items[0].DisplayValue;
      { For-Loop }
      if Items[i].DisplayName = 'For-Loop' then begin
        eLine := eLine + 'for(' + Items[i].Items[0].DisplayValue + '; ' + Items[i].Items[1].DisplayValue + '; ' + Items[i].Items[2].DisplayValue;
        if Items[i].Items[3].ReadOnly then
          eLine := eLine + ') {'
        else
          eLine := eLine + ') ' + GetFunc(Items[i].Items[3]);
      end;
      { While-Loop }
      if Items[i].DisplayName = 'While-Loop' then begin
        eLine := eLine + 'while (';
        for k := 0 to Items[i].Count - 2 do begin
          if k = 0 then
            eLine := eLine + Items[i].Items[k].Items[0].DisplayValue
          else
            eLine := eLine + #32 + Items[i].Items[k].Items[0].DisplayValue + #32 + Items[i].Items[k].Items[1].DisplayValue;
        end;
        
        if Items[i].Items[Items[i].Count - 1].ReadOnly then
          eLine := eLine + ') {'
        else
          eLine := eLine + ') ' + Items[i].Items[Items[i].Count - 1].DisplayValue;
      end;
      { Assignment }
      if Items[i].DisplayName = 'Assignment' then begin
        eLine := eLine + Items[i].Items[0].DisplayValue;
        eLine := eLine + #32 + Items[i].Items[2].DisplayValue + #32;
        eLine := eLine + GetFunc(Items[i].Items[1]);
      end;
    end;
  end;
  eBraceTexts.Free;

  if Trim(eLine) <> '' then
    frmMain.sciEditor.Lines[eCILine] := eLine;
  frmMain.mnuModified.Caption := lModified;
  ActiveDoc.Modified := True;
end;

initialization

  FItems := TObjectList.Create;
  eCILine := -1;

finalization

  FItems.Free;

end.

