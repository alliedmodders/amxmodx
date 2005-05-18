unit UnitTextAnalyze; // Unit for analyzing code for code explorer, auto
                      // complete and call tips

interface

uses SysUtils, Classes, ComCtrls, Windows, Forms, Messages, ScintillaLanguageManager;

procedure UpdateList(Code: String);
procedure AddAutoComplete(eAdd: String; AddCallTip: Boolean);

var eExpand1, eExpand2, eExpand3, eExpand4, eExpand5: Boolean;

implementation

uses UnitfrmMain, UnitFunc;

procedure UpdateList(Code: String);
var i, k: integer;
    eStr, eStr2: TStringList;
    eEndVariables: Boolean;
    eParent1, eParent2, eParent3, eParent4, eParent5: TTreeNode;
    eBracketsOpen, eStartLine: Integer;
    eTemp: String;
begin
  eStr := TStringList.Create;
  eStr2 := TStringList.Create;
  eStr.Text := Code;

  frmMain.sacComplete.AStrings.Text := frmMain.StdAutoComplete;
  frmMain.cltEditor.ApiStrings.Text := frmMain.StdCallTips;
  frmMain.trvFunctions.Items.BeginUpdate;
  for i := 0 to 4 do
    frmMain.trvFunctions.Items[i].DeleteChildren;

  for i := eStr.Count -1 downto 0 do begin
    eStr[i] := TrimLeft(eStr[i]);
    if (Pos('//', eStr[i]) > 0) then
      eStr[i] := Copy(eStr[i], 1, Pos('//', eStr[i]) -2);
    if (Pos('/*', eStr[i]) > 0) then
      eStr[i] := Copy(eStr[i], 1, Pos('/*', eStr[i]) -2);
    if (Pos('*', eStr[i]) = 1) then
      eStr.Delete(i);
    if (Pos('*/', eStr[i]) > 0) then
      eStr[i] := Copy(eStr[i], 1, Pos('*/', eStr[i]) -2);
  end;    

  eEndVariables := False;
  eBracketsOpen := -1;
  eStartLine := -1;
  eParent1 := frmMain.trvFunctions.Items[3];
  eParent2 := frmMain.trvFunctions.Items[1];
  eParent3 := frmMain.trvFunctions.Items[4];
  eParent4 := frmMain.trvFunctions.Items[0];
  eParent5 := frmMain.trvFunctions.Items[2];
  for i := 0 to eStr.Count -1 do begin
    // Included:
    if Pos('#include', LowerCase(eStr[i])) = 1 then begin
      if (ExtractFileExt(Between(eStr[i], '<', '>')) = '') and (ExtractFileExt(Between(eStr[i], '"', '"')) = '') then begin
        if Pos('<', eStr[i]) > 0 then
          frmMain.trvFunctions.Items.AddChild(eParent1, Between(eStr[i], '<', '>') + '.inc')
        else
          frmMain.trvFunctions.Items.AddChild(eParent1, Between(eStr[i], '"', '"') + '.inc');
      end
      else begin
        if Pos('<', eStr[i]) > 0 then
          frmMain.trvFunctions.Items.AddChild(eParent1, Between(eStr[i], '<', '>'))
        else
          frmMain.trvFunctions.Items.AddChild(eParent1, Between(eStr[i], '"', '"'));
      end;
    end;
    // Defined
    if Pos('#define', LowerCase(eStr[i])) = 1 then begin
      eStr[i] := Copy(eStr[i], 8, Length(eStr[i]));
      if Pos(#32, eStr[i]) <> 0 then begin
        frmMain.trvFunctions.Items.AddChild(eParent2, Copy(TrimLeft(eStr[i]), 1, Pos(#32, TrimLeft(eStr[i])) -1));
        AddAutoComplete(Copy(TrimLeft(eStr[i]), 1, Pos(#32, TrimLeft(eStr[i])) -1), False);
      end
      else begin
        frmMain.trvFunctions.Items.AddChild(eParent2, TrimLeft(eStr[i]));
        AddAutoComplete(TrimLeft(eStr[i]), False);
      end;
    end;
    // Variables and constants
    if not eEndVariables then begin
      if Pos('new', LowerCase(eStr[i])) = 1 then begin
        if Pos(',', eStr[i]) > 0 then begin
          eStr2.Text := StringReplace(Trim(Copy(eStr[i], 4, Length(eStr[i]))), ',', #13, [rfReplaceAll]);
          for k := 0 to eStr2.Count -1 do begin
            if Pos('=', eStr[i]) = 0 then
              frmMain.trvFunctions.Items.AddChild(eParent3, Trim(eStr2[k]))
            else
              frmMain.trvFunctions.Items.AddChild(eParent4, Trim(eStr2[k]));
            AddAutoComplete(Trim(eStr2[k]), False); 
          end;
        end
        else if Pos('{', Trim(eStr[i])) = Length(Trim(eStr[i])) then begin
          if Pos('=', eStr[i]) = 0 then
            frmMain.trvFunctions.Items.AddChild(eParent3, TrimLeft(Copy(eStr[i], 4, Length(eStr[i]))) + ' Array }')
          else
            frmMain.trvFunctions.Items.AddChild(eParent4, TrimLeft(Copy(eStr[i], 4, Length(eStr[i]))) + ' Array }');
          AddAutoComplete(Trim(Copy(eStr[i], 1, Pos('{', eStr[i]) -1)), False);
        end
        else begin
          if Pos('=', eStr[i]) = 0 then
            frmMain.trvFunctions.Items.AddChild(eParent3, TrimLeft(Copy(eStr[i], 4, Length(eStr[i]))))
          else
            frmMain.trvFunctions.Items.AddChild(eParent4, TrimLeft(Copy(eStr[i], 4, Length(eStr[i]))));
          AddAutoComplete(TrimLeft(Copy(eStr[i], 4, Length(eStr[i]))), False);
        end;
      end;
    end;
    // Functions
    if (Pos('forward', LowerCase(eStr[i])) = 1) or (Pos('public', LowerCase(eStr[i])) = 1) or (Pos('native', LowerCase(eStr[i])) = 1) or (Pos('stock', LowerCase(eStr[i])) = 1) then begin
      eBracketsOpen := 0;
      eEndVariables := True;
      if ((Pos('{', eStr[i]) = 0)) or (Pos('{', eStr[i]) <> 0) and (Pos('}', eStr[i]) <> 0) then begin
        eTemp := Copy(eStr[i], 1, Pos(#32, eStr[i]));
        frmMain.FunctionType.Add(eTemp);
        frmMain.trvFunctions.Items.AddChild(eParent5, Copy(eStr[i], Pos(#32, eStr[i]) +1, Length(eStr[i])));
        AddAutoComplete(Copy(eStr[i], Pos(#32, eStr[i]) +1, Length(eStr[i])), True);
      end;
    end;
    if Pos('{', eStr[i]) <> 0 then begin
      if eStartLine = -1 then
        eStartLine := i;
      Inc(eBracketsOpen, 1);
    end;
    if Pos('}', eStr[i]) <> 0 then begin
      Inc(eBracketsOpen, -1);
      if (eBracketsOpen = 0) then begin
        eStr[eStartLine] := StringReplace(eStr[eStartLine], '{', '', [rfReplaceAll]);
        eStr[eStartLine] := TrimRight(eStr[eStartLine]);
        if (Pos(#32, eStr[eStartLine]) <> 0) and (Pos(#32, eStr[eStartLine]) < Pos('(', eStr[eStartLine])) then
          eTemp := Copy(eStr[eStartLine], 1, Pos(#32, eStr[eStartLine]))
        else
          eTemp := '';
        frmMain.FunctionType.Add(eTemp);
        frmMain.trvFunctions.Items.AddChild(eParent5, Copy(eStr[eStartLine], Pos(#32, eStr[eStartLine]) +1, Length(eStr[eStartLine])));
        AddAutoComplete(Copy(eStr[eStartLine], Pos(#32, eStr[eStartLine]) +1, Length(eStr[eStartLine])), True);
        eStartLine := -1;
        eBracketsOpen := -2;
      end;
    end;
  end;

  for i := frmMain.trvFunctions.Items.Count -1 downto 0 do begin
    if Assigned(frmMain.trvFunctions.Items[i].Parent) then begin
      frmMain.trvFunctions.Items[i].ImageIndex := 20;
      frmMain.trvFunctions.Items[i].SelectedIndex := 20;
    end
    else begin
      frmMain.trvFunctions.Items[i].ImageIndex := 18;
      frmMain.trvFunctions.Items[i].SelectedIndex := 18;
    end;
    
    if frmMain.trvFunctions.Items[i].Text = '' then // Remove empty items
      frmMain.trvFunctions.Items.Delete(frmMain.trvFunctions.Items[i]); 
  end;
  eStr.Free;
  eStr2.Free;
  frmMain.trvFunctions.Items.AlphaSort(True);
  frmMain.trvFunctions.FullExpand;
  if not eExpand1 then
    eParent1.Collapse(False);
  if not eExpand2 then
    eParent2.Collapse(False);
  if not eExpand3 then
    eParent3.Collapse(False);
  if not eExpand4 then
    eParent4.Collapse(False);
  if not eExpand5 then
    eParent5.Collapse(False);
  frmMain.trvFunctions.Perform(WM_VSCROLL, SB_TOP, 0);
  frmMain.trvFunctions.Items.EndUpdate;
  Application.ProcessMessages;
end;

procedure AddAutoComplete(eAdd: String; AddCallTip: Boolean);
function AlreadyAdded: Boolean;
var i: integer;
    eTemp: String;
begin
  Result := False;
  eTemp := Trim(eAdd);
  for i := 0 to frmMain.sacComplete.AStrings.Count -1 do begin
    if Trim(frmMain.sacComplete.AStrings[i]) = eAdd then
      Result := True;
    if Trim(frmMain.sacComplete.AStrings[i] + '()') = eAdd then
      Result := True;
  end;
end;

begin
  if not AlreadyAdded then begin
    if Pos('(', eAdd) <> 0 then
      frmMain.sacComplete.AStrings.Add(Copy(eAdd, 1, Pos('(', eAdd) -1))
    else
      frmMain.sacComplete.AStrings.Add(eAdd);

    if AddCallTip then
      frmMain.cltEditor.ApiStrings.Add(eAdd);
  end;
end;

end.
