unit UnitCodeExplorerUpdater;

interface

uses
  Classes, Forms, SysUtils, ComCtrls, Windows, ScintillaLanguageManager,
  Dialogs, CommCtrl;

type
  TCodeExplorerUpdater = class(TThread)
  private
    eConstants: TStringList;
    eDefined: TStringList;
    eCVars: TStringList;
    eIncluded: TStringList;
    eMethodsDefault, eMethodsEvents, eStocks: TStringList;
    eNatives: TStringList;
    eForwards: TStringList;
    eVariables: TStringList;

    eCode: TStringList;

    eAutoComplete, eCallTips, eKeywords: string;
    eLastActive: Integer;
    eActive: Integer;
  protected
    procedure Execute; override;
    procedure GetCode;
    procedure SetValuesPawn;
  end;

function GetNode(eText: string): TTreeNode;

implementation

uses UnitfrmMain, UnitLanguages, UnitMainTools, UnitCodeUtils,
  UnitTextAnalyze, UnitfrmSettings, UnitPlugins;

{ TCodeExplorerUpdater }

procedure TCodeExplorerUpdater.Execute;
function CheckAU: Boolean;
begin
  Result := True;
  if (eLastActive = eActive) and (frmSettings.chkAUDisable.Checked) then begin
    if frmMain.sciEditor.Lines.Count > StrToIntDef(frmSettings.txtAUDisable.Text, 1500) then
      Result := False;
  end;
  eLastActive := eActive;
end;

var eStr: TStringList;
begin
  eCode := TStringList.Create;
  eConstants := TStringList.Create;
  eDefined := TStringList.Create;
  eCVars := TStringList.Create;
  eIncluded := TStringList.Create;
  eMethodsDefault := TStringList.Create;
  eMethodsEvents := TStringList.Create;
  eStocks := TStringList.Create;
  eNatives := TStringList.Create;
  eForwards := TStringList.Create;
  eVariables := TStringList.Create;
  eStr := TStringList.Create;

  repeat
    Synchronize(GetCode);
    eAutoComplete := '';
    eCallTips := '';
    eKeywords := '';

    if (not Application.Terminated) and (Started) and (not frmMain.pnlLoading.Visible) and (frmMain.trvExplorer.Visible) and (Assigned(ActiveDoc)) then begin
      if (Plugin_UpdateCodeExplorer(GetCurrLang.Name, ActiveDoc.FileName, frmMain.tbDocs.SelectedTab.Caption, True)) and (frmMain.stlIDEs.ItemIndex = 0) then begin
        try
          if CheckAU then begin
            if Plugin_UpdateCodeExplorer(GetCurrLang.Name, ActiveDoc.FileName, frmMain.stlIDEs.Strings[frmMain.stlIDEs.ItemIndex], True) then begin
              // analyze code
              with ParseCodePawn(eCode, ExtractFileName(ActiveDoc.FileName)) do begin
                eConstants.Assign(Constants);
                eDefined.Assign(Defined);
                eCVars.Assign(CVars);
                eIncluded.Assign(Included);
                eMethodsDefault.Assign(MethodsDefault);
                eMethodsEvents.Assign(Events);
                eStocks.Assign(Stocks);
                eNatives.Assign(Natives);
                eForwards.Assign(Forwards);
                eVariables.Assign(Variables);

                eAutoComplete := eAutoComplete + #13 + AutoComplete.Text;
                eCallTips := eCallTips + #13 + CallTips.Text;
                eKeywords := eKeywords + #13 + HighlightKeywords.Text;

                DestroyResult;
              end;
              // apply changes
              Synchronize(SetValuesPawn);
            end;
          end;
        except
          if FindWindow(nil, 'Delphi 7') <> 0 then // This is "Debug Mode"
            //madExcept.HandleException;
        end;
      end;
      Sleep(1000);
    end
    else
      Sleep(50);
  until not Started;

  eCode.Free;
  eConstants.Free;
  eDefined.Free;
  eCVars.Free;
  eIncluded.Free;
  eMethodsDefault.Free;
  eMethodsEvents.Free;
  eStocks.Free;
  eNatives.Free;
  eForwards.Free;
  eVariables.Free;
  eStr.Free;
end;

procedure TCodeExplorerUpdater.GetCode;
begin
  if Assigned(ActiveDoc) then begin
    eCode.Assign(frmMain.sciEditor.Lines);
    eActive := ActiveDoc.Index;
  end;
end;

function GetNode(eText: string): TTreeNode;
var i: integer;
begin
  Result := nil;

  for i := 0 to frmMain.trvExplorer.Items.Count - 1 do begin
    if (frmMain.trvExplorer.Items[i].Text = eText) then begin
      if (frmMain.trvExplorer.Items[i].ImageIndex = 42) or (frmMain.trvExplorer.Items[i].ImageIndex = 43) then begin
        Result := frmMain.trvExplorer.Items[i];
        exit;
      end;
    end;
  end;
end;

procedure TCodeExplorerUpdater.SetValuesPawn;
var exConstants, exDefined, exIncluded, exMethods, exDefault, exEvents,
  exStocks, exNatives, exForwards, exVariables, exCVars: Boolean;
  i, eSelStart, eSelLength: integer;
  LineMaxSubord: integer;
  eStr: TStringList;
  eScrollPosX, eScrollPosY: Integer;
  eTempNode: TTreeNode;
begin
  if Application.Terminated then exit;
  if (frmMain.trvExplorer.Items.Count = 0) or (eActive <> ActiveDoc.Index) then exit;

  frmMain.trvExplorer.Items.BeginUpdate;
  try
    eScrollPosX := GetScrollPos(frmMain.trvExplorer.Handle, SB_HORZ);
    eScrollPosY := GetScrollPos(frmMain.trvExplorer.Handle, SB_VERT);

  // Get Expanded-State and delete children
    with GetNode('Constants') do begin
      exConstants := (Expanded) and (Count <> 0);
      DeleteChildren;
    end;
    with GetNode('CVars') do begin
      exCVars := (Expanded) and (Count <> 0);
      DeleteChildren;
    end;
    with GetNode('Defined') do begin
      exDefined := (Expanded) and (Count <> 0);
      DeleteChildren;
    end;
    with GetNode('Included') do begin
      exIncluded := (Expanded) and (Count <> 0);
      DeleteChildren;
    end;
    with GetNode('Default') do begin
      exDefault := (Expanded) and (Count <> 0);
      DeleteChildren;
    end;
    with GetNode('Events') do begin
      exEvents := (Expanded) and (Count <> 0);
      DeleteChildren;
    end;
    with GetNode('Stocks') do begin
      exStocks := (Expanded) and (Count <> 0);
      DeleteChildren;
    end;
    with GetNode('Methods') do begin
      exMethods := (Expanded) and (Count <> 0);
      DeleteChildren;
    end;
    with GetNode('Natives') do begin
      exNatives := (Expanded) and (Count <> 0);
      DeleteChildren;
    end;
    with GetNode('Forwards') do begin
      exForwards := (Expanded) and (Count <> 0);
      DeleteChildren;
    end;
    with GetNode('Variables') do begin
      exVariables := (Expanded) and (Count <> 0);
      DeleteChildren;
    end;
  // Create new children
    with frmMain.trvExplorer.Items.AddChild(GetNode('Defined'), 'CVars') do begin
      ImageIndex := 42;
      SelectedIndex := 42;
    end;
    with frmMain.trvExplorer.Items.AddChild(GetNode('Methods'), 'Default') do begin
      ImageIndex := 42;
      SelectedIndex := 42;
    end;
    with frmMain.trvExplorer.Items.AddChild(GetNode('Methods'), 'Events') do begin
      ImageIndex := 42;
      SelectedIndex := 42;
    end;
    with frmMain.trvExplorer.Items.AddChild(GetNode('Methods'), 'Stocks') do begin
      ImageIndex := 42;
      SelectedIndex := 42;
    end;

    eTempNode := GetNode('Constants');
    for i := 0 to eConstants.Count - 1 do begin
      with frmMain.trvExplorer.Items.AddChildObject(eTempNode, eConstants[i], Pointer(eConstants.Objects[i])) do begin
        ImageIndex := 48;
        SelectedIndex := 48;
      end;
    end;
    eTempNode := GetNode('Defined');
    for i := 0 to eDefined.Count - 1 do begin
      with frmMain.trvExplorer.Items.AddChildObject(eTempNode, eDefined[i], Pointer(eDefined.Objects[i])) do begin
        ImageIndex := 48;
        SelectedIndex := 48;
      end;
    end;
    eTempNode := GetNode('CVars');
    for i := 0 to eCVars.Count - 1 do begin
      with frmMain.trvExplorer.Items.AddChildObject(eTempNode, eCVars[i], Pointer(eCVars.Objects[i])) do begin
        ImageIndex := 35;
        SelectedIndex := 35;
      end;
    end;
  // Sort items
    eIncluded.Sort;
    eMethodsDefault.Sort;
    eMethodsEvents.Sort;
    eStocks.Sort;
    eNatives.Sort;
    eForwards.Sort;
    eVariables.Sort;
  // Add items
    eTempNode := GetNode('Included');
    for i := 0 to eIncluded.Count - 1 do begin
      with frmMain.trvExplorer.Items.AddChildObject(eTempNode, eIncluded[i], Pointer(eIncluded.Objects[i])) do begin
        ImageIndex := 34;
        SelectedIndex := 34;
      end;
    end;
    eTempNode := GetNode('Default');
    for i := 0 to eMethodsDefault.Count - 1 do begin
      with frmMain.trvExplorer.Items.AddChildObject(eTempNode, eMethodsDefault[i], Pointer(eMethodsDefault.Objects[i])) do begin
        ImageIndex := 12;
        SelectedIndex := 12;
      end;
    end;
    eTempNode := GetNode('Events');
    for i := 0 to eMethodsEvents.Count - 1 do begin
      with frmMain.trvExplorer.Items.AddChildObject(eTempNode, eMethodsEvents[i], Pointer(eMethodsEvents.Objects[i])) do begin
        ImageIndex := 47;
        SelectedIndex := 47;
      end;
    end;
    eTempNode := GetNode('Stocks');
    for i := 0 to eStocks.Count - 1 do begin
      with frmMain.trvExplorer.Items.AddChildObject(eTempNode, eStocks[i], Pointer(eStocks.Objects[i])) do begin
        ImageIndex := 12;
        SelectedIndex := 12;
      end;
    end;
    eTempNode := GetNode('Natives');
    for i := 0 to eNatives.Count - 1 do begin
      with frmMain.trvExplorer.Items.AddChildObject(eTempNode, eNatives[i], Pointer(eNatives.Objects[i])) do begin
        ImageIndex := 47;
        SelectedIndex := 47;
      end;
    end;
    eTempNode := GetNode('Forwards');
    for i := 0 to eForwards.Count - 1 do begin
      with frmMain.trvExplorer.Items.AddChildObject(eTempNode, eForwards[i], Pointer(eForwards.Objects[i])) do begin
        ImageIndex := 47;
        SelectedIndex := 47;
      end;
    end;
    eTempNode := GetNode('Variables');
    for i := 0 to eVariables.Count - 1 do begin
      with frmMain.trvExplorer.Items.AddChildObject(eTempNode, eVariables[i], Pointer(eVariables.Objects[i])) do begin
        ImageIndex := 35;
        SelectedIndex := 35;
      end;
    end;

    GetNode('Constants').Expanded := exConstants;
    GetNode('Defined').Expanded := exDefined;
    GetNode('CVars').Expanded := exCVars;
    GetNode('Included').Expanded := exIncluded;
    GetNode('Methods').Expanded := exMethods;
    GetNode('Default').Expanded := exDefault;
    GetNode('Events').Expanded := exEvents;
    GetNode('Stocks').Expanded := exStocks;
    GetNode('Natives').Expanded := exNatives;
    GetNode('Forwards').Expanded := exForwards;
    GetNode('Variables').Expanded := exVariables;

    SetScrollPos(frmMain.trvExplorer.Handle, SB_HORZ, eScrollPosX, False);
    SetScrollPos(frmMain.trvExplorer.Handle, SB_VERT, eScrollPosY, False);
  except
    // well, yes.
  end;
  frmMain.trvExplorer.Items.EndUpdate;

  if (not frmMain.sciEditor.Lines.Count > StrToIntDef(frmSettings.txtAUDisable.Text, 1500)) or ((not frmMain.pnlLoading.Visible) and (not frmMain.sciEditor.AutoCActive) and (not frmMain.sciEditor.CallTipActive)) then begin
    frmMain.sciAutoComplete.AStrings.Text := eAutoComplete;
    for i := frmMain.sciAutoComplete.AStrings.Count - 1 downto 0 do begin
      if Length(Trim(frmMain.sciAutoComplete.AStrings[i])) <= 1 then
        frmMain.sciAutoComplete.AStrings.Delete(i);
    end;
    frmMain.sciCallTips.ApiStrings.Text := eCallTips;
    for i := frmMain.sciCallTips.ApiStrings.Count - 1 downto 0 do begin
      if Length(Trim(frmMain.sciCallTips.ApiStrings[i])) <= 1 then
        frmMain.sciCallTips.ApiStrings.Delete(i);
    end;

    with TSciKeywords(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Find('Pawn').Keywords.Items[1])) do begin
      eStr := TStringList.Create;
      for i := 0 to frmMain.sciEditor.Lines.Count - 1 do begin
        if not frmMain.sciEditor.GetFoldExpanded(i) then
          eStr.Add(IntToStr(i));
      end;

      Keywords.Text := eKeywords;
      frmMain.sciEditor.LanguageManager.Update;

      for i := 0 to frmMain.sciEditor.Lines.Count - 1 do begin
        if eStr.IndexOf(IntToStr(i)) <> -1 then begin
          LineMaxSubord := frmMain.sciEditor.GetLastChild(i, -1);
          frmMain.sciEditor.SetFoldExpanded(i, False);
          if LineMaxSubord > i then
            frmMain.sciEditor.HideLines(i + 1, LineMaxSubord);
        end;
      end;

      eStr.Free;
    end;
  end;

  Plugin_UpdateCodeExplorer(GetCurrLang.Name, ActiveDoc.FileName, frmMain.stlIDEs.Strings[frmMain.stlIDEs.ItemIndex], False);
end;

end.

