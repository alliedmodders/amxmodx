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
    
    eAutoComplete, eCallTips, eKeywords: String;
  protected
    procedure Execute; override;
    procedure GetCode;
    procedure SetValuesPAWN;
  end;

implementation

uses UnitfrmMain, UnitLanguages, UnitMainTools, UnitCodeUtils,
  UnitTextAnalyze, UnitfrmSettings, UnitPlugins;

{ TCodeExplorerUpdater }

procedure TCodeExplorerUpdater.Execute;
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

    if (not Application.Terminated) and (Started) and (not frmMain.pnlLoading.Visible) and (frmMain.trvExplorer.Visible) then begin
      if Plugin_UpdateCodeExplorer(GetCurrLang.Name, ActiveDoc.FileName, frmMain.tsMain.Items[frmMain.tsMain.ActiveTabIndex].Caption, True) then begin
        try
          if (frmMain.tsMain.ActiveTabIndex = 0) then begin
            // analyze code
            with ParseCodePAWN(eCode, ExtractFileName(ActiveDoc.FileName)) do begin
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
            Synchronize(SetValuesPAWN);
          end;
        except
          // GABEM
        end;
      end;
      Sleep(1000);
    end
    else
      Sleep(50);
  until (Application.Terminated);

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
  eCode.Assign(frmMain.sciEditor.Lines);
end;

procedure TCodeExplorerUpdater.SetValuesPAWN;
function GetNode(eText: String): TTreeNode;
var i: integer;
begin
  Result := nil;

  for i := 0 to frmMain.trvExplorer.Items.Count -1 do begin
    if (frmMain.trvExplorer.Items[i].Text = eText) then begin
      if (frmMain.trvExplorer.Items[i].ImageIndex = 42) or (frmMain.trvExplorer.Items[i].ImageIndex = 43) then begin
        Result := frmMain.trvExplorer.Items[i];
        exit;
      end;
    end;
  end;
end;

var exConstants, exDefined, exIncluded, exMethods, exDefault, exEvents,
    exStocks, exNatives, exForwards, exVariables, exCVars: Boolean;
    i, eSelStart, eSelLength: integer;
    LineMaxSubord: integer;
    eStr: TStringList;
    eScrollPosX, eScrollPosY: Integer;
begin
  if frmMain.trvExplorer.Items.Count = 0 then exit;

  frmMain.trvExplorer.Items.BeginUpdate;
  eScrollPosX := GetScrollPos(frmMain.trvExplorer.Handle, SB_HORZ);
  eScrollPosY := GetScrollPos(frmMain.trvExplorer.Handle, SB_VERT);

  // Get Expanded-State and delete children
  with GetNode('Constants') do begin
    exConstants := Expanded;
    DeleteChildren;
  end;
  with GetNode('CVars') do begin
    exCVars := Expanded;
    DeleteChildren;
  end;
  with GetNode('Defined') do begin
    exDefined := Expanded;
    DeleteChildren;
  end;
  with GetNode('Included') do begin
    exIncluded := Expanded;
    DeleteChildren;
  end;
  with GetNode('Default') do begin
    exDefault := Expanded;
    DeleteChildren;
  end;
  with GetNode('Events') do begin
    exEvents := Expanded;
    DeleteChildren;
  end;
  with GetNode('Stocks') do begin
    exStocks := Expanded;
    DeleteChildren;
  end;
  with GetNode('Methods') do begin
    exMethods := Expanded;
    DeleteChildren;
  end;
  with GetNode('Natives') do begin
    exNatives := Expanded;
    DeleteChildren;
  end;
  with GetNode('Forwards') do begin
    exForwards := Expanded;
    DeleteChildren;
  end;
  with GetNode('Variables') do begin
    exVariables := Expanded;
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

  for i := 0 to eConstants.Count -1 do begin
    with frmMain.trvExplorer.Items.AddChildObject(GetNode('Constants'), eConstants[i], Pointer(eConstants.Objects[i])) do begin
      ImageIndex := 48;
      SelectedIndex := 48;
    end;
  end;
  for i := 0 to eDefined.Count -1 do begin
    with frmMain.trvExplorer.Items.AddChildObject(GetNode('Defined'), eDefined[i], Pointer(eDefined.Objects[i])) do begin
      ImageIndex := 48;
      SelectedIndex := 48;
    end;
  end;
  for i := 0 to eCVars.Count -1 do begin
    with frmMain.trvExplorer.Items.AddChildObject(GetNode('CVars'), eCVars[i], Pointer(eCVars.Objects[i])) do begin
      ImageIndex := 35;
      SelectedIndex := 35;
    end;
  end;
  for i := 0 to eIncluded.Count -1 do begin
    with frmMain.trvExplorer.Items.AddChildObject(GetNode('Included'), eIncluded[i], Pointer(eIncluded.Objects[i])) do begin
      ImageIndex := 34;
      SelectedIndex := 34;
    end;
  end;
  for i := 0 to eMethodsDefault.Count -1 do begin
    with frmMain.trvExplorer.Items.AddChildObject(GetNode('Default'), eMethodsDefault[i], Pointer(eMethodsDefault.Objects[i])) do begin
      ImageIndex := 12;
      SelectedIndex := 12;
    end;
  end;
  for i := 0 to eMethodsEvents.Count -1 do begin
    with frmMain.trvExplorer.Items.AddChildObject(GetNode('Events'), eMethodsEvents[i], Pointer(eMethodsEvents.Objects[i])) do begin
      ImageIndex := 47;
      SelectedIndex := 47;
    end;
  end;
  for i := 0 to eStocks.Count -1 do begin
    with frmMain.trvExplorer.Items.AddChildObject(GetNode('Stocks'), eStocks[i], Pointer(eStocks.Objects[i])) do begin
      ImageIndex := 12;
      SelectedIndex := 12;
    end;
  end;
  for i := 0 to eNatives.Count -1 do begin
    with frmMain.trvExplorer.Items.AddChildObject(GetNode('Natives'), eNatives[i], Pointer(eNatives.Objects[i])) do begin
      ImageIndex := 47;
      SelectedIndex := 47;
    end;
  end;
  for i := 0 to eForwards.Count -1 do begin
    with frmMain.trvExplorer.Items.AddChildObject(GetNode('Forwards'), eForwards[i], Pointer(eForwards.Objects[i])) do begin
      ImageIndex := 47;
      SelectedIndex := 47;
    end;
  end;
  for i := 0 to eVariables.Count -1 do begin
    with frmMain.trvExplorer.Items.AddChildObject(GetNode('Variables'), eVariables[i], Pointer(eVariables.Objects[i])) do begin
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
  frmMain.trvExplorer.Items.EndUpdate;

  if (not frmMain.pnlLoading.Visible) and (not frmMain.sciEditor.AutoCActive) and (not frmMain.sciEditor.CallTipActive) then begin
    frmMain.sciAutoComplete.AStrings.Text := eAutoComplete;
    for i := frmMain.sciAutoComplete.AStrings.Count -1 downto 0 do begin
      if Length(Trim(frmMain.sciAutoComplete.AStrings[i])) <= 1 then
        frmMain.sciAutoComplete.AStrings.Delete(i);
    end;
    frmMain.sciCallTips.ApiStrings.Text := eCallTips;
    for i := frmMain.sciCallTips.ApiStrings.Count -1 downto 0 do begin
      if Length(Trim(frmMain.sciCallTips.ApiStrings[i])) <= 1 then
        frmMain.sciCallTips.ApiStrings.Delete(i);
    end;

    with TSciKeywords(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Find('Pawn').Keywords.Items[1])) do begin
      eStr := TStringList.Create;
      for i := 0 to frmMain.sciEditor.Lines.Count -1 do begin
        if not frmMain.sciEditor.GetFoldExpanded(i) then
          eStr.Add(IntToStr(i));
      end;
      
      Keywords.Text := eKeywords;
      frmMain.sciEditor.LanguageManager.Update;

      for i := 0 to frmMain.sciEditor.Lines.Count -1 do begin
        if eStr.IndexOf(IntToStr(i)) <> -1 then begin
          LineMaxSubord := frmMain.sciEditor.GetLastChild(i, -1);
          frmMain.sciEditor.SetFoldExpanded(i, False);
          if LineMaxSubord > i then
            frmMain.sciEditor.HideLines(i+1, LineMaxSubord);
        end;
      end;

      eStr.Free;
    end;
  end;

  Plugin_UpdateCodeExplorer(GetCurrLang.Name, ActiveDoc.FileName, frmMain.tsMain.Items[frmMain.tsMain.ActiveTabIndex].Caption, False);
end;

end.
