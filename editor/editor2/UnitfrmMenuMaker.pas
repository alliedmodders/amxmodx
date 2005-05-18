unit UnitfrmMenuMaker;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TFlatMemoUnit, XPStyleActnCtrls, ActnList,
  ActnMan, ToolWin, ActnCtrls, ComCtrls, CorelButton, ImgList,
  TFlatEditUnit, TFlatCheckBoxUnit;

type
  TfrmMenuMaker = class(TForm)
    pnlButtons: TPanel;
    cmdCancel: TCorelButton;
    ilButtons: TImageList;
    amButtons: TActionManager;
    acPaste: TAction;
    acCopy: TAction;
    acCut: TAction;
    acClear: TAction;
    acMenu: TAction;
    acGrey: TAction;
    acRed: TAction;
    acWhite: TAction;
    acYellow: TAction;
    cmdNext: TCorelButton;
    nbkPages: TNotebook;
    atbButtons: TActionToolBar;
    rtfEditor: TRichEdit;
    pnlSettings: TPanel;
    chkRegisterMenuCommand: TFlatCheckBox;
    chkAddComment: TFlatCheckBox;
    txtKeys: TFlatEdit;
    lblKeys: TLabel;
    bvlSpace: TBevel;
    lblSettings: TLabel;
    lblName: TLabel;
    txtMenuName: TFlatEdit;
    txtTime: TFlatEdit;
    chkUseTime: TFlatCheckBox;
    chkAppendOnlyMenuText: TFlatCheckBox;
    lblNote: TLabel;
    lblSettingsPlayers: TLabel;
    pnlSettingsPlayers: TPanel;
    lblMenu: TLabel;
    txtMenu: TFlatEdit;
    lblHelp: TLabel;
    chkAlive: TFlatCheckBox;
    bvlSpace2: TBevel;
    chkRegister: TFlatCheckBox;
    chkComments: TFlatCheckBox;
    chkImmunity: TFlatCheckBox;
    procedure acCopyExecute(Sender: TObject);
    procedure acCutExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acClearExecute(Sender: TObject);
    procedure acYellowExecute(Sender: TObject);
    procedure acWhiteExecute(Sender: TObject);
    procedure acRedExecute(Sender: TObject);
    procedure acGreyExecute(Sender: TObject);
    procedure txtKeysChange(Sender: TObject);
    procedure cmdCancelClick(Sender: TObject);
    procedure cmdNextClick(Sender: TObject);
    procedure rtfEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure rtfEditorKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure rtfEditorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rtfEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chkUseTimeClick(Sender: TObject);
    procedure txtMenuNameKeyPress(Sender: TObject; var Key: Char);
    procedure chkAppendOnlyMenuTextClick(Sender: TObject);
    procedure txtTimeChange(Sender: TObject);
    procedure rtfEditorChange(Sender: TObject);
    procedure acMenuExecute(Sender: TObject);
    procedure lblHelpMouseEnter(Sender: TObject);
    procedure lblHelpMouseLeave(Sender: TObject);
    procedure lblHelpClick(Sender: TObject);
  private
    Editing: Boolean;
    FDefaultMenu: Boolean;
    procedure SetDefaultMenu(const Value: Boolean);
  public
    property DefaultMenu: Boolean read FDefaultMenu write SetDefaultMenu;
    function GetColoredMenu: String;
    procedure SetButton(Action: TAction);
    procedure UpdateCurColor;
    procedure Reset;
  end;

var
  frmMenuMaker: TfrmMenuMaker;

implementation

uses UnitAddMenu, UnitfrmSelectMenu, UnitfrmMain, UnitHowToMakePlayerMenu;

{$R *.DFM}

procedure TfrmMenuMaker.acCopyExecute(Sender: TObject);
begin
  rtfEditor.CopyToClipboard;
end;

procedure TfrmMenuMaker.acCutExecute(Sender: TObject);
begin
  rtfEditor.CutToClipboard;
end;

procedure TfrmMenuMaker.acPasteExecute(Sender: TObject);
begin
  rtfEditor.PasteFromClipboard;
end;

procedure TfrmMenuMaker.acClearExecute(Sender: TObject);
begin
  rtfEditor.Clear;
  rtfEditor.SelAttributes.Color := clWhite;
  UpdateCurColor;
end;

procedure TfrmMenuMaker.acYellowExecute(Sender: TObject);
begin
  rtfEditor.SelAttributes.Color := clYellow;
  SetButton(acYellow);
end;

procedure TfrmMenuMaker.acWhiteExecute(Sender: TObject);
begin
  rtfEditor.SelAttributes.Color := clWhite;
  SetButton(acWhite);
end;

procedure TfrmMenuMaker.acRedExecute(Sender: TObject);
begin
  rtfEditor.SelAttributes.Color := clRed;
  SetButton(acRed);
end;

procedure TfrmMenuMaker.acGreyExecute(Sender: TObject);
begin
  rtfEditor.SelAttributes.Color := clGray;
  SetButton(acGrey);  
end;

procedure TfrmMenuMaker.SetButton(Action: TAction);
begin
  if Action <> acYellow then
    acYellow.Checked := False;
  if Action <> acWhite then
    acWhite.Checked := False;
  if Action <> acRed then
    acRed.Checked := False;
  if Action <> acGrey then
    acGrey.Checked := False;
  Action.Checked := True;
end;

procedure TfrmMenuMaker.txtKeysChange(Sender: TObject);
begin
  try
    StrToInt(Trim((Sender As TFlatEdit).Text));
  except
    if Sender = txtKeys then begin
      (Sender As TFlatEdit).Text := '1';
      SysUtils.Beep;
    end
    else
      (Sender As TFlatEdit).Text := '';
  end;
end;

procedure TfrmMenuMaker.cmdCancelClick(Sender: TObject);
begin
  if nbkPages.PageIndex = 0 then
    ModalResult := mrCancel
  else begin
    nbkPages.PageIndex := 0;
    if not DefaultMenu then begin
      lblNote.Visible := True;
      lblHelp.Visible := True;
    end;
    cmdCancel.Caption := 'Cancel';
    cmdNext.Caption := '&Next >';
  end;
end;

procedure TfrmMenuMaker.cmdNextClick(Sender: TObject);
function IsNumeric(eChar: Char): Boolean;
begin
  Result := Pos(eChar, '0123456789') <> 0;
end;
var i: integer;
    eColoredMenu: String;
begin
  if Editing then begin
    eColoredMenu := GetColoredMenu;
    frmMain.sciEditor.Lines[frmSelectMenu.eLines[frmSelectMenu.GetItemIndex]] := StringReplace(frmMain.sciEditor.Lines[frmSelectMenu.eLines[frmSelectMenu.GetItemIndex]], '"' + frmSelectMenu.eMenuStr[frmSelectMenu.GetItemIndex] + '"', '"' + eColoredMenu + '"', []);
    Editing := False;
    ModalResult := mrOk;
  end
  else if nbkPages.PageIndex = 0 then begin // Editor
    if DefaultMenu then begin
      txtKeys.Text := '0';
      for i := 0 to rtfEditor.Lines.Count -1 do begin
        if Length(rtfEditor.Lines[i]) <> 0 then begin
          if IsNumeric(rtfEditor.Lines[i][1]) then
            txtKeys.Text := txtKeys.Text + rtfEditor.Lines[i][1];
        end;
      end;
      if Length(txtKeys.Text) <> 1 then
        txtKeys.Text := Copy(txtKeys.Text, 2, Length(txtKeys.Text));
      nbkPages.PageIndex := 1;
    end
    else begin
      if Pos('$players', LowerCase(rtfEditor.Lines.Text)) = 0 then begin
        MessageBox(Handle, 'You forgot to set the players.', 'Warning', MB_ICONWARNING);
        exit;
      end;
      
      if (Pos('$next', LowerCase(rtfEditor.Lines.Text)) = 0) and (Pos('$back', LowerCase(rtfEditor.Lines.Text)) = 0) then
        MessageBox(Handle, 'You should set a "Next" and a "Back" key.', 'Warning', MB_ICONWARNING)
      else if Pos('$next', LowerCase(rtfEditor.Lines.Text)) = 0 then
        MessageBox(Handle, 'You should set a "Next"-key.', 'Warning', MB_ICONWARNING)
      else if Pos('$exitorback', LowerCase(rtfEditor.Lines.Text)) = 0 then
        MessageBox(Handle, 'You should set a "Back"-key.', 'Warning', MB_ICONWARNING);

      nbkPages.PageIndex := 2;
    end;
    cmdCancel.Caption := '< &Back';
    cmdNext.Caption := 'Finish';
  end
  else if nbkPages.PageIndex = 1 then begin // Default finish
    if (txtMenuName.Text = '') and (not chkAppendOnlyMenuText.Checked) then
      MessageBox(Handle, 'Invalid menu name.', 'Warning', MB_ICONWARNING)
    else begin
      Screen.Cursor := crHourGlass;
      if AddMenu then
        ModalResult := mrOk
      else
        MessageBox(Handle, 'Menu already exists. Please choose another name.', 'Warning', MB_ICONWARNING);
      Screen.Cursor := crDefault;
    end;
  end
  else begin // Player finish
    if (txtMenu.Text = '') then
      MessageBox(Handle, 'Invalid menu name.', 'Warning', MB_ICONWARNING)
    else begin
      if AddPlayerMenu then
        ModalResult := mrOk;
      Screen.Cursor := crDefault;
    end;
  end;
  lblNote.Visible := False;
  lblHelp.Visible := False;
end;

procedure TfrmMenuMaker.UpdateCurColor;
begin
  case rtfEditor.SelAttributes.Color of
    clYellow: SetButton(acYellow);
    clWhite: SetButton(acWhite);
    clRed: SetButton(acRed);
    clGray: SetButton(acGrey);
  end;
  atbButtons.RecreateControls;
end;

procedure TfrmMenuMaker.rtfEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateCurColor;
end;

procedure TfrmMenuMaker.rtfEditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateCurColor;
end;

procedure TfrmMenuMaker.rtfEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateCurColor;
end;

procedure TfrmMenuMaker.rtfEditorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateCurColor;
end;

procedure TfrmMenuMaker.chkUseTimeClick(Sender: TObject);
begin
  txtTime.Enabled := chkUseTime.Checked;
  if not chkUseTime.Checked then
    txtTime.Text := '-1';
end;

procedure TfrmMenuMaker.txtMenuNameKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #32 then
    Key := #0;
end;

procedure TfrmMenuMaker.chkAppendOnlyMenuTextClick(Sender: TObject);
begin
  lblKeys.Enabled := not chkAppendOnlyMenuText.Checked;
  txtKeys.Enabled := not chkAppendOnlyMenuText.Checked;
  chkAddComment.Enabled := not chkAppendOnlyMenuText.Checked;
  chkRegisterMenuCommand.Enabled := not chkAppendOnlyMenuText.Checked;
  chkUseTime.Enabled := not chkAppendOnlyMenuText.Checked;
  txtTime.Enabled := not chkAppendOnlyMenuText.Checked;
  lblName.Enabled := not chkAppendOnlyMenuText.Checked;
  txtMenuName.Enabled := not chkAppendOnlyMenuText.Checked;
end;

procedure TfrmMenuMaker.txtTimeChange(Sender: TObject);
begin
  try
    if StrToInt(txtTime.Text) < -1 then begin
      txtTime.Text := '-1';
      SysUtils.Beep;
    end;
  except
    txtTime.Text := '1000';
    SysUtils.Beep;
  end;
end;

procedure TfrmMenuMaker.rtfEditorChange(Sender: TObject);
begin
  cmdNext.Enabled := rtfEditor.Text <> '';
end;

procedure TfrmMenuMaker.SetDefaultMenu(const Value: Boolean);
begin
  FDefaultMenu := Value;
  lblNote.Visible := not Value;
  lblHelp.Visible := not Value;
  acMenu.Enabled := Value;
  Editing := False;
  if Value then
    Caption := 'Menu Maker'
  else
    Caption := 'Player Menu Maker';
end;

procedure TfrmMenuMaker.acMenuExecute(Sender: TObject);
var eTemp: String;
begin
  if frmSelectMenu.ShowModal = mrOk then begin
    atbButtons.RecreateControls;
    if frmSelectMenu.GetItemIndex = -1 then
      frmSelectMenu.lstMenu.Selected[0] := True;
      
    DefaultMenu := True;
    Editing := True;
    rtfEditor.Clear;
    rtfEditor.SelAttributes.Color := clWhite;
    eTemp := frmSelectMenu.eMenuStr[frmSelectMenu.GetItemIndex];
    if eTemp <> '' then begin
      while Length(eTemp) <> 0 do begin
        if eTemp[1] = '\' then begin
          if Length(eTemp) <> 1 then begin
            case LowerCase(eTemp[2])[1] of
              'w': rtfEditor.SelAttributes.Color := clWhite;
              'r': rtfEditor.SelAttributes.Color := clRed;
              'd': rtfEditor.SelAttributes.Color := clGray;
              'y': rtfEditor.SelAttributes.Color := clYellow;
            end;
          end;
          Delete(eTemp, 1, 2);
        end
        else if Copy(eTemp, 1, 2) = '^n' then begin
          rtfEditor.SelText := #13#10;
          Delete(eTemp, 1, 2);
        end
        else begin
          rtfEditor.SelText := eTemp[1];
          Delete(eTemp, 1, 1);
        end;
     end;
    end;
    cmdNext.Caption := 'Finish edit'; 
  end
  else
    atbButtons.RecreateControls;
end;

function TfrmMenuMaker.GetColoredMenu: String;
var i: integer;
    eCurColor: TColor;
begin
  eCurColor := clWhite;
  Result := '';
  for i := 0 to Length(rtfEditor.Lines.Text) -1 do begin
    rtfEditor.SelStart := i;
    if rtfEditor.SelAttributes.Color <> eCurColor then begin
      eCurColor := rtfEditor.SelAttributes.Color;
      case eCurColor of
        clWhite : Result := Result + '\w';
        clYellow: Result := Result + '\y';
        clRed   : Result := Result + '\r';
        clGray  : Result := Result + '\d';
      end;
    end;
    Result := Result + rtfEditor.Lines.Text[i+1];
  end;
  rtfEditor.SelStart := 0;
  Result := StringReplace(Result, #13, '^n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
end;

procedure TfrmMenuMaker.Reset;
begin
  rtfEditor.Clear;
  rtfEditor.SelAttributes.Color := clWhite;
  txtKeys.Text := '1';
  txtMenu.Clear;
  txtMenuName.Clear;
  UpdateCurColor;
end;

procedure TfrmMenuMaker.lblHelpMouseEnter(Sender: TObject);
begin
  lblHelp.Font.Color := clBlue;
  lblHelp.Font.Style := [fsUnderline];
end;

procedure TfrmMenuMaker.lblHelpMouseLeave(Sender: TObject);
begin
  lblHelp.Font.Color := clWindowText;
  lblHelp.Font.Style := [];
end;

procedure TfrmMenuMaker.lblHelpClick(Sender: TObject);
procedure Append(eText: String);
begin
  rtfEditor.SelText := eText + #13#10;
  rtfEditor.SelStart := Length(rtfEditor.Lines.Text);
end;

begin
  if frmHowToMakePlayerMenu.ShowModal = mrYes then begin
    rtfEditor.Clear;
    rtfEditor.SelAttributes.Color := clYellow;
    Append('Kick player');
    Append('');
    rtfEditor.SelAttributes.Color := clWhite;
    Append('$players(1,8,%n. %v)');
    Append('$next(9,9. Next)');
    Append('');
    Append('$exitorback(0,0. Exit,0. Back)');
  end;
end;

end.
