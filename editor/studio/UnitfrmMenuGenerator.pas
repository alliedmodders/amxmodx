unit UnitfrmMenuGenerator;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TBXDkPanels, SpTBXDkPanels, JvExControls,
  JvComponent, JvPageList, TFlatRadioButtonUnit, TFlatMemoUnit,
  TFlatEditUnit, TB2Dock, TB2Toolbar, TBX, SpTBXItem, ImgList, TB2Item,
  Dialogs, TFlatComboBoxUnit, ComCtrls, ClipBrd, TFlatCheckBoxUnit,
  SpTBXControls;

type
  TfrmMenuGenerator = class(TForm)
    jplMain: TJvPageList;
    jspSelectType: TJvStandardPage;
    pnlSelectType: TPanel;
    lblSelectInfo: TLabel;
    optSimpleOldMenu: TSpTBXRadioButton;
    optPlayerMenu: TSpTBXRadioButton;
    optSimpleMenu: TSpTBXRadioButton;
    cmdCancel: TSpTBXButton;
    cmdNext: TSpTBXButton;
    jspOldMenuAdd1: TJvStandardPage;
    optOldPlayerMenu: TSpTBXRadioButton;
    jspNewMenuAdd: TJvStandardPage;
    ilImages: TImageList;
    pnlBack: TPanel;
    pnlMenu: TPanel;
    tbxMenuItems: TSpTBXToolbar;
    mnuAdd: TSpTBXItem;
    mnuRemove: TSpTBXItem;
    sepItems: TSpTBXSeparatorItem;
    mnuUp: TSpTBXItem;
    mnuMoveDown: TSpTBXItem;
    lstNMenuItems: TListBox;
    txtNTitle: TFlatEdit;
    lblNTitle: TLabel;
    lblMenuNItemsCaption: TLabel;
    cmdNCreate: TSpTBXButton;
    cmdBack: TSpTBXButton;
    pnlAccess: TPanel;
    lblAccess: TLabel;
    cboAccess: TFlatComboBox;
    lblState: TLabel;
    tbxColors: TSpTBXToolbar;
    mnuYellow: TSpTBXItem;
    mnuWhite: TSpTBXItem;
    mnuRed: TSpTBXItem;
    mnuGray: TSpTBXItem;
    pnlControls: TPanel;
    rtfMenu: TRichEdit;
    sepColors: TSpTBXSeparatorItem;
    mnuCopy: TSpTBXItem;
    cmdOldNext1: TSpTBXButton;
    cmdOldBack1: TSpTBXButton;
    lblHelp: TLabel;
    jspOldMenuAdd2: TJvStandardPage;
    pnlSettings: TPanel;
    txtKeys: TFlatEdit;
    lblKeys: TLabel;
    txtMenuName: TFlatEdit;
    lblName: TLabel;
    chkAddComment: TFlatCheckBox;
    chkRegisterMenuCommand: TFlatCheckBox;
    chkUseTime: TFlatCheckBox;
    txtTime: TFlatEdit;
    lblSettings: TLabel;
    cmdOldNext2: TSpTBXButton;
    cmdOldBack2: TSpTBXButton;
    procedure mnuAddClick(Sender: TObject);
    procedure mnuRemoveClick(Sender: TObject);
    procedure mnuUpClick(Sender: TObject);
    procedure mnuMoveDownClick(Sender: TObject);
    procedure cmdNCreateClick(Sender: TObject);
    procedure cmdBackClick(Sender: TObject);
    procedure cmdNextClick(Sender: TObject);
    procedure lblHelpMouseEnter(Sender: TObject);
    procedure lblHelpMouseLeave(Sender: TObject);
    procedure mnuYellowClick(Sender: TObject);
    procedure mnuWhiteClick(Sender: TObject);
    procedure mnuRedClick(Sender: TObject);
    procedure mnuGrayClick(Sender: TObject);
    procedure rtfMenuMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rtfMenuKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmdOldNext1Click(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure cmdOldNext2Click(Sender: TObject);
    procedure lblHelpClick(Sender: TObject);
  private
    procedure UpdateColor;
    procedure UpdateMenu(Sender: TObject);
  end;

var
  frmMenuGenerator: TfrmMenuGenerator;

implementation

uses UnitLanguages, UnitMenuGenerators, UnitCodeUtils, UnitMainTools;

{$R *.DFM}

procedure TfrmMenuGenerator.mnuAddClick(Sender: TObject);
var eStr: String;
begin
  if InputQuery(lAddItemCaption, lAddItemPrompt, eStr) then begin
    if (optPlayerMenu.Checked) and (UpperCase(eStr) = 'PLAYERS') then begin
      eStr := 'PLAYERS';
      if lstNMenuItems.Items.IndexOf('PLAYERS') <> -1 then begin
        MessageBox(Handle, PChar(lPlayersAlreadyAdded), PChar(Application.Title), MB_ICONERROR);
      end;
    end;
    lstNMenuItems.ItemIndex := lstNMenuItems.Items.Add(eStr);
  end;
end;

procedure TfrmMenuGenerator.mnuRemoveClick(Sender: TObject);
begin
  if lstNMenuItems.ItemIndex <> -1 then
    lstNMenuItems.DeleteSelected;
end;

procedure TfrmMenuGenerator.mnuUpClick(Sender: TObject);
begin
  if lstNMenuItems.ItemIndex > 0 then
    lstNMenuItems.Items.Exchange(lstNMenuItems.ItemIndex, lstNMenuItems.ItemIndex -1);
end;

procedure TfrmMenuGenerator.mnuMoveDownClick(Sender: TObject);
begin
  if (lstNMenuItems.ItemIndex <> -1) and (lstNMenuItems.ItemIndex <> lstNMenuItems.Items.Count -1) then
    lstNMenuItems.Items.Exchange(lstNMenuItems.ItemIndex, lstNMenuItems.ItemIndex +1); 
end;

procedure TfrmMenuGenerator.cmdNCreateClick(Sender: TObject);
begin
  if txtNTitle.Text = '' then
    MessageBox(Handle, PChar(lEnterTitle), PChar(Application.Title), MB_ICONERROR)
  else if lstNMenuItems.Items.Count = 0 then
    MessageBox(Handle, PChar(lAddItems), PChar(Application.Title), MB_ICONERROR)
  else if self.optSimpleMenu.Checked then begin
    GenerateSimpleMenu;
    ModalResult := mrOk;
  end;
end;

procedure TfrmMenuGenerator.cmdBackClick(Sender: TObject);
begin
  if jplMain.ActivePage = jspOldMenuAdd2 then
    jplMain.ActivePage := jspOldMenuAdd1
  else begin
    jplMain.ActivePageIndex := 0;
    lblState.Caption := 'Menu Generator';
  end;
end;

procedure TfrmMenuGenerator.cmdNextClick(Sender: TObject);
begin
  // New style
  if optSimpleMenu.Checked then begin
    jplMain.ActivePageIndex := 1;
    lblState.Caption := 'Create a simple menu';
  end;
  // Old style
  if optSimpleOldMenu.Checked then begin
    jplMain.ActivePageIndex := 2;
    lblState.Caption := 'Create a simple menu';
    lblHelp.Hide;
  end;
  if optOldPlayerMenu.Checked then begin
    jplMain.ActivePageIndex := 2;
    lblState.Caption := 'Create a player menu';
    lblHelp.Show;
  end;
end;

procedure TfrmMenuGenerator.lblHelpMouseEnter(Sender: TObject);
begin
  lblHelp.Font.Color := clHotLight;
  lblHelp.Font.Style := [fsUnderline];
end;

procedure TfrmMenuGenerator.lblHelpMouseLeave(Sender: TObject);
begin
  lblHelp.Font.Color := clWindowText;
  lblHelp.Font.Style := [];
end;

procedure TfrmMenuGenerator.mnuYellowClick(Sender: TObject);
begin
  rtfMenu.SelAttributes.Color := clYellow;
  UpdateMenu(Sender);
end;

procedure TfrmMenuGenerator.mnuWhiteClick(Sender: TObject);
begin
  rtfMenu.SelAttributes.Color := clWhite;
  UpdateMenu(Sender);
end;

procedure TfrmMenuGenerator.mnuRedClick(Sender: TObject);
begin
  rtfMenu.SelAttributes.Color := clRed;
  UpdateMenu(Sender);
end;

procedure TfrmMenuGenerator.mnuGrayClick(Sender: TObject);
begin
  rtfMenu.SelAttributes.Color := clGray;
  UpdateMenu(Sender);
end;

procedure TfrmMenuGenerator.UpdateColor;
begin
  case rtfMenu.SelAttributes.Color of
    clYellow: mnuYellow.Checked := True;
    clWhite: mnuWhite.Checked := True;
    clRed: mnuRed.Checked := True;
    clGray: mnuGray.Checked := True;
  end;
end;

procedure TfrmMenuGenerator.UpdateMenu(Sender: TObject);
begin
  mnuYellow.Checked := Sender = mnuYellow;
  mnuWhite.Checked := Sender = mnuWhite;
  mnuRed.Checked := Sender = mnuRed;
  mnuGray.Checked := Sender = mnuGray;
end;

procedure TfrmMenuGenerator.rtfMenuMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateColor;
end;

procedure TfrmMenuGenerator.rtfMenuKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateColor;
end;

procedure TfrmMenuGenerator.cmdOldNext1Click(Sender: TObject);
var i, k: integer;
    a, b: integer;
begin
  if Trim(rtfMenu.Text) = '' then begin
    MessageBox(Handle, 'The menu is empty!', PChar(Application.Title), MB_ICONERROR);
    exit;
  end;

  txtKeys.Clear;
  for i := 0 to rtfMenu.Lines.Count -1 do begin
    if IsNumeric(Copy(rtfMenu.Lines[i], 1, 1)) then
      txtKeys.Text := txtKeys.Text + rtfMenu.Lines[i][1];
    if lblHelp.Visible then begin
      try
        if Pos('$players(', LowerCase(rtfMenu.Lines[i])) = 1 then begin
          a := StrToInt(Trim(Between(rtfMenu.Lines[i], '$players(', ',')));
          b := StrToInt(Trim(Between(rtfMenu.Lines[i], '$players(' + IntToStr(a) + ',', ',')));
          for k := a to b do
            txtKeys.Text := txtKeys.Text + IntToStr(k);
        end;
        
        if Pos('$next(', LowerCase(rtfMenu.Lines[i])) = 1 then begin
          a := StrToInt(Trim(Between(rtfMenu.Lines[i], '$next(', ',')));
          txtKeys.Text := txtKeys.Text + IntToStr(a);
        end;

        if Pos('$exitorback(', LowerCase(rtfMenu.Lines[i])) = 1 then begin
          a := StrToInt(Trim(Between(rtfMenu.Lines[i], '$exitorback(', ',')));
          txtKeys.Text := txtKeys.Text + IntToStr(a);
        end;
      except
        MessageBox(Handle, PChar('Invalid menu.'), PChar(Application.Title), MB_ICONERROR);
      end;
    end;
  end;
  jplMain.ActivePageIndex := 3;
end;

procedure TfrmMenuGenerator.mnuCopyClick(Sender: TObject);
begin
  Clipboard.SetTextBuf(PChar(GetColoredMenu));
  MessageBox(Handle, 'Okay, menu copied to clipboard.', PChar(Application.Title), MB_ICONINFORMATION);
end;

procedure TfrmMenuGenerator.cmdOldNext2Click(Sender: TObject);
begin
  if (StrToIntDef(txtKeys.Text, -1) = -1) then
    MessageBox(Handle, 'Invalid Keys (only keys between 0 and 9 allowed', 'Error', MB_ICONWARNING)
  else begin
    if optSimpleOldMenu.Checked then
      AddOldMenu
    else
      AddOldPlayerMenu;
    ModalResult := mrOk;
  end;
end;

procedure TfrmMenuGenerator.lblHelpClick(Sender: TObject);
var eMsg: String;
begin
  eMsg :=        '1. Choose a menu title (e.g. Kick player)' + #13;
  eMsg := eMsg + '2. Set dynamic players with $players in this format:' + #13;
  eMsg := eMsg + '   $players(StartKey, StopKey, Caption)' + #13;
  eMsg := eMsg + '   Variables in caption are: %n (Key) and %v(Player)' + #13;
  eMsg := eMsg + '3. Set Next and Back keys using $next(Key, Caption)' + #13;
  eMsg := eMsg + '   and $exitorback(Key, ExitCaption, BackCaption)' + #13;
  eMsg := eMsg + #13;
  eMsg := eMsg + 'Show example?';

  if MessageBox(Handle, PChar(eMsg), PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then begin
    rtfMenu.Clear;
    rtfMenu.SelAttributes.Color := clYellow;
    rtfMenu.SelText := 'Kick player' + #13 + #13;
    rtfMenu.SelStart := Length(rtfMenu.Lines.Text);
    rtfMenu.SelAttributes.Color := clWhite;
    rtfMenu.SelText := #13 + '$players(1,8,%n. %v)';
    rtfMenu.SelStart := Length(rtfMenu.Lines.Text);
    rtfMenu.SelAttributes.Color := clWhite;
    rtfMenu.SelText := #13 + #13 + '$next(9,9. Next)';
    rtfMenu.SelStart := Length(rtfMenu.Lines.Text);
    rtfMenu.SelAttributes.Color := clWhite;
    rtfMenu.SelText := #13 + '$exitorback(0,0. Exit,0. Back)';
    rtfMenu.SelStart := 0;
  end;
end;

end.
