unit UnitfrmReplace;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TntStdCtrls, SpTBXEditors, SpTBXDkPanels,
  TBXDkPanels, SpTBXControls;

type
  TfrmReplace = class(TForm)
    lblSearchFor: TLabel;
    cboSearchFor: TSpTBXComboBox;
    lblReplaceWith: TLabel;
    cboReplaceWith: TSpTBXComboBox;
    pnlOptions: TSpTBXGroupBox;
    chkCaseSensivity: TSpTBXCheckBox;
    chkWholeWordsOnly: TSpTBXCheckBox;
    chkSearchFromCaret: TSpTBXCheckBox;
    chkSelectedTextOnly: TSpTBXCheckBox;
    chkRegularExpression: TSpTBXCheckBox;
    pnlDirection: TSpTBXGroupBox;
    chkForward: TSpTBXCheckBox;
    chkBackward: TSpTBXCheckBox;
    cmdOK: TSpTBXButton;
    cmdCancel: TSpTBXButton;
    bvlReplaceAll: TBevel;
    chkReplaceAll: TSpTBXCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OnDirectionChange(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    eChange: Boolean;
  end;

var
  frmReplace: TfrmReplace;

implementation

uses UnitLanguages;

{$R *.DFM}

procedure TfrmReplace.FormCreate(Sender: TObject);
begin
  eChange := True;
end;

procedure TfrmReplace.OnDirectionChange(Sender: TObject);
begin
  if not eChange then exit;
  eChange := False;
  chkForward.Checked := Sender = chkForward;
  chkBackward.Checked := Sender = chkBackward;
  eChange := True;
end;

procedure TfrmReplace.cmdOKClick(Sender: TObject);
begin
  if cboSearchFor.Text = '' then
    MessageBox(Handle, PChar(lEnterSearchText), PChar(Application.Title), MB_ICONERROR)
  else begin
    if cboSearchFor.Items.IndexOf(cboSearchFor.Text) = -1 then
      cboSearchFor.Items.Add(cboSearchFor.Text);
    if (cboReplaceWith.Text <> '') and (cboReplaceWith.Items.IndexOf(cboReplaceWith.Text) = -1) then
      cboReplaceWith.Items.Add(cboReplaceWith.Text); 
    ModalResult := mrOK;
  end;
end;

procedure TfrmReplace.FormShow(Sender: TObject);
begin
  cboSearchFor.SetFocus;
end;

end.
