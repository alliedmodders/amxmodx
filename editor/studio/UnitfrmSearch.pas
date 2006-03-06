unit UnitfrmSearch;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TntStdCtrls, SpTBXEditors, SpTBXDkPanels,
  TBXDkPanels, Dialogs, SpTBXControls;

type
  TfrmSearch = class(TForm)
    pnlOptions: TSpTBXGroupBox;
    lblSearchFor: TLabel;
    cboSearchFor: TSpTBXComboBox;
    pnlDirection: TSpTBXGroupBox;
    chkCaseSensivity: TSpTBXCheckBox;
    chkWholeWordsOnly: TSpTBXCheckBox;
    chkSearchFromCaret: TSpTBXCheckBox;
    chkSelectedTextOnly: TSpTBXCheckBox;
    chkRegularExpression: TSpTBXCheckBox;
    cmdOK: TSpTBXButton;
    cmdCancel: TSpTBXButton;
    chkForward: TSpTBXCheckBox;
    chkBackward: TSpTBXCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OnDirectionClick(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure cboSearchForKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  public
    eChange: Boolean;
  end;

var
  frmSearch: TfrmSearch;

implementation

uses UnitLanguages, UnitPlugins;

{$R *.DFM}

procedure TfrmSearch.FormCreate(Sender: TObject);
begin
  eChange := True;
end;

procedure TfrmSearch.OnDirectionClick(Sender: TObject);
begin
  if not eChange then exit;
  eChange := False;
  chkForward.Checked := Sender = chkForward;
  chkBackward.Checked := Sender = chkBackward;
  eChange := True;
end;

procedure TfrmSearch.cmdOKClick(Sender: TObject);
begin
  if cboSearchFor.Text = '' then
    MessageBox(Handle, PChar(lEnterSearchText), PChar(Application.Title), MB_ICONERROR)
  else begin
    if cboSearchFor.Items.IndexOf(cboSearchFor.Text) = -1 then
      cboSearchFor.Items.Add(cboSearchFor.Text);
    ModalResult := mrOK;
  end;
end;

procedure TfrmSearch.cboSearchForKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    cmdOk.Click;
    Key := #0;
  end;
end;

procedure TfrmSearch.FormShow(Sender: TObject);
begin
  cboSearchFor.SetFocus;
end;

end.
