unit UnitfrmIRCPaster;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TBXDkPanels, SpTBXDkPanels, SpTBXEditors,
  SpTBXControls;

type
  TfrmIRCPaster = class(TForm)
    lblState: TLabel;
    chkColors: TSpTBXCheckBox;
    pnlPasteSettings: TSpTBXPanel;
    optAll: TSpTBXRadioButton;
    optLines: TSpTBXRadioButton;
    txtFrom: TSpTBXEdit;
    txtTo: TSpTBXEdit;
    lblTo: TLabel;
    optSelectedLines: TSpTBXRadioButton;
    cmdPaste: TSpTBXButton;
    cmdCancel: TSpTBXButton;
    chkDelay: TSpTBXCheckBox;
    lblChannel: TLabel;
    txtChannel: TSpTBXEdit;
    chkLineNumbers: TSpTBXCheckBox;
    procedure txtChange(Sender: TObject);
    procedure cmdPasteClick(Sender: TObject);
  end;

var
  frmIRCPaster: TfrmIRCPaster;

implementation

uses UnitfrmMain, UnitMainTools, UnitLanguages;

{$R *.DFM}

procedure TfrmIRCPaster.txtChange(Sender: TObject);
begin
  if not IsNumeric(TSpTBXEdit(Sender).Text) then
    TSpTBXEdit(Sender).Text := '1'
  else if StrToInt(TSpTBXEdit(Sender).Text) = 0 then
    TSpTBXEdit(Sender).Text := '1'
  else if StrToInt(TSpTBXEdit(Sender).Text) > frmMain.sciEditor.Lines.Count then
    TSpTBXEdit(Sender).Text := IntToStr(frmMain.sciEditor.Lines.Count);

  if StrToInt(txtFrom.Text) > StrToInt(txtTo.Text) then
    txtTo.Text := txtFrom.Text;
end;

procedure TfrmIRCPaster.cmdPasteClick(Sender: TObject);
begin
  if Length(txtChannel.Text) <= 1 then
    MessageBox(Handle, PChar(lInvalidChannel), PChar(Application.Title), MB_ICONERROR)
  else if (frmMain.sciEditor.Lines.Count > 25) and (optAll.Checked) then begin
    if MessageBox(Handle, PChar(lWarnBigPluginPaste), PChar(Application.Title), MB_ICONWARNING + MB_YESNO) = mrYes then
      ModalResult := mrOk;
  end
  else
    ModalResult := mrOk;
end;

end.
