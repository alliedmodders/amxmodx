unit UnitfrmGoToLine;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, mbTBXEdit, mbTBXValidateEdit, TBXDkPanels,
  SpTBXDkPanels, SpTBXEditors;

type
  TfrmGoToLine = class(TForm)
    lblCaption: TLabel;
    txtGoToLine: TSpTBXEdit;
    cmdOK: TSpTBXButton;
    cmdCancel: TSpTBXButton;
    procedure txtGoToLineChange(Sender: TObject);
    procedure txtGoToLineKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  end;

var
  frmGoToLine: TfrmGoToLine;

implementation

uses UnitMainTools;

{$R *.DFM}

procedure TfrmGoToLine.txtGoToLineChange(Sender: TObject);
begin
  if not IsNumeric(txtGoToLine.Text) then
    txtGoToLine.Text := '1'
  else if txtGoToLine.Text = '0' then
    txtGoToLine.Text := '1';
end;

procedure TfrmGoToLine.txtGoToLineKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    cmdOK.Click;
    Key := #0;
  end;
end;

procedure TfrmGoToLine.FormShow(Sender: TObject);
begin
  txtGoToLine.SetFocus;
end;

end.
