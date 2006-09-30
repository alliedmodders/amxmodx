unit UnitfrmGoToLine;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, mbTBXEdit, mbTBXValidateEdit, TBXDkPanels,
  SpTBXDkPanels, SpTBXEditors, SpTBXControls;

type
  TfrmGoToLine = class(TForm)
    pnlBG: TSpTBXPanel;
    lblCaption: TLabel;
    cmdCancel: TSpTBXButton;
    cmdOK: TSpTBXButton;
    txtGoToLine: TSpTBXEdit;
    procedure txtGoToLineKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure txtGoToLineChange(Sender: TObject);
  end;

var
  frmGoToLine: TfrmGoToLine;

implementation

uses UnitMainTools;

{$R *.DFM}

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
  txtGoToLine.SelectAll;
end;

procedure TfrmGoToLine.txtGoToLineChange(Sender: TObject);
begin
  cmdOK.Enabled := StrToIntDef(txtGoToLine.Text, -1) > 0;
end;

end.
