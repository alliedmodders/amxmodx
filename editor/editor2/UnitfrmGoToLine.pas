unit UnitfrmGoToLine;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TFlatSpeedButtonUnit, TFlatEditUnit;

type
  TfrmGoToLine = class(TForm)
    lblInfo: TLabel;
    txtLine: TFlatEdit;
    cmdOK: TFlatSpeedButton;
    cmdCancel: TFlatSpeedButton;
    procedure cmdOKClick(Sender: TObject);
    procedure txtLineKeyPress(Sender: TObject; var Key: Char);
  end;

var
  frmGoToLine: TfrmGoToLine;

implementation

uses UnitfrmMain;

{$R *.DFM}

procedure TfrmGoToLine.cmdOKClick(Sender: TObject);
begin
  try
    if (StrToInt(txtLine.Text) < 0) or (StrToInt(txtLine.Text) > frmMain.sciEditor.Lines.Count) then
      raise Exception.Create('Invalid Line')
    else
      ModalResult := mrOK;
  except
    MessageBox(Handle, 'Invalid value. Check the entered line and press OK again.', 'Error', MB_ICONERROR);
  end;
end;

procedure TfrmGoToLine.txtLineKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    cmdOk.Click;
    Key := #0;
  end;
end;

end.
