unit UnitfrmSaveDialog;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, CheckLst, TFlatSpeedButtonUnit;

type
  TfrmSaveDialog = class(TForm)
    lstFiles: TCheckListBox;
    shpFiles: TShape;
    cmdSave: TFlatSpeedButton;
    cmdCancel: TFlatSpeedButton;
    lblInfo: TLabel;
    procedure lstFilesClickCheck(Sender: TObject);
  public
    SaveCaption: String;
    CloseCaption: String;
  end;

var
  frmSaveDialog: TfrmSaveDialog;

implementation

{$R *.DFM}

procedure TfrmSaveDialog.lstFilesClickCheck(Sender: TObject);
var i: integer;
begin
  for i := 0 to lstFiles.Items.Count -1 do begin
    if lstFiles.Checked[i] then begin
      cmdSave.Caption := SaveCaption;
      exit;
    end;
  end;
  cmdSave.Caption := CloseCaption;
end;

end.
