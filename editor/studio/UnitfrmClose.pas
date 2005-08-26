unit UnitfrmClose;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, CheckLst, TFlatSpeedButtonUnit, ComCtrls,
  JvExComCtrls, JvComCtrls, JvCheckTreeView;

type
  TfrmClose = class(TForm)
    shpFiles: TShape;
    cmdSave: TFlatSpeedButton;
    cmdCancel: TFlatSpeedButton;
    lblInfo: TLabel;
    trvFiles: TJvCheckTreeView;
    procedure trvFilesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  end;

var
  frmClose: TfrmClose;

implementation

uses UnitLanguages;

{$R *.DFM}

procedure TfrmClose.trvFilesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var eClose: Boolean;
    i, k: integer;
begin
  if Assigned(trvFiles.GetNodeAt(X, Y)) then begin
    if not Assigned(trvFiles.GetNodeAt(X, Y).Parent) then begin
      eClose := trvFiles.Checked[trvFiles.GetNodeAt(X, Y)];
      with trvFiles.GetNodeAt(X, Y) do begin
        for i := 0 to Count -1 do
          trvFiles.Checked[Item[i]] := eClose;
      end;
    end;
  end;

  eClose := True;
  for i := 0 to trvFiles.Items.Count -1 do begin
    for k := 0 to trvFiles.Items[i].Count -1 do begin
      if (trvFiles.Checked[trvFiles.Items[i].Item[k]]) then begin
        eClose := False;
        break;
      end;
    end;
  end;

  if eClose then
    cmdSave.Caption := lCloseCaption
  else
    cmdSave.Caption := lSaveCaption;
end;

end.
