unit UnitfrmMOTDGen;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, mxFlatControls, TBXDkPanels, SpTBXDkPanels;

type
  TfrmMOTDGen = class(TForm)
    txtMOTD: TmxFlatMemo;
    cmdClose: TSpTBXButton;
    cmdCopy: TSpTBXButton;
    procedure cmdCopyClick(Sender: TObject);
  end;

var
  frmMOTDGen: TfrmMOTDGen;

implementation

{$R *.DFM}

procedure TfrmMOTDGen.cmdCopyClick(Sender: TObject);
begin
  txtMOTD.CopyToClipboard;
  txtMOTD.CopyToClipboard;
end;

end.
