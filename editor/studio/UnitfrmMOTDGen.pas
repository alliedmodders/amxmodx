unit UnitfrmMOTDGen;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, ClipBrd,
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
  Clipboard.SetTextBuf(txtMOTD.Lines.GetText); 
end;

end.
