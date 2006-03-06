unit UnitfrmConnGen;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, SpTBXEditors, TntStdCtrls, TBXDkPanels,
  SpTBXDkPanels, SpTBXControls;

type
  TfrmConnGen = class(TForm)
    lblState: TLabel;
    pnlSettings: TPanel;
    lblHost: TLabel;
    txtHost: TSpTBXEdit;
    lblPort: TLabel;
    txtPort: TSpTBXEdit;
    lblProtocol: TLabel;
    cboProtocol: TSpTBXComboBox;
    cmdOK: TSpTBXButton;
    cmdCancel: TSpTBXButton;
    lblSocketName: TLabel;
    txtName: TSpTBXEdit;
    procedure txtNameKeyPress(Sender: TObject; var Key: Char);
  end;

var
  frmConnGen: TfrmConnGen;

implementation

{$R *.DFM}

procedure TfrmConnGen.txtNameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #32 then
    Key := #0;
end;

end.
