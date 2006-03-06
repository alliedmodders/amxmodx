unit UnitfrmParamEdit;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TBXDkPanels, SpTBXDkPanels, mbTBXMemo,
  SpTBXEditors, SpTBXControls;

type
  TfrmParamEdit = class(TForm)
    txtInformation: TmbTBXMemo;
    cmdOk: TSpTBXButton;
    cmdCancel: TSpTBXButton;
    lblFunction: TLabel;
    txtFunction: TSpTBXEdit;
    lblItems: TLabel;
    Label1: TLabel;
  end;

var
  frmParamEdit: TfrmParamEdit;

implementation

{$R *.DFM}

end.
