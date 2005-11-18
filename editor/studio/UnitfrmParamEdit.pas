unit UnitfrmParamEdit;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TBXDkPanels, SpTBXDkPanels, mbTBXMemo,
  SpTBXEditors;

type
  TfrmParamEdit = class(TForm)
    txtInformation: TmbTBXMemo;
    cmdOk: TSpTBXButton;
    cmdCancel: TSpTBXButton;
    lblFunction: TLabel;
    txtFunction: TSpTBXEdit;
    lblItems: TLabel;
  end;

var
  frmParamEdit: TfrmParamEdit;

implementation

{$R *.DFM}

end.
