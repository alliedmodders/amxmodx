unit UnitfrmAutoIndent;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TFlatCheckBoxUnit, TFlatButtonUnit,
  TFlatEditUnit, TFlatRadioButtonUnit;

type
  TfrmAutoIndent = class(TForm)
    cmdClose: TFlatButton;
    pnlCheckboxes: TPanel;
    chkUnindentPressingClosingBrace: TFlatCheckBox;
    chkUnindentLine: TFlatCheckBox;
    chkIndentOpeningBrace: TFlatCheckBox;
    pnlIndentStyle: TPanel;
    optTwoSpaces: TFlatRadioButton;
    optTabs: TFlatRadioButton;
    Label1: TLabel;
    optSomethingElse: TFlatRadioButton;
    txtSomethingElse: TFlatEdit;
  end;

var
  frmAutoIndent: TfrmAutoIndent;

implementation

{$R *.DFM}

end.
