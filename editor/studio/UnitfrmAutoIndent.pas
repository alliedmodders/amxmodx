unit UnitfrmAutoIndent;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TFlatCheckBoxUnit, TFlatButtonUnit;

type
  TfrmAutoIndent = class(TForm)
    cmdClose: TFlatButton;
    pnlCheckboxes: TPanel;
    chkUnindentPressingClosingBrace: TFlatCheckBox;
    chkUnindentLine: TFlatCheckBox;
    chkIndentOpeningBrace: TFlatCheckBox;
  end;

var
  frmAutoIndent: TfrmAutoIndent;

implementation

{$R *.DFM}

end.
