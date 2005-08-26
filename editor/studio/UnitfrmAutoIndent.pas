unit UnitfrmAutoIndent;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TFlatCheckBoxUnit, TFlatButtonUnit;

type
  TfrmAutoIndent = class(TForm)
    chkUnindentPressingClosingBrace: TFlatCheckBox;
    chkUnindentLine: TFlatCheckBox;
    chkIndentOpeningBrace: TFlatCheckBox;
    cmdClose: TFlatButton;
  end;

var
  frmAutoIndent: TfrmAutoIndent;

implementation

{$R *.DFM}

end.
