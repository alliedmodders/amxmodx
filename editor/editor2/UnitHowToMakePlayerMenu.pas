unit UnitHowToMakePlayerMenu;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, CorelButton;

type
  TfrmHowToMakePlayerMenu = class(TForm)
    lblHowTo: TLabel;
    txtTutorial: TMemo;
    cmdOK: TCorelButton;
    cmdExample: TCorelButton;
  end;

var
  frmHowToMakePlayerMenu: TfrmHowToMakePlayerMenu;

implementation

{$R *.DFM}

end.
