unit UnitfrmAbout;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, ShellAPI, TFlatSpeedButtonUnit;

type
  TfrmAbout = class(TForm)
    pnlInfo: TPanel;
    imgAMXX: TImage;
    lblCopyright: TLabel;
    lblComments: TLabel;
    lblCoder: TLabel;
    FlatSpeedButton1: TFlatSpeedButton;
    procedure imgAMXXClick(Sender: TObject);
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.DFM}

procedure TfrmAbout.imgAMXXClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.amxmodx.org/', nil, nil, SW_SHOW);
end;

end.
