unit UnitfrmProxy;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TFlatButtonUnit, TFlatComboBoxUnit,
  TFlatEditUnit;

type
  TfrmProxy = class(TForm)
    cmdClose: TFlatButton;
    lblProxy: TLabel;
    txtHost: TFlatEdit;
    cboProxy: TFlatComboBox;
    lblHost: TLabel;
    txtPort: TFlatEdit;
    lblPort: TLabel;
    lblUsername: TLabel;
    txtUsername: TFlatEdit;
    txtPassword: TFlatEdit;
    lblPassword: TLabel;
    procedure cboProxyChange(Sender: TObject);
    procedure txtPortChange(Sender: TObject);
  public
    procedure EnableControls(Enable: Boolean);
  end;

var
  frmProxy: TfrmProxy;

implementation

{$R *.DFM}

{ TfrmProxy }

procedure TfrmProxy.EnableControls(Enable: Boolean);
begin
  lblHost.Enabled := Enable;
  lblPassword.Enabled := Enable;
  lblPort.Enabled := Enable;
  lblUsername.Enabled := Enable;
  txtHost.Enabled := Enable;
  txtPassword.Enabled := Enable;
  txtPort.Enabled := Enable;
  txtUsername.Enabled := Enable;
end;

procedure TfrmProxy.cboProxyChange(Sender: TObject);
begin
  EnableControls(cboProxy.ItemIndex <> 0); // 0 = None
end;

procedure TfrmProxy.txtPortChange(Sender: TObject);
var i: integer;
begin
  if txtPort.Text = '' then
    txtPort.Text := '8080'
  else begin
    // check if value is numeric...
    for i := Length(txtPort.Text) downto 1 do begin
      if Pos(txtPort.Text[i], '0123456789') = 0 then begin
        txtPort.Text := '8080';
        txtPort.SelStart := 4;
        exit;
      end;
    end;
  end;
end;

end.
