unit UnitfrmLoopGenerator;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TFlatRadioButtonUnit, TFlatEditUnit,
  TFlatButtonUnit;

type
  TfrmLoopGenerator = class(TForm)
    optWhile: TFlatRadioButton;
    pnlWhileCondition: TPanel;
    lblWhileCondition: TLabel;
    txtWhileCondition: TFlatEdit;
    optFor: TFlatRadioButton;
    pnlForLoop: TPanel;
    lblForVariable: TLabel;
    txtVariable: TFlatEdit;
    lblForCondition: TLabel;
    txtForCondition: TFlatEdit;
    Label1: TLabel;
    txtForAction: TFlatEdit;
    cmdGenerate: TFlatButton;
    procedure txtVariableKeyPress(Sender: TObject; var Key: Char);
    procedure txtWhileConditionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmdGenerateClick(Sender: TObject);
  end;

var
  frmLoopGenerator: TfrmLoopGenerator;

implementation

{$R *.DFM}

procedure TfrmLoopGenerator.txtVariableKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #32 then
    Key := #0;
end;

procedure TfrmLoopGenerator.txtWhileConditionKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then
    cmdGenerate.Click;
end;

procedure TfrmLoopGenerator.cmdGenerateClick(Sender: TObject);
begin
  if (txtWhileCondition.Text = '') and (optWhile.Checked) then
    MessageBox(Handle, 'You forgot to enter the while condition', 'Warning', MB_ICONWARNING)
  else if (optFor.Checked) then begin
    if (txtVariable.Text = '') or (txtForCondition.Text = '') or (txtForAction.Text = '') then
      MessageBox(Handle, 'You must fill out each field to generate a FOR-loop.', 'Warning', MB_ICONWARNING)
    else
      ModalResult := mrOk;
  end
  else
    ModalResult := mrOk;
end;

end.
