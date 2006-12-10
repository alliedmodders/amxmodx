unit UnitfrmHudMsgGenerator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TBXDkPanels, SpTBXDkPanels, ExtCtrls, StdCtrls, SpTBXEditors,
  mbTBXEdit, mbTBXSpinEdit, mbTBXFloatSpinEdit, TB2Item, TBX, SpTBXItem,
  TB2Dock, TB2Toolbar, ImgList, TFlatEditUnit, TFlatButtonUnit,
  TFlatMemoUnit, Math, TFlatCheckBoxUnit;

type
  TfrmHudMsgGenerator = class(TForm)
    pnlPosition: TPanel;
    lblPosition: TLabel;
    lblXPos: TLabel;
    lblYPos: TLabel;
    pnlHudmessage: TPanel;
    imgHudmessage: TImage;
    pnlColor: TPanel;
    lblColor: TLabel;
    imgColor: TImage;
    txtXPos: TFlatEdit;
    txtYPos: TFlatEdit;
    cmdSelectColor: TFlatButton;
    cmdGenerate: TFlatButton;
    cmdCancel: TFlatButton;
    lblHudMsg: TLabel;
    pnlText: TPanel;
    txtText: TFlatEdit;
    txtTimeToShow: TFlatEdit;
    lblTimeToShow: TLabel;
    lblText: TLabel;
    lblOther: TLabel;
    chkXCenter: TFlatCheckBox;
    chkYCenter: TFlatCheckBox;
    procedure lblHudMsgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblHudMsgMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lblHudMsgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure txtXPosKeyPress(Sender: TObject; var Key: Char);
    procedure txtYPosKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure txtTextKeyPress(Sender: TObject; var Key: Char);
    procedure cmdSelectColorClick(Sender: TObject);
    procedure txtTextChange(Sender: TObject);
    procedure txtTimeToShowKeyPress(Sender: TObject; var Key: Char);
    procedure chkXCenterClick(Sender: TObject);
    procedure chkYCenterClick(Sender: TObject);
    procedure txtPosExit(Sender: TObject);
    procedure txtTimeToShowExit(Sender: TObject);
  private
    eDown: Boolean;
    eStartPos: TPoint;
    procedure PaintColor;
    procedure CenterX;
    procedure CenterY;
  public
    CurrColor: TColor;
  end;

var
  frmHudMsgGenerator: TfrmHudMsgGenerator;

implementation

uses UnitfrmSelectColor, UnitMainTools;

{$R *.dfm}

procedure TfrmHudMsgGenerator.lblHudMsgMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  eStartPos.X := X;
  eStartPos.Y := Y;
  eDown := True;
end;

procedure TfrmHudMsgGenerator.lblHudMsgMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if eDown then begin
    { X Pos }
    if not chkXCenter.Checked then begin
      lblHudMsg.Left := lblHudMsg.Left + (X - eStartPos.X);
      if lblHudMsg.Left < 0 then
        lblHudMsg.Left := 0
      else if lblHudMsg.Left > pnlHudmessage.Width then
        lblHudMsg.Left := pnlHudmessage.Width;
      txtXPos.Text := FloatToStrF(lblHudMsg.Left / pnlHudmessage.Width, ffFixed, -2, 2);
    end;
    
    { Y Pos }
    if not chkYCenter.Checked then begin
     lblHudMsg.Top := lblHudMsg.Top + (Y - eStartPos.Y);
      if lblHudMsg.Top < 0 then
        lblHudMsg.Top := 0
      else if lblHudMsg.Top > pnlHudmessage.Height then
        lblHudMsg.Top := pnlHudmessage.Height;
      txtYPos.Text := FloatToStrF(lblHudMsg.Top / pnlHudmessage.Height, ffFixed, -2, 2);
    end;
  end;
end;

procedure TfrmHudMsgGenerator.lblHudMsgMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  eDown := False;
end;

procedure TfrmHudMsgGenerator.PaintColor;
begin
  imgColor.Canvas.Pen.Color := $008396A0;
  imgColor.Canvas.Brush.Color := CurrColor;
  imgColor.Canvas.Rectangle(0, 0, 31, 31);
end;

procedure TfrmHudMsgGenerator.txtXPosKeyPress(Sender: TObject;
  var Key: Char);
var eXVal: Real;       
begin
  if Key = '.' then
    Key := ','
  else if Key = #13 then begin
    try
      eXVal := RoundTo(StrToFloat(txtXPos.Text), -2);
      txtXPos.Text := FloatToStr(eXVal);
      if Pos(',', txtXPos.Text) = 0 then
        txtXPos.Text := txtXPos.Text + ',0';
      lblHudMsg.Left := Round(eXVal * pnlHudmessage.Width);
      Key := #0;
    except
      txtXPos.Text := '0,00';
      lblHudMsg.Left := 0;
    end;
  end;
end;

procedure TfrmHudMsgGenerator.txtYPosKeyPress(Sender: TObject;
  var Key: Char);
var eYVal: Real;
begin
  if Key = '.' then
    Key := ','
  else if Key = #13 then begin
    try
      eYVal := RoundTo(StrToFloat(txtYPos.Text), -2);
      txtYPos.Text := FloatToStr(eYVal);
      if Pos(',', txtYPos.Text) = 0 then
        txtYPos.Text := txtYPos.Text + ',0';
      lblHudMsg.Top := Round(eYVal * pnlHudmessage.Height);
      Key := #0;
    except
      txtYPos.Text := '0,00';
      lblHudMsg.Left := 0;
    end;
  end;
end;

procedure TfrmHudMsgGenerator.FormShow(Sender: TObject);
begin
  CurrColor := clRed;
  PaintColor;
end;

procedure TfrmHudMsgGenerator.txtTextKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then begin
    txtText.SelText := '^n';
    Key := #0;
  end;
end;

procedure TfrmHudMsgGenerator.cmdSelectColorClick(Sender: TObject);
begin
  frmSelectColor.chkDefault1.Enabled := False;
  frmSelectColor.chkDefault2.Enabled := False;
  frmSelectColor.chkNone1.Enabled := False;
  frmSelectColor.chkNone2.Enabled := False;
  
  ShowColorDialog(CurrColor, imgColor);
  lblHudMsg.Font.Color := CurrColor;

  frmSelectColor.chkDefault1.Enabled := True;
  frmSelectColor.chkDefault2.Enabled := True;
  frmSelectColor.chkNone1.Enabled := True;
  frmSelectColor.chkNone2.Enabled := True;
end;

procedure TfrmHudMsgGenerator.txtTextChange(Sender: TObject);
begin
  if txtText.Text = '' then
    lblHudMsg.Caption := 'Custom Hudmessage'
  else
    lblHudMsg.Caption := StringReplace(txtText.Text, '^n', #13, [rfReplaceAll]);

  if chkXCenter.Checked then
    CenterX;
  if chkYCenter.Checked then
    CenterY;
end;

procedure TfrmHudMsgGenerator.txtTimeToShowKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = '.' then
    Key := ',';
end;

procedure TfrmHudMsgGenerator.chkXCenterClick(Sender: TObject);
var eChar: Char;
begin
  if chkXCenter.Checked then begin
    txtXPos.Text := '-1,0';
    CenterX;
  end
  else begin
    txtXPos.Text := '0,5';
    eChar := #13;
    txtXPosKeyPress(Sender, eChar);
  end;
    
  txtXPos.Enabled := not chkXCenter.Checked;
end;

procedure TfrmHudMsgGenerator.chkYCenterClick(Sender: TObject);
var eChar: Char;
begin
  if chkYCenter.Checked then begin
    txtYPos.Text := '-1,0';
    CenterY;
  end
  else begin
    txtYPos.Text := '0,5';
    eChar := #13;
    txtYPosKeyPress(Sender, eChar);
  end;
    
  txtYPos.Enabled := not chkYCenter.Checked;
end;

procedure TfrmHudMsgGenerator.CenterX;
begin
  lblHudMsg.Left := (pnlHudmessage.Width div 2) - (lblHudMsg.Width div 2);
end;

procedure TfrmHudMsgGenerator.CenterY;
begin
  lblHudMsg.Top := (pnlHudmessage.Height div 2) - (lblHudMsg.Height div 2);
end;

procedure TfrmHudMsgGenerator.txtPosExit(Sender: TObject);
var eChar: Char;
begin
  eChar := #13;
  if Sender = txtXPos then
    txtXPos.OnKeyPress(txtXPos, eChar)
  else
    txtYPos.OnKeyPress(txtXPos, eChar);
end;

procedure TfrmHudMsgGenerator.txtTimeToShowExit(Sender: TObject);
var eVal: Real;
begin
  try
    eVal := Round(StrToFloat(txtTimeToShow.Text));
    if eVal < 0 then
      txtTimeToShow.Text := '0,0';
  except
    txtTimeToShow.Text := '12,0';
  end;
end;

end.
