unit UnitfrmSelectColor; // adapted from OfficeMoreColorsDialog

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ComCtrls, HexaColorPicker, HSLColorPicker, StdCtrls, ExtCtrls, RGBHSLUtils,
  mbXPSpinEdit, mbColorPreview, mbXPSizeGrip, StrUtils, HTMLColors;

type
  TfrmSelectColor = class(TForm)
    Pages: TPageControl;
    Standard: TTabSheet;
    Custom: TTabSheet;
    Hexa: THexaColorPicker;
    HSL: THSLColorPicker;
    lblColor2: TLabel;
    lblColor1: TLabel;
    lblColorModel: TLabel;
    ColorModel: TComboBox;
    LRed: TLabel;
    LGreen: TLabel;
    LBlue: TLabel;
    lblNew: TLabel;
    lblCurrent: TLabel;
    cmdOK: TButton;
    cmdCancel: TButton;
    ERed: TmbXPSpinEdit;
    EGreen: TmbXPSpinEdit;
    EBlue: TmbXPSpinEdit;
    NewSwatch: TmbColorPreview;
    OldSwatch: TmbColorPreview;
    bvlSpace1: TBevel;
    bvlSpace2: TBevel;
    chkDefault1: TCheckBox;
    chkNone1: TCheckBox;
    chkNone2: TCheckBox;
    chkDefault2: TCheckBox;
    procedure ColorModelChange(Sender: TObject);
    procedure HSLChange(Sender: TObject);
    procedure ERedChange(Sender: TObject);
    procedure EGreenChange(Sender: TObject);
    procedure EBlueChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HexaChange(Sender: TObject);
    procedure NewSwatchColorChange(Sender: TObject);
    procedure OldSwatchColorChange(Sender: TObject);
    function GetHint(c: TColor): string;
    procedure SetAllToSel(c: TColor);
    procedure PagesChange(Sender: TObject);
    procedure checkMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  end;

var
  frmSelectColor: TfrmSelectColor;
  h, s, l: integer;

implementation

{$R *.dfm}

procedure TfrmSelectColor.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_CAPTION or WS_SYSMENU;
  Params.ExStyle := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
end;

procedure TfrmSelectColor.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Self.Handle, WM_SETICON, 1, 0);
end;

procedure TfrmSelectColor.ColorModelChange(Sender: TObject);
begin
  case ColorModel.ItemIndex of
    0:
      begin
        LRed.Caption := '&Red:';
        LGreen.Caption := '&Green:';
        LBlue.Caption := '&Blue:';
        ERed.MaxValue := 255;
        EGreen.MaxValue := 255;
        EBlue.MaxValue := 255;
        ERed.Value := GetRValue(NewSwatch.Color);
        EGreen.Value := GetGValue(NewSwatch.Color);
        EBlue.Value := GetBValue(NewSwatch.Color);
      end;
    1:
      begin
        LRed.Caption := 'H&ue:';
        LGreen.Caption := '&Sat:';
        LBlue.Caption := '&Lum:';
        ERed.MaxValue := 238;
        EGreen.MaxValue := 240;
        EBlue.MaxValue := 240;
        RGBtoHSLRange(NewSwatch.Color, h, s, l);
        ERed.Value := h;
        EGreen.Value := s;
        EBlue.Value := l;
      end;
  end;
end;

procedure TfrmSelectColor.HSLChange(Sender: TObject);
begin
  if HSL.Manual then
    case ColorModel.ItemIndex of
      0:
        begin
          ERed.Value := HSL.RValue;
          EGreen.Value := HSL.GValue;
          EBlue.Value := HSL.BValue;
          NewSwatch.Color := HSL.SelectedColor;
        end;
      1:
        begin
          ERed.Value := HSL.HValue;
          EGreen.Value := HSL.SValue;
          EBlue.Value := HSL.LValue;
          NewSwatch.Color := HSL.SelectedColor;
        end;
    end;
end;

procedure TfrmSelectColor.ERedChange(Sender: TObject);
begin
  if (ERed.Text <> '') and ERed.Focused then
    case ColorModel.ItemIndex of
      0:
        begin
          HSL.RValue := ERed.Value;
          NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
        end;
      1:
        begin
          HSL.HValue := ERed.Value;
          NewSwatch.Color := HSLRangeToRGB(ERed.Value, EGreen.Value, EBlue.Value);
        end;
    end;
end;

procedure TfrmSelectColor.EGreenChange(Sender: TObject);
begin
  if (EGreen.Text <> '') and EGreen.Focused then
    case ColorModel.ItemIndex of
      0:
        begin
          HSL.GValue := EGreen.Value;
          NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
        end;
      1:
        begin
          HSL.SValue := EGreen.Value;
          NewSwatch.Color := HSLRangeToRGB(ERed.Value, EGreen.Value, EBlue.Value);
        end;
    end;
end;

procedure TfrmSelectColor.EBlueChange(Sender: TObject);
begin
  if (EBlue.Text <> '') and EBlue.Focused then
    case ColorModel.ItemIndex of
      0:
        begin
          HSL.BValue := EBlue.Value;
          NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
        end;
      1:
        begin
          HSL.LValue := EBlue.Value;
          NewSwatch.Color := HSLRangeToRGB(ERed.Value, EGreen.Value, EBlue.Value);
        end;
    end;
end;

procedure TfrmSelectColor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN: ModalResult := mrOK;
    VK_ESCAPE: ModalResult := mrCancel;
  end;
end;

procedure TfrmSelectColor.HexaChange(Sender: TObject);
begin
  NewSwatch.Color := Hexa.SelectedColor;
end;

function TfrmSelectColor.GetHint(c: TColor): string;
begin
  Result := Format('RGB(%u, %u, %u)'#13'Hex: %s', [GetRValue(c), GetGValue(c), GetBValue(c), ColorToHex(c)]);
end;

procedure TfrmSelectColor.NewSwatchColorChange(Sender: TObject);
begin
  NewSwatch.Hint := GetHint(NewSwatch.Color);
end;

procedure TfrmSelectColor.OldSwatchColorChange(Sender: TObject);
begin
  OldSwatch.Hint := GetHint(OldSwatch.Color);
  SetAllToSel(OldSwatch.Color);
end;

procedure TfrmSelectColor.SetAllToSel(c: TColor);
begin
  case Pages.ActivePageIndex of
  // Standard Page
    0: Hexa.SelectedColor := c;
  // Custom Page
    1:
      begin
        HSL.SelectedColor := c;
        case ColorModel.ItemIndex of
          0:
            begin
              ERed.Value := GetRValue(c);
              EGreen.Value := GetGValue(c);
              EBlue.Value := GetBValue(c);
            end;
          1:
            begin
              RGBtoHSLRange(c, h, s, l);
              ERed.Value := h;
              EGreen.Value := s;
              EBlue.Value := l;
            end;
        end;
      end;
  end;
  NewSwatch.Color := c;
end;

procedure TfrmSelectColor.PagesChange(Sender: TObject);
begin
  SetAllToSel(NewSwatch.Color);
end;

procedure TfrmSelectColor.checkMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Sender = chkDefault1) or (Sender = chkDefault2) then begin
    chkDefault1.Checked := (Sender as TCheckBox).Checked;
    chkDefault2.Checked := (Sender as TCheckBox).Checked;
    chkNone1.Checked := False;
    chkNone2.Checked := False;
    newSwatch.Color := clDefault;
  end
  else if (Sender = chkNone1) or (Sender = chkNone2) then begin
    chkNone1.Checked := (Sender as TCheckBox).Checked;
    chkNone2.Checked := (Sender as TCheckBox).Checked;
    chkDefault1.Checked := False;
    chkDefault2.Checked := False;
    newSwatch.Color := clNone;
  end
  else begin
    if Standard.Visible then
      newSwatch.Color := Hexa.Color
    else
      newSwatch.Color := HSL.Color;
  end;

  lblColor1.Enabled := not (Sender as TCheckBox).Checked;
  bvlSpace1.Enabled := not (Sender as TCheckBox).Checked;
  Hexa.Enabled := not (Sender as TCheckBox).Checked;
  lblColor2.Enabled := not (Sender as TCheckBox).Checked;
  lblColorModel.Enabled := not (Sender as TCheckBox).Checked;
  LRed.Enabled := not (Sender as TCheckBox).Checked;
  LGreen.Enabled := not (Sender as TCheckBox).Checked;
  LBlue.Enabled := not (Sender as TCheckBox).Checked;
  bvlSpace2.Enabled := not (Sender as TCheckBox).Checked;
  HSL.Enabled := not (Sender as TCheckBox).Checked;
  ColorModel.Enabled := not (Sender as TCheckBox).Checked;
  ERed.Enabled := not (Sender as TCheckBox).Checked;
  EGreen.Enabled := not (Sender as TCheckBox).Checked;
  EBlue.Enabled := not (Sender as TCheckBox).Checked;
end;

end.

