(**
 * TCorelButton v1.0
 * ---------------------------------------------------------------------------
 * A standard TButton which mimic the buttons used in the new Corel products
 * (e.g. WordPerfect Suite and Corel Photopaint).
 *
 * Copyright 1998, Peter Theill.  All Rights Reserved.
 *
 * This component can be freely used and distributed in commercial and private
 * environments, provied this notice is not modified in any way and there is
 * no charge for it other than nomial handling fees.  Contact me directly for
 * modifications to this agreement.
 * ----------------------------------------------------------------------------
 * Feel free to contact me if you have any questions, comments or suggestions
 * at peter@conquerware.dk
 *
 * The latest version will always be available on the web at:
 *   http://www.conquerware.dk/delphi/
 *
 * See CorelButton.txt for notes, known issues and revision history.
 * ----------------------------------------------------------------------------
 * Last modified: September 6, 1998
 * ----------------------------------------------------------------------------
 *)
unit CorelButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TCorelButton = class(TButton)
  private
    FCanvas: TCanvas;
    IsFocused: Boolean;

    FIsMouseOver: Boolean;
    FCanSelect: Boolean;

    procedure CNMeasureItem(var Msg: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure SetCanSelect(const Value: Boolean);

  protected
    procedure CreateParams(var Params: TCreateParams); override;

    procedure SetButtonStyle(ADefault: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
     property CanSelect: Boolean read FCanSelect write SetCanSelect default True;

  end;

procedure Register;

implementation

constructor TCorelButton.Create(AOwner: TComponent);
begin

  { Do standard stuff }
  inherited Create(AOwner);

  FCanvas := TCanvas.Create;

  FIsMouseOver := False;

  { Set width and height of button }
  Width := 75;
  Height := 23;

end;

destructor TCorelButton.Destroy;
begin
  FCanvas.Free;

  inherited Destroy;
end;

procedure TCorelButton.CMMouseEnter(var Message: TMessage);
begin

  if (not FIsMouseOver) then
    Invalidate;

end;

procedure TCorelButton.CMMouseLeave(var Message: TMessage);
begin

  if (FIsMouseOver) then
    Invalidate;

end;

procedure TCorelButton.CNMeasureItem(var Msg: TWMMeasureItem);
begin
  with Msg.MeasureItemStruct^ do begin
    itemWidth := Width;
    itemHeight := Height;
  end;
  Msg.Result := 1;
end;

procedure TCorelButton.CNDrawItem(var Msg: TWMDrawItem);
begin
  DrawItem(Msg.DrawItemStruct^);
  Msg.Result := 1;
end;

procedure TCorelButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown, IsDefault: Boolean;
  R: TRect;
//  Flags: Longint;
  CursorPos: TPoint;
  BtnRect: TRect;

begin

  FCanvas.Handle := DrawItemStruct.hDC;
  try
    R := ClientRect;

    with DrawItemStruct do begin
      IsDown := (itemState and ODS_SELECTED) <> 0;
      IsDefault := (itemState and ODS_FOCUS) <> 0;
    end;

    GetCursorPos(CursorPos);
    BtnRect.TopLeft := Parent.ClientToScreen(Point(Left, Top));
    BtnRect.BottomRight := Parent.ClientToScreen(Point(Left + Width,
       Top + Height));
    FIsMouseOver := PtInRect(BtnRect, CursorPos);

//    Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
//    if IsDown then Flags := Flags or DFCS_PUSHED;
//    if DrawItemStruct.itemState and ODS_DISABLED <> 0 then
//      Flags := Flags or DFCS_INACTIVE;

    FCanvas.Brush.Color := clBtnFace;

    if {(csDesigning in ComponentState) OR} (IsDefault) or (FCanSelect) and (IsFocused) then begin

      FCanvas.Pen.Color := clWindowText;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Style := bsSolid;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      InflateRect(R, -1, -1);

    end;

    FCanvas.FillRect(R);

    if (csDesigning in ComponentState) OR (FIsMouseOver) then begin

      FCanvas.Pen.Color := clWindowText;
      FCanvas.MoveTo(R.Right-1, R.Top);
      FCanvas.LineTo(R.Right-1, R.Bottom-1);
      FCanvas.LineTo(R.Left-1, R.Bottom-1);

      FCanvas.Pen.Color := clBtnHighlight;
      FCanvas.MoveTo(R.Left, R.Bottom-2);
      FCanvas.LineTo(R.Left, R.Top);
      FCanvas.LineTo(R.Right-1, R.Top);

      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.MoveTo(R.Right-2, R.Top+1);
      FCanvas.LineTo(R.Right-2, R.Bottom-2);
      FCanvas.LineTo(R.Left, R.Bottom-2);

    end else begin

      FCanvas.Pen.Color := clBtnHighlight;
      FCanvas.Pen.Width := 1;
      FCanvas.MoveTo(R.Left, R.Bottom-2);
      FCanvas.LineTo(R.Left, R.Top);
      FCanvas.LineTo(R.Right-1, R.Top);

      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.LineTo(R.Right-1, R.Bottom-1);
      FCanvas.LineTo(R.Left-1, R.Bottom-1);

    end;

    if {(csDesigning in ComponentState) OR} (IsDown) then begin

      FCanvas.Brush.Color := clBtnFace;
      FCanvas.FillRect(R);

      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.Pen.Width := 1;
      FCanvas.MoveTo(R.Left, R.Bottom-2);
      FCanvas.LineTo(R.Left, R.Top);
      FCanvas.LineTo(R.Right-1, R.Top);

      FCanvas.Pen.Color := clBtnHighlight;
      FCanvas.LineTo(R.Right-1, R.Bottom-1);
      FCanvas.LineTo(R.Left-1, R.Bottom-1);

    end;

    if {(csDesigning in ComponentState) OR} (IsFocused) and (IsDefault) and (FCanSelect) then begin

      InflateRect(R, -3, -3);
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);

    end;

    if (IsDown) then
      OffsetRect(R, 1, 1);

    { Draw caption of button }
    with FCanvas do begin
      FCanvas.Font := Self.Font;
      Brush.Style := bsClear;
      Font.Color := clBtnText;
      if Enabled or ((DrawItemStruct.itemState and ODS_DISABLED) = 0) then begin
        DrawText(Handle, PChar(Caption), Length(Caption), R, DT_CENTER or
         DT_VCENTER or DT_SINGLELINE);
      end else begin
        OffsetRect(R, 1, 1);
        Font.Color := clBtnHighlight;
        DrawText(Handle, PChar(Caption), Length(Caption), R, DT_CENTER or
         DT_VCENTER or DT_SINGLELINE);
        OffsetRect(R, -1, -1);
        Font.Color := clBtnShadow;
        DrawText(Handle, PChar(Caption), Length(Caption), R, DT_CENTER or
         DT_VCENTER or DT_SINGLELINE);
      end;
    end;

  finally
    FCanvas.Handle := 0;
  end;
end;

procedure TCorelButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCorelButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCorelButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;

procedure TCorelButton.SetButtonStyle(ADefault: Boolean);
begin

  if ADefault <> IsFocused then begin
    IsFocused := ADefault;
    Refresh;
  end;
end;

procedure TCorelButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style OR BS_OWNERDRAW;
end;


procedure Register;
begin
  RegisterComponents('Standard', [TCorelButton]);
end;

procedure TCorelButton.SetCanSelect(const Value: Boolean);
begin
  FCanSelect := Value;
  Repaint;
end;

end.
