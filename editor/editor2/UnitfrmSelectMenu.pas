unit UnitfrmSelectMenu;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, CorelButton, TFlatListBoxUnit;

type
  TfrmSelectMenu = class(TForm)
    cmdOK: TCorelButton;
    cmdCancel: TCorelButton;
    lblSelect: TLabel;
    lstMenu: TFlatListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    eMenuStr: TStringList;
    eLines: array of Integer;
    function GetItemIndex: Integer;
  end;

var
  frmSelectMenu: TfrmSelectMenu;

implementation

uses UnitfrmMenuMaker, UnitfrmMain, UnitAddMenu, UnitFunc, UnitTextAnalyze;

{$R *.DFM}

procedure TfrmSelectMenu.FormCreate(Sender: TObject);
begin
  eMenuStr := TStringList.Create;
end;

procedure TfrmSelectMenu.FormDestroy(Sender: TObject);
begin
  eMenuStr.Free;
end;

procedure TfrmSelectMenu.FormShow(Sender: TObject);
var i: integer;
    eTemp: String;
begin
  eMenuStr.Clear;
  lstMenu.Items.Clear;
  SetLength(eLines, 0);
  for i := 0 to frmMain.sciEditor.Lines.Count -1 do begin
    if Pos('show_menu', Trim(LowerCase(frmMain.sciEditor.Lines[i]))) = 1 then begin
      SetLength(eLines, eMenuStr.Count +1);
      eLines[eMenuStr.Count] := i;
      eTemp := frmMain.sciEditor.Lines[i];
      if CountChars(frmMain.sciEditor.Lines[i], '"') >= 4 then begin
        eMenuStr.Add(Between(eTemp, '"', '"'));
        while CountChars(eTemp, '"') > 2 do
          Delete(eTemp, 1, 1);
        lstMenu.Items.Add(Between(eTemp, '"', '"')); 
      end
      else begin
        eMenuStr.Add(Between(eTemp, '"', '"'));
        lstMenu.Items.Add(Format('Unknown Menu on line %s', [IntToStr(i)]));
      end;
    end;
  end;
  cmdOK.Enabled := lstMenu.Items.Count <> 0;
end;

function TfrmSelectMenu.GetItemIndex: Integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to lstMenu.Items.Count -1 do begin
    if lstMenu.Selected[i] then
      Result := i;
  end;
end;

end.
