unit UnitfrmDebug;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms;

type
  TfrmDebug = class(TForm)
    cmdMore: TButton;
    pnlCompile: TPanel;
    lblFile: TStaticText;
    lblStatus: TStaticText;
    lblHints: TStaticText;
    lblWarnings: TStaticText;
    lblErrors: TStaticText;
    cmdCancelOkay: TButton;
    bvlDelimeter: TBevel;
    lstOutput: TListBox;
    procedure cmdMoreClick(Sender: TObject);
    procedure cmdCancelOkayClick(Sender: TObject);
    procedure lstOutputDblClick(Sender: TObject);
  private
    FCompiling: Boolean;
    procedure SetCompiling(const Value: Boolean);
  public
    property Compiling: Boolean read FCompiling write SetCompiling;
  end;

var
  frmDebug: TfrmDebug;

const DEFAULT_HEIGHT = 165;
      MAX_HEIGHT = 277;

implementation

uses UnitfrmMain, UnitFunc, UnitfrmOptions;

{$R *.DFM}

procedure TfrmDebug.cmdMoreClick(Sender: TObject);
begin
  if Height = DEFAULT_HEIGHT then begin
    Height := MAX_HEIGHT;
    cmdMore.Caption := '<< Compiler Output';
  end
  else begin
    Height := DEFAULT_HEIGHT;
    cmdMore.Caption := 'Compiler Output >>';
  end;
end;

procedure TfrmDebug.SetCompiling(const Value: Boolean);
begin
  FCompiling := Value;
  if Value then
    cmdCancelOkay.Caption := 'Cancel'
  else
    cmdCancelOkay.Caption := 'Close';
end;

procedure TfrmDebug.cmdCancelOkayClick(Sender: TObject);
var i: integer;
begin
  if Compiling then begin
    i := GetProcId('amxxsc.exe');
    if i <> -1 then
      KillIt(i);
    Compiling := False;
  end
  else begin
    Hide;
    frmMain.Show;
  end;
end;

procedure TfrmDebug.lstOutputDblClick(Sender: TObject);
begin
  if lstOutput.ItemIndex <> -1 then begin
    if Pos('Warning', lstOutput.Items[lstOutput.ItemIndex]) = 1 then
      MessageBox(Handle, PChar(lstOutput.Items[lstOutput.ItemIndex]), 'Warning', MB_ICONWARNING)
    else if Pos('Error', lstOutput.Items[lstOutput.ItemIndex]) = 1 then
      MessageBox(Handle, PChar(lstOutput.Items[lstOutput.ItemIndex]), 'Error', MB_ICONERROR)
    else
      MessageBox(Handle, PChar(lstOutput.Items[lstOutput.ItemIndex]), 'Information', MB_ICONINFORMATION)
  end;
end;

end.
