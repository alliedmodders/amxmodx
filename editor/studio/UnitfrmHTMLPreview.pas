unit UnitfrmHTMLPreview;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, OleCtrls, SHDocVw, ActiveX;

type
  TfrmHTMLPreview = class(TForm)
    wbPreview: TWebBrowser;
    tmrLoad: TTimer;
    procedure tmrLoadTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure LoadCode(eHTML: String);
  end;

var
  frmHTMLPreview: TfrmHTMLPreview;

implementation

uses UnitMainTools, UnitfrmMain;

{$R *.DFM}

{ TfrmHTMLPreview }

procedure TfrmHTMLPreview.LoadCode(eHTML: String);
var sl: TStringList;
    ms: TMemoryStream;
begin
  wbPreview.Navigate('about:blank');
  while wbPreview.ReadyState < READYSTATE_INTERACTIVE do
   Application.ProcessMessages;

  if Assigned(wbPreview.Document) then
  begin
    sl := TStringList.Create;
    try
      ms := TMemoryStream.Create;
      try
        sl.Text := eHTML;
        sl.SaveToStream(ms);
        ms.Seek(0, 0);
        (wbPreview.Document as IPersistStreamInit).Load(TStreamAdapter.Create(ms));
      finally
        ms.Free;
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure TfrmHTMLPreview.tmrLoadTimer(Sender: TObject);
begin
  if not Started then exit;
  
  if (Visible) and (not Focused) then begin
    if (LowerCase(ExtractFileExt(ActiveDoc.FileName)) = '.htm') or (LowerCase(ExtractFileExt(ActiveDoc.FileName)) = '.html') then
      LoadCode(frmMain.sciEditor.Lines.Text);
  end;
end;

procedure TfrmHTMLPreview.FormShow(Sender: TObject);
begin
  Left := frmMain.Left + frmMain.Width - Width - 50;
  Top := frmMain.Top + 30;
end;

end.
