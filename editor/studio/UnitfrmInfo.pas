unit UnitfrmInfo;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TBXDkPanels, SpTBXDkPanels, JvExControls,
  JvComponent, JvScrollText, ShellAPI, IdHTTP, jpeg, Dialogs;

type
  TfrmInfo = class(TForm)
    lblInfo3: TLabel;
    lblInfo2: TLabel;
    lblInfo1: TLabel;
    imgAMXXLarge: TImage;
    cmdClose: TSpTBXButton;
    lblCHelp: TLabel;
    lblScintilla1: TLabel;
    lblScintilla2: TLabel;
    pnlImages: TPanel;
    imgDoom4: TImage;
    imgHampster: TImage;
    imgYams: TImage;
    procedure imgHampsterClick(Sender: TObject);
    procedure imgDoom4Click(Sender: TObject);
    procedure imgYamsClick(Sender: TObject);
  end;

var
  frmInfo: TfrmInfo;

implementation

{$R *.DFM}

procedure TfrmInfo.imgHampsterClick(Sender: TObject);
begin
  MessageBox(Handle, 'Hampster!', 'Your computer! wtf', MB_ICONERROR);
  MessageBox(Handle, 'ZOMG YES it''s a hampster!', 'zomg', MB_ICONINFORMATION);
  MessageBox(Handle, 'hampster hampster hampster hampster hampster hampster hampster hampster hampster hampster hampster hampster hampster hampster hampster hampster!', 'ham ham hampster', MB_ICONWARNING);
end;

procedure TfrmInfo.imgDoom4Click(Sender: TObject);
begin
  MessageBox(Handle, 'gaben', 'doom', MB_ICONWARNING);
  MessageBox(Handle, 'gaben gaben gaben gaben gaben gaben!', 'doom', MB_ICONERROR);
  MessageBox(Handle, 'gab gab gab gab gab GABEN GABEN GABEN GAAGAGAGABEN! gabenygabgab gaben da gaben0r gabbagaben >_<', 'doom', MB_ICONINFORMATION);  
end;

procedure TfrmInfo.imgYamsClick(Sender: TObject);
begin
  MessageBox(Handle, 'mmm.. yams', 'personal farmer', MB_ICONINFORMATION);
  MessageBox(Handle, 'yam yam yam.. oh a yam! yaaaam yaaaam yayayammm yaaamamamamam and some yaaams', 'gran farmer', MB_ICONERROR);
  MessageBox(Handle, 'yams? deadly yams? >:(', 'not a farmer', MB_ICONQUESTION);
end;

end.
