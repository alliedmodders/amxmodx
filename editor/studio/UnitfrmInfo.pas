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
    pnlGallery: TPanel;
    Label1: TLabel;
    imgGabeN: TImage;
    imgBurger: TImage;
    Label2: TLabel;
    Label3: TLabel;
    imgYams: TImage;
    procedure imgGabeNDblClick(Sender: TObject);
    procedure imgBurgerDblClick(Sender: TObject);
    procedure imgYamsDblClick(Sender: TObject);
  end;

var
  frmInfo: TfrmInfo;

implementation

{$R *.DFM}

procedure TfrmInfo.imgGabeNDblClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar('http://sniperbeamer.de/gallery/gaben.php'), nil, nil, SW_SHOW);
end;

procedure TfrmInfo.imgBurgerDblClick(Sender: TObject);
begin
  MessageBeep(MB_ICONQUESTION);
  if MessageBox(Handle, 'Do you want a BIG TASTY BURGER?', 'all-in-one-messagebox', MB_ICONQUESTION + MB_YESNO) = mrYes then begin
    ShellExecute(Handle, 'open', 'http://www.amxmodx.org/forums/viewtopic.php?t=14658&karma_up=8284', nil, nil, SW_SHOW);
    Sleep(5000);
    MessageBox(Handle, 'zomg you won''t get one. GabeN (tm) has already eaten each burger on THIS F**KIN'' BURGERLESS WORLD >_< :( Sorry.', 'all-in-one-messagebox', MB_ICONERROR);
  end;
end;

procedure TfrmInfo.imgYamsDblClick(Sender: TObject);
begin
  MessageBox(Handle, 'myam(s) myam(s), myam(s)...', 'all-in-one-messagebox', MB_ICONINFORMATION);
end;

end.
