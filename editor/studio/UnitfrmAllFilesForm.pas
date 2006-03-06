unit UnitfrmAllFilesForm;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TBXDkPanels, SpTBXDkPanels, mbTBXListBox,
  mbTBXCheckListBox, SpTBXControls;

type
  TfrmAllFilesForm = class(TForm)
    lblCaption: TLabel;
    lstFiles: TmbTBXCheckListBox;
    cmdOK: TSpTBXButton;
    cmdCancel: TSpTBXButton;
    procedure FormShow(Sender: TObject);
  end;

var
  frmAllFilesForm: TfrmAllFilesForm;

implementation

{$R *.DFM}

procedure TfrmAllFilesForm.FormShow(Sender: TObject);
begin
  lstFiles.SetFocus;
end;

end.
