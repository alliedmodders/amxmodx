unit UnitfrmOptions;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, ComCtrls, TFlatEditUnit,
  TFlatSpeedButtonUnit, TFlatCheckBoxUnit, TFlatComboBoxUnit, FileCtrl,
  TFlatTabControlUnit, TFlatRadioButtonUnit, Dialogs;

type
  TfrmSettings = class(TForm)
    lblSettings: TLabel;
    cmdClose: TFlatSpeedButton;
    odHalfLife: TOpenDialog;
    ftcPages: TFlatTabControl;
    nbkPages: TNotebook;
    pnlHighlighter: TPanel;
    lblComments: TLabel;
    lblDirectives: TLabel;
    lblOperators: TLabel;
    lblStrings: TLabel;
    lblKeywords: TLabel;
    lblActiveLine: TLabel;
    cboComments: TColorBox;
    cboDirectives: TColorBox;
    cboOperators: TColorBox;
    cboStrings: TColorBox;
    cboKeywords: TColorBox;
    cboActiveLine: TColorBox;
    pnlGeneralSettings: TPanel;
    lblAMXX: TLabel;
    cmdBrowseAMXX: TFlatSpeedButton;
    cmdBrowseHalfLife: TFlatSpeedButton;
    lblHalfLife: TLabel;
    cmdBrowseSave: TFlatSpeedButton;
    lblSave: TLabel;
    txtAMXXPath: TFlatEdit;
    txtHalfLife: TFlatEdit;
    txtSave: TFlatEdit;
    pnlGeneral: TPanel;
    lblFoldingStyle: TLabel;
    chkAutoComplete: TFlatCheckBox;
    chkHighlighting: TFlatCheckBox;
    cboFoldingStyle: TFlatComboBox;
    chkHints: TFlatCheckBox;
    chkAutoIndent: TFlatCheckBox;
    chkBrackets: TFlatCheckBox;
    lblHighlighter: TLabel;
    lblEditor: TLabel;
    lblDirectories: TLabel;
    lblCompilerSettings: TLabel;
    lblFTP: TLabel;
    lblMisc: TLabel;
    lblView: TLabel;
    pnlCharCompleter: TPanel;
    chkAutoCloseBrackets: TFlatCheckBox;
    chkAutoCloseQuotes: TFlatCheckBox;
    pnlCompilerSettings: TPanel;
    optWindow: TFlatRadioButton;
    optList: TFlatRadioButton;
    pnlFTP: TPanel;
    lblHost: TLabel;
    lblPort: TLabel;
    lblUser: TLabel;
    lblPassword: TLabel;
    lblStandardDir: TLabel;
    cmdCheckFTP: TFlatSpeedButton;
    txtHost: TFlatEdit;
    txtPort: TFlatEdit;
    txtUser: TFlatEdit;
    txtPassword: TFlatEdit;
    txtStandardDir: TFlatEdit;
    pnlMisc: TPanel;
    chkReload: TFlatCheckBox;
    chkAutoAddPlugins: TFlatCheckBox;
    pnlView: TPanel;
    lblCodeExplorer: TLabel;
    cboCodeExplorer: TFlatComboBox;
    chkStatusbar: TFlatCheckBox;
    lblCharCompleter: TLabel;
    procedure cmdBrowseAMXXClick(Sender: TObject);
    procedure cmdBrowseHalfLifeClick(Sender: TObject);
    procedure cmdBrowseSaveClick(Sender: TObject);
    procedure chkAutoAddPluginsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ftcPagesTabChanged(Sender: TObject);
    procedure cmdCheckFTPClick(Sender: TObject);
  end;

var
  frmSettings: TfrmSettings;

implementation

uses UnitfrmMain;

{$R *.DFM}

procedure TfrmSettings.cmdBrowseAMXXClick(Sender: TObject);
var eDir: String;
begin
  if SelectDirectory('Please select your AMXX directory:', 'C:', eDir) then
    txtAMXXPath.Text := eDir;
end;

procedure TfrmSettings.cmdBrowseHalfLifeClick(Sender: TObject);
begin
  if odHalfLife.Execute then
    txtHalfLife.Text := odHalfLife.FileName;
end;

procedure TfrmSettings.cmdBrowseSaveClick(Sender: TObject);
var eDir: String;
begin
  if SelectDirectory('Please select the directory where your files shall be saved:', 'C:', eDir) then
    txtSave.Text := eDir;
end;

procedure TfrmSettings.chkAutoAddPluginsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (chkAutoAddPlugins.Checked) and (Button = mbLeft) then
    MessageBox(Handle, 'Notice: This function works only with plugins which are saved in $AMXXDIR$\scripting.', 'Information', MB_ICONINFORMATION); 
end;

procedure TfrmSettings.ftcPagesTabChanged(Sender: TObject);
begin
  nbkPages.PageIndex := ftcPages.ActiveTab;
end;

procedure TfrmSettings.cmdCheckFTPClick(Sender: TObject);
begin
  try
    with frmMain.IdFTP do begin
      Host := txtHost.Text;
      Port := StrToInt(txtPort.Text);
      Username := txtUser.Text;
      Password := txtPassword.Text;
      try
        Connect;
        ChangeDir(txtStandardDir.Text);
        MessageBox(Handle, 'Test successfully done!', 'Information', MB_ICONINFORMATION)
      except
        on E: Exception do
          MessageBox(Handle, PChar(E.Message), 'Error', MB_ICONWARNING);
      end;
      if Connected then
        Disconnect;
    end;
  except
    MessageBox(Handle, 'Invalid FTP port.', 'Warning', MB_ICONWARNING);
  end;
end;

end.
