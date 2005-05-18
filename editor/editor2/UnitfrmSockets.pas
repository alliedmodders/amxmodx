unit UnitfrmSockets;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, TFlatEditUnit,
  TFlatRadioButtonUnit, TFlatButtonUnit, IdUDPBase, IdUDPClient,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, UnitReadThread,
  ActnList;

type
  TfrmSocketTerminal = class(TForm)
    pnlSettings: TPanel;
    rtfEnter: TRichEdit;
    rtfReceived: TRichEdit;
    lblStatusCaption: TLabel;
    lblStatus: TLabel;
    lblSettings: TLabel;
    pnlSettings2: TPanel;
    lblHost: TLabel;
    txtHost: TFlatEdit;
    txtPort: TFlatEdit;
    lblPort: TLabel;
    optUDP: TFlatRadioButton;
    optTCP: TFlatRadioButton;
    cmdConnect: TFlatButton;
    IdTCPClient: TIdTCPClient;
    IdUDPClient: TIdUDPClient;
    alCopyPaste: TActionList;
    acCopy: TAction;
    acPaste: TAction;
    acUndo: TAction;
    acSelectAll: TAction;
    procedure txtPortChange(Sender: TObject);
    procedure cmdConnectClick(Sender: TObject);
    procedure optTCPClick(Sender: TObject);
    procedure IdTCPClientConnected(Sender: TObject);
    procedure IdTCPClientDisconnected(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rtfEnterKeyPress(Sender: TObject; var Key: Char);
    procedure IdUDPClientStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: String);
    procedure acCopyExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
  private
    ReadThread: TReadThread;
  public
    procedure Append(eText: String; eColor: TColor = clBlack);
    procedure SetStatus(eStatus: String; eColor: TColor);
    procedure OnRead(eRead: String);
    procedure EnableControls(eValue: Boolean);
  end;

var
  frmSocketTerminal: TfrmSocketTerminal;

implementation

{$R *.dfm}

procedure TfrmSocketTerminal.Append(eText: String; eColor: TColor);
begin
  eText := Format('[%s] %s', [TimeToStr(Time), eText]);
  rtfReceived.SelStart := Length(rtfReceived.Lines.Text);
  rtfReceived.SelAttributes.Color := eColor;
  rtfReceived.SelText := eText + #13#10;
  rtfReceived.Perform(WM_VSCROLL, SB_BOTTOM, 0); 
end;

procedure TfrmSocketTerminal.OnRead(eRead: String);
begin
  Append(eRead, clWindowText);
end;

procedure TfrmSocketTerminal.SetStatus(eStatus: String; eColor: TColor);
begin
  lblStatus.Caption := eStatus;
  lblStatus.Font.Color := eColor;
end;

procedure TfrmSocketTerminal.txtPortChange(Sender: TObject);
begin
  try
    StrToInt(txtPort.Text);
  except
    txtPort.Text := '1';
  end;
end;

procedure TfrmSocketTerminal.cmdConnectClick(Sender: TObject);
begin
  if Tag = 0 then begin
    if optTCP.Checked then begin
      IdTCPClient.Host := txtHost.Text;
      IdTCPClient.Port := StrToInt(txtPort.Text);
      EnableControls(False);
      Append('Connecting to ' + txtHost.Text + ':' + txtPort.Text + '...', clHighlight);
      try
        IdTCPClient.Connect;
        ReadThread := TReadThread.Create(True);
        ReadThread.ReadTCP := True;
        ReadThread.Resume;
      except
        on E: Exception do begin
          MessageBox(Handle, PChar('Couldn''t connect to server:' + #13 + E.Message), 'Warning', MB_ICONWARNING);
          EnableControls(True);
        end;
      end;
    end
    else begin
      IdUDPClient.Host := txtHost.Text;
      IdUDPClient.Port := StrToInt(txtPort.Text);
      EnableControls(False);
      try
        IdUDPClient.Active := True;
        ReadThread := TReadThread.Create(True);
        ReadThread.ReadTCP := False;
        ReadThread.Resume;
        SetStatus('socket active', clGreen);
        Append('Opened socket to ' + txtHost.Text + ':' + txtPort.Text + '!', clGreen);
      except
        on E: Exception do begin
          MessageBox(Handle, PChar('Couldn''t activate socket:' + #13 + E.Message), 'Warning', MB_ICONWARNING);
          EnableControls(True);
        end;
      end;
    end;
  end
  else begin
    if optTCP.Checked then begin
      Screen.Cursor := crHourGlass;
      IdTCPClient.Disconnect;
      ReadThread.Terminate;
      while Tag <> 0 do begin
        Sleep(5);
        Application.ProcessMessages;
      end;
      Screen.Cursor := crDefault;
    end
    else begin
      Screen.Cursor := crHourGlass;
      IdUDPClient.Active := False;
      ReadThread.Terminate;
      EnableControls(True);
      SetStatus('socket inactive', clRed);
      Append('Closed socket to ' + txtHost.Text + ':' + txtPort.Text + '!', clRed);
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmSocketTerminal.optTCPClick(Sender: TObject);
begin
  if optTCP.Checked then begin
    if not IdTCPClient.Connected then
      SetStatus('not connected', clRed);
  end
  else begin
    if not IdUDPClient.Active then
      SetStatus('socket inactive', clRed);
  end;
end;

procedure TfrmSocketTerminal.EnableControls(eValue: Boolean);
begin
  txtHost.Enabled := eValue;
  txtPort.Enabled := eValue;
  lblHost.Enabled := eValue;
  lblPort.Enabled := eValue;
  optTCP.Enabled  := eValue;
  optUDP.Enabled  := eValue;
  if eValue then begin
    cmdConnect.Caption := 'Connect';
    Tag := 0;
  end
  else begin
    cmdConnect.Caption := 'Disconnect';
    Tag := 1;
  end;
end;

procedure TfrmSocketTerminal.IdTCPClientConnected(Sender: TObject);
begin
  Append('Established connection to ' + txtHost.Text + ':' + txtPort.Text, clGreen);
  SetStatus('connected', clGreen);
end;

procedure TfrmSocketTerminal.IdTCPClientDisconnected(Sender: TObject);
begin
  Append('Disconnected from ' + txtHost.Text + ':' + txtPort.Text, clMaroon);
  EnableControls(True);
  SetStatus('not connected', clRed);
end;

procedure TfrmSocketTerminal.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Tag = 1 then
    cmdConnect.Click;
end;

procedure TfrmSocketTerminal.rtfEnterKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Tag = 1 then begin
    if (Key = #13) and (rtfEnter.Text <> '') then begin
      if IdTCPClient.Connected then
        IdTCPClient.WriteLn(rtfEnter.Text)
      else
        IdUDPClient.Send(rtfEnter.Text); 
      Append(rtfEnter.Text, clNavy);
      rtfEnter.Clear;
      Key := #0;
    end;
  end;
end;

procedure TfrmSocketTerminal.IdUDPClientStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: String);
begin
  Append(AStatusText, clGray);
end;

procedure TfrmSocketTerminal.acCopyExecute(Sender: TObject);
begin
  if (ActiveControl is TRichEdit) then
    TRichEdit(ActiveControl).CopyToClipboard;
  if (ActiveControl is TFlatEdit) then
    TFlatEdit(ActiveControl).CopyToClipboard;
end;

procedure TfrmSocketTerminal.acPasteExecute(Sender: TObject);
begin
  if (ActiveControl is TRichEdit) then
    TRichEdit(ActiveControl).PasteFromClipboard;
  if (ActiveControl is TFlatEdit) then
    TFlatEdit(ActiveControl).PasteFromClipboard;
end;

procedure TfrmSocketTerminal.acUndoExecute(Sender: TObject);
begin
  if (ActiveControl is TRichEdit) then
    TRichEdit(ActiveControl).Undo;
  if (ActiveControl is TFlatEdit) then
    TFlatEdit(ActiveControl).Undo;
end;

procedure TfrmSocketTerminal.acSelectAllExecute(Sender: TObject);
begin
  if (ActiveControl is TRichEdit) then
    TRichEdit(ActiveControl).SelectAll;
  if (ActiveControl is TFlatEdit) then
    TFlatEdit(ActiveControl).SelectAll;
end;

end.
