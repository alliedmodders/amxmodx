unit UnitReadThread;

interface

uses
  Classes, SysUtils, Graphics;

type
  TReadThread = class(TThread)
  public
    ReadTCP: Boolean;
  protected
    Read: String;
    procedure Execute; override;
    procedure AddRead;
  end;

implementation

uses UnitfrmSockets;

{ TReadThread }

procedure TReadThread.AddRead;
begin
  frmSocketTerminal.OnRead(Read);
end;

procedure TReadThread.Execute;
begin
  if ReadTCP then begin
    frmSocketTerminal.IdTCPClient.ReadTimeout := 50;
    repeat
      try
        Read := frmSocketTerminal.IdTCPClient.ReadLn;
        Synchronize(AddRead);
      except
        // nothing
      end;
    until (Terminated) or (not frmSocketTerminal.IdTCPClient.Connected);
  end
  else begin
    frmSocketTerminal.IdUDPClient.ReceiveTimeout := 50;
    repeat
      try
        Read := frmSocketTerminal.IdUDPClient.ReceiveString;
        if Read <> '' then
          Synchronize(AddRead);
      except
        // nothing
      end;
    until (Terminated) or (not frmSocketTerminal.IdUDPClient.Active);
  end;
  Free;
  Read := 'fu@u';
  Synchronize(AddRead);
end;

end.
 