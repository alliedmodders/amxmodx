unit UnitReadThread; // from AMXX-Edit v2

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

uses UnitfrmSocketsTerminal;

{ TReadThread }

procedure TReadThread.AddRead;
begin
  frmSocketsTerminal.OnRead(Read);
end;

procedure TReadThread.Execute;
begin
  if ReadTCP then begin
    frmSocketsTerminal.IdTCPClient.ReadTimeout := 50;
    repeat
      try
        Read := frmSocketsTerminal.IdTCPClient.ReadLn;
        Synchronize(AddRead);
      except
        // nothing
      end;
    until (Terminated) or (not frmSocketsTerminal.IdTCPClient.Connected);
  end
  else begin
    frmSocketsTerminal.IdUDPClient.ReceiveTimeout := 50;
    repeat
      try
        Read := frmSocketsTerminal.IdUDPClient.ReceiveString;
        if Read <> '' then // if ReadTimeout then Read = ''
          Synchronize(AddRead);
      except
        // nothing
      end;
    until (Terminated) or (not frmSocketsTerminal.IdUDPClient.Active);
  end;
  Free;
end;

end.
 