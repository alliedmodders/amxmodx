library HelloWorld;


uses
  SysUtils,
  Classes,
  Windows,
  Messages,
  studioapi;

{$R *.res}

{ Commands here }

procedure PluginLoad(LoadInfo: PLoadInfo); cdecl;
begin
  // Set Plugin infos after load
  LoadInfo.sPluginName := 'Hello World';
  LoadInfo.sPluginDescription := 'Simple Hello World example (Delphi).';
  // Create menu item in Tools with the caption "Hello World! [Delphi]" and with ImageIndex -1 (no image)
  SendStudioMsg(SCM_MENU_ADDITEM, 'Tools->Hello World! [Delphi]', -1);
end;

procedure PluginUnload; cdecl;
begin
  // Remove menu stuff etc.
  SendStudioMsg(SCM_REMOVE_MENUITEM, 'Hello World! [Delphi]', -1);
end;

function CustomItemClick(pCaption: PChar): Integer; cdecl;
begin
  // Handle custom click
  if pCaption = 'Hello World! [Delphi]' then begin
    SendStudioMsg(SCM_EDITOR_SETTEXT, 'Hello World!' + #13 + #13 + 'This is a simple example for creating plugins for AMXX-Studio in Delphi.', 0);
    Result := PLUGIN_HANDLED;
  end
  else
    Result := PLUGIN_CONTINUE;
end;

exports // Export all DLL functions
  PluginLoad,
  PluginUnload,
  CustomItemClick;

begin

end.

