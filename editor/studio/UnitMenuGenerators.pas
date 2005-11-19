unit UnitMenuGenerators;

interface

uses SysUtils, Classes, Windows, Forms, Graphics;

procedure GenerateSimpleMenu;

{ Yes, a part is copied from AMXX-Edit v2. I'm too lazy to rewrite everything... }
{ gaben }

function AddOldMenu: Boolean;
function AddOldPlayerMenu: Boolean;
function GetFirst(eStart: String; eSearchMain: Boolean): Integer;
function GetLast(eStart: String; eSearchMain: Boolean): Integer;
function GetLine(eExpression: String; eAllowFunction, eBreak: Boolean): Integer;
function AddIfDoesntExist(eInclude: String): Boolean;
function GetColoredMenu: String;
function PluginInitLine: Integer;

implementation

uses UnitCodeUtils, UnitfrmMain, UnitfrmMenuGenerator, UnitLanguages,
  UnitMainTools;

function GetLine(eExpression: String; eAllowFunction, eBreak: Boolean): Integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to frmMain.sciEditor.Lines.Count -1 do begin
    if IsAtStart(eExpression, frmMain.sciEditor.Lines[i], eAllowFunction) then begin
      Result := i;
      if eBreak then
        Break;
    end;
  end;
end;

procedure GenerateSimpleMenu;
var eRP, eVL: Integer; // RegisterPlugin, VariableLine
    eStr: TStringList;
    eIndents, eInternalTitle: String;
    i: integer;
begin
  { Get Line Numbers }
  eVL := GetLine('#define', False, False);
  if eVL = -1 then
    eVL := GetLine('#include', False, False);
  if eVL = -1 then
    eVL := 0;

  eRP := PluginInitLine;

  if eRP <> -1 then begin
    eInternalTitle := StringReplace(frmMenuGenerator.txtNTitle.Text, #32, '', [rfReplaceAll]);
    eStr := TStringList.Create;
    // Register Vars
    eVL := eVL +1;
    eIndents := GetIndents(eVL);
    eStr.Add(eIndents + 'new m' + eInternalTitle + ' // Menu');
    eStr.Add(eIndents + 'new mcb' + eInternalTitle + ' // Menu Callback');
    frmMain.sciEditor.Lines.Insert(eVL, eStr.Text);


    // Register function
    eRP := PluginInitLine;
    eIndents := GetIndents(eRP);
    eStr.Clear;
    eStr.Add(eIndents + '/* Menu ' + frmMenuGenerator.txtNTitle.Text + ' */');
    eStr.Add(eIndents + '/* Use menu_display(id, m' + eInternalTitle + ', 0) to show the menu to an user. */');
    eStr.Add(eIndents + 'm' + eInternalTitle + ' = menu_create("' + frmMenuGenerator.txtNTitle.Text + '", "mh_' + eInternalTitle + '")');
    eStr.Add(eIndents + 'mcb' + eInternalTitle + ' = menu_makecallback("mcb_' + eInternalTitle + '")');
    for i := 0 to frmMenuGenerator.lstNMenuItems.Items.Count -1 do
      eStr.Add(eIndents + 'menu_additem(m' + eInternalTitle + ', "' + frmMenuGenerator.lstNMenuItems.Items[i] + '", "ma_' + eInternalTitle + '", ' + frmMenuGenerator.cboAccess.Text + ', mcb' + eInternalTitle + ')');
    eStr.Add(eIndents + '/* Menu End */');
    frmMain.sciEditor.Lines.Insert(eRP, eStr.Text);
    // Rest
    eStr.Clear;
    eStr.Add('');
    eStr.Add('');
    eStr.Add('/* Menu ' + frmMenuGenerator.txtNTitle.Text + ' */');
    eStr.Add(''); 
    eStr.Add('public mh_' + eInternalTitle + '(id, menu, item) {');
    eStr.Add(#9 + '/* This event is called when someone presses a key on this menu */');
    eStr.Add('}');
    eStr.Add('');
    eStr.Add('public ma_' + eInternalTitle + '(id) {');
    eStr.Add(#9 + '/* This event is called when an item was selected */');
    eStr.Add('}');
    eStr.Add('');
    eStr.Add('public mcb_' + eInternalTitle + '(id, menu, item) {');
    eStr.Add(#9 + '/* This is the callback-event, here you can set items enabled or disabled. */');
    eStr.Add(#9 + '/* If you want to enable an item, use: return ITEM_ENABLED */');
    eStr.Add(#9 + '/* If you want to disable an item, use: return ITEM_DISABLED */');
    eStr.Add('}');
    eStr.Add(''); 
    frmMain.sciEditor.Lines.AddStrings(eStr); 

    eStr.Free;
  end
  else
    MessageBox(frmMenuGenerator.Handle, PChar(lInvalidPlugin), PChar(Application.Title), MB_ICONERROR);

  ActiveDoc.Modified := True;
  frmMain.mnuModified.Caption := lModified;
end;

{ Normal Menu }

function AddOldMenu: Boolean;
var eColoredMenu, DefinedKeys: String;
    i: integer;
    eStr: TStringList;
begin
  Result := GetFirst('public Show' + frmMenuGenerator.txtMenuName.Text, True) = -1;
  if not Result then
    exit;
    
  eStr := TStringList.Create;
  { Transform text }
  eColoredMenu := GetColoredMenu;
  { Add functions }
  eStr.Add(Format('public Show%s(id) {', [frmMenuGenerator.txtMenuName.Text]));
  eStr.Add('	show_menu(id, Keys' + frmMenuGenerator.txtMenuName.Text + ', "' + eColoredMenu + '", -1, "' + frmMenuGenerator.txtMenuName.Text + '")');
  if frmMenuGenerator.chkAddComment.Checked then
    eStr[eStr.Count -1] := eStr[eStr.Count -1] + ' // Display menu';
  eStr.Add('}');
  eStr.Add('');
  eStr.Add('public Pressed' + frmMenuGenerator.txtMenuName.Text + '(id, key) {');
  if frmMenuGenerator.chkAddComment.Checked then begin
    eStr.Add('	/* Menu:');
    for i := 0 to frmMenuGenerator.rtfMenu.Lines.Count -1 do
      eStr.Add('	* ' + frmMenuGenerator.rtfMenu.Lines[i]);
    eStr.Add('	*/');
    eStr.Add(''); 
  end;
  eStr.Add('	switch (key) {');
  DefinedKeys := '';
  for i := 1 to Length(frmMenuGenerator.txtKeys.Text) do begin
    if frmMenuGenerator.txtKeys.Text[i] = '0' then begin
      DefinedKeys := DefinedKeys + '|(1<<9)';
      if frmMenuGenerator.chkAddComment.Checked then
        eStr.Add('		case 9: { // 0')
      else
        eStr.Add('		case 9: {');
      eStr.Add('			');
      eStr.Add('		}');
    end
    else begin
      DefinedKeys := DefinedKeys + '|(1<<' + IntToStr(StrToInt(frmMenuGenerator.txtKeys.Text[i]) -1) + ')';
      if frmMenuGenerator.chkAddComment.Checked then
        eStr.Add('		case ' + IntToStr(StrToInt(frmMenuGenerator.txtKeys.Text[i]) -1) + ': { // ' + frmMenuGenerator.txtKeys.Text[i])
      else
        eStr.Add('		case ' + IntToStr(StrToInt(frmMenuGenerator.txtKeys.Text[i]) -1) + ': {');
      eStr.Add('			');
      eStr.Add('		}');
    end;
  end;
  Delete(DefinedKeys, 1, 1);
  if frmMenuGenerator.chkAddComment.Checked then
    DefinedKeys := DefinedKeys + ' // Keys: ' + frmMenuGenerator.txtKeys.Text;
  eStr.Add('	}');
  eStr.Add('}');
  // Insert
  AddIfDoesntExist('amxmodx');
  i := GetLast('#define', True) +1;
  if i = 0 then
    i := GetLast('#include', True) +1;
    
  frmMain.sciEditor.Lines.Insert(i, Format('#define Keys%s %s', [frmMenuGenerator.txtMenuName.Text, DefinedKeys]));
  frmMain.sciEditor.Lines.Text := frmMain.sciEditor.Lines.Text + #13 + eStr.Text;
  if frmMenuGenerator.chkRegisterMenuCommand.Checked then begin
    i := GetFirst('register_plugin', True) +2;
    if i = 1 then
      i := GetFirst('public plugin_init()', True) +2;
    if i = 1 then begin
      eStr.Clear;
      eStr.Add('public plugin_init() {');
      eStr.Add('	register_menucmd(register_menuid("' + frmMenuGenerator.txtMenuName.Text + '"), Keys' + frmMenuGenerator.txtMenuName.Text + ', "Pressed' + frmMenuGenerator.txtMenuName.Text + '")');
      eStr.Add('}');
      frmMain.sciEditor.Lines.Insert(GetFirst('#define', True) +2, '');
      frmMain.sciEditor.Lines.Insert(GetFirst('#define', True) +3, eStr.Text);
    end
    else
      frmMain.sciEditor.Lines.Insert(i, '	register_menucmd(register_menuid("' + frmMenuGenerator.txtMenuName.Text + '"), Keys' + frmMenuGenerator.txtMenuName.Text + ', "Pressed' + frmMenuGenerator.txtMenuName.Text + '")');
  end;
  eStr.Free;
  ActiveDoc.Modified := True;
  frmMain.mnuModified.Caption := lModified;
end;

{ Player Menu }

function AddOldPlayerMenu: Boolean;
function PrepareItem(eItem: String; eDisabled: Boolean): String; // Remove colors etc.
begin
  eItem := StringReplace(eItem, '\w', '', [rfReplaceAll, rfIgnoreCase]);
  eItem := StringReplace(eItem, '\y', '', [rfReplaceAll, rfIgnoreCase]);
  eItem := StringReplace(eItem, '\r', '', [rfReplaceAll, rfIgnoreCase]);
  eItem := StringReplace(eItem, '\d', '', [rfReplaceAll, rfIgnoreCase]);
  eItem := StringReplace(eItem, '%n', '%i', [rfIgnoreCase]);
  eItem := StringReplace(eItem, '%v', '%s', [rfIgnoreCase]);
  if eDisabled then
    eItem := '\d' + eItem
  else
    eItem := '\w' + eItem;
  Result := eItem + '^n';
end;

var i: integer;
    eStr: TStringList;
    ePlayersFrom, ePlayersTo: Integer; // Players
    ePlayerFormat: String;
    eNext, eExit: Integer; // Next and Back/Exit
    eNextText, eBackText, eExitText: String;
    eCurLineIndex: Integer; // Current ..
    eCurLine: String;       // .. line
    DefinedKeys: String; // Action Keys
begin
  Result := GetFirst('public Show' + frmMenuGenerator.txtMenuName.Text, True) = -1;
  if not Result then begin
    MessageBox(frmMenuGenerator.Handle, 'Menu already exists. Please choose another name.', 'Warning', MB_ICONWARNING);
    exit;
  end;

  {
    Kick player

    $players(1,8,%n. %v)
    $next(9,9. Next)

    $exitorback(0, 0. Exit, 0. Back)
  }

  eCurLine := frmMenuGenerator.rtfMenu.Lines[0];
  eCurLineIndex := 0;
  eNext := -1;
  eExit := -1;
  eStr := TStringList.Create;
  // Prepare Values
  try
    { Players }
    eCurLineIndex := GetFirst('$players', False);
    eCurLine := frmMenuGenerator.rtfMenu.Lines[eCurLineIndex];
    ePlayersFrom := StrToInt(Between(LowerCase(Trim(eCurLine)), '$players(', ','));
    ePlayersTo := StrToInt(Between(Trim(LowerCase(eCurLine)), ',', ','));
    while CountChars(eCurLine, ',') > 1 do
      Delete(eCurLine, 1, 1);
    ePlayerFormat := Between(LowerCase(eCurLine), ',', ')');
    { Next }
    if GetFirst('$next', False) <> -1 then begin
      eCurLineIndex := GetFirst('$next', False);
      eCurLine := frmMenuGenerator.rtfMenu.Lines[eCurLineIndex];
      eNext := StrToInt(Trim(Between(eCurLine, '(', ',')));
      eNextText := Between(eCurLine, ',', ')');
    end;
    { Exit or Back }
    if GetFirst('$exitorback', False) <> -1 then begin
      eCurLineIndex := GetFirst('$exitorback', False);
      eCurLine := frmMenuGenerator.rtfMenu.Lines[eCurLineIndex];
      eExit := StrToInt(Trim(Between(eCurLine, '(', ',')));
      eExitText := Between(eCurLine, ',', ',');
      while CountChars(eCurLine, ',') > 1 do
        Delete(eCurLine, 1, 1);
      eBackText := Between(eCurLine, ',', ')');
    end;
  except
    MessageBox(frmMenuGenerator.Handle, PChar(Format('Syntax error at line %s: ' + #13 + '%s', [IntToStr(eCurLineIndex +1), frmMenuGenerator.rtfMenu.Lines[eCurLineIndex]])), 'Error', MB_ICONERROR);
    Result := False;
    exit;
  end;
  // Check Keys
  { Players }
  if (ePlayersFrom < 0) or (ePlayersFrom > ePlayersTo) then begin
    MessageBox(frmMenuGenerator.Handle, 'Invalid start key (players)', 'Warning', MB_ICONWARNING);
    Result := False;
    exit;
  end;
  if (ePlayersTo < 0) or (ePlayersTo > 9) then begin
    MessageBox(frmMenuGenerator.Handle, 'Invalid stop key (players)', 'Warning', MB_ICONWARNING);
    Result := False;
    exit;
  end;
  { Next, Exit and Custom keys}
  eCurLine := '';
  for i := ePlayersFrom to ePlayersTo do
    eCurLine := eCurLine + IntToStr(i);

  if Pos(IntToStr(eNext), eCurLine) > 0 then begin
    MessageBox(frmMenuGenerator.Handle, PChar(Format('"Next" key already in use (%s). Delete it or choose another one and try again.', [IntToStr(eNext)])), 'Warning', MB_ICONWARNING);
    Result := False;
    exit;
  end;
  if Pos(IntToStr(eExit), eCurLine) > 0 then begin
    MessageBox(frmMenuGenerator.Handle, PChar(Format('"Exit" key already in use (%s). Delete it or choose another one and try again.', [IntToStr(eExit)])), 'Warning', MB_ICONWARNING);
    Result := False;
    exit;
  end;
  eCurLine := eCurLine + IntToStr(eNext);
  eCurLine := eCurLine + IntToStr(eExit);
  // Insert Code
  try
    { Includes }
    AddIfDoesntExist('amxmodx');
    AddIfDoesntExist('amxmisc');
    { Define Keys }
    DefinedKeys := '';
    if Length(eCurLine) <> 0 then begin
      for i := 1 to Length(eCurLine) do begin
        if eCurLine[i] = '0' then
          DefinedKeys := DefinedKeys + '|(1<<9)'
        else begin
          eCurLine[i] := IntToStr(StrToInt(eCurLine[i]) -1)[1];
          DefinedKeys := DefinedKeys + '|(1<<' + eCurLine[i] + ')';
        end;
      end;
      Delete(DefinedKeys, 1, 1);
    end;
    i := GetLast('#define', True) +1;
    if i = 0 then
      i := GetLast('#include', True) +1;

    frmMain.sciEditor.Lines.Insert(i, Format('#define Keys%s %s', [frmMenuGenerator.txtMenuName.Text, DefinedKeys]));
    frmMain.sciEditor.Lines.Insert(i +1, '');
    frmMain.sciEditor.Lines.Insert(i +2, 'new MenuPos' + frmMenuGenerator.txtMenuName.Text);
    frmMain.sciEditor.Lines.Insert(i +3, 'new MenuPlayers' + frmMenuGenerator.txtMenuName.Text + '[32]');
    { Register }
    i := GetFirst('register_plugin', True) +2;
    if i = 1 then
      i := GetFirst('public plugin_init()', True) +2;
    if i = 1 then begin
      eStr.Clear;
      eStr.Add('public plugin_init() {');
      eStr.Add('	register_menucmd(register_menuid("' + frmMenuGenerator.txtMenuName.Text + '"), Keys' + frmMenuGenerator.txtMenuName.Text + ', "Pressed' + frmMenuGenerator.txtMenuName.Text + '")');
      eStr.Add('}');
      frmMain.sciEditor.Lines.Insert(GetFirst('#define', True) +2, '');
      frmMain.sciEditor.Lines.Insert(GetFirst('#define', True) +3, eStr.Text);
    end
    else
      frmMain.sciEditor.Lines.Insert(i, '	register_menucmd(register_menuid("' + frmMenuGenerator.txtMenuName.Text + '"), Keys' + frmMenuGenerator.txtMenuName.Text + ', "Pressed' + frmMenuGenerator.txtMenuName.Text + '")');
    { Show Menu Functions (thx to xeroblood for code example) }
    eStr.Clear;
    eStr.Add('public Show' + frmMenuGenerator.txtMenuName.Text + '(id) {');
    eStr.Add('	ShowMenu' + frmMenuGenerator.txtMenuName.Text + '(id, MenuPos' + frmMenuGenerator.txtMenuName.Text + ' = 0)');
    eStr.Add('	return PLUGIN_HANDLED');
    eStr.Add('}');
    eStr.Add('');
    eStr.Add('public ShowMenu' + frmMenuGenerator.txtMenuName.Text + '(id, position) {');
    if frmMenuGenerator.chkAddComment.Checked then
      eStr.Add('	// Menu stuff //');
    eStr.Add('	if (position < 0) { return 0; }');
    eStr.Add('	');
    eStr.Add('	new i, k');
    eStr.Add('	new MenuBody[255]');
    eStr.Add('	new CurrentKey = ' + IntToStr(ePlayersFrom -1));
    eStr.Add('	new Start = position * ' + IntToStr(ePlayersTo - ePlayersFrom));
    eStr.Add('	new Num');
    eStr.Add('	new UserName[32]');
    eStr.Add('	');
    eStr.Add('	get_players(MenuPlayers' + frmMenuGenerator.txtMenuName.Text + ', Num)');
    eStr.Add('	if (Start >= Num) { Start = position = MenuPos' + frmMenuGenerator.txtMenuName.Text + ' = 0; }');
    eCurLine := GetColoredMenu;
    eCurLine := Copy(eCurLine, 1, Pos('$players', eCurLine) -5);
    Insert('\R%d/%d', eCurLine, Pos('^n', eCurLine));
    eStr.Add('	new Len = format(MenuBody, 255, "' + eCurLine + '", position+1, (Num / ' + IntToStr(ePlayersTo - ePlayersFrom) + ' + ((Num % ' + IntToStr(ePlayersTo - ePlayersFrom) + ') ? 1 : 0 )) )');
    eStr.Add('	new End = Start + ' + IntToStr(ePlayersTo - ePlayersFrom));
    if eExit = 0 then
      eStr.Add('	new Keys = (1<<9)')
    else
      eStr.Add('	new Keys = (1<<' + IntToStr(eExit -1) + ')');
    eStr.Add('	if (End > Num) { End = Num; }');
    eStr.Add('	');
    eStr.Add('	for(i=Start;i<End;i++) {');
    eStr.Add('		k = MenuPlayers' + frmMenuGenerator.txtMenuName.Text + '[i]');
    eStr.Add('		get_user_name(k, UserName, 31)');
    // Any conditions?
//    if (frmMenuGenerator.chkImmunity.Checked) and (frmMenuGenerator.chkAlive.Checked) then
//      eStr.Add('		if ((get_user_flags(k) & ADMIN_IMMUNITY) || !is_user_alive(k)) {')
//    else if (frmMenuGenerator.chkImmunity.Checked) then
//      eStr.Add('		if (get_user_flags(k) & ADMIN_IMMUNITY) {')
//    else if (frmMenuGenerator.chkAlive.Checked) then
//      eStr.Add('		if (!is_user_alive(k)) {');
//    if (frmMenuGenerator.chkImmunity.Checked) or (frmMenuGenerator.chkAlive.Checked) then begin
//      eStr.Add('			CurrentKey++');
//      eStr.Add('			Len += format(MenuBody[Len], (255-Len), "' + PrepareItem(ePlayerFormat, True) + '", CurrentKey, UserName)');
//      eStr.Add('		}');
//      eStr.Add('		else {');
//      eStr.Add('			Keys |= (1<<CurrentKey++)');
//      eStr.Add('			Len += format(MenuBody[Len], (255-Len), "' + PrepareItem(ePlayerFormat, False) + '", CurrentKey, UserName)');
//      eStr.Add('		}');
//    end
//    else begin
      eStr.Add('		Keys |= (1<<CurrentKey++)');
      eStr.Add('		Len += format(MenuBody[Len], (255-Len), "' + PrepareItem(ePlayerFormat, False) + '", CurrentKey, UserName)');
//    end;
    eStr.Add('	}');
    eStr.Add('	if (End != Num) {');
    eStr.Add('		format(MenuBody[Len], (255-Len), "^n\w' + eNextText + '^n%s", position ? "\w' + eBackText + '" : "\w' + eExitText + '")');
    eStr.Add('		Keys |= (1<<' + IntToStr(eNext -1) + ')');
    eStr.Add('	}');
    eStr.Add('	else {');
    eStr.Add('		format(MenuBody[Len], (255-Len), "^n%s", position ? "\w' + eBackText + '" : "\w' + eExitText + '")');
    eStr.Add('	}');
    eStr.Add('	show_menu(id, Keys, MenuBody, -1, "' + frmMenuGenerator.txtMenuName.Text + '")');
    eStr.Add('	return 0');
    eStr.Add('}');
    eStr.Add('');
    eStr.Add('public Pressed' + frmMenuGenerator.txtMenuName.Text + '(id, key) {');
    eStr.Add('	switch (key) {');
    if frmMenuGenerator.chkAddComment.Checked then begin
      if eNext <> 0 then
        eStr.Add('		case ' + IntToStr(eNext -1) + ': ShowMenu' + frmMenuGenerator.txtMenuName.Text + '(id, ++MenuPos' + frmMenuGenerator.txtMenuName.Text + ') // More Option')
      else
        eStr.Add('		case 9: ShowMenu' + frmMenuGenerator.txtMenuName.Text + '(id, ++MenuPos' + frmMenuGenerator.txtMenuName.Text + ') // More Option');

      if eExit <> 0 then
        eStr.Add('		case ' + IntToStr(eExit -1) + ': ShowMenu' + frmMenuGenerator.txtMenuName.Text + '(id, --MenuPos' + frmMenuGenerator.txtMenuName.Text + ') // Back Option')
      else
        eStr.Add('		case 9: ShowMenu' + frmMenuGenerator.txtMenuName.Text + '(id, --MenuPos' + frmMenuGenerator.txtMenuName.Text + ') // Back Option');
    end
    else begin
      if eNext <> 0 then
        eStr.Add('		case ' + IntToStr(eNext -1) + ': ShowMenu' + frmMenuGenerator.txtMenuName.Text + '(id, ++MenuPos' + frmMenuGenerator.txtMenuName.Text + ')')
      else
        eStr.Add('		case 9: ShowMenu' + frmMenuGenerator.txtMenuName.Text + '(id, ++MenuPos' + frmMenuGenerator.txtMenuName.Text + ')');
      if eExit <> 0 then
        eStr.Add('		case ' + IntToStr(eExit -1) + ': ShowMenu' + frmMenuGenerator.txtMenuName.Text + '(id, --MenuPos' + frmMenuGenerator.txtMenuName.Text + ')')
      else
        eStr.Add('		case 9: ShowMenu' + frmMenuGenerator.txtMenuName.Text + '(id, --MenuPos' + frmMenuGenerator.txtMenuName.Text + ')');
    end;
    eStr.Add('		default: {');
    if frmMenuGenerator.chkAddComment.Checked then
      eStr.Add('			// Get User ID and Username');
    eStr.Add('			new PlayerID = MenuPlayers' + frmMenuGenerator.txtMenuName.Text + '[MenuPos' + frmMenuGenerator.txtMenuName.Text + ' * ' + IntToStr(ePlayersTo - ePlayersFrom) + ' + key]');
    eStr.Add('			new UserName[32]');
    eStr.Add('			get_user_name(PlayerID, UserName, 31)');
    if frmMenuGenerator.chkAddComment.Checked then
      eStr.Add('			// Do actions here')
    else
      eStr.Add('			');
    eStr.Add('		}');
    eStr.Add('	}');
    eStr.Add('	return PLUGIN_HANDLED');
    eStr.Add('}');
    frmMain.sciEditor.Lines.Text := frmMain.sciEditor.Lines.Text + #13 + eStr.Text;
  except
    MessageBox(frmMenuGenerator.Handle, PChar('An error occured while inserting code!'), 'Warning', MB_ICONWARNING);
  end;
  eStr.Free;
  frmMain.mnuModified.Caption := lModified;
  ActiveDoc.Modified := True;
end;

{ Functions }

function GetFirst(eStart: String; eSearchMain: Boolean): Integer;
var i: integer;
begin
  eStart := LowerCase(Trim(eStart));
  Result := -1;
  if eSearchMain then begin
    for i := 0 to frmMain.sciEditor.Lines.Count -1 do begin
      if Pos(eStart, LowerCase(Trim(frmMain.sciEditor.Lines[i]))) = 1 then begin
        Result := i;
        exit;
      end;
    end;
  end
  else begin
    for i := 0 to frmMenuGenerator.rtfMenu.Lines.Count -1 do begin
      if Pos(eStart, LowerCase(Trim(frmMenuGenerator.rtfMenu.Lines[i]))) = 1 then begin
        Result := i;
        exit;
      end;
    end;
  end;
end;

function GetLast(eStart: String; eSearchMain: Boolean): Integer;
var i: integer;
begin
  eStart := LowerCase(Trim(eStart));
  Result := -1;
  if eSearchMain then begin
    for i := 0 to frmMain.sciEditor.Lines.Count -1 do begin
      if Pos(eStart, LowerCase(Trim(frmMain.sciEditor.Lines[i]))) = 1 then
        Result := i;
    end;
  end
  else begin
    for i := 0 to frmMenuGenerator.rtfMenu.Lines.Count -1 do begin
      if Pos(eStart, LowerCase(Trim(frmMenuGenerator.rtfMenu.Lines[i]))) = 1 then
        Result := i;
    end;
  end;
end;

function AddIfDoesntExist(eInclude: String): Boolean;
var i: integer;
begin
  Result := True;
  eInclude := LowerCase(eInclude);
  for i := 0 to frmMain.sciEditor.Lines.Count -1 do begin
    if (Pos('<', frmMain.sciEditor.Lines[i]) <> 0) or (Pos('"', frmMain.sciEditor.Lines[i]) <> 0) then begin
      if LowerCase(Between(frmMain.sciEditor.Lines[i], '<', '>')) = eInclude then
        exit;
      if LowerCase(Between(frmMain.sciEditor.Lines[i], '"', '"')) = eInclude then
        exit;
    end;
  end;

  i := GetLast('#include', True);
  if i = -1 then
    i := 0;
    
  frmMain.sciEditor.Lines.Insert(i, '#include <' + eInclude + '>');
end;

function GetColoredMenu: String;
var i: integer;
    eCurColor: TColor;
begin
  eCurColor := clWhite;
  Result := '';
  for i := 0 to Length(frmMenuGenerator.rtfMenu.Lines.Text) -1 do begin
    frmMenuGenerator.rtfMenu.SelStart := i;
    if frmMenuGenerator.rtfMenu.SelAttributes.Color <> eCurColor then begin
      eCurColor := frmMenuGenerator.rtfMenu.SelAttributes.Color;
      case eCurColor of
        clWhite : Result := Result + '\w';
        clYellow: Result := Result + '\y';
        clRed   : Result := Result + '\r';
        clGray  : Result := Result + '\d';
      end;
    end;
    Result := Result + frmMenuGenerator.rtfMenu.Lines.Text[i+1];
  end;
  frmMenuGenerator.rtfMenu.SelStart := 0;
  Result := StringReplace(Result, #13, '^n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
end;

function PluginInitLine: Integer;
begin
  Result := GetLine('register_concmd', True, False);
  if Result = -1 then
    Result := GetLine('register_clcmd', True, False);
  if Result = -1 then
    Result := GetLine('register_plugin', True, True);
  if Result = -1 then
    Result := GetLine('public plugin_init', True, True);
end;

end.
