unit UnitAddMenu;

interface

uses SysUtils, Classes, Graphics, Dialogs, Windows;

function AddMenu: Boolean;
function AddPlayerMenu: Boolean;
function GetFirst(eStart: String; eSearchMain: Boolean): Integer;
function GetLast(eStart: String; eSearchMain: Boolean): Integer;
function AddIfDoesntExist(eInclude: String): Boolean;

implementation

uses UnitfrmMenuMaker, UnitfrmMain, UnitTextAnalyze, UnitFunc;

{ Normal Menu }

function AddMenu: Boolean;
var eColoredMenu, DefinedKeys: String;
    i: integer;
    eStr: TStringList;
begin
  Result := GetFirst('public Show' + frmMenuMaker.txtMenuName.Text, True) = -1;
  if not Result then
    exit;
    
  eStr := TStringList.Create;
  { Transform text }
  eColoredMenu := frmMenuMaker.GetColoredMenu;
  if frmMenuMaker.chkAppendOnlyMenuText.Checked then begin
    frmMain.sciEditor.Lines.Add('// Created menu: ' + eColoredMenu);
    exit; 
  end;
  { Add functions }
  eStr.Add(Format('public Show%s(id) {', [frmMenuMaker.txtMenuName.Text]));
  eStr.Add('	show_menu(id, Keys' + frmMenuMaker.txtMenuName.Text + ', "' + eColoredMenu + '", -1, "' + frmMenuMaker.txtMenuName.Text + '")');
  if frmMenuMaker.chkAddComment.Checked then
    eStr[eStr.Count -1] := eStr[eStr.Count -1] + ' // Display menu';
  eStr.Add('}');
  eStr.Add('');
  eStr.Add('public Pressed' + frmMenuMaker.txtMenuName.Text + '(id, key) {');
  if frmMenuMaker.chkAddComment.Checked then begin
    eStr.Add('	/* Menu:');
    for i := 0 to frmMenuMaker.rtfEditor.Lines.Count -1 do
      eStr.Add('	* ' + frmMenuMaker.rtfEditor.Lines[i]);
    eStr.Add('	*/');
    eStr.Add(''); 
  end;
  eStr.Add('	switch (key) {');
  DefinedKeys := '';
  for i := 1 to Length(frmMenuMaker.txtKeys.Text) do begin
    if frmMenuMaker.txtKeys.Text[i] = '0' then begin
      DefinedKeys := DefinedKeys + '|(1<<9)';
      if frmMenuMaker.chkAddComment.Checked then
        eStr.Add('		case 9: { // 0')
      else
        eStr.Add('		case 9: {');
      eStr.Add('			');
      eStr.Add('		}');
    end
    else begin
      DefinedKeys := DefinedKeys + '|(1<<' + IntToStr(StrToInt(frmMenuMaker.txtKeys.Text[i]) -1) + ')';
      if frmMenuMaker.chkAddComment.Checked then
        eStr.Add('		case ' + IntToStr(StrToInt(frmMenuMaker.txtKeys.Text[i]) -1) + ': { // ' + frmMenuMaker.txtKeys.Text[i])
      else
        eStr.Add('		case ' + IntToStr(StrToInt(frmMenuMaker.txtKeys.Text[i]) -1) + ': {');
      eStr.Add('			');
      eStr.Add('		}');
    end;
  end;
  Delete(DefinedKeys, 1, 1);
  if frmMenuMaker.chkAddComment.Checked then
    DefinedKeys := DefinedKeys + ' // Keys: ' + frmMenuMaker.txtKeys.Text;
  eStr.Add('	}');
  eStr.Add('}');
  // Insert
  AddIfDoesntExist('amxmodx');
  i := GetFirst('#define', True) +2;
  if i = 1 then
    i := GetFirst('#include', True) +2;
  if i = 1 then
    i := 0;
    
  frmMain.sciEditor.Lines.Insert(i, Format('#define Keys%s %s', [frmMenuMaker.txtMenuName.Text, DefinedKeys]));
  frmMain.sciEditor.Lines.Text := frmMain.sciEditor.Lines.Text + #13 + eStr.Text;
  if frmMenuMaker.chkRegisterMenuCommand.Checked then begin
    i := GetFirst('register_plugin', True) +2;
    if i = 1 then
      i := GetFirst('public plugin_init()', True) +2;
    if i = 1 then begin
      eStr.Clear;
      eStr.Add('public plugin_init() {');
      eStr.Add('	register_menucmd(register_menuid("' + frmMenuMaker.txtMenuName.Text + '"), Keys' + frmMenuMaker.txtMenuName.Text + ', "Pressed' + frmMenuMaker.txtMenuName.Text + '")');
      eStr.Add('}');
      frmMain.sciEditor.Lines.Insert(GetFirst('#define', True) +2, '');
      frmMain.sciEditor.Lines.Insert(GetFirst('#define', True) +3, eStr.Text);
    end
    else
      frmMain.sciEditor.Lines.Insert(i, '	register_menucmd(register_menuid("' + frmMenuMaker.txtMenuName.Text + '"), Keys' + frmMenuMaker.txtMenuName.Text + ', "Pressed' + frmMenuMaker.txtMenuName.Text + '")');
  end;
  frmMain.SetModified;
  eStr.Free;
  UpdateList(frmMain.sciEditor.Lines.Text);
end;

{ Player Menu }

function AddPlayerMenu: Boolean;
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
  Result := GetFirst('public Show' + frmMenuMaker.txtMenu.Text, True) = -1;
  if not Result then begin
    MessageBox(frmMenuMaker.Handle, 'Menu already exists. Please choose another name.', 'Warning', MB_ICONWARNING);
    exit;
  end;

  {
    Kick player

    $players(1,8,%n. %v)
    $next(9,9. Next)

    $exitorback(0, 0. Exit, 0. Back)
  }

  eCurLine := frmMenuMaker.rtfEditor.Lines[0];
  eCurLineIndex := 0;
  eNext := -1;
  eExit := -1;
  eStr := TStringList.Create;
  // Prepare Values
  try
    { Players }
    eCurLineIndex := GetFirst('$players', False);
    eCurLine := frmMenuMaker.rtfEditor.Lines[eCurLineIndex];
    ePlayersFrom := StrToInt(Between(LowerCase(Trim(eCurLine)), '$players(', ','));
    ePlayersTo := StrToInt(Between(Trim(LowerCase(eCurLine)), ',', ','));
    while CountChars(eCurLine, ',') > 1 do
      Delete(eCurLine, 1, 1);
    ePlayerFormat := Between(LowerCase(eCurLine), ',', ')');
    { Next }
    if GetFirst('$next', False) <> -1 then begin
      eCurLineIndex := GetFirst('$next', False);
      eCurLine := frmMenuMaker.rtfEditor.Lines[eCurLineIndex];
      eNext := StrToInt(Trim(Between(eCurLine, '(', ',')));
      eNextText := Between(eCurLine, ',', ')');
    end;
    { Exit or Back }
    if GetFirst('$exitorback', False) <> -1 then begin
      eCurLineIndex := GetFirst('$exitorback', False);
      eCurLine := frmMenuMaker.rtfEditor.Lines[eCurLineIndex];
      eExit := StrToInt(Trim(Between(eCurLine, '(', ',')));
      eExitText := Between(eCurLine, ',', ',');
      while CountChars(eCurLine, ',') > 1 do
        Delete(eCurLine, 1, 1);
      eBackText := Between(eCurLine, ',', ')');
    end;
  except
    MessageBox(frmMenuMaker.Handle, PChar(Format('Syntax error at line %s: ' + #13 + '%s', [IntToStr(eCurLineIndex +1), frmMenuMaker.rtfEditor.Lines[eCurLineIndex]])), 'Error', MB_ICONERROR);
    Result := False;
    exit;
  end;
  // Check Keys
  { Players }
  if (ePlayersFrom < 0) or (ePlayersFrom > ePlayersTo) then begin
    MessageBox(frmMenuMaker.Handle, 'Invalid start key (players)', 'Warning', MB_ICONWARNING);
    Result := False;
    exit;
  end;
  if (ePlayersTo < 0) or (ePlayersTo > 9) then begin
    MessageBox(frmMenuMaker.Handle, 'Invalid stop key (players)', 'Warning', MB_ICONWARNING);
    Result := False;
    exit;
  end;
  { Next, Exit and Custom keys}
  eCurLine := '';
  for i := ePlayersFrom to ePlayersTo do
    eCurLine := eCurLine + IntToStr(i);

  if Pos(IntToStr(eNext), eCurLine) > 0 then begin
    MessageBox(frmMenuMaker.Handle, PChar(Format('"Next" key already in use (%s). Delete it or choose another one and try again.', [IntToStr(eNext)])), 'Warning', MB_ICONWARNING);
    Result := False;
    exit;
  end;
  if Pos(IntToStr(eExit), eCurLine) > 0 then begin
    MessageBox(frmMenuMaker.Handle, PChar(Format('"Exit" key already in use (%s). Delete it or choose another one and try again.', [IntToStr(eExit)])), 'Warning', MB_ICONWARNING);
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
    i := GetLast('#define', True) +2;
    if i = 1 then
      i := GetLast('#include', True) +2;
    if i = 1 then
      i := 0;
    frmMain.sciEditor.Lines.Insert(i, Format('#define Keys%s %s', [frmMenuMaker.txtMenu.Text, DefinedKeys]));
    frmMain.sciEditor.Lines.Insert(i +1, 'new MenuPos' + frmMenuMaker.txtMenu.Text);
    frmMain.sciEditor.Lines.Insert(i +2, 'new MenuPlayers' + frmMenuMAker.txtMenu.Text + '[32]');
    { Register }
    i := GetFirst('register_plugin', True) +2;
    if i = 1 then
      i := GetFirst('public plugin_init()', True) +2;
    if i = 1 then begin
      eStr.Clear;
      eStr.Add('public plugin_init() {');
      eStr.Add('	register_menucmd(register_menuid("' + frmMenuMaker.txtMenu.Text + '"), Keys' + frmMenuMaker.txtMenu.Text + ', "Pressed' + frmMenuMaker.txtMenu.Text + '")');
      eStr.Add('}');
      frmMain.sciEditor.Lines.Insert(GetFirst('#define', True) +2, '');
      frmMain.sciEditor.Lines.Insert(GetFirst('#define', True) +3, eStr.Text);
    end
    else
      frmMain.sciEditor.Lines.Insert(i, '	register_menucmd(register_menuid("' + frmMenuMaker.txtMenu.Text + '"), Keys' + frmMenuMaker.txtMenu.Text + ', "Pressed' + frmMenuMaker.txtMenu.Text + '")');
    { Show Menu Functions (thx to xeroblood for code example) }
    eStr.Clear;
    eStr.Add('public Show' + frmMenuMaker.txtMenu.Text + '(id) {');
    eStr.Add('	ShowMenu' + frmMenuMaker.txtMenu.Text + '(id, MenuPos' + frmMenuMaker.txtMenu.Text + ' = 0)');
    eStr.Add('	return PLUGIN_HANDLED');
    eStr.Add('}');
    eStr.Add('');
    eStr.Add('public ShowMenu' + frmMenuMaker.txtMenu.Text + '(id, position) {');
    if frmMenuMaker.chkComments.Checked then
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
    eStr.Add('	get_players(MenuPlayers' + frmMenuMaker.txtMenu.Text + ', Num)');
    eStr.Add('	if (Start >= Num) { Start = position = MenuPos' + frmMenuMaker.txtMenu.Text + ' = 0; }');
    eCurLine := frmMenuMaker.GetColoredMenu;
    eCurLine := Copy(eCurLine, 1, Pos('$players', eCurLine) -3);
    Insert('\R%d/%d^n\w', eCurLine, Pos('^n', eCurLine));
    eStr.Add('	new Len = format(MenuBody, 255, "' + eCurLine + '", position+1, (Num / ' + IntToStr(ePlayersTo - ePlayersFrom) + ' + ((Num % ' + IntToStr(ePlayersTo - ePlayersFrom) + ') ? 1 : 0 )) )');
    eStr.Add('	new End = Start + ' + IntToStr(ePlayersTo - ePlayersFrom));
    if eExit = 0 then
      eStr.Add('	new Keys = (1<<9)')
    else
      eStr.Add('	new Keys = (1<<' + IntToStr(eExit -1) + ')');
    eStr.Add('	if (End > Num) { End = Num; }');
    eStr.Add('	');
    eStr.Add('	for(i=Start;i<End;i++) {');
    eStr.Add('		k = MenuPlayers' + frmMenuMaker.txtMenu.Text + '[i]');
    eStr.Add('		get_user_name(k, UserName, 31)');
    // Any conditions?
    if (frmMenuMaker.chkImmunity.Checked) and (frmMenuMaker.chkAlive.Checked) then
      eStr.Add('		if ((get_user_flags(k) & ADMIN_IMMUNITY) || !is_user_alive(k)) {')
    else if (frmMenuMaker.chkImmunity.Checked) then
      eStr.Add('		if (get_user_flags(k) & ADMIN_IMMUNITY) {')
    else if (frmMenuMaker.chkAlive.Checked) then
      eStr.Add('		if (!is_user_alive(k)) {');
    if (frmMenuMaker.chkImmunity.Checked) or (frmMenuMaker.chkAlive.Checked) then begin
      eStr.Add('			CurrentKey++');
      eStr.Add('			Len += format(MenuBody[Len], (255-Len), "' + PrepareItem(ePlayerFormat, True) + '", CurrentKey, UserName)');
      eStr.Add('		}');
      eStr.Add('		else {');
      eStr.Add('			Keys |= (1<<CurrentKey++)');
      eStr.Add('			Len += format(MenuBody[Len], (255-Len), "' + PrepareItem(ePlayerFormat, False) + '", CurrentKey, UserName)');
      eStr.Add('		}');
    end
    else begin
      eStr.Add('		Keys |= (i<<CurrentKey++)');
      eStr.Add('		Len += format(MenuBody[Len], (255-Len), "' + PrepareItem(ePlayerFormat, False) + '", CurrentKey, UserName)');
    end;
    eStr.Add('	}');
    eStr.Add('	if (End != Num) {');
    eStr.Add('		format(MenuBody[Len], (255-Len), "^n\w' + eNextText + '^n%s", position ? "\w' + eBackText + '" : "\w' + eExitText + '")');
    eStr.Add('		Keys |= (1<<' + IntToStr(eNext -1) + ')');
    eStr.Add('	}');
    eStr.Add('	else {');
    eStr.Add('		format(MenuBody[Len], (255-Len), "^n%s", position ? "\w' + eBackText + '" : "\w' + eExitText + '")');
    eStr.Add('	}');
    eStr.Add('	show_menu(id, Keys, MenuBody, -1)');
    eStr.Add('	return 0');
    eStr.Add('}');
    eStr.Add('');
    eStr.Add('public Pressed' + frmMenuMaker.txtMenu.Text + '(id, key) {');
    eStr.Add('	switch (key) {');
    if frmMenuMaker.chkComments.Checked then begin
      if eNext <> 0 then
        eStr.Add('		case ' + IntToStr(eNext -1) + ': ShowMenu' + frmMenuMaker.txtMenu.Text + '(id, ++MenuPos' + frmMenuMaker.txtMenu.Text + ') // More Option')
      else
        eStr.Add('		case 9: ShowMenu' + frmMenuMaker.txtMenu.Text + '(id, ++MenuPos' + frmMenuMaker.txtMenu.Text + ') // More Option');

      if eExit <> 0 then
        eStr.Add('		case ' + IntToStr(eExit -1) + ': ShowMenu' + frmMenuMaker.txtMenu.Text + '(id, --MenuPos' + frmMenuMaker.txtMenu.Text + ') // Back Option')
      else
        eStr.Add('		case 9: ShowMenu' + frmMenuMaker.txtMenu.Text + '(id, --MenuPos' + frmMenuMaker.txtMenu.Text + ') // Back Option');
    end
    else begin
      if eNext <> 0 then
        eStr.Add('		case ' + IntToStr(eNext -1) + ': ShowMenu' + frmMenuMaker.txtMenu.Text + '(id, ++MenuPos' + frmMenuMaker.txtMenu.Text + ')')
      else
        eStr.Add('		case 9: ShowMenu' + frmMenuMaker.txtMenu.Text + '(id, ++MenuPos' + frmMenuMaker.txtMenu.Text + ')');
      if eExit <> 0 then
        eStr.Add('		case ' + IntToStr(eExit -1) + ': ShowMenu' + frmMenuMaker.txtMenu.Text + '(id, --MenuPos' + frmMenuMaker.txtMenu.Text + ')')
      else
        eStr.Add('		case 9: ShowMenu' + frmMenuMaker.txtMenu.Text + '(id, --MenuPos' + frmMenuMaker.txtMenu.Text + ')');
    end;
    eStr.Add('		default: {');
    if frmMenuMaker.chkComments.Checked then
      eStr.Add('			// Get User ID and Username');
    eStr.Add('			new PlayerID = MenuPlayers' + frmMenuMaker.txtMenu.Text + '[MenuPos' + frmMenuMaker.txtMenu.Text + ' * ' + IntToStr(ePlayersTo - ePlayersFrom) + ' + key]');
    eStr.Add('			new UserName[32]');
    eStr.Add('			get_user_name(PlayerID, UserName, 31)');
    if frmMenuMaker.chkComments.Checked then
      eStr.Add('			// Do actions here')
    else
      eStr.Add('			');
    eStr.Add('		}');
    eStr.Add('	}');
    eStr.Add('	return PLUGIN_HANDLED');
    eStr.Add('}');
    frmMain.sciEditor.Lines.Text := frmMain.sciEditor.Lines.Text + #13 + eStr.Text; 
  except
    MessageBox(frmMenuMaker.Handle, PChar('An error occured while inserting code!'), 'Warning', MB_ICONWARNING);
  end;
  frmMain.SetModified;
  eStr.Free;
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
    for i := 0 to frmMenuMaker.rtfEditor.Lines.Count -1 do begin
      if Pos(eStart, LowerCase(Trim(frmMenuMaker.rtfEditor.Lines[i]))) = 1 then begin
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
    for i := 0 to frmMenuMaker.rtfEditor.Lines.Count -1 do begin
      if Pos(eStart, LowerCase(Trim(frmMenuMaker.rtfEditor.Lines[i]))) = 1 then
        Result := i;
    end;
  end;
end;

function AddIfDoesntExist(eInclude: String): Boolean;
var i: integer;
    eLine: String;
begin
  Result := True;
  eInclude := RemoveSpaces(LowerCase(eInclude));
  for i := 0 to frmMain.sciEditor.Lines.Count -1 do begin
    eLine := LowerCase(RemoveSpaces(frmMain.sciEditor.Lines[i]));
    eLine := StringReplace(eLine, '<', '', [rfReplaceAll]);
    eLine := StringReplace(eLine, '>', '', [rfReplaceAll]);
    eLine := StringReplace(eLine, '"', '', [rfReplaceAll]);
    if eLine = '#include' + eInclude then begin
      Result := False;
      exit;
    end;
  end;

  i := GetLast('#include', True);
  if i = -1 then
    i := 0;
    
  frmMain.sciEditor.Lines.Insert(i, '#include <' + eInclude + '>');
end;

end.
