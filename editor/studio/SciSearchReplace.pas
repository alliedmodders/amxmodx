//CE_Desc_Include(helpdescriptions.txt)
{$Include SciCommonDef.Inc}
unit SciSearchReplace;
{
 Unit    : SciSearchReplace
 Purpose : Search and Replace for TScintilla based on Synedit Dialogs
 Created : 20/03/2003
 Original Author  : Kiriakos Vlahos (kvlahos@london.edu)
 History : 29/09/2004 Initial Release with Delphi Scintilla Interface Components
                      Changed Editor property from TScintilla to TScintillaBase class.
                      Wasn't any need for the extra properties to use this dialog.
                      hdalis (hdalis@users.sourceforge.net)
           06/02/2005 Fixed a bug that caused the beginundoaction to be started,
                      but not finished.. i.e it treated all changes after a replace all
                      to belonging to the same undo operation..
                      hdalis (hdalis@users.sourceforge.net)
           15/02/2005 Somewhat fixed a bug which caused the component to hang when
                      search/replace for the regular expression '$'..
                      it became an endless loop..
                      if SelWord is true, we get the word under the caret as the searchword
                      instead of the need to select the word first.. If there isn't a word
                      under the caret, uses the previous searchtext if any..
                      hdalis (hdalis@users.sourceforge.net)
           07/29/2005 Fixed "Search from caret"-bug
}

interface
Uses
  Types, Classes, Controls, Forms, SciLexer;

Type

  TSciSearchReplace = class(TComponent)
  private
    FSearchForSelWord : boolean;
    FEditor : TScintillaBase;
    FSearchFromCaretInt: boolean;
    FFoundText : String;
    FOnTextFound : TNotifyEvent;
    FOnTextNotFound : TNotifyEvent;
    FOnTextReplaced : TNotifyEvent;
  protected
      procedure Notification(AComponent: TComponent;
                Operation: TOperation);  override;
  public
    // Search Options
    SearchBackwards: boolean;
    SearchCaseSensitive: boolean;
    SearchSelectionOnly: boolean;
    SearchWholeWords: boolean;
    SearchRegex: boolean;
    SearchText: string;
    SearchTextHistory: string;
    ReplaceText: string;
    ReplaceTextHistory: string;
    ReplacedCount : Integer;

    property FoundText : string read fFoundText;
    procedure DoSearchReplaceText(AReplace, ABackwards: boolean);
    procedure ShowSearchReplaceDialog(AReplace: boolean);
    constructor Create(AOwner : TComponent);override;
  published
    property SearchForSelWord : boolean read FSearchForSelWord write FSearchForSelWord;
    property SearchFromCaret: boolean read FSearchFromCaretInt write FSearchFromCaretInt;
    property Editor : TScintillaBase read FEditor write FEditor;
    property OnTextFound : TNotifyEvent read FOnTextFound write FOnTextFound;
    property OnTextNotFound : TNotifyEvent read FOnTextNotFound write FOnTextNotFound;
    property OnTextReplaced : TNotifyEvent read FOnTextReplaced write FOnTextReplaced;
  end;

implementation

Uses
 SciSearchTextDlg, SciConfirmReplaceDlg, SciReplaceTextDlg, SciSupport,sciUtils;

var ConfirmReplaceDialog: TConfirmReplaceDialog;

{ TSciSearchReplace }
constructor TSciSearchReplace.Create(AOwner : TComponent);
begin
  ReplacedCount:=0;
  SearchFromCaret:=True;
  Inherited;
end;

procedure TSciSearchReplace.DoSearchReplaceText(AReplace, ABackwards: boolean);
var
  Options: Integer;
  StartPosition, EndPosition : Integer;
  TargetStart, TargetEnd, posFind : Integer;
  APos: TPoint;
  EditRect: TRect;
  DlgRes : Integer;
  lastMatch,lenTarget,MovePastEOL : Integer;
  chNext : Integer;
  findLen : Integer;
  LenFound, LenReplaced : Integer;
//  lastMatch : Integer;
  ConfirmReplaceDialog: TConfirmReplaceDialog;
  doendundo : Boolean;
begin
  doendundo:=false;
  ConfirmReplaceDialog := nil;
  if not Assigned(FEditor) then Exit;
  Options := 0;
  if SearchCaseSensitive then
    Options := Options or SCFIND_MATCHCASE;
  if SearchWholeWords then
    Options := Options or SCFIND_WHOLEWORD;
  if SearchRegex then
    Options := Options or SCFIND_REGEXP;
  if SearchText='' then Exit;
  if ABackwards then
  begin
    if fSearchFromCaretInt and not SearchSelectionOnly then
      StartPosition := FEditor.GetSelectionStart - 1
    else if SearchSelectionOnly then
      StartPosition := FEditor.GetSelectionEnd
    else
      StartPosition := FEditor.GetLength;
    if SearchSelectionOnly then
      EndPosition := FEditor.GetSelectionStart
    else
      EndPosition := 0;
  end else
  begin
    if fSearchFromCaretInt and not SearchSelectionOnly then
      StartPosition := FEditor.GetSelectionEnd + 1
    else if SearchSelectionOnly then
      StartPosition := FEditor.GetSelectionStart
    else
      StartPosition := 0;
    if SearchSelectionOnly then
      EndPosition := FEditor.GetSelectionEnd
    else
      EndPosition := FEditor.GetLength;
  end;
  findLen:=Length(SearchText);

  with FEditor do
  begin
    SetTargetStart(StartPosition);
    SetTargetEnd(EndPosition);
    SetSearchFlags(Options);
    posFind := SearchInTarget(findLen, PChar(SearchText));
    if (posFind < 0) then
    begin
      if Assigned(FOnTextNotFound) then
        FOnTextNotFound(Self);
    end else
    begin
      lastMatch:=posFind;
      TargetStart := GetTargetStart;
      TargetEnd := GetTargetEnd;
      LenFound := TargetEnd - TargetStart;
      LenReplaced := LenFound;
      EnsureRangeVisible(TargetStart, TargetEnd);
      SetSel(TargetStart, TargetEnd);
			FFoundText := FEditor.SelText;
      if Assigned(FOnTextFound) then
        FOnTextFound(Self);

      // Replace code
      if AReplace then
      begin
        DlgRes := mrYes;

        if ConfirmReplaceDialog = nil then
          ConfirmReplaceDialog := TConfirmReplaceDialog.Create(Application);
        ReplacedCount:=0;
        while (posFind >= 0) and (DlgRes <> mrCancel) do
        begin
          lenTarget:=GetTargetEnd-GetTargetStart;
          movePastEOL:=0;
          if lenTarget<=0 then
          begin
            chNext:=GetCharAt(GetTargetEnd);
            if (chNext=10) or (chNext=13) then MovePastEOL:=1;
          end;
          if not (DlgRes = mrYesToAll) then
          begin
            APos := Point(PointXFromPosition(TargetStart), PointYFromPosition(TargetStart));
            APos := ClientToScreen(APos);
            EditRect := FEditor.ClientRect;
            EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
            EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

            ConfirmReplaceDialog.PrepareShow(EditRect, APos.X, APos.Y,
              APos.Y + 2 * FEditor.TextHeight(LineFromPosition(TargetStart)), SearchText);
            DlgRes :=ConfirmReplaceDialog.ShowModal;
            if (DlgRes = mrYesToAll) and (doendundo=false) then
            begin
              FEditor.BeginUndoAction;
              doendundo:=True;
            end;

          end;


          if DlgRes in [mrYes, mrYesToAll] then
          begin
            // Replace
            if SearchRegex then
              LenReplaced := ReplaceTargetRE(Length(ReplaceText), PChar(ReplaceText))
            else
              LenReplaced := ReplaceTarget(Length(ReplaceText), PChar(ReplaceText));
            Inc(ReplacedCount);

            lastMatch:=posFind + lenReplaced + movepastEOL;
            if lenTarget=0 then
            lastMatch:=PositionAfter(lastMatch);

            TargetEnd := TargetStart + LenReplaced -1+movePastEOL;
            if Assigned(FOnTextReplaced) then FOnTextReplaced(Self);
          end;
          if DlgRes in [mrYes, mrNo, mrYesToAll] then
          begin
            // carry on
            if lastMatch>=endPosition then
            begin
              posFind:=-1;
            end else
            begin
              if ABackwards then
              begin
                SetTargetStart(TargetStart - 1);
                SetTargetEnd(EndPosition);
              end else
              begin
                SetTargetStart(TargetEnd + 1);
                EndPosition := EndPosition + LenReplaced - LenFound;
                SetTargetEnd(EndPosition);
              end;
              SetTargetEnd(EndPosition);
              SetSearchFlags(Options);
              posFind := SearchInTarget(Length(SearchText), PChar(SearchText));
            end;
            if posFind >= 0 then
            begin
              TargetStart := GetTargetStart;
              TargetEnd := GetTargetEnd;
              lastMatch:=TargetStart;
              LenFound := TargetEnd - TargetStart;
              LenReplaced := LenFound;
              EnsureRangeVisible(TargetStart, TargetEnd);
              SetSel(TargetStart, TargetEnd);
            end;
          end else
            break;
        end;   // While
        if doendundo then
          FEditor.EndUndoAction;

        // Restore original selection if Searching in Selection
        if SearchSelectionOnly then
        begin
          if ABackwards then
            SetSel(EndPosition, StartPosition)
          else
            SetSel(StartPosition, EndPosition);
          EnsureRangeVisible(GetSelectionStart, GetSelectionEnd);
        end;
      end;  // if AReplace
    end;  //if (posFind < 0)
  end; // with FEditor

  if ConfirmReplaceDialog <> nil then
  begin
    ConfirmReplaceDialog.Free;
    ConfirmReplaceDialog := nil;
  end;
end;

procedure TSciSearchReplace.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FEditor) and (Operation = opRemove) then FEditor := nil;
end;

procedure TSciSearchReplace.ShowSearchReplaceDialog(AReplace: boolean);
var
  dlg: TForm;
  SelectedText : string;
  SearchFromCursor: Boolean;
  SearchInSelectionOnly: Boolean;
  SearchRegularExpression: Boolean;
begin
  if not Assigned(FEditor) then Exit;
  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do
  try
    // assign search options
    SearchBackwards := Self.SearchBackwards;
    SearchCaseSensitive := Self.SearchCaseSensitive;
    SearchFromCursor := Self.SearchFromCaret;
    SearchInSelectionOnly := Self.SearchSelectionOnly;
    SelectedText := FEditor.SelText;
    if (SelectedText <> '') and (Pos(#10, SelectedText) > 0) or (Pos(#13, SelectedText) > 0) then
      SearchInSelectionOnly := True
    else
      SearchInSelectionOnly := False;

    // start with last search text

    if FSearchForSelWord and not SearchInSelectionOnly
    then
    begin
      if Editor.SelectionWord(True)<>'' then
        SearchText:=Editor.SelectionWord(True)
      else
        SearchText := Self.SearchText;
    end else
      SearchText := Self.SearchText;
    SearchTextHistory := Self.SearchTextHistory;
    if AReplace then
    with dlg as TTextReplaceDialog do
    begin
      ReplaceText := Self.ReplaceText;
      ReplaceTextHistory := Self.ReplaceTextHistory;
    end;
    SearchWholeWords := Self.SearchWholeWords;
    if ShowModal = mrOK then
    begin
      Self.SearchBackwards := SearchBackwards;
      Self.SearchCaseSensitive := SearchCaseSensitive;
      Self.SearchFromCaret := SearchFromCursor;
      Self.SearchSelectionOnly := SearchInSelectionOnly;
      Self.SearchWholeWords := SearchWholeWords;
      Self.SearchRegex := SearchRegularExpression;
      Self.SearchText := SearchText;
      Self.SearchTextHistory := SearchTextHistory;

      if AReplace then
        with dlg as TTextReplaceDialog do
        begin
          Self.ReplaceText := ReplaceText;
          Self.ReplaceTextHistory := ReplaceTextHistory;
        end;
      fSearchFromCaretInt := Self.SearchFromCaret;
      if SearchText <> '' then
      begin
        DoSearchReplaceText(AReplace, Self.SearchBackwards);
        fSearchFromCaretInt := True;
      end;
      Self.SearchSelectionOnly := False;
    end;
  finally
    dlg.Free;
  end;
end;

initialization
  ConfirmReplaceDialog := nil;
end.
