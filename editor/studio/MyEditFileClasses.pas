{
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specificlanguage governing rights and limitations under
the License.

The Original Code is MyEditFileClasses.pas
The Original Code is part of the MyEditor project, written by
Jan Martin Pettersen for the Delphi Scintilla Interface Components
Copyright © 2004,2005, Jan Martin Pettersen. All Rights Reserved.
The Initial Developer of the Original Code is Jan Martin Pettersen
}
{
	History:   23/07/2005 Initial Release
}
unit MyEditFileClasses;
interface
uses Classes,SciLexer,UtfFunct,SciStreamDefault;

type

  {This code is somewhat a mess at the moment, but it seems to work however..}

  TSciMyStream=class(TSciStreamDefault)
  protected
    FMode : UniMode;
  public
    constructor Create(Editor : TScintillaBase);override;
    procedure SaveToStream(Stream : TStream);override;
    procedure LoadFromStream(Stream : TStream);override;
    function GetData : Integer;override;
    procedure SetData(Value : Integer);override;
  end;

implementation
uses SciSupport,SysUtils,Math,sciUtils;

constructor TSciMyStream.Create(Editor : TScintillaBase);
begin
  inherited Create(Editor);
  FMode:=uni8bit;
end;


procedure TSciMyStream.SaveToStream(Stream : TStream);
var
  ms : TMemoryStream;
  UniString : String;
  UString : PChar;
  nullch : AnsiChar;
  Writer : UtfWrite;
  procedure internalSaveToStream(Stream : TStream);
  var
    buf : array[0..UniBufSize+1] of Char;
    docLen,i : LongInt;
    grabSize : LongInt;
    rng : TTextRange;
  begin
    if (not assigned(Stream)) then Exit;
    with FEditor do
    begin
      i:=0;
      docLen:=GetLength;
      if docLen=0 then Exit;
      while i<docLen do
      begin
        grabSize:=docLen-i;
        if grabSize>UniBufSize then
          grabSize:=UniBufSize;
        rng.chrg.cpMin:=i;
        rng.chrg.cpMax:=i+grabSize;
        rng.lpstrText:=@buf;
        FEditor.GetTextRange(@rng);
        Stream.Write(buf,grabSize);
        Inc(i,grabSize);
      end;
    end;
  end;
begin
  ms:=nil;
  Writer:=nil;
  if not assigned(FEditor) then Exit;
  try
    try
      Writer:=UtfWrite.Create;
      Writer.Encoding:=FMode;
      Writer.DestStream:=Stream;
      ms:=TMemoryStream.Create;
      nullch:=#0;
      if FEditor.GetCodePage<>SC_CP_UTF8 then //If the control isn't using the UTF8 format.
      begin
        if FMode<>uni8bit then //Save in a unicode format
        begin
          internalSaveToStream(ms); //Save to the memorystream used for conversion
          ms.Seek(0,soFromEnd);
          ms.Write(nullch,SizeOf(nullch)); //Write ending null, so the memorybuffer can be used as a string
          UniString:=AnsiToUTF8(PChar(ms.Memory)); //Convert to UTF8 so the encoder can work with it.
          ms.Clear;
          UString:=PChar(UniString);
          Writer.Write(UString,Length(UString)); //Perform encoding
        end else
        begin
          internalSaveToStream(Stream);
        end;
      end else //otherwise..
      begin
        internalSaveToStream(ms); //Save to the memorystream used for conversion
        ms.Seek(0,soFromEnd);
        ms.Write(nullch,SizeOf(nullch)); //Write ending null, so the memorybuffer can be used as a string
        UString:=PChar(ms.Memory);
        if FMode=uni8bit then //If we are ordered to save in Ansi,8bit format.
        begin
          UniString:=UTF8ToAnsi(UString); //Convert the UTF8 retrieved from the control to Ansi
          ms.Clear;
          UString:=PChar(UniString);
        end;
        Writer.Write(UString,Length(UString));
      end;
    except
      raise;
    end;
  finally
    if assigned(ms) then FreeAndNil(ms);
    if assigned(Writer) then FreeAndNil(Writer);
  end;
end;

procedure TSciMyStream.LoadFromStream(Stream : TStream);
var
  buf : array[0..UniBufSize+1] of Char;
  OldUseUnicode : Boolean;
  NumRead,NumCvt : Integer;
  Converter: UtfRead;
  siz : LongInt;
  nbuf : PChar;
  oldoffs : Integer;
  tmpstr : String;
  ms : TMemoryStream;
begin
  Converter:=nil;
  ms:=nil;
  if not assigned(Stream) then Exit;
  if not assigned(FEditor) then Exit;
  try
    try
      if Stream.Size>2 then
      begin
        oldoffs:=Stream.Position;
        Stream.Read(buf,3);
        DetectEncoding(PByte(@buf),3,FMode); //Detect the encoding used in the file.. uni8bit is returned if unknown/ansi.
        Stream.Seek(oldoffs,soFromBeginning);
        if FMode<>uni8bit then //If not ansi/unknown
        begin
            Converter:=UtfRead.Create;
            with FEditor do
            begin
              OldUseUnicode := (FEditor.GetCodePage=SC_CP_UTF8);
              siz:=Stream.Size;
              if OldUseUnicode=False then
              begin
                ms:=TMemoryStream.Create; //Create a temporary memorystream to store the utf8 data to be converted to ansi
              end;
              NumRead:=Stream.Read(buf,Min(siz,UniBufSize));
              while (NumRead>0) do
              begin
                NumCvt:=Converter.Convert(buf,NumRead);
                nbuf:=Converter.getNewBuf;
                if (assigned(nbuf)) and (NumCvt>0) then
                begin
                  if OldUseUnicode=False then
                  begin
                    ms.Write(nbuf^,NumCvt);
                  end else
                  FEditor.AddText(NumCvt,nbuf);
                end;
                NumRead:=Stream.Read(buf,Min(siz,UniBufSize));
                Dec(siz,NumRead);
              end;
              if (OldUseUnicode=False) and (assigned(ms)) then //Do the conversion of the UTF8 data to Ansi
              begin
                ms.Seek(0,soFromEnd);
                buf[0]:=#0; //Write the ending null so we can use the memorybuffer as a string
                ms.Write(buf,1);
                tmpstr:=UTF8ToAnsi(PChar(ms.Memory));//Convert to ANSI
                ms.Clear;
                FEditor.AddTextStr(tmpstr);
                tmpstr:='';
              end;
            end;
        end else
        begin
          if FEditor.GetCodePage=SC_CP_UTF8 then //If the editor control is expecting UTF8 and we have Ansi/Unknown data, convert it to UTF8..
          begin
            ms:=TMemoryStream.Create;
            ms.CopyFrom(Stream,0);
            ms.Seek(0,soFromEnd);
            buf[0]:=#0; //Write the ending null so we can use the memorybuffer as a string
            ms.Write(buf,1);
            tmpstr:=AnsiToUTF8(PChar(ms.Memory)); //Convert to UTF8
            ms.Clear;
            nbuf:=PChar(tmpstr);
            ms.Write(nbuf^,Length(tmpstr));
            ms.Seek(0,soFromBeginning);
            inherited LoadFromStream(ms);
          end else //Otherwise just call the default loader
          inherited LoadFromStream(Stream);
        end;
      end else //otherwise just call the default loader
        inherited LoadFromStream(Stream);
    except
      raise;
    end;
  finally
    if assigned(Converter) then FreeAndNil(Converter);
    if assigned(ms) then FreeAndNil(ms);
  end;
end;

function TSciMyStream.GetData : Integer;
begin
  Result:=Integer(FMode); //We return the current mode/last mode detected when we loaded a file.
end;
procedure TSciMyStream.SetData(Value : Integer);
begin
  FMode:=UniMode(Value); //Sets a new mode
end;


end.
