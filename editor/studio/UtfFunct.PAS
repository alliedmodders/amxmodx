{
 This file is translated to Delphi from the file referenced below
 by Jan Martin Pettersen (hdalis@users.sourceforge.net)
 23/07/2005.

 Some code in this file is also taken from the SciTE (Neil Hodgson)
}
// Utf8_16.cxx
// Copyright (C) 2002 Scott Kirkwood
//
// Permission to use, copy, modify, distribute and sell this code
// and its documentation for any purpose is hereby granted without fee,
// provided that the above copyright notice appear in all copies or
// any derived copies.  Scott Kirkwood makes no representations
// about the suitability of this software for any purpose.
// It is provided "as is" without express or implied warranty.
////////////////////////////////////////////////////////////////////////////////
unit UtfFunct;
interface

uses Windows,SysUtils,Classes,Math;
const
  UniBufSize=32000;
 
type
  Utf16=Word;
  Utf8=Byte;
  TUtf8Array=array[0..1] of Utf8;
  PUtf8=^TUtf8Array;
  TUtf16Array=array[0..1] of Utf16;
  PUtf16=^TUtf16Array;
  
  //uniCookie isn't used yet..
  UniMode=(uni8Bit, uni16BE, uni16LE, uniUTF8,uniCookie);
  //States for the unicode Next functions..
  eState=(eStart,e2Bytes2,e3Bytes2,e3Bytes3);
  
  // Reads UTF-16 and outputs UTF-8   
  Utf16_Iter=class(TObject)
  private
    m_eEncoding : UniMode;
    m_eState : eState;
    m_pBuf : PByte;
    m_pRead : PByte;
    m_pEnd : PByte;    
    m_nCur : Utf8;
    m_nCur16 : Utf16;
  public
    constructor Create;
    procedure Reset;
    procedure Set_(const pbuf : PByte;nLen : Cardinal;eEncoding : UniMode);
    function More : Boolean;
    procedure Next;
    function Get : Utf8;
  end;
  
  
  // Reads UTF-8 and outputs UTF-16
  Utf8_Iter =class(TObject) 
  private
    m_eEncoding : UniMode;
    m_eState : eState;
    m_pBuf : PByte;
    m_pRead : PByte;
    m_pEnd : PByte;    
    m_nCur16 : Utf16;
    procedure toStart;
    procedure Swap;    
  public
    constructor Create;
    procedure Reset;
    procedure Set_(const pbuf : PByte;nLen : Cardinal;eEncoding : UniMode);    
    function More : Boolean; //bool
    procedure Next;
    function Get : Utf16;
    function canGet : Boolean;
  end;

  // Reads UTF16 and outputs UTF8
  UtfRead=class(TObject)
  private
    m_eEncoding : UniMode;
    m_pBuf : PByte;
    m_nBufSize : Cardinal;
    m_bFirstRead : Boolean;    
    m_pNewBuf : PByte;
    m_nLen : Cardinal;
    m_Iter16 : Utf16_Iter;
  public
    constructor Create;
    destructor Destroy;override;
    function getEncoding : UniMode;
    function getNewBuf : PChar;
    function Convert(buf : PChar; len : Cardinal) : Cardinal;
    procedure Reset;
    property Encoding : UniMode read m_eEncoding;
  end;
  
  // Read in a UTF-8 buffer and write out to UTF-16 or UTF-8
  UtfWrite=class(TObject)
  private
    m_eEncoding : UniMode;
    m_pBuf : PUtf16;
    m_nBufSize : Cardinal;
    m_bFirstWrite : Boolean;
    m_pFile : TStream;
    procedure SetDestStream(Value : TStream);
    procedure SetEncoding(eType : UniMode);
  public
    constructor Create;
    function Write(const Buffer; Count : Cardinal) : LongInt;
    property DestStream : TStream read m_pFile write SetDestStream;
    property Encoding : UniMode read m_eEncoding write SetEncoding;
  end;

//Returns the UTF8 length of the buffer 'uptr'.
function UTF8Length(const wideSrc : PWideChar; wideLen : Cardinal) : Cardinal;
//Transforms UCS2 to UTF8.
procedure UTF8FromUCS2(const wideSrc : PWideChar; wideLen : Cardinal; utfDestBuf : PChar; utfDestLen : Cardinal);
function UTF8ToAnsiP(const srcBuffer : PChar;len : Integer;destBuffer : PChar) : Integer;
function DetectEncoding(buf : PByte;len : Integer;var Encoding : UniMode) : Integer;

implementation

const
  k_boms : array[uni8bit..uniUTF8,0..2] of Utf8=(
  ($00,$00,$00),
  ($FE,$FF,$00),
  ($FF,$FE,$00),
  ($EF,$BB,$BF));

function UTF8ToAnsiP(const srcBuffer : PChar;len : Integer;destBuffer : PChar) : Integer;
var
  tmpbuffer : String;
  srcLen,i,destLen : Integer;
begin
  Result:=0;
  if (not assigned(srcBuffer)) or (not assigned(destBuffer)) then Exit;
  if len=-1 then
    srcLen:=Length(srcBuffer)
  else
    srcLen:=len;
  tmpbuffer:=UTF8ToAnsi(Copy(srcBuffer,1,srcLen));
  destLen:=Length(tmpbuffer);
  for i:=1 to destLen do
    destBuffer[i-1]:=tmpbuffer[i];
  destBuffer[destLen]:=#0;
  Result:=destLen;
end;

function UTF8Length(const wideSrc : PWideChar; wideLen : Cardinal) : Cardinal;
var
  i,len : Cardinal;
  uch : Cardinal;
begin
	len := 0;
  i:=0;
  while((i<wideLen) and (Cardinal(wideSrc[i])<>0)) do
  begin
    uch:=Cardinal(wideSrc[i]);
    if (uch < $80) then
      Inc(len)
    else if (uch < $800) then
      Inc(len,2)
    else
      Inc(len,3);
    Inc(i);
  end;
  Result:=len;
end;

procedure UTF8FromUCS2(const wideSrc : PWideChar; wideLen : Cardinal; utfDestBuf : PChar; utfDestLen : Cardinal);
var
  k : Integer;
  i : Cardinal;
  uch : Cardinal;
begin
	k:= 0;
  i:=0;
  while((i<wideLen)and(Cardinal(wideSrc[i])<>0)) do
  begin
    uch:=Cardinal(wideSrc[i]);
    if uch<$80 then
    begin
      utfDestBuf[k] := Char(uch);
      Inc(k);
    end else
    if (uch<$800) then
    begin
      utfDestBuf[k]:=Char($C0 or (uch shr 6));
      Inc(k);
      utfDestBuf[k] := Char($80 or (uch and $3f));
      Inc(k);
    end else
    begin
			utfDestBuf[k] := Char($E0 or (uch shr 12));
      Inc(k);
			utfDestBuf[k] := Char($80 or ((uch shr 6) and $3f));
      Inc(k);
			utfDestBuf[k] := Char($80 or (uch and $3f));
      Inc(k);
    end;
  end;
  utfDestBuf[utfDestLen]:=#0;
end;

function DetectEncoding(buf : PByte;len : Integer;var Encoding : UniMode) : Integer;
var
  nRet : Integer;
  pbTmp : PByteArray;
begin
	Encoding := uni8bit;
  pbTmp:=PByteArray(buf);
	nRet := 0;
	if (len > 1) then
  begin
		if ((pbTmp[0]=k_Boms[uni16BE][0]) and (pbTmp[1]=k_Boms[uni16BE][1])) then
    begin
			Encoding := uni16BE;
			nRet := 2;
		end else 
    if ((pbTmp[0]=k_Boms[uni16LE][0]) and (pbTmp[1]=k_Boms[uni16LE][1])) then
    begin
			Encoding := uni16LE;
			nRet := 2;
		end else 
    if ((len>2) and (pbTmp[0]=k_Boms[uniUTF8][0]) and (pbTmp[1]=k_Boms[uniUTF8][1]) and (pbTmp[2]=k_Boms[uniUTF8][2])) then
    begin
			Encoding := uniUTF8;
			nRet := 3;
		end;
	end;
	Result:=nRet;
end;

   
constructor Utf16_Iter.Create;
begin
  Reset;
end;
procedure Utf16_Iter.Reset;
begin
	m_pBuf := nil;
	m_pRead := nil;
	m_pEnd := nil;
	m_eState := eStart;
	m_nCur := 0;
	m_nCur16 := 0;
	m_eEncoding := uni8bit;
end;
procedure Utf16_Iter.Set_(const pbuf : PByte;nLen : Cardinal;eEncoding : UniMode);
begin
	m_pBuf := pBuf;
	m_pRead := pBuf;
	m_pEnd := pBuf;
  Inc(m_pEnd,nLen);
	m_eEncoding := eEncoding;
  Next;
end;

procedure Utf16_Iter.Next;
begin
	case m_eState of
	eStart:
  begin
		if (m_eEncoding = uni16LE) then
    begin
			m_nCur16 := Utf16(m_pRead^);
      Inc(m_pRead);      
			m_nCur16 := m_nCur16 or Utf16((m_pRead^ shl 8));
		end else 
    begin
			m_nCur16 := Utf16(m_pRead^ shl 8);
      Inc(m_pRead);
			m_nCur16 := m_nCur16 or m_pRead^;
		end;
		Inc(m_pRead);

		if (m_nCur16 < $80) then
    begin
			m_nCur := Byte(m_nCur16 and $FF);
			m_eState := eStart;
		end else 
    if (m_nCur16 < $800) then
    begin
			m_nCur := Byte($C0 or (m_nCur16 shr 6));
			m_eState := e2Bytes2;
		end else 
    begin
			m_nCur := Byte($E0 or (m_nCur16 shr 12));
			m_eState := e3Bytes2;
		end;
    
  end;
	e2Bytes2:
  begin
		m_nCur := Byte($80 or (m_nCur16 and $3F));
		m_eState := eStart;
  end;
	e3Bytes2:
  begin
		m_nCur := Byte($80 or ((m_nCur16 shr 6) and $3F));
		m_eState := e3Bytes3;
  end;
	e3Bytes3:
  begin
		m_nCur := Byte($80 or (m_nCur16 and $3F));
		m_eState := eStart;
  end;
  end;
end;

function Utf16_Iter.More : Boolean;
begin
  Result:=Cardinal(m_pRead) <= Cardinal(m_pEnd);
end;

function Utf16_Iter.Get : Utf8;
begin
  Result:=m_nCur;
end;

constructor Utf8_Iter.Create;
begin
  Reset;
end;

procedure Utf8_Iter.Reset;
begin
	m_pBuf := nil;
	m_pRead := nil;
	m_pEnd := nil;
	m_eState := eStart;
	m_nCur16 := 0;
	m_eEncoding := uni8bit; 
end;

procedure Utf8_Iter.Set_(const pbuf : PByte;nLen : Cardinal;eEncoding : UniMode);
begin
	m_pBuf := pBuf;
	m_pRead := pBuf;
	m_pEnd := pBuf;
  Inc(m_pEnd,nLen);
	m_eEncoding := eEncoding;
  Next;
end;

procedure Utf8_Iter.Next;
begin
	case (m_eState) of
    eStart:
    begin
      if (($E0 and m_pRead^) = $E0) then
      begin
        m_nCur16 := Utf16(((not $E0) and m_pRead^) shl 12);
        m_eState := e3Bytes2;
      end else if (($C0 and m_pRead^) = $C0) then
      begin
        m_nCur16 := Utf16((not $C0 and m_pRead^) shl 6);
        m_eState := e2Bytes2;
      end else 
      begin
        m_nCur16 := m_pRead^;
        toStart;
      end;
    end;
    e2Bytes2:
    begin
      m_nCur16 :=m_nCur16 or utf8($3F and m_pRead^);
      toStart;
    end;
    e3Bytes2:
    begin
      m_nCur16 :=m_nCur16 or utf16(($3F and m_pRead^) shl 6);
      m_eState := e3Bytes3;
    end;
    e3Bytes3:
    begin
      m_nCur16 :=m_nCur16 or utf8($3F and m_pRead^);
      toStart;
    end;
	end;
	Inc(m_pRead);
end;

function Utf8_Iter.More : Boolean;
begin
  Result:=Cardinal(m_pRead) <= Cardinal(m_pEnd);
end;

function Utf8_Iter.Get : Utf16;
begin
  Result:=m_nCur16;
end;
function Utf8_Iter.canGet : Boolean;
begin
  Result:=m_eState = eStart;
end;
procedure Utf8_Iter.toStart;
begin
	m_eState := eStart;
	if (m_eEncoding = uni16BE) then
		Swap;
end;
procedure Utf8_Iter.Swap;
var
  p : PUtf8;
  swapbyte : Utf8;
begin
	p := PUtf8(@m_nCur16);	
  swapbyte := p[0];
	p[0]:= p[1];
  p[1]:=swapbyte;
end;

constructor UtfRead.Create;
begin
	m_eEncoding := uni8bit;
	m_nBufSize := 0;
	m_pNewBuf := nil;
	m_bFirstRead := True;
end;

destructor UtfRead.Destroy;
begin
	if ((m_eEncoding <> uni8bit) and (m_eEncoding <> uniUTF8)) then
  begin
		if assigned(m_pNewBuf) then FreeMem(m_pNewBuf);
	end;
  inherited;
end;

function UtfRead.getEncoding : UniMode;
begin
  Result:=m_eEncoding;
end;
function UtfRead.getNewBuf : PChar;
begin
  Result:=PChar(m_pNewBuf);
end;

procedure UtfRead.Reset;
begin
  m_bFirstRead:=True;
  m_nBufSize:=0;
  m_eEncoding :=uni8Bit;
end;

function UtfRead.Convert(buf : PChar; len : Cardinal) : Cardinal;
var
  nSkip : Cardinal;
  newSize : Cardinal;
  pCur,pTemp : PByte;
begin
  m_Iter16:=Utf16_Iter.Create;
  try
    m_pBuf := PByte(buf);
    m_nLen := len;

    nSkip := 0;
    if (m_bFirstRead) then
    begin
      nSkip := DetectEncoding(m_pBuf,m_nLen,m_eEncoding);
      m_bFirstRead := False;
    end;

    if (m_eEncoding = uni8bit) then
    begin
      // Do nothing, pass through
      m_nBufSize := 0;
      m_pNewBuf := m_pBuf;
      Result:=len;
      Exit;
    end;

    if (m_eEncoding = uniUTF8) then
    begin
      // Pass through after BOM
      m_nBufSize := 0;
      m_pNewBuf := m_pBuf;
      Inc(m_pNewBuf,nSkip);
      Result:=len - nSkip;
      Exit;
    end;

    // Else...
    //newSize := len + len div 2 + 1;
    newSize:=len*2+1;
    if (m_nBufSize <> newSize) then
    begin
      FreeMem(m_pNewBuf);
      m_pNewBuf:=nil;
      GetMem(m_pNewBuf,newSize);
      m_nBufSize := newSize;
    end;

    pCur := m_pNewBuf;
    pTemp:=m_pBuf;
    Inc(pTemp,nSkip);
    m_Iter16.Set_(pTemp, len - nSkip, m_eEncoding);
    while(m_Iter16.More) do
    begin
      pCur^:=m_Iter16.Get;
      Inc(PCur);
      m_Iter16.Next;
    end;
	// Return number of bytes writen out
  finally
    FreeAndNil(m_Iter16);
  end;
	Result:=Cardinal(pCur) - Cardinal(m_pNewBuf);
end;

constructor UtfWrite.Create;
begin
	m_eEncoding := uni8bit;
	m_pFile := nil;
	m_pBuf := nil;
	m_bFirstWrite := true;
	m_nBufSize := 0;
end;

procedure UtfWrite.SetEncoding(eType : UniMode);
begin
  m_eEncoding := eType;
end;
procedure UtfWrite.SetDestStream(Value : TStream);
begin
  m_pFile:=Value;
  m_bFirstWrite:=True;
end;

function UtfWrite.Write(const Buffer; Count : Cardinal) : LongInt;
var
  iter8 : Utf8_Iter;
  pCur : ^Utf16;
  ret : LongInt;
  pTemp : PChar;
begin
  if Count=0 then
  begin
    Result:=0;
    Exit;
  end;
  iter8:=Utf8_Iter.Create;
  try
    if (not assigned(m_pFile)) then
    begin
      Result:=0;
      Exit;
    end;
    if (m_eEncoding = uni8bit) then
    begin
      // Normal write
      m_bFirstWrite:=False;
      Result:=m_pFile.Write(PChar(Buffer)^, Count);
      Exit;
    end;
  
    if (m_eEncoding = uniUTF8) then
    begin
      pTemp:=PChar(Buffer);
      if (m_bFirstWrite) then
      begin
        m_pFile.Write(k_Boms[m_eEncoding], 3);
        m_bFirstWrite := false;
      end;
      Result:=m_pFile.Write(pTemp^, Count);
      Exit;
    end;
  
    if (Count > m_nBufSize) then
    begin
      m_nBufSize := Count;
      if assigned(m_pBuf) then FreeMem(m_pBuf);
      m_pBuf := nil;
      GetMem(m_pBuf,SizeOf(Utf16)*(Count+1));
    end;
  
    if (m_bFirstWrite) then
    begin
      if ((m_eEncoding = uni16BE) or (m_eEncoding = uni16LE)) then
      begin
        // Write the BOM
        m_pFile.Write(k_Boms[m_eEncoding],2);
      end;
      m_bFirstWrite := false;
    end;
  
    iter8.set_(PByte(Buffer), Count, m_eEncoding);
    pCur := @m_pBuf[0];
    while(iter8.More) do
    begin
      if (iter8.canGet) then
      begin
        pCur^ := iter8.Get;
        Inc(pCur);
      end;
      iter8.Next;
    end;
    ret := m_pFile.Write(m_pBuf^,Cardinal(pCur)-Cardinal(m_pBuf));
  finally
    if assigned(iter8) then FreeAndNil(iter8);
  end;
	Result:=ret;
end;

end.