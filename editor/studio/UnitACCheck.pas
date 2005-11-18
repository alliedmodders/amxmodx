unit UnitACCheck;
// collection component written by Jens Schumann and MaxHub (maximov)

interface

Uses SysUtils, Classes;

type

  TJsCollection = class(TCollection)
  private
    FCollectionname : String;
    procedure SetCollectionname(const Value: String);
  public
    procedure AfterConstruction; override;
    procedure Assign(Source : TPersistent); override;
    procedure SaveToFile(const Filename : TFilename);
    procedure SaveToStream(Stream : TStream); virtual;
    procedure LoadFromFile(const Filename : TFilename);
    procedure LoadFromStream(Stream : TStream); virtual;   
  published
    property Collectionname : String read FCollectionname write SetCollectionname;
  end;

  TmxJsCollection = class(TJsCollection)
  private
    FBinary : Boolean;
  public
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    property  Binary : Boolean read FBinary write FBinary;
  published
    property Collectionname stored false;
  end;
     

  TWriterExt = class(TWriter)
  public
    procedure WriteCollectionProperties(Value : TCollection);
  end;

  TReaderExt = class(TReader)
  public
    procedure ReadCollectionProperties(Value: TCollection);
  end;

  TACFunction = class(TCollectionItem)
  private
    FName: String;
    FItems: TStringList;
  published
    property Name: String read FName write FName;
    property Items: TStringList read FItems write FItems;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  end;


var eACList: TmxJsCollection;

implementation

uses TypInfo;

const
  iFilerBufferSize = 4096;
  FilerSignatureEx: array[1..4] of Char = 'TPF0';
  cInvalidName = ' is not a valid CollectionName!';

{ TJsCollection }

procedure TJsCollection.AfterConstruction;
begin
  inherited;
  FCollectionname := copy(className,2,length(className)-1)
end;

procedure TJsCollection.Assign(Source: TPersistent);
begin
  If Source is TJsCollection then
    FCollectionname:=TJsCollection(Source).Collectionname;
  inherited Assign(Source);
end;

procedure TJsCollection.LoadFromFile(const Filename: TFilename);
var
  FileStream : TFileStream;
begin
  Clear;
  FileStream:=TFileStream.Create(Filename,fmOpenRead);
  Try
    LoadFromStream(FileStream);
  Finally
    FileStream.Free;
    end;
end;

procedure TJsCollection.LoadFromStream(Stream: TStream);
var
  Reader  : TReaderExt;
begin
  Reader:=TReaderExt.Create(Stream,iFilerBufferSize);
  Try
    Reader.ReadCollectionProperties(Self);
  Finally
    Reader.Free;
    end;
end;

procedure TJsCollection.SaveToFile(const Filename: TFilename);
var
  FileStream : TFileStream;
begin
  FileStream:=TFileStream.Create(Filename,fmCreate);
  Try
    SaveToStream(FileStream);
  Finally
    FileStream.Free;
    end;
end;

procedure TJsCollection.SaveToStream(Stream: TStream);
var
  Writer       : TWriterExt;
begin
  Writer:=TWriterExt.Create(Stream,iFilerBufferSize);
  Try
    Writer.WriteCollectionProperties(Self);
    Writer.WriteListEnd;
  Finally
    Writer.Free;
    end;
end;

procedure TJsCollection.SetCollectionname(const Value: String);
begin
  if not IsValidIdent(Value)
  then raise exception.Create(#39+Value+#39+cInValidName)
  else FCollectionname := Value;
end;

{ TWriterExt }


procedure TWriterExt.WriteCollectionProperties(Value: TCollection);
begin
  WriteProperties(Value);
  WriteStr('items');
  inherited WriteCollection(Value);
end;

{ TReaderExt }

procedure TReaderExt.ReadCollectionProperties(Value: TCollection);
var propName:string;
    oldPos:integer;
begin
  while not EndOfList do
  begin
    oldPos :=  Position;
    propName := ReadStr;
    if propName = 'items' then
    begin
      ReadValue;
      inherited ReadCollection(Value);  
    end else  begin
      Position := oldPos;
      ReadProperty(value);
    end;
  end;  
end;


{ TmxJsCollection }

procedure TmxJsCollection.LoadFromStream(aStream: TStream);
var Reader       : TReaderExt;
    StreamInner  : TStream;
    format       : TStreamOriginalFormat;
    oldPos       : Int64;
    SigBuffer    : array[1..4] of Char;
begin
  // automatisch feststellen ob binär oder text
  oldPos := aStream.Position;
  aStream.ReadBuffer(SigBuffer[1],sizeOf(SigBuffer));
  FBinary := SigBuffer = FilerSignatureEx;
  aStream.Position := oldPos;
 
  if FBinary
  then StreamInner := aStream
  else StreamInner := TMemoryStream.Create; 
             
  try   
    if not FBinary then
    begin
      format := sofBinary;                             
      ObjectTextToBinary(aStream,StreamInner,format);
      StreamInner.Position := 0;
    end; 
                         
    Reader := TReaderExt.Create(StreamInner,iFilerBufferSize);     
    try
      Reader.ReadSignature; 
      Reader.ReadStr; // ClassName           
      FCollectionname := Reader.ReadStr; // Collectionname 

      Reader.ReadCollectionProperties(self);

      Reader.ReadListEnd;                             
      Reader.ReadListEnd;
    finally
      Reader.Free;
    end;
  finally
    if not FBinary then StreamInner.Free;
  end;
end;


procedure TmxJsCollection.SaveToStream(aStream: TStream);
var Writer       : TWriterExt;
    StreamInner  : TStream;
    format       : TStreamOriginalFormat;         
begin             
  if FBinary
  then StreamInner := aStream
  else StreamInner := TMemoryStream.Create; 
               
  try                                                     
    Writer := TWriterExt.Create(StreamInner,iFilerBufferSize);       
    try         
      Writer.WriteSignature;
      Writer.WriteStr(ClassName);                   
      Writer.WriteStr(Collectionname);   

      Writer.WriteCollectionProperties(self);
      
      Writer.WriteListEnd;
      Writer.WriteListEnd;                                 
    finally
      Writer.Free;
    end;
    if not FBinary then
    begin
      StreamInner.Position := 0; 
      format := sofText;                               
      ObjectBinaryToText(StreamInner,aStream,format);
    end;       
  finally
    if not FBinary then StreamInner.Free;
  end;
end;

{ TACFunction }

constructor TACFunction.Create(ACollection: TCollection);
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor TACFunction.Destroy;
begin
  FItems.Free;
  inherited;
end;

end.
