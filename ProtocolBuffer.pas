{
Subject to the terms of the MIT license: Copyright (c) 2025 HHS Software Corp.

Release date:  December 2025. release # 3.

ProtocolBuffer.pas - is an engine to send and recv data in Google's Protocol Buffer standard.

This code by Ross Hemingway - HHS Software Corp.
  https://github.com/HHS-Software/Protocol-Buffer_Delphi_FPC

This is a generic engine to send / recv data (not json or xml), via the Google Protocol Buffer v3.
  Most of the standards are applied here, but not all.  What we have here is the basics to get data
    moved back and forth.  The 6 wire datatypes are coded here.
There is a second file that you customize with the specifics of each of the proto message definition.
  There is a code generator utility called protoc-pascal to do this.

ProtocolBuffer.pas is designed from scratch using information from various sources.

1/  The Protocol Buffer documentation source of Google at https://github.com/protocolbuffers/protobuf
2/  Test tools like  https://www.protobufpal.com/  and  https://protobuf-decoder.netlify.app

The objective of this library is to simplify all aspects as much as possible, and just address the
  read / write of data to the serial stream.  The users project code will handle the larger objects, memory,
  proto record types, storing of data, etc.

This code is good for 32/64 bit and unicode use.  This code will convert to/from UTF8 as needed.
  Note: the code here handles the all proto v3 specifications.  But it does not do many of the complex problems
    addressed by the Google sample code and classes.
}


unit ProtocolBuffer;

interface

{$IFDEF FPC}
{$mode delphi}{$H+}
uses SysUtils, Classes;
{$ELSE}
uses System.Classes, System.SysUtils, System.AnsiStrings;
{$ENDIF}

type
  EProtocolBufferRecursion = class(Exception);
            // The basic Google Wire data types.
  TProtoBufWireDataType = (pbwtVarInt, pbwtFixed64, pbwtLenDelimit, pbwtStartGrp, pbwtEndGrp, pbwtFixed32, pbwtUnknown);
            // The specific data types defined in .proto message structs
  TProtoBufWireDataSubType = (pbwstVarIntInt32, pbwstVarIntInt64, pbwstVarIntUInt32, pbwstVarIntUInt64, pbwstVarIntSInt32, pbwstVarIntSInt64, pbwstVarIntBool, pbwstVarIntEnum,
                             pbwstI64Fixed64, pbwstI64SFixed64, pbwstI64Double,
                             pbwstLenString, pbwstLenBytes, pbwstNestedMessage, pbwstRepteadNestedMessage, pbwstMapMessage, // added three helper types
                             pbwstSGroup,
                             pbwstEGroup,
                             pbwstI32Fixed32, pbwstI32SFixed32, pbwstI32Float,
                             pbwstUnknown, pbwstUnUsed);
  TProtoBufWireDataSubTypeSet = set of TProtoBufWireDataSubType;

  // Some generic types for use as repeat fields - extend as needed.
  TPBArrayOfInteger = array of Integer;
  TPBArrayOfInt64 = array of Int64;
  TPBArrayOfString = array of string;
  TPBArrayOfDouble = array of Double;
  TPBArrayOfSingle = array of Single;
  TPBArrayOfBoolean = array of Boolean;

  // Some generic types for use as nested map array - extend as needed.
  PTProtoBufMapStringString = ^TProtoBufMapStringString;
  TProtoBufMapStringString = record
    Key: string;    // 1
    Value: string;  // 2
  end;
  TProtoBufMapStringStringArray = array of TProtoBufMapStringString;
  PTProtoBufMapStringInteger = ^TProtoBufMapStringInteger;
  TProtoBufMapStringInteger = record
    Key: string;    // 1
    Value: Integer; // 2
  end;
  TProtoBufMapStringIntegerArray = array of TProtoBufMapStringInteger;
  PTProtoBufMapStringDouble = ^TProtoBufMapStringDouble;
  TProtoBufMapStringDouble = record
    Key: string;    // 1
    Value: Double;  // 2
  end;
  TProtoBufMapStrDoubleArray = array of TProtoBufMapStringDouble;
  PTProtoBufMapIntegerInteger = ^TProtoBufMapIntegerInteger;
  TProtoBufMapIntegerInteger = record
    Key: Integer;   // 1
    Value: Integer; // 2
  end;
  TProtoBufMapIntegerIntegerArray = array of TProtoBufMapIntegerInteger;


  //  The main object class.  All Proto objects inherit from this.
  TProtocolBuffer = class(TObject)
    FProtoFieldMaxID: Integer;
    FTouchedBits: array of Boolean;
    FParentObject: TProtocolBuffer;
    FParentIndex: Integer;
  private
    function GetTouched(Index: Integer): Boolean;
    procedure SetTouched(Index: Integer; const Value: Boolean);
    function EncodeToStream_Internal(Stream: TStream; var RecurseN: Integer): Integer;
    function DecodeToProto_Internal(Stream: TStream; Bytes: Integer; var RecurseN: Integer): Integer;
    procedure DecodeDatafromStream(WireType: TProtoBufWireDataType;
            DataSubType: TProtoBufWireDataSubType; Stream: TStream;
            var Bytes: Integer; var FieldIndexIDRepeat: LongWord);
    class procedure EncodeDataToStream(WireType: TProtoBufWireDataType; DataSubType: TProtoBufWireDataSubType;
            pValue: Pointer; Size: Integer; Stream: TStream);
  public
    constructor Create(MaxID: Integer);
    destructor Destroy; override;
    // Use this class method as a simple encoder, only when a flat proto struct exists.
    class procedure AppendSimpleValue(FieldIdx: Integer; DataSubType: TProtoBufWireDataSubType;
            pValue: Pointer; Stream: TStream);
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; virtual; abstract;
    function ReadFieldByIndex(var FieldldIndexRepeat: LongWord; var Size: Integer): Pointer; virtual; abstract;
    procedure WriteFieldByIndex(FieldldIndexRepeat: LongWord; Value: Pointer; Size: Integer); virtual; abstract;
    function GetNestedArrayObjectByIndex(var FieldIndexIDRepeat: LongWord): Pointer; virtual;
    procedure SetParentDetails(ParentIndex: Integer; ParentObject: TProtocolBuffer);
    // Removing a touched flag prevents sending zeros and empty strings
    // note: ProtoBuf standard initializes all fields to zero / empty, so unsent fields are still valid.
    procedure UnsetTouchedFlagForDefaultVals(DataSubTypes: TProtoBufWireDataSubTypeSet);
    procedure ResetTouchedFlags;
    function LoSetField: Integer;
    function HiSetField: Integer;
    // The main methods to serialize / deserialize data to a stream object
    function EncodeToStream(Stream: TStream): Integer;
    function DecodeToProto(Stream: TStream; Bytes: Integer): Integer;
    // Max Id might differ from count if reservations in .proto message exist
    property FieldMaxID: Integer read FProtoFieldMaxID;
    // Touched field number match the proto index number (1 based)
    property FieldTouched[Index: Integer]: Boolean read GetTouched write SetTouched;
  end;


  // Generic conversion functions
  function ProtoBufSubTypeToWireType(WireDataSubType: TProtoBufWireDataSubType): TProtoBufWireDataType;
  function ProtoBufTagWireType(Tag: Word): TProtoBufWireDataType;  // the wire type (the lower 3 bits).
  function ProtoBufTagFieldIndex(Tag: Word): Word;  // the field number (the upper 29 bits).
  function ProtoBufMakeTag(FieldIndex: Word; WireDataType: TProtoBufWireDataType): Word; overload;
  function ProtoBufMakeTag(FieldIndex: Word; WireDataSubType: TProtoBufWireDataSubType): Word; overload;

const
  PB_FIELD_IDX_MASK  = $0FFF;
  PB_FIELD_SUB_IDX_MASK = $F000;
  PB_FIELD_REPEAT_IDX_MASK = $FFFF0000;
  PB_FIELD_SUB_IDX_OFFSET = 12;
  PB_FIELD_REPEAT_IDX_OFFSET = 16;

implementation

const
  PB_TAG_TYPE_BITS = 3;
  PB_TAG_TYPE_MASK = (1 shl PB_TAG_TYPE_BITS) - 1;
  ProtoBufRecursionLimit = 64;
  VARINT_BYTES = 10;


{ TProtocolBuffer }

constructor TProtocolBuffer.Create(MaxID: Integer);
begin
  inherited Create;
  SetLength(FTouchedBits, MaxID + 1);
  FProtoFieldMaxId := MaxID;
end;

destructor TProtocolBuffer.Destroy;
begin
  FTouchedBits := nil;
  inherited;
end;


{  **  encode / decode tags  **  }

function ProtoBufTagWireType(Tag: Word): TProtoBufWireDataType;
var Value: Cardinal;
begin
  Result := pbwtUnKnown;
  Value := Tag and PB_TAG_TYPE_MASK;
  if Value < Cardinal(pbwtUnKnown) then
    Result := TProtoBufWireDataType(Value);
end;

function ProtoBufTagFieldIndex(Tag: Word): Word;
begin
  Result := Tag shr PB_TAG_TYPE_BITS;
end;

function ProtoBufMakeTag(FieldIndex: Word; WireDataType: TProtoBufWireDataType): Word;
begin
  Result := (FieldIndex shl PB_TAG_TYPE_BITS) or Ord(WireDataType);
end;

function ProtoBufSubTypeToWireType(WireDataSubType: TProtoBufWireDataSubType): TProtoBufWireDataType;
begin
  case WireDataSubType of
    pbwstVarIntInt32..pbwstVarIntEnum: Result := pbwtVarInt;    // 0
    pbwstI64Fixed64..pbwstI64Double: Result := pbwtFixed64;     // 1
    pbwstLenString..pbwstMapMessage: Result := pbwtLenDelimit;  // 2
    pbwstSGroup: Result := pbwtStartGrp;                        // 3
    pbwstEGroup: Result := pbwtEndGrp;                          // 4
    pbwstI32Fixed32..pbwstI32Float: Result := pbwtFixed32;      // 5
    else
      Result := pbwtUnknown;
  end;
end;

function ProtoBufMakeTag(FieldIndex: Word; WireDataSubType: TProtoBufWireDataSubType): Word;
var WireDataType: TProtoBufWireDataType;
begin
  WireDataType := ProtoBufSubTypeToWireType(WireDataSubType);
  Result := (FieldIndex shl PB_TAG_TYPE_BITS) or Ord(WireDataType);
end;


{ ** Decode / read / de-serialize data from stream ** }

function ReadVarInt(var Bytes: Integer; Stream: TStream): UInt64;
var i: Integer; temp64: UInt64; aByte: Byte;
begin
  Result := 0;
  i := 0;
  while (i < VARINT_BYTES) and (Bytes > 0) do
    begin
      Stream.Read(aByte, 1);
      dec(Bytes);
      temp64 := aByte and $7F;
      temp64 := temp64 shl (i * 7);
      Result := Result or temp64;
      if aByte and $80 = 0 then  // MSB marker test
        Break;
      inc(i);
    end;
end;

procedure TProtocolBuffer.DecodeDatafromStream(
  WireType: TProtoBufWireDataType; DataSubType: TProtoBufWireDataSubType;
  Stream: TStream; var Bytes: Integer; var FieldIndexIDRepeat: LongWord);

  var aByte: Byte;
    i: Integer;
    temp64, Value64: UInt64;
    temp32, Value32: UInt32;
    pa: pAnsiChar;
    p: Pointer;
    ValueS64: Int64;
    ValueS32: Int32;
    s: string;

begin
  case WireType of

    // 0	VARINT
    pbwtVarInt: begin
      Value64 := ReadVarInt(Bytes, Stream);
      case DataSubType of
        pbwstVarIntSInt32: begin        // zigzag
          Value32 := UInt32(Value64);
          if Value32 and 1 > 0 then // -ve
            begin
              Value32 := Value32 shr 1;
              ValueS32 := Integer(Value32 xor $FFFFFFFF);
            end
          else
            ValueS32 := Value32 shr 1;  // +ve
          WriteFieldByIndex(FieldIndexIDRepeat, @ValueS32, 0);
        end;
        pbwstVarIntSInt64: begin        // zigzag
          if Value64 and 1 > 0 then // -ve
            begin
              Value64 := Value64 shr 1;
              ValueS64 := Int64(Value64 xor $FFFFFFFFFFFFFFFF);
            end
          else
            ValueS64 := Int64(Value64 shr 1); // +ve
          WriteFieldByIndex(FieldIndexIDRepeat, @ValueS64, 0);
        end
      else
        WriteFieldByIndex(FieldIndexIDRepeat, @Value64, 0);
      end;
    end;

    // 1	I64
    pbwtFixed64: begin
      Value64 := 0;
      for i := 0 to 7 do
        begin
          Stream.Read(aByte, 1);
          dec(Bytes);
          temp64 := aByte;
          temp64 := temp64 shl (i * 8);
          Value64 := Value64 or temp64;
        end;
      WriteFieldByIndex(FieldIndexIDRepeat, @Value64, 0);
    end;

    // 5	I32
    pbwtFixed32: begin
      Value32 := 0;
      for i := 0 to 3 do
        begin
          Stream.Read(aByte, 1);
          dec(Bytes);
          temp32 := aByte;
          temp32 := temp32 shl (i * 8);
          Value32 := Value32 or temp32;
        end;
      WriteFieldByIndex(FieldIndexIDRepeat, @Value32, 0);
    end;

    // 2	LEN
    pbwtLenDelimit: begin
      case DataSubType of
        pbwstLenString: begin
          // get the LEN VarInt
          Value64 := ReadVarInt(Bytes, Stream);
          if Value64 = 0 then
            Exit;
          pa := AllocMem(Value64 + 1);  // nulled chars
          Stream.Read(pa^, Value64);
          dec(Bytes, Value64);
          s := UTF8ToString(pa);
          WriteFieldByIndex(FieldIndexIDRepeat, @s, Integer(Value64));
          FreeMem(pa);
        end;

        pbwstLenBytes: begin
          // get the LEN VarInt
          Value64 := ReadVarInt(Bytes, Stream);
          if Value64 = 0 then
            Exit;
          // receiver allocates its own mem and copies to it.
          GetMem(p, Integer(Value64));
          Stream.Read(p^, Value64);
          WriteFieldByIndex(FieldIndexIDRepeat, p, Integer(Value64));
          FreeMem(p);
          dec(Bytes, Value64);
        end;
      end;
    end;
  end;
end;

function TProtocolBuffer.DecodeToProto_Internal(Stream: TStream; Bytes: Integer; var RecurseN: Integer): Integer;
var BytesStart, Size: Integer;
    WireType: TProtoBufWireDataType;
    Tag, LastTag: Word;
    FieldIndexIDRepeat: LongWord;
    FieldIdx, FieldSubIdx, Repeated: Word;
    Value64: UInt64;
    DataSubType: TProtoBufWireDataSubType;
    pValue: Pointer;

begin
  BytesStart := Bytes;
  Repeated := 0;
  LastTag := 0;

  while (Stream.Position < Stream.Size) and (Bytes > 0) do
    begin
      Tag := ReadVarInt(Bytes, Stream);
      if Tag = LastTag then
        inc(Repeated)
      else
        Repeated := 0;
      LastTag := Tag;

      FieldIdx := ProtoBufTagFieldIndex(Tag);
      FieldIndexIDRepeat := FieldIdx or (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);

      WireType := ProtoBufTagWireType(Tag);
      DataSubType := WireDataSubType(FieldIdx);

      case WireType of

        // 0 VARINT  // 1 I64   // 5 I32
        pbwtVarInt, pbwtFixed64, pbwtFixed32:
          DecodeDatafromStream(WireType, DataSubType, Stream, Bytes, FieldIndexIDRepeat);

        // 2	LEN
        pbwtLenDelimit: begin

          case DataSubType of

            // string, bytes
            pbwstLenString, pbwstLenBytes:
              DecodeDatafromStream(WireType, DataSubType, Stream, Bytes, FieldIndexIDRepeat);

            // nested messges
            pbwstNestedMessage, pbwstRepteadNestedMessage: begin
              Value64 := ReadVarInt(Bytes, Stream);
              if Value64 = 0 then
                Continue;
              if Value64 > Bytes then
                Break;

              // do a recursion for a nested message
              if RecurseN >= ProtoBufRecursionLimit then
                raise EProtocolBufferRecursion.Create('Nested message recursion limit exceeded');

              if DataSubType = pbwstNestedMessage then
                pValue := ReadFieldByIndex(FieldIndexIDRepeat, Size)
              else
                pValue := GetNestedArrayObjectByIndex(FieldIndexIDRepeat); // repeated nested - must create objects

              inc(RecurseN);
              Size := TProtocolBuffer(pValue).DecodeToProto_Internal(Stream, Integer(Value64), RecurseN);
              dec(RecurseN);
              Bytes := Bytes - Size;
            end;

            pbwstMapMessage: begin
              Value64 := ReadVarInt(Bytes, Stream);
              if Value64 = 0 then
                Continue;
              if Value64 > Bytes then
                Break;

              if Repeated = 0 then
                Repeated := 1;

              // Key filed
              Tag := ReadVarInt(Bytes, Stream); // map index tag (1,2)
              FieldSubIdx := ProtoBufTagFieldIndex(Tag);
              FieldIndexIDRepeat := FieldIdx or (FieldSubIdx shl PB_FIELD_SUB_IDX_OFFSET) or
                                      (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);
              DataSubType := WireDataSubType(FieldIndexIDRepeat);
              WireType := ProtoBufSubTypeToWireType(DataSubType);
              DecodeDatafromStream(WireType, DataSubType, Stream, Bytes, FieldIndexIDRepeat);

              // Value field
              Tag := ReadVarInt(Bytes, Stream); // map index tag (1,2)
              FieldSubIdx := ProtoBufTagFieldIndex(Tag);
              FieldIndexIDRepeat := FieldIdx or (FieldSubIdx shl PB_FIELD_SUB_IDX_OFFSET) or
                                      (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);
              DataSubType := WireDataSubType(FieldIndexIDRepeat);
              WireType := ProtoBufSubTypeToWireType(DataSubType);
              DecodeDatafromStream(WireType, DataSubType, Stream, Bytes, FieldIndexIDRepeat);

            end;

            pbwstUnknown, pbwstUnUsed: begin
              Value64 := ReadVarInt(Bytes, Stream);
              // Will occur when new fields exist but this proto def is old.
              //   dump it - and expect a loss of sync.
              if Stream.Position + Int64(Value64) <= Stream.Size then
                Stream.Position := Stream.Position + Int64(Value64)
              else
                Stream.Seek(0, soEnd);
            end;
          end;
        end;

        // 3	SGROUP, 4	EGROUP
        pbwtStartGrp, pbwtEndGrp: begin
          // hope we never see one of these :-)
        end;

      end; // case WireType of

    end;   // while (Stream.Position < Stream.Size) and (Bytes > 0) do

  Result := (BytesStart - Bytes); // Result is bytes read, passed back to caller.

end;

function TProtocolBuffer.DecodeToProto(Stream: TStream; Bytes: Integer): Integer;
var RecurseN: Integer;
begin
  RecurseN := 1;
  Result := DecodeToProto_Internal(Stream, Bytes, RecurseN);
end;


{ ** Encode / write / serialize data to stream ** }

procedure WriteVarInt(Stream: TStream; temp64: UInt64);
var aByte: Byte;
begin
  repeat
    aByte := temp64 and $7F;
    temp64 := temp64 shr 7;
    if temp64 > 0 then
      aByte := aByte or $80;
    Stream.Write(aByte, 1);
  until temp64 = 0;
end;

class procedure TProtocolBuffer.EncodeDataToStream(WireType: TProtoBufWireDataType;
        DataSubType: TProtoBufWireDataSubType; pValue: Pointer; Size: Integer; Stream: TStream);
var
  temp32: UInt32;
  temp64: UInt64;
  tempS64: Int64;
  tempS32, n: Integer;
  s: string;
  su: UTF8String;
begin
  case DataSubType of
    pbwstVarIntInt32..pbwstVarIntEnum: begin
      case DataSubType of
        pbwstVarIntInt32, pbwstVarIntUInt32, pbwstVarIntSInt32, pbwstVarIntBool, pbwstVarIntEnum: begin
          temp32 := pLongword(pValue)^;
          if DataSubType = pbwstVarIntSInt32 then
            begin
              tempS32 := pInteger(pValue)^;
              if tempS32 >= 0 then       // zigzag
                temp32 := tempS32 * 2
              else
                temp32 := UInt32((tempS32 shl 1) xor -1);
              temp64 := temp32;
            end
          else if DataSubType in [pbwstVarIntBool, pbwstVarIntEnum] then
            temp64 := temp32 and $FF    // erase undefined higher bytes.
          else
            temp64 := temp32;
        end;
        pbwstVarIntSInt64: begin
          tempS64 := pInt64(pValue)^;
          if tempS64 >= 0 then       // zigzag
            temp64 := UInt64(tempS64 * 2)
          else
            temp64 := UInt64((tempS64 shl 1) xor -1);
        end;
      else
        temp64 := pUint64(pValue)^;
      end;
      WriteVarInt(Stream, temp64);
    end;

    pbwstLenString, pbwstLenBytes: begin
      if DataSubType = pbwstLenBytes then
        begin
          WriteVarInt(Stream, Size);
          Stream.Write(pByte(pValue)^, Size);
        end
      else  // pbwstLenString
        begin
          s := pString(pValue)^;
          su := UTF8Encode(s);
          n := Length(su);
          WriteVarInt(Stream, n);
          Stream.Write(pRawByteString(su)^, n);
        end;
    end;

    pbwstI64Fixed64, pbwstI64SFixed64, pbwstI64Double: begin
      temp64 := pUint64(pValue)^;
      Stream.Write(temp64, 8);
    end;

    pbwstI32Fixed32, pbwstI32SFixed32, pbwstI32Float: begin
      temp32 := pLongword(pValue)^;
      Stream.Write(temp32, 4);
    end;
  end;
end;

// called recursively for each nested class
function TProtocolBuffer.EncodeToStream_Internal(Stream: TStream; var RecurseN: Integer): Integer;
var
  NestedStream: TMemoryStream;
  t, Minbit, Maxbit, Size: Integer;
  Bytes: Int64;
  pValue: Pointer;
  WireType: TProtoBufWireDataType;
  DataSubType: TProtoBufWireDataSubType;
  FieldIdx, FieldSubIdx, Repeated: Word;
  FieldIndexIDRepeat: LongWord;
  Tag: Word;

begin
  Result := 0;
  Bytes := Stream.Position;
  Minbit := LoSetField;
  Maxbit := HiSetField;

  // no fields set / empty proto
  if (Maxbit < 1) or (Minbit < 1) then
    Exit;

  t := Minbit - 1;
  while t < Maxbit do
    begin
      inc(t);
      if not FieldTouched[t] then
        Continue;

      FieldIndexIDRepeat := LongWord(t); // zeroes the repeated flag

      repeat
        DataSubType := WireDataSubType(t);
        WireType := ProtoBufSubTypeToWireType(DataSubType);

        // loop for repeated
        FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
        Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
        Size := 0;

        if DataSubType in [pbwstNestedMessage, pbwstRepteadNestedMessage] then
          begin
            if RecurseN >= ProtoBufRecursionLimit then
              raise EProtocolBufferRecursion.Create('Nested message recursion limit exceeded');

            Tag := ProtoBufMakeTag(FieldIdx, WireType);
            pValue := ReadFieldByIndex(FieldIndexIDRepeat, Size);
            if pValue = nil then  // when repeat nested have nbeen set to count of 0;
              Continue;
            NestedStream := TMemoryStream.Create;

            inc(RecurseN);
            Size := TProtocolBuffer(pValue).EncodeToStream_Internal(NestedStream, RecurseN);
            dec(RecurseN);

            if Size > 0 then
              begin
                WriteVarInt(Stream, Tag);
                WriteVarInt(Stream, Size);
                NestedStream.Position := 0;
                Stream.CopyFrom(NestedStream, NestedStream.Size);
              end;
            NestedStream.Free;
            Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET; // set to 0 when no more iterations
            Continue;
          end;

        if DataSubType = pbwstMapMessage then
          begin
            Tag := ProtoBufMakeTag(FieldIdx, WireType);  // the nest tag
            WriteVarInt(Stream, Tag);

            NestedStream := TMemoryStream.Create;

            if Repeated = 0 then
              Repeated := 1;
            FieldSubIdx := 1;
            FieldIndexIDRepeat := FieldIdx or (FieldSubIdx shl PB_FIELD_SUB_IDX_OFFSET) or
                                    (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);

            DataSubType := WireDataSubType(FieldIndexIDRepeat);
            WireType := ProtoBufSubTypeToWireType(DataSubType);
            FieldSubIdx := FieldIndexIDRepeat and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;
            Tag := ProtoBufMakeTag(FieldSubIdx, WireType);
            WriteVarInt(NestedStream, Tag);
            pValue := ReadFieldByIndex(FieldIndexIDRepeat, Size);
            if pValue = nil then
              Continue;
            EncodeDataToStream(WireType, DataSubType, pValue, Size, NestedStream);

            DataSubType := WireDataSubType(FieldIndexIDRepeat);
            WireType := ProtoBufSubTypeToWireType(DataSubType);
            FieldSubIdx := FieldIndexIDRepeat and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;
            Tag := ProtoBufMakeTag(FieldSubIdx, WireType);
            WriteVarInt(NestedStream, Tag);
            pValue := ReadFieldByIndex(FieldIndexIDRepeat, Size);
            EncodeDataToStream(WireType, DataSubType, pValue, Size, NestedStream);

            WriteVarInt(Stream, NestedStream.Size);
            NestedStream.Position := 0;
            Stream.CopyFrom(NestedStream, NestedStream.Size);
            NestedStream.Free;

            Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET; // set to 0 when no more iterations
            Continue;
          end;

        // else normal fields
        Tag := ProtoBufMakeTag(FieldIdx, WireType);
        WriteVarInt(Stream, Tag);
        pValue := ReadFieldByIndex(FieldIndexIDRepeat, Size);
        if pValue = nil then  // no data field / empty null string / mistake / bug
          Break;
        EncodeDataToStream(WireType, DataSubType, pValue, Size, Stream);

        Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET; // set to 0 when no more iterations
      until Repeated = 0;  // repeats

    end; // field array

  Result := Integer(Stream.Position - Bytes);
end;

function TProtocolBuffer.EncodeToStream(Stream: TStream): Integer;
var RecurseN: Integer;
begin
  RecurseN := 1;
  Result := EncodeToStream_Internal(Stream, RecurseN);
end;

// Use as a simple encoder, only when a small flat proto struct exists
class procedure TProtocolBuffer.AppendSimpleValue(FieldIdx: Integer; DataSubType: TProtoBufWireDataSubType;
        pValue: Pointer; Stream: TStream);
var WireType: TProtoBufWireDataType; Tag: Word;
begin
  WireType := ProtoBufSubTypeToWireType(DataSubType);
  Tag := ProtoBufMakeTag(FieldIdx, WireType);
  WriteVarInt(Stream, Tag);
  EncodeDataToStream(WireType, DataSubType, pValue, 0, Stream);
end;


{  **  extra functions  **  }

function TProtocolBuffer.GetNestedArrayObjectByIndex(var FieldIndexIDRepeat: LongWord): Pointer;
begin
  Result := nil;  //  some descendents override this method.  Only used with a RPT array of nested objects.
end;

function TProtocolBuffer.GetTouched(Index: Integer): Boolean;
begin
  Result := FTouchedBits[Index];
end;

procedure TProtocolBuffer.ResetTouchedFlags;
var Count: Integer;
begin
  Count := Length(FTouchedBits);
  FTouchedBits := nil;
  SetLength(FTouchedBits, Count);
end;

procedure TProtocolBuffer.SetParentDetails(ParentIndex: Integer; ParentObject: TProtocolBuffer);
begin
  FParentIndex := ParentIndex;
  FParentObject := ParentObject;
end;

procedure TProtocolBuffer.SetTouched(Index: Integer; const Value: Boolean);
begin
  if Index <= High(FTouchedBits) then
    FTouchedBits[Index] := Value;
  if FParentObject <> nil then   // objective here to set parent (nested) touched flag.
    FParentObject.SetTouched(FParentIndex, true);
end;

//  Used to unset touched flags of "default" values (0, empty str) - no need to send these.
procedure TProtocolBuffer.UnsetTouchedFlagForDefaultVals(DataSubTypes: TProtoBufWireDataSubTypeSet);
var wdst: TProtoBufWireDataSubType; j, Size: Integer; k: Longword; pValue: Pointer;
begin
  for j := 1 to FProtoFieldMaxID do
    begin
      k := j;
      wdst := WireDataSubType(k);
      if not (wdst in DataSubTypes) then
        Continue;
      pValue := ReadFieldByIndex(k, Size);
      if pValue = nil then
        Continue;
      case wdst of
        pbwstVarIntInt32, pbwstVarIntUInt32, pbwstVarIntSInt32, pbwstI32Fixed32, pbwstI32SFixed32:
          FieldTouched[j] := not (pInteger(pValue)^ = 0);
        pbwstVarIntInt64, pbwstVarIntUInt64, pbwstVarIntSInt64, pbwstI64Fixed64, pbwstI64SFixed64:
          FieldTouched[j] := not (pInt64(pValue)^ = 0);
        pbwstVarIntBool, pbwstVarIntEnum:
          FieldTouched[j] := not (pByte(pValue)^ = 0);
        pbwstI64Double:
          FieldTouched[j] := not (pDouble(pValue)^ = 0);
        pbwstLenString:
          FieldTouched[j] := Length(pString(pValue)^) > 0;
        pbwstI32Float:
          FieldTouched[j] := not (pSingle(pValue)^ = 0);
      end;
    end;
end;

function TProtocolBuffer.LoSetField: Integer;
var i: Integer;
begin
  Result := -1;
  for i := 1 to High(FTouchedBits) do
    if FTouchedBits[i] then
      begin
        Result := i;
        Break;
      end;
end;

function TProtocolBuffer.HiSetField: Integer;
var i: Integer;
begin
  Result := -1;
  for i := High(FTouchedBits) downto 1 do
    if FTouchedBits[i] then
      begin
        Result := i;
        Break;
      end;
end;

end.
