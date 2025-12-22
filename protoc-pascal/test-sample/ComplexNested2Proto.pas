{
Subject to the terms of the MIT license: Copyright (c) 2025 HHS Software Corp.
  Created with protoc-pascal.exe   Version v1.02  (2025)
    https://github.com/HHS-Software/Protocol-Buffer_Delphi_FPC
    A utility to read and convert the Google Protocol Buffer v3 system into pascal code.
    The code is designed for the v3 proto.  Later proto versions possibly too.
}
unit ComplexNested2Proto;

interface

{$IFDEF FPC}
{$mode delphi}{$H+}
uses SysUtils, Classes,
{$ELSE}
uses System.Classes, System.SysUtils,
{$ENDIF}
 ProtocolBuffer;

type
  // forward declarations
  TComplexNested2Proto = class;
  TGoogleAnyProto = class;

  TPBComplexNested2Enums2 = (pbcoN0, pbcoN1, pbcoN2, pbcoScopeErr);
  TPBMessyEnum = (pbmeE0, pbmeE1, pbmeE2, pbmeScopeErr);
  pTPBMessyEnum = ^TPBMessyEnum;
  pTPBComplexNested2Enums2 = ^TPBComplexNested2Enums2;

  TComplexNested2Proto = class(TProtocolBuffer)
  private
    FAnint: Integer;  // 1 Req
    FAnenum: TPBComplexNested2Enums2;  // 2 Opt
    procedure SetAnint(const Value: Integer);
    procedure SetAnenum(const Value: TPBComplexNested2Enums2);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    property Anint: Integer read FAnint write SetAnint;  // 1
    property Anenum: TPBComplexNested2Enums2 read FAnenum write SetAnenum;  // 2
  end;

  TComplexProto = class(TProtocolBuffer)
  private
    FAstringInComplex: string;  // 1 Req
    FANested2Msg: TComplexNested2Proto;  // 2 Opt
    procedure SetAstringInComplex(const Value: string);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    property AstringInComplex: string read FAstringInComplex write SetAstringInComplex;  // 1
    property ANested2Msg: TComplexNested2Proto read FANested2Msg;  // 2
  end;

  TEmptyMessageProto = class(TProtocolBuffer)
  private
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
  end;

  TMessyProto = class(TProtocolBuffer)
  private
    FStringfield: string;  // 1 Opt
    FBoolfield: Boolean;  // 2 Opt
    FEnumfield: TPBMessyEnum;  // 4 Opt
    FIntfield: Int64;  // 3 Opt
    FEmptyfield: string;  // 6 Opt
    FBytesfield: Pointer;  // 12 Opt
    FBytesfieldSize: Integer;
    FAstring: string;  // 9 Opt
    FInteger: Integer;  // 8 Opt
    procedure SetStringfield(const Value: string);
    procedure SetBoolfield(const Value: Boolean);
    procedure SetEnumfield(const Value: TPBMessyEnum);
    procedure SetIntfield(const Value: Int64);
    procedure SetEmptyfield(const Value: string);
    procedure SetAstring(const Value: string);
    procedure SetInteger(const Value: Integer);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    procedure SetBytesfield(const Value: Pointer; Size: Integer);
    property Stringfield: string read FStringfield write SetStringfield;  // 1
    property Boolfield: Boolean read FBoolfield write SetBoolfield;  // 2
    property Enumfield: TPBMessyEnum read FEnumfield write SetEnumfield;  // 4
    property Intfield: Int64 read FIntfield write SetIntfield;  // 3
    property Emptyfield: string read FEmptyfield write SetEmptyfield;  // 6
    property Bytesfield: Pointer read FBytesfield;  // 12
    property BytesfieldSize: Integer read FBytesfieldSize;
    property Astring: string read FAstring write SetAstring;  // 9
    property Integer: Integer read FInteger write SetInteger;  // 8
  end;

  TErrorStatusProto = class(TProtocolBuffer)
  private
    FMessage: string;  // 1 
    FDetailsArray: array of TGoogleAnyProto;  // 2 Rep
    procedure SetMessage(const Value: string);
    function GetDetailsArray(Index: Integer): TGoogleAnyProto;
    function GetDetailsArrayCount: Integer;
    procedure SetDetailsArrayCount(const Value: Integer);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    function GetNestedArrayObjectByIndex(var FieldIndexIDRepeat: LongWord): Pointer; override;
    property Message: string read FMessage write SetMessage;  // 1
    property DetailsArray[Index: Integer]: TGoogleAnyProto read GetDetailsArray;  // 2
    property DetailsArrayCount: Integer read GetDetailsArrayCount write SetDetailsArrayCount;
  end;

  TGoogleAnyProto = class(TProtocolBuffer)
  private
    FTypeURL: string;  // 1 
    FValue: Pointer;  // 2 
    FValueSize: Integer;
    procedure SetTypeURL(const Value: string);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    procedure SetValue(const Value: Pointer; Size: Integer);
    property TypeURL: string read FTypeURL write SetTypeURL;  // 1
    property Value: Pointer read FValue;  // 2
    property ValueSize: Integer read FValueSize;
  end;

implementation

{ TComplexNested2Proto }

const
  ComplexNested2FieldMaxID = 2;
  ComplexNested2WireDataSubTypes: array [0..ComplexNested2FieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstVarIntInt32, pbwstVarIntEnum);

constructor TComplexNested2Proto.Create;
begin
  inherited Create(ComplexNested2FieldMaxID);
  FieldTouched[1] := true;
end;

constructor TComplexNested2Proto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TComplexNested2Proto.Destroy;
begin
  inherited;
end;

function TComplexNested2Proto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= LongWord(FieldMaxID) then
    Result := ComplexNested2WireDataSubTypes[Index];
end;

function TComplexNested2Proto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Result := nil;

  case FieldIdx of
    1: Result := @FAnint;
    2: Result := @FAnenum;
  end;
end;

procedure TComplexNested2Proto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;

  case FieldIdx of
    1: FAnint := pInteger(Value)^;
    2: if pInteger(Value)^ > Ord(High(TPBComplexNested2Enums2)) then FAnenum := High(TPBComplexNested2Enums2) else FAnenum := pTPBComplexNested2Enums2(Value)^;
  end;
end;

procedure TComplexNested2Proto.SetAnint(const Value: Integer);
begin
  FAnint := Value;
  FieldTouched[1] := true;
end;

procedure TComplexNested2Proto.SetAnenum(const Value: TPBComplexNested2Enums2);
begin
  FAnenum := Value;
  FieldTouched[2] := true;
end;

{ TComplexProto }

const
  ComplexFieldMaxID = 2;
  ComplexWireDataSubTypes: array [0..ComplexFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstLenString, pbwstNestedMessage);

constructor TComplexProto.Create;
begin
  inherited Create(ComplexFieldMaxID);
  FANested2Msg := TComplexNested2Proto.CreateAsNested(2, Self);
  FieldTouched[1] := true;
end;

constructor TComplexProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TComplexProto.Destroy;
begin
  FANested2Msg.Free;
  inherited;
end;

function TComplexProto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= LongWord(FieldMaxID) then
    Result := ComplexWireDataSubTypes[Index];
end;

function TComplexProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Result := nil;

  case FieldIdx of
    1: Result := @FAstringInComplex;
    2: Result := FANested2Msg;
  end;
end;

procedure TComplexProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;

  case FieldIdx of
    1: FAstringInComplex := pstring(Value)^;
    //2: FANested2Msg; // nested
  end;
end;

procedure TComplexProto.SetAstringInComplex(const Value: string);
begin
  FAstringInComplex := Value;
  FieldTouched[1] := true;
end;

{ TEmptyMessageProto }

const
  EmptyMessageFieldMaxID = 0;
  EmptyMessageWireDataSubTypes: array [0..EmptyMessageFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnUsed);

constructor TEmptyMessageProto.Create;
begin
  inherited Create(EmptyMessageFieldMaxID);
end;

constructor TEmptyMessageProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TEmptyMessageProto.Destroy;
begin
  inherited;
end;

function TEmptyMessageProto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= LongWord(FieldMaxID) then
    Result := EmptyMessageWireDataSubTypes[Index];
end;

function TEmptyMessageProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
begin
  Result := nil;
  //  No properties - empty function
end;

procedure TEmptyMessageProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
begin
  //  No properties - empty function
end;

{ TMessyProto }

const
  MessyFieldMaxID = 12;
  MessyWireDataSubTypes: array [0..MessyFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstLenString, pbwstVarIntBool, pbwstVarIntInt64, pbwstVarIntEnum, pbwstUnused, pbwstLenString, 
    pbwstUnused, pbwstVarIntInt32, pbwstLenString, pbwstUnused, pbwstUnused, pbwstLenBytes);

constructor TMessyProto.Create;
begin
  inherited Create(MessyFieldMaxID);
  FEmptyfield := '';
  FIntfield := 11;
  FBoolfield := true;
  FStringfield := 'abc';
end;

constructor TMessyProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TMessyProto.Destroy;
begin
  FreeMem(FBytesfield);
  inherited;
end;

function TMessyProto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= LongWord(FieldMaxID) then
    Result := MessyWireDataSubTypes[Index];
end;

function TMessyProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Result := nil;

  case FieldIdx of
    1: Result := @FStringfield;
    2: Result := @FBoolfield;
    4: Result := @FEnumfield;
    3: Result := @FIntfield;
    6: Result := @FEmptyfield;
    12: begin Result := FBytesfield; Size := FBytesfieldSize; end;
    9: Result := @FAstring;
    8: Result := @FInteger;
  end;
end;

procedure TMessyProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;

  case FieldIdx of
    1: FStringfield := pstring(Value)^;
    2: FBoolfield := pBoolean(Value)^;
    4: if pInteger(Value)^ > Ord(High(TPBMessyEnum)) then FEnumfield := High(TPBMessyEnum) else FEnumfield := pTPBMessyEnum(Value)^;
    3: FIntfield := pInt64(Value)^;
    6: FEmptyfield := pstring(Value)^;
    12: SetBytesfield(Value, Size);
    9: FAstring := pstring(Value)^;
    8: FInteger := pInteger(Value)^;
  end;
end;

procedure TMessyProto.SetStringfield(const Value: string);
begin
  FStringfield := Value;
  FieldTouched[1] := true;
end;

procedure TMessyProto.SetBoolfield(const Value: Boolean);
begin
  FBoolfield := Value;
  FieldTouched[2] := true;
end;

procedure TMessyProto.SetEnumfield(const Value: TPBMessyEnum);
begin
  FEnumfield := Value;
  FieldTouched[4] := true;
end;

procedure TMessyProto.SetIntfield(const Value: Int64);
begin
  FIntfield := Value;
  FieldTouched[3] := true;
end;

procedure TMessyProto.SetEmptyfield(const Value: string);
begin
  FEmptyfield := Value;
  FieldTouched[6] := true;
end;

procedure TMessyProto.SetBytesfield(const Value: Pointer; Size: Integer);
begin;
  FreeMem(FBytesfield);
  GetMem(FBytesfield, Size);
  FBytesfieldSize := Size;
  Move(Value^, FBytesfield^, Size);
  FieldTouched[12] := true;
end;

procedure TMessyProto.SetAstring(const Value: string);
begin
  FAstring := Value;
  FieldTouched[9] := true;
end;

procedure TMessyProto.SetInteger(const Value: Integer);
begin
  FInteger := Value;
  FieldTouched[8] := true;
end;

{ TErrorStatusProto }

const
  ErrorStatusFieldMaxID = 2;
  ErrorStatusWireDataSubTypes: array [0..ErrorStatusFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstLenString, pbwstRepteadNestedMessage);

constructor TErrorStatusProto.Create;
begin
  inherited Create(ErrorStatusFieldMaxID);
end;

constructor TErrorStatusProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TErrorStatusProto.Destroy;
var i: Integer;
begin
  for i := 0 to High(FDetailsArray) do
    FDetailsArray[i].Free;

  inherited;
end;

function TErrorStatusProto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= LongWord(FieldMaxID) then
    Result := ErrorStatusWireDataSubTypes[Index];
end;

function TErrorStatusProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx, FieldSubIdx, Repeated: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  FieldSubIdx := FieldIndexIDRepeat and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;  // 1 based
  Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
  Result := nil;

  case FieldIdx of
    1: Result := @FMessage;
    2:begin  // nested - repeated
          if Repeated < Length(FDetailsArray) then
            begin
              Result := FDetailsArray[Repeated];
              inc(Repeated);
            end;
          if Repeated >= Length(FDetailsArray) then
            Repeated := 0;
      end;
  end;
  FieldIndexIDRepeat := FieldIdx or (FieldSubIdx shl PB_FIELD_SUB_IDX_OFFSET) or
                                    (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);
end;

procedure TErrorStatusProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;

  case FieldIdx of
    1: FMessage := pstring(Value)^;
    //2: FDetailsArray; // nested - repeated
  end;
end;

procedure TErrorStatusProto.SetMessage(const Value: string);
begin
  FMessage := Value;
  FieldTouched[1] := true;
end;

function TErrorStatusProto.GetDetailsArray(Index: Integer): TGoogleAnyProto;
begin
  if Index >= Length(FDetailsArray) then
    SetDetailsArrayCount(Index + 1);
  Result := FDetailsArray[Index];
end;

function TErrorStatusProto.GetDetailsArrayCount: Integer;
begin
  Result := Length(FDetailsArray);
end;

procedure TErrorStatusProto.SetDetailsArrayCount(const Value: Integer);
var i, l: Integer;
begin
  l := Length(FDetailsArray);
  if Value > l then
    begin
      SetLength(FDetailsArray, Value);
      for i := l to Value - 1 do
        FDetailsArray[i] := TGoogleAnyProto.CreateAsNested(2, Self);
    end
  else if Value < l then
    begin
      for i := l -1 downto Value do
        FDetailsArray[i].Free;
      SetLength(FDetailsArray, Value);
    end;
end;

function TErrorStatusProto.GetNestedArrayObjectByIndex(var FieldIndexIDRepeat: LongWord): Pointer;
var FieldIdx, Repeated: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
  Result := nil;

  case FieldIdx of
    2:begin
      if Repeated >= GetDetailsArrayCount then
        SetDetailsArrayCount(Repeated + 1);
      Result := GetDetailsArray(Repeated);
      inc(Repeated);
    end;

  end;
  FieldIndexIDRepeat := FieldIdx or (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);
end;

{ TGoogleAnyProto }

const
  GoogleAnyFieldMaxID = 2;
  GoogleAnyWireDataSubTypes: array [0..GoogleAnyFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstLenString, pbwstLenBytes);

constructor TGoogleAnyProto.Create;
begin
  inherited Create(GoogleAnyFieldMaxID);
end;

constructor TGoogleAnyProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TGoogleAnyProto.Destroy;
begin
  FreeMem(FValue);
  inherited;
end;

function TGoogleAnyProto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= LongWord(FieldMaxID) then
    Result := GoogleAnyWireDataSubTypes[Index];
end;

function TGoogleAnyProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Result := nil;

  case FieldIdx of
    1: Result := @FTypeURL;
    2: begin Result := FValue; Size := FValueSize; end;
  end;
end;

procedure TGoogleAnyProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;

  case FieldIdx of
    1: FTypeURL := pstring(Value)^;
    2: SetValue(Value, Size);
  end;
end;

procedure TGoogleAnyProto.SetTypeURL(const Value: string);
begin
  FTypeURL := Value;
  FieldTouched[1] := true;
end;

procedure TGoogleAnyProto.SetValue(const Value: Pointer; Size: Integer);
begin;
  FreeMem(FValue);
  GetMem(FValue, Size);
  FValueSize := Size;
  Move(Value^, FValue^, Size);
  FieldTouched[2] := true;
end;

end.
