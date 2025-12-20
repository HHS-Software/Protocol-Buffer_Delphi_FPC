{
Subject to the terms of the MIT license: Copyright (c) 2025 HHS Software Corp.
  Created with protoc-pascal.exe   Version v1.00  (2025)
    https://github.com/HHS-Software/Protocol-Buffer_Delphi_FPC
    A utility to read and convert the Google Protocol Buffer v3 system into pascal code.
    The code is designed for the v3 proto.  Later proto versions possibly too.
}
unit SampleProto;

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
  TGoogleAnyProto = class;
  TTwoIntsProto = class;

  TPBComplexNested2Enums2 = (pbcoN0, pbcoN1, pbcoN2, pbcoScopeErr);
  TPBMessyEnum = (pbmeE1, pbmeE2, pbmeScopeErr);
  pTPBMessyEnum = ^TPBMessyEnum;
  pTPBComplexNested2Enums2 = ^TPBComplexNested2Enums2;

  TSampleProto = class(TProtocolBuffer)
  private
    FReqId: Integer;  // 1 Opt
    FField2: Double;  // 2 Req
    FSomeString: string;  // 3 Opt
    FSomeInts: TPBArrayOfInteger;  // 4 Rep
    FMapStrStr: TProtoBufMapStringStringArray;  // 5 Map Rep
    procedure SetReqId(const Value: Integer);
    procedure SetField2(const Value: Double);
    procedure SetSomeString(const Value: string);
    function GetSomeInts(Index: Integer): Integer;
    procedure SetSomeInts(Index: Integer; const Value: Integer);
    function GetSomeIntsCount: Integer;
    procedure SetSomeIntsCount(const Value: Integer);
    function GetMapStrStr(Index: Integer): TProtoBufMapStringString;
    procedure SetMapStrStr(Index: Integer; const Value: TProtoBufMapStringString);
    function GetMapStrStrCount: Integer;
    procedure SetMapStrStrCount(const Value: Integer);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    property ReqId: Integer read FReqId write SetReqId;  // 1
    property Field2: Double read FField2 write SetField2;  // 2
    property SomeString: string read FSomeString write SetSomeString;  // 3
    property SomeInts[Index: Integer]: Integer read GetSomeInts write SetSomeInts;  // 4
    property SomeIntsCount: Integer read GetSomeIntsCount write SetSomeIntsCount;  // 4
    property MapStrStr[Index: Integer]: TProtoBufMapStringString read GetMapStrStr write SetMapStrStr;  // 5
    property MapStrStrCount: Integer read GetMapStrStrCount write SetMapStrStrCount;  // 5
  end;

  TTwoIntsProto = class(TProtocolBuffer)
  private
    FAnIntOne: Integer;  // 1 Opt
    FAnIntTwo: Integer;  // 2 Opt
    procedure SetAnIntOne(const Value: Integer);
    procedure SetAnIntTwo(const Value: Integer);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    property AnIntOne: Integer read FAnIntOne write SetAnIntOne;  // 1
    property AnIntTwo: Integer read FAnIntTwo write SetAnIntTwo;  // 2
  end;

  TSimpleWithNestedProto = class(TProtocolBuffer)
  private
    FATwoInts: TTwoIntsProto;  // 1 Opt
    FManyTwoIntsArray: array of TTwoIntsProto;  // 2 Rep
    function GetManyTwoIntsArray(Index: Integer): TTwoIntsProto;
    function GetManyTwoIntsArrayCount: Integer;
    procedure SetManyTwoIntsArrayCount(const Value: Integer);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    function GetNestedArrayObjectByIndex(var FieldIndexIDRepeat: LongWord): Pointer; override;
    property ATwoInts: TTwoIntsProto read FATwoInts;  // 1
    property ManyTwoIntsArray[Index: Integer]: TTwoIntsProto read GetManyTwoIntsArray;  // 2
    property ManyTwoIntsArrayCount: Integer read GetManyTwoIntsArrayCount write SetManyTwoIntsArrayCount;
  end;

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
    procedure SetAstringInComplex(const Value: string);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    property AstringInComplex: string read FAstringInComplex write SetAstringInComplex;  // 1
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
    FBytesfield: Pointer;  // 8 Opt
    FBytesfieldSize: Integer;
    procedure SetStringfield(const Value: string);
    procedure SetBoolfield(const Value: Boolean);
    procedure SetEnumfield(const Value: TPBMessyEnum);
    procedure SetIntfield(const Value: Int64);
    procedure SetEmptyfield(const Value: string);
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
    property Bytesfield: Pointer read FBytesfield;  // 8
    property BytesfieldSize: Integer read FBytesfieldSize;
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

{ TSampleProto }

const
  SampleFieldMaxID = 5;
  SampleWireDataSubTypes: array [0..SampleFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstVarIntInt32, pbwstI64Double, pbwstLenString, pbwstVarIntInt32, pbwstMapMessage);
  SampleMapStrStrWireDataSubTypes: array [0..2] of TProtoBufWireDataSubType =
    (pbwstUnknown, pbwstLenString, pbwstLenString);

constructor TSampleProto.Create;
begin
  inherited Create(SampleFieldMaxID);
end;

constructor TSampleProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TSampleProto.Destroy;
begin
  inherited;
end;

function TSampleProto.WireDataSubType(Index: LongWord): TProtoBufWireDataSubType;
var FieldIdx, FieldSubIdx: Word;
begin
  FieldIdx := Index and PB_FIELD_IDX_MASK;
  FieldSubIdx := Index and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;
  Result := pbwstUnknown;
  if FieldIdx <= FieldMaxID then
    Result := SampleWireDataSubTypes[FieldIdx];
  if FieldSubIdx > 0 then
    case FieldIdx of
      5: Result := SampleMapStrStrWireDataSubTypes[FieldSubIdx];
    end;
end;

function TSampleProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx, FieldSubIdx, Repeated: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  FieldSubIdx := FieldIndexIDRepeat and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;  // 1 based
  Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
  Result := nil;

  case FieldIdx of
    1: Result := @FReqId;
    2: Result := @FField2;
    3: Result := @FSomeString;
    4: begin
          if Repeated < Length(FSomeInts) then
            begin
              Result := @FSomeInts[Repeated];
              inc(Repeated);
            end;
          if Repeated >= Length(FSomeInts) then
            Repeated := 0;
        end;
    5: begin
          if Repeated <= Length(FMapStrStr) then
             case FieldSubIdx of
               1:begin
                   Result := @FMapStrStr[Repeated-1].Key;
                   inc(FieldSubIdx);
                 end;
               2:begin
                   Result := @FMapStrStr[Repeated-1].Value;
                   inc(Repeated);
                   dec(FieldSubIdx);
                 end;
             end;
          if Repeated > Length(FMapStrStr) then
            Repeated := 0;
        end;
  end;
  FieldIndexIDRepeat := FieldIdx or (FieldSubIdx shl PB_FIELD_SUB_IDX_OFFSET) or
                                    (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);
end;

procedure TSampleProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx, FieldSubIdx, Repeated: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  FieldSubIdx := FieldIndexIDRepeat and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;  // 1 based
  Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;

  case FieldIdx of
    1: FReqId := pInteger(Value)^;
    2: FField2 := pDouble(Value)^;
    3: FSomeString := pstring(Value)^;
    4: begin
          if Repeated = $FFFF then //  -1 clears the array
            FSomeInts := nil
          else
            begin
              if Repeated >= Length(FSomeInts) then
                SetLength(FSomeInts, Repeated + 1);
              FSomeInts[Repeated] := pInteger(Value)^;
            end;
        end;
    5: begin
          if Repeated = $FFFF then //  -1 clears the array
            FMapStrStr := nil
          else if Repeated > 0 then
            begin
              if Repeated > Length(FMapStrStr) then
                SetLength(FMapStrStr, Repeated);
              case FieldSubIdx of
                1: FMapStrStr[Repeated -1].Key := pString(Value)^;
                2: FMapStrStr[Repeated -1].Value := pString(Value)^;
              end;
            end;
        end;
  end;
end;

procedure TSampleProto.SetReqId(const Value: Integer);
begin
  FReqId := Value;
  FieldTouched[1] := true;
end;

procedure TSampleProto.SetField2(const Value: Double);
begin
  FField2 := Value;
  FieldTouched[2] := true;
end;

procedure TSampleProto.SetSomeString(const Value: string);
begin
  FSomeString := Value;
  FieldTouched[3] := true;
end;

function TSampleProto.GetSomeInts(Index: Integer): Integer;
begin
  if Index <= Length(FSomeInts) then
    Result := FSomeInts[Index]
  else
    Result := FSomeInts[0];
end;

procedure TSampleProto.SetSomeInts(Index: Integer; const Value: Integer);
var i: Integer;
begin
  i := Length(FSomeInts);
  if Index >= i then
    SetLength(FSomeInts, Index + 1);
  FSomeInts[Index] := Value;
  FieldTouched[4] := true;
end;

function TSampleProto.GetSomeIntsCount: Integer;
begin
  Result := Length(FSomeInts);
end;

procedure TSampleProto.SetSomeIntsCount(const Value: Integer);
begin
  SetLength(FSomeInts, Value);
end;

function TSampleProto.GetMapStrStr(Index: Integer): TProtoBufMapStringString;
begin
  if Index <= Length(FMapStrStr) then
    Result := FMapStrStr[Index]
  else
    Result := FMapStrStr[0];
end;

procedure TSampleProto.SetMapStrStr(Index: Integer; const Value: TProtoBufMapStringString);
var i: Integer;
begin
  i := Length(FMapStrStr);
  if Index >= i then
    SetLength(FMapStrStr, Index + 1);
  FMapStrStr[Index] := Value;
  FieldTouched[5] := true;
end;

function TSampleProto.GetMapStrStrCount: Integer;
begin
  Result := Length(FMapStrStr);
end;

procedure TSampleProto.SetMapStrStrCount(const Value: Integer);
begin
  SetLength(FMapStrStr, Value);
end;

{ TTwoIntsProto }

const
  TwoIntsFieldMaxID = 2;
  TwoIntsWireDataSubTypes: array [0..TwoIntsFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstVarIntInt32, pbwstVarIntInt32);

constructor TTwoIntsProto.Create;
begin
  inherited Create(TwoIntsFieldMaxID);
end;

constructor TTwoIntsProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TTwoIntsProto.Destroy;
begin
  inherited;
end;

function TTwoIntsProto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= FieldMaxID then
    Result := TwoIntsWireDataSubTypes[Index];
end;

function TTwoIntsProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Result := nil;

  case FieldIdx of
    1: Result := @FAnIntOne;
    2: Result := @FAnIntTwo;
  end;
end;

procedure TTwoIntsProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;

  case FieldIdx of
    1: FAnIntOne := pInteger(Value)^;
    2: FAnIntTwo := pInteger(Value)^;
  end;
end;

procedure TTwoIntsProto.SetAnIntOne(const Value: Integer);
begin
  FAnIntOne := Value;
  FieldTouched[1] := true;
end;

procedure TTwoIntsProto.SetAnIntTwo(const Value: Integer);
begin
  FAnIntTwo := Value;
  FieldTouched[2] := true;
end;

{ TSimpleWithNestedProto }

const
  SimpleWithNestedFieldMaxID = 2;
  SimpleWithNestedWireDataSubTypes: array [0..SimpleWithNestedFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstNestedMessage, pbwstRepteadNestedMessage);

constructor TSimpleWithNestedProto.Create;
begin
  inherited Create(SimpleWithNestedFieldMaxID);
  FATwoInts := TTwoIntsProto.CreateAsNested(1, Self);

end;

constructor TSimpleWithNestedProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TSimpleWithNestedProto.Destroy;
var i: Integer;
begin
  FATwoInts.Free;

  for i := 0 to High(FManyTwoIntsArray) do
    FManyTwoIntsArray[i].Free;

  inherited;
end;

function TSimpleWithNestedProto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= FieldMaxID then
    Result := SimpleWithNestedWireDataSubTypes[Index];
end;

function TSimpleWithNestedProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx, FieldSubIdx, Repeated: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  FieldSubIdx := FieldIndexIDRepeat and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;  // 1 based
  Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
  Result := nil;

  case FieldIdx of
    1: Result := FATwoInts;
    2:begin  // nested - repeated
          if Repeated < Length(FManyTwoIntsArray) then
            begin
              Result := FManyTwoIntsArray[Repeated];
              inc(Repeated);
            end;
          if Repeated >= Length(FManyTwoIntsArray) then
            Repeated := 0;
      end;
  end;
  FieldIndexIDRepeat := FieldIdx or (FieldSubIdx shl PB_FIELD_SUB_IDX_OFFSET) or
                                    (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);
end;

procedure TSimpleWithNestedProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;

//  case FieldIdx of
    //1: FATwoInts; // nested
    //2: FManyTwoIntsArray; // nested - repeated
//  end;
end;

function TSimpleWithNestedProto.GetManyTwoIntsArray(Index: Integer): TTwoIntsProto;
begin
  if Index >= Length(FManyTwoIntsArray) then
    SetManyTwoIntsArrayCount(Index + 1);
  Result := FManyTwoIntsArray[Index];
end;

function TSimpleWithNestedProto.GetManyTwoIntsArrayCount: Integer;
begin
  Result := Length(FManyTwoIntsArray);
end;

procedure TSimpleWithNestedProto.SetManyTwoIntsArrayCount(const Value: Integer);
var i, l: Integer;
begin
  l := Length(FManyTwoIntsArray);
  if Value > l then
    begin
      SetLength(FManyTwoIntsArray, Value);
      for i := l to Value - 1 do
        FManyTwoIntsArray[i] := TTwoIntsProto.CreateAsNested(2, Self);
    end
  else if Value < l then
    begin
      for i := l -1 downto Value do
        FManyTwoIntsArray[i].Free;
      SetLength(FManyTwoIntsArray, Value);
    end;
end;

function TSimpleWithNestedProto.GetNestedArrayObjectByIndex(var FieldIndexIDRepeat: LongWord): Pointer;
var FieldIdx, Repeated: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
  Result := nil;

  case FieldIdx of
    2:begin
      if Repeated >= GetManyTwoIntsArrayCount then
        SetManyTwoIntsArrayCount(Repeated + 1);
      Result := GetManyTwoIntsArray(Repeated);
      inc(Repeated);
    end;

  end;
  FieldIndexIDRepeat := FieldIdx or (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);
end;

{ TComplexNested2Proto }

const
  ComplexNested2FieldMaxID = 2;
  ComplexNested2WireDataSubTypes: array [0..ComplexNested2FieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstVarIntInt32, pbwstVarIntEnum);

constructor TComplexNested2Proto.Create;
begin
  inherited Create(ComplexNested2FieldMaxID);
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
  if Index <= FieldMaxID then
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
  ComplexFieldMaxID = 1;
  ComplexWireDataSubTypes: array [0..ComplexFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstLenString);

constructor TComplexProto.Create;
begin
  inherited Create(ComplexFieldMaxID);
end;

constructor TComplexProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TComplexProto.Destroy;
begin
  inherited;
end;

function TComplexProto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= FieldMaxID then
    Result := ComplexWireDataSubTypes[Index];
end;

function TComplexProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Result := nil;

  case FieldIdx of
    1: Result := @FAstringInComplex;
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
  if Index <= FieldMaxID then
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
  MessyFieldMaxID = 8;
  MessyWireDataSubTypes: array [0..MessyFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstLenString, pbwstVarIntBool, pbwstVarIntInt64, pbwstVarIntEnum, pbwstUnused, pbwstLenString, 
    pbwstUnused, pbwstLenBytes);

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
  if Index <= FieldMaxID then
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
    8: begin Result := FBytesfield; Size := FBytesfieldSize; end;
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
    8: SetBytesfield(Value, Size);
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
  if Index <= FieldMaxID then
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
  if Index <= FieldMaxID then
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
