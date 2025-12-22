{
Subject to the terms of the MIT license: Copyright (c) 2025 HHS Software Corp.
  Created with protoc-pascal.exe   Version v1.02  (2025)
    https://github.com/HHS-Software/Protocol-Buffer_Delphi_FPC
    A utility to read and convert the Google Protocol Buffer v3 system into pascal code.
    The code is designed for the v3 proto.  Later proto versions possibly too.
}
unit NestedExampleProto;

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
  TAddressDetailProto = class;
  TNestedProto = class;

  TNestedExampleProto = class(TProtocolBuffer)
  private
    FOrder: Integer;  // 1 Opt
    FNestedField: TNestedProto;  // 2 Opt
    FItem: string;  // 3 Opt
    FManyNestedFieldArray: array of TNestedProto;  // 4 Rep
    procedure SetOrder(const Value: Integer);
    procedure SetItem(const Value: string);
    function GetManyNestedFieldArray(Index: Integer): TNestedProto;
    function GetManyNestedFieldArrayCount: Integer;
    procedure SetManyNestedFieldArrayCount(const Value: Integer);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    function GetNestedArrayObjectByIndex(var FieldIndexIDRepeat: LongWord): Pointer; override;
    property Order: Integer read FOrder write SetOrder;  // 1
    property NestedField: TNestedProto read FNestedField;  // 2
    property Item: string read FItem write SetItem;  // 3
    property ManyNestedFieldArray[Index: Integer]: TNestedProto read GetManyNestedFieldArray;  // 4
    property ManyNestedFieldArrayCount: Integer read GetManyNestedFieldArrayCount write SetManyNestedFieldArrayCount;
  end;

  TNestedProto = class(TProtocolBuffer)
  private
    FAge: Integer;  // 1 Opt
    FName: string;  // 2 Opt
    FAddress: TAddressDetailProto;  // 3 Opt
    procedure SetAge(const Value: Integer);
    procedure SetName(const Value: string);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    property Age: Integer read FAge write SetAge;  // 1
    property Name: string read FName write SetName;  // 2
    property Address: TAddressDetailProto read FAddress;  // 3
  end;

  TAddressDetailProto = class(TProtocolBuffer)
  private
    FNumber: Integer;  // 1 Opt
    FStreet: string;  // 2 Opt
    FVille: string;  // 3 Opt
    FCity: string;  // 4 Opt
    procedure SetNumber(const Value: Integer);
    procedure SetStreet(const Value: string);
    procedure SetVille(const Value: string);
    procedure SetCity(const Value: string);
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
    property Number: Integer read FNumber write SetNumber;  // 1
    property Street: string read FStreet write SetStreet;  // 2
    property Ville: string read FVille write SetVille;  // 3
    property City: string read FCity write SetCity;  // 4
  end;

implementation

{ TNestedExampleProto }

const
  NestedExampleFieldMaxID = 4;
  NestedExampleWireDataSubTypes: array [0..NestedExampleFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstVarIntInt32, pbwstNestedMessage, pbwstLenString, pbwstRepteadNestedMessage);

constructor TNestedExampleProto.Create;
begin
  inherited Create(NestedExampleFieldMaxID);
  FNestedField := TNestedProto.CreateAsNested(2, Self);

end;

constructor TNestedExampleProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TNestedExampleProto.Destroy;
var i: Integer;
begin
  FNestedField.Free;

  for i := 0 to High(FManyNestedFieldArray) do
    FManyNestedFieldArray[i].Free;

  inherited;
end;

function TNestedExampleProto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= LongWord(FieldMaxID) then
    Result := NestedExampleWireDataSubTypes[Index];
end;

function TNestedExampleProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx, FieldSubIdx, Repeated: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  FieldSubIdx := FieldIndexIDRepeat and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;  // 1 based
  Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
  Result := nil;

  case FieldIdx of
    1: Result := @FOrder;
    2: Result := FNestedField;
    3: Result := @FItem;
    4:begin  // nested - repeated
          if Repeated < Length(FManyNestedFieldArray) then
            begin
              Result := FManyNestedFieldArray[Repeated];
              inc(Repeated);
            end;
          if Repeated >= Length(FManyNestedFieldArray) then
            Repeated := 0;
      end;
  end;
  FieldIndexIDRepeat := FieldIdx or (FieldSubIdx shl PB_FIELD_SUB_IDX_OFFSET) or
                                    (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);
end;

procedure TNestedExampleProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;

  case FieldIdx of
    1: FOrder := pInteger(Value)^;
    //2: FNestedField; // nested
    3: FItem := pstring(Value)^;
    //4: FManyNestedFieldArray; // nested - repeated
  end;
end;

procedure TNestedExampleProto.SetOrder(const Value: Integer);
begin
  FOrder := Value;
  FieldTouched[1] := true;
end;

procedure TNestedExampleProto.SetItem(const Value: string);
begin
  FItem := Value;
  FieldTouched[3] := true;
end;

function TNestedExampleProto.GetManyNestedFieldArray(Index: Integer): TNestedProto;
begin
  if Index >= Length(FManyNestedFieldArray) then
    SetManyNestedFieldArrayCount(Index + 1);
  Result := FManyNestedFieldArray[Index];
end;

function TNestedExampleProto.GetManyNestedFieldArrayCount: Integer;
begin
  Result := Length(FManyNestedFieldArray);
end;

procedure TNestedExampleProto.SetManyNestedFieldArrayCount(const Value: Integer);
var i, l: Integer;
begin
  l := Length(FManyNestedFieldArray);
  if Value > l then
    begin
      SetLength(FManyNestedFieldArray, Value);
      for i := l to Value - 1 do
        FManyNestedFieldArray[i] := TNestedProto.CreateAsNested(4, Self);
    end
  else if Value < l then
    begin
      for i := l -1 downto Value do
        FManyNestedFieldArray[i].Free;
      SetLength(FManyNestedFieldArray, Value);
    end;
end;

function TNestedExampleProto.GetNestedArrayObjectByIndex(var FieldIndexIDRepeat: LongWord): Pointer;
var FieldIdx, Repeated: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
  Result := nil;

  case FieldIdx of
    4:begin
      if Repeated >= GetManyNestedFieldArrayCount then
        SetManyNestedFieldArrayCount(Repeated + 1);
      Result := GetManyNestedFieldArray(Repeated);
      inc(Repeated);
    end;

  end;
  FieldIndexIDRepeat := FieldIdx or (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);
end;

{ TNestedProto }

const
  NestedFieldMaxID = 3;
  NestedWireDataSubTypes: array [0..NestedFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstVarIntInt32, pbwstLenString, pbwstNestedMessage);

constructor TNestedProto.Create;
begin
  inherited Create(NestedFieldMaxID);
  FAddress := TAddressDetailProto.CreateAsNested(3, Self);
end;

constructor TNestedProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TNestedProto.Destroy;
begin
  FAddress.Free;
  inherited;
end;

function TNestedProto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= LongWord(FieldMaxID) then
    Result := NestedWireDataSubTypes[Index];
end;

function TNestedProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Result := nil;

  case FieldIdx of
    1: Result := @FAge;
    2: Result := @FName;
    3: Result := FAddress;
  end;
end;

procedure TNestedProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;

  case FieldIdx of
    1: FAge := pInteger(Value)^;
    2: FName := pstring(Value)^;
    //3: FAddress; // nested
  end;
end;

procedure TNestedProto.SetAge(const Value: Integer);
begin
  FAge := Value;
  FieldTouched[1] := true;
end;

procedure TNestedProto.SetName(const Value: string);
begin
  FName := Value;
  FieldTouched[2] := true;
end;

{ TAddressDetailProto }

const
  AddressDetailFieldMaxID = 4;
  AddressDetailWireDataSubTypes: array [0..AddressDetailFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstVarIntInt32, pbwstLenString, pbwstLenString, pbwstLenString);

constructor TAddressDetailProto.Create;
begin
  inherited Create(AddressDetailFieldMaxID);
end;

constructor TAddressDetailProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TAddressDetailProto.Destroy;
begin
  inherited;
end;

function TAddressDetailProto.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= LongWord(FieldMaxID) then
    Result := AddressDetailWireDataSubTypes[Index];
end;

function TAddressDetailProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Result := nil;

  case FieldIdx of
    1: Result := @FNumber;
    2: Result := @FStreet;
    3: Result := @FVille;
    4: Result := @FCity;
  end;
end;

procedure TAddressDetailProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;

  case FieldIdx of
    1: FNumber := pInteger(Value)^;
    2: FStreet := pstring(Value)^;
    3: FVille := pstring(Value)^;
    4: FCity := pstring(Value)^;
  end;
end;

procedure TAddressDetailProto.SetNumber(const Value: Integer);
begin
  FNumber := Value;
  FieldTouched[1] := true;
end;

procedure TAddressDetailProto.SetStreet(const Value: string);
begin
  FStreet := Value;
  FieldTouched[2] := true;
end;

procedure TAddressDetailProto.SetVille(const Value: string);
begin
  FVille := Value;
  FieldTouched[3] := true;
end;

procedure TAddressDetailProto.SetCity(const Value: string);
begin
  FCity := Value;
  FieldTouched[4] := true;
end;

end.
