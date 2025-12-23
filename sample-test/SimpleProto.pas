{
Subject to the terms of the MIT license: Copyright (c) 2025 HHS Software Corp.
  Created with protoc-pascal.exe   Version v1.02  (2025)
    https://github.com/HHS-Software/Protocol-Buffer_Delphi_FPC
    A utility to read and convert the Google Protocol Buffer v3 system into pascal code.
    The code is designed for the v3 proto.  Later proto versions possibly too.
}
unit SimpleProto;

interface

{$IFDEF FPC}
{$mode delphi}{$H+}
uses SysUtils, Classes,
{$ELSE}
uses System.Classes, System.SysUtils,
{$ENDIF}
 ProtocolBuffer;

type

  TSimpleProto = class(TProtocolBuffer)
  private
    FReqId: Integer;  // 1 Opt
    FField2: Double;  // 2 Opt
    FSomeString: string;  // 3 Opt
    FManyInts: TPBArrayOfInteger;  // 4 Rep
    FMapStrStr: TProtoBufMapStringStringArray;  // 5 Map Rep
    procedure SetReqId(const Value: Integer);
    procedure SetField2(const Value: Double);
    procedure SetSomeString(const Value: string);
    function GetManyInts(Index: Integer): Integer;
    procedure SetManyInts(Index: Integer; const Value: Integer);
    function GetManyIntsCount: Integer;
    procedure SetManyIntsCount(const Value: Integer);
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
    property ManyInts[Index: Integer]: Integer read GetManyInts write SetManyInts;  // 4
    property ManyIntsCount: Integer read GetManyIntsCount write SetManyIntsCount;  // 4
    property MapStrStr[Index: Integer]: TProtoBufMapStringString read GetMapStrStr write SetMapStrStr;  // 5
    property MapStrStrCount: Integer read GetMapStrStrCount write SetMapStrStrCount;  // 5
  end;

implementation

{ TSimpleProto }

const
  SimpleFieldMaxID = 5;
  SimpleWireDataSubTypes: array [0..SimpleFieldMaxID] of TProtoBufWireDataSubType = (
    pbwstUnused, pbwstVarIntInt32, pbwstI64Double, pbwstLenString, pbwstVarIntInt32, pbwstMapMessage);
  SimpleMapStrStrWireDataSubTypes: array [0..2] of TProtoBufWireDataSubType =
    (pbwstUnknown, pbwstLenString, pbwstLenString);

constructor TSimpleProto.Create;
begin
  inherited Create(SimpleFieldMaxID);
end;

constructor TSimpleProto.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor TSimpleProto.Destroy;
begin
  inherited;
end;

function TSimpleProto.WireDataSubType(Index: LongWord): TProtoBufWireDataSubType;
var FieldIdx, FieldSubIdx: Word;
begin
  FieldIdx := Index and PB_FIELD_IDX_MASK;
  FieldSubIdx := Index and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;
  Result := pbwstUnknown;
  if FieldIdx <= FieldMaxID then
    Result := SimpleWireDataSubTypes[FieldIdx];
  if FieldSubIdx > 0 then
    case FieldIdx of
      5: Result := SimpleMapStrStrWireDataSubTypes[FieldSubIdx];
    end;
end;

function TSimpleProto.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
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
          if Repeated < Length(FManyInts) then
            begin
              Result := @FManyInts[Repeated];
              inc(Repeated);
            end;
          if Repeated >= Length(FManyInts) then
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

procedure TSimpleProto.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
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
            FManyInts := nil
          else
            begin
              if Repeated >= Length(FManyInts) then
                SetLength(FManyInts, Repeated + 1);
              FManyInts[Repeated] := pInteger(Value)^;
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

procedure TSimpleProto.SetReqId(const Value: Integer);
begin
  FReqId := Value;
  FieldTouched[1] := true;
end;

procedure TSimpleProto.SetField2(const Value: Double);
begin
  FField2 := Value;
  FieldTouched[2] := true;
end;

procedure TSimpleProto.SetSomeString(const Value: string);
begin
  FSomeString := Value;
  FieldTouched[3] := true;
end;

function TSimpleProto.GetManyInts(Index: Integer): Integer;
begin
  if Index <= Length(FManyInts) then
    Result := FManyInts[Index]
  else
    Result := FManyInts[0];
end;

procedure TSimpleProto.SetManyInts(Index: Integer; const Value: Integer);
var i: Integer;
begin
  i := Length(FManyInts);
  if Index >= i then
    SetLength(FManyInts, Index + 1);
  FManyInts[Index] := Value;
  FieldTouched[4] := true;
end;

function TSimpleProto.GetManyIntsCount: Integer;
begin
  Result := Length(FManyInts);
end;

procedure TSimpleProto.SetManyIntsCount(const Value: Integer);
begin
  SetLength(FManyInts, Value);
end;

function TSimpleProto.GetMapStrStr(Index: Integer): TProtoBufMapStringString;
begin
  if Index <= Length(FMapStrStr) then
    Result := FMapStrStr[Index]
  else
    Result := FMapStrStr[0];
end;

procedure TSimpleProto.SetMapStrStr(Index: Integer; const Value: TProtoBufMapStringString);
var i: Integer;
begin
  i := Length(FMapStrStr);
  if Index >= i then
    SetLength(FMapStrStr, Index + 1);
  FMapStrStr[Index] := Value;
  FieldTouched[5] := true;
end;

function TSimpleProto.GetMapStrStrCount: Integer;
begin
  Result := Length(FMapStrStr);
end;

procedure TSimpleProto.SetMapStrStrCount(const Value: Integer);
begin
  SetLength(FMapStrStr, Value);
end;

end.
