

//  This file is a template that is used in conjunction with protoc-pascal.exe,
//  that generates the final T<someproto>.pas files.  This file CANNOT be compiled.
//  You can make some limited edits or additions below.  i.e. to add a new property, or a
//  new procedure.  Be sure to use the existing place holder tokens for PROTONAME, etc.
//  For major changes, edits to the Generator code will be required as well.


{*UNITNAME*}unit;

interface

{$IFDEF FPC}
{$mode delphi}{$H+}
uses SysUtils, Classes,
{$ELSE}
uses System.Classes, System.SysUtils,
{$ENDIF}
{*USES*}
 ProtocolBuffer;

type
{*FORWARDS*}
{*ENUMTYPE*}
{*INTROEND*}


{*TYPEHEADERSTART*}
  {*TPROTONAME*} = class(TProtocolBuffer)
  private
{*PRIVATEFIELD*}
{*PRIVATESETGET*}
  public
    constructor Create;
    constructor CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
    destructor Destroy; override;
    function WireDataSubType(Index: LongWord): TProtoBufWireDataSubType; override;
    function ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer; override;
    procedure WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer); override;
{*PUBLICFUNCPROC*}
{*PROPERTIES*}
  end;

{*TYPEHEADEREND*}

implementation


{*TYPEBODYSTART*}
{ {*TPROTONAME*} }

const
  {*PROTONAME*}FieldMaxID = {*MAXID*};
  {*PROTONAME*}WireDataSubTypes: array [0..{*PROTONAME*}FieldMaxID] of TProtoBufWireDataSubType = (
{*WIRESUBTYPES*}
{*WIRESUBTYPESMAPS*}

constructor {*TPROTONAME*}.Create;
begin
  inherited Create({*PROTONAME*}FieldMaxID);
{*NESTEDCREATE*}
{*ONCREATEDEFAULT*}
end;

constructor {*TPROTONAME*}.CreateAsNested(ParentIndex: Integer; Parent: TProtocolBuffer);
begin
  Create;
  SetParentDetails(ParentIndex, Parent);
end;

destructor {*TPROTONAME*}.Destroy;
{*NESTEDFREEVAR*}
begin
{*NESTEDFREE*}
  inherited;
end;

{*WIREDATASUBTYPEFUNC*}
function {*TPROTONAME*}.WireDataSubType(Index: Longword): TProtoBufWireDataSubType;
begin
  Result := pbwstUnknown;
  if Index <= FieldMaxID then
    Result := {*PROTONAME*}WireDataSubTypes[Index];
end;
{*WIREDATASUBTYPEFUNCEND*}

{*WIREDATASUBMAPTYPEFUNC*}
function {*TPROTONAME*}.WireDataSubType(Index: LongWord): TProtoBufWireDataSubType;
var FieldIdx, FieldSubIdx: Word;
begin
  FieldIdx := Index and PB_FIELD_IDX_MASK;
  FieldSubIdx := Index and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;
  Result := pbwstUnknown;
  if FieldIdx <= FieldMaxID then
    Result := {*PROTONAME*}WireDataSubTypes[FieldIdx];
  if FieldSubIdx > 0 then
    case FieldIdx of
{*WIREDATASUBMAPTYPEFUNCCASE*}
    end;
end;
{*WIREDATASUBMAPTYPEFUNCEND*}

function {*TPROTONAME*}.ReadFieldByIndex(var FieldIndexIDRepeat: LongWord; var Size: Integer): Pointer;
var FieldIdx, FieldSubIdx, Repeated: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  FieldSubIdx := FieldIndexIDRepeat and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;  // 1 based
  Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
  Result := nil;

  case FieldIdx of
{*READFIELDBYINDEX*}
  end;
  FieldIndexIDRepeat := FieldIdx or (FieldSubIdx shl PB_FIELD_SUB_IDX_OFFSET) or
                                    (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);
{*READFIELDBYINDEXSIZECASE*}
end;

procedure {*TPROTONAME*}.WriteFieldByIndex(FieldIndexIDRepeat: LongWord; Value: Pointer; Size: Integer);
var FieldIdx, FieldSubIdx, Repeated: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  FieldSubIdx := FieldIndexIDRepeat and PB_FIELD_SUB_IDX_MASK shr PB_FIELD_SUB_IDX_OFFSET;  // 1 based
  Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
  if FieldIdx <= FieldMaxID then
    FieldTouched[FieldIdx] := true;
{*WRITEFIELDBYINDEXTOUCHDADJUST*}

  case FieldIdx of
{*WRITEFIELDBYINDEX*}
  end;
end;

{*SETGETMETHODS*}

{*TYPEBODYEND*}

end.


{*READFIELDBYINDEXARRAY*}
          if Repeated < Length({*FPROPNAME*}) then
            begin
              Result := @{*FPROPNAME*}[Repeated];
              inc(Repeated);
            end;
          if Repeated >= Length({*FPROPNAME*}) then
            Repeated := 0;
{*READFIELDBYINDEXARRAYEND*}

{*WRITEFIELDBYINDEXARRAY*}
          if Repeated = $FFFF then //  -1 clears the array
            {*FPROPNAME*} := nil
          else
            begin
              if Repeated >= Length({*FPROPNAME*}) then
                SetLength({*FPROPNAME*}, Repeated + 1);
              {*FPROPNAME*}[Repeated] := p{*DATATYPE*}(Value)^;
            end;
{*WRITEFIELDBYINDEXARRAYEND*}

{*READMAPFIELDBYINDEX*}
          if Repeated <= Length({*FPROPNAME*}) then
             case FieldSubIdx of
               1:begin
                   Result := @{*FPROPNAME*}[Repeated-1].Key;
                   inc(FieldSubIdx);
                 end;
               2:begin
                   Result := @{*FPROPNAME*}[Repeated-1].Value;
                   inc(Repeated);
                   dec(FieldSubIdx);
                 end;
             end;
          if Repeated > Length({*FPROPNAME*}) then
            Repeated := 0;
{*READMAPFIELDBYINDEXEND*}

{*WRITEMAPFIELDBYINDEX*}
          if Repeated = $FFFF then //  -1 clears the array
            {*FPROPNAME*} := nil
          else if Repeated > 0 then
            begin
              if Repeated > Length({*FPROPNAME*}) then
                SetLength({*FPROPNAME*}, Repeated);
              case FieldSubIdx of
                1: {*FPROPNAME*}[Repeated -1].Key := p{*DATATYPEKEY*}(Value)^;
                2: {*FPROPNAME*}[Repeated -1].Value := p{*DATATYPEVALUE*}(Value)^;
              end;
            end;
{*WRITEMAPFIELDBYINDEXEND*}


{*GETSETCOUNTMETHODSARRAY*}
function {*TPROTONAME*}.{*GETITEMARRAYFUNCNAME*};
begin
  if Index <= Length({*FPROPNAME*}) then
    Result := {*FPROPNAME*}[Index]
  else
    Result := {*FPROPNAME*}[0];
end;

procedure {*TPROTONAME*}.{*SETITEMARRAYFUNCNAME*};
var i: Integer;
begin
  i := Length({*FPROPNAME*});
  if Index >= i then
    SetLength({*FPROPNAME*}, Index + 1);
  {*FPROPNAME*}[Index] := Value;
  FieldTouched[{*INDEXNUM*}] := true;
end;
{*GETSETCOUNTMETHODSARRAYEND*}


{*SETPROPARRAYCOUNT*}
function {*TPROTONAME*}.{*GETCOUNTARRAYFUNCNAME*};
begin
  Result := Length({*FPROPNAME*});
end;

procedure {*TPROTONAME*}.{*SETCOUNTARRAYFUNCNAME*};
begin
  SetLength({*FPROPNAME*}, Value);
end;
{*SETPROPARRAYCOUNTEND*}


{*GETSETNESTEDPROPARRAYCOUNT*}
function {*TPROTONAME*}.{*GETNESTEDITEMARRAYFUNCNAME*};
begin
  if Index >= Length({*FPROPNAME*}) then
    {*SETNESTEDCOUNTARRAYFUNCNAMENOPARAM*}(Index + 1);
  Result := {*FPROPNAME*}[Index];
end;

function {*TPROTONAME*}.{*GETNESTEDCOUNTARRAYFUNCNAME*};
begin
  Result := Length({*FPROPNAME*});
end;

procedure {*TPROTONAME*}.{*SETNESTEDCOUNTARRAYFUNCNAME*};
var i, l: Integer;
begin
  l := Length({*FPROPNAME*});
  if Value > l then
    begin
      SetLength({*FPROPNAME*}, Value);
      for i := l to Value - 1 do
        {*FPROPNAME*}[i] := {*TNESTEDPROTONAME*}.CreateAsNested({*INDEXNUM*}, Self);
    end
  else if Value < l then
    begin
      for i := l -1 downto Value do
        {*FPROPNAME*}[i].Free;
      SetLength({*FPROPNAME*}, Value);
    end;
end;
{*GETSETNESTEDPROPARRAYCOUNTEND*}


{*GETNESTEDARRAYOBJECTBYINDEX*}
function {*TPROTONAME*}.GetNestedArrayObjectByIndex(var FieldIndexIDRepeat: LongWord): Pointer;
var FieldIdx, Repeated: Word;
begin
  FieldIdx := FieldIndexIDRepeat and PB_FIELD_IDX_MASK;
  Repeated := FieldIndexIDRepeat shr PB_FIELD_REPEAT_IDX_OFFSET;  // 1 based
  Result := nil;

  case FieldIdx of
{*GETNESTEDARRAYOBJECTBYINDEXCASEINSERT*}
  end;
  FieldIndexIDRepeat := FieldIdx or (Repeated shl PB_FIELD_REPEAT_IDX_OFFSET);
end;
{*GETNESTEDARRAYOBJECTBYINDEXEND*}


{*SETBYTESARRAY*}
procedure {*TPROTONAME*}.{*SETITEMARRAYFUNCNAME*};
begin;
  FreeMem({*FPROPNAME*});
  GetMem({*FPROPNAME*}, Size);
  {*FPROPNAME*}Size := Size;
  Move(Value^, {*FPROPNAME*}^, Size);
  FieldTouched[{*INDEXNUM*}] := true;
end;
{*SETBYTESARRAYEND*}


