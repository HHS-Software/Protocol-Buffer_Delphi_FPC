unit UprotocGenerator;

interface

{$IFDEF FPC}
{$mode delphi}{$H+}
uses SysUtils, Classes, Math,
{$ELSE}
uses System.Classes, System.SysUtils, System.Math, Winapi.Windows,
{$ENDIF}
  UprotocConst;

 function TokenToPascal(ProtoTokenList: TList; DelphiFileStrList: TStringList;
                    ClassPrefix, ClassName: string; var UnitNameStr: string): Boolean;

 var
   DefaultsFile: string = '';
   SourceVersionFile: string = '';
   Prefix: string = '';
   UseIfValuePropTest: Boolean;
   TemplateStrList: TStringList;


implementation

uses Uprotoc;

function TokenToPascal(ProtoTokenList: TList; DelphiFileStrList: TStringList;
                    ClassPrefix, ClassName: string; var UnitNameStr: string): Boolean;

var i, j, f, t, ln, ln2,
  MaxIDInt: Integer;

  PrefixStr,
  ProtoNameStr,
  EnumNameStr,
  EnumNameStrRaw,
  UsesStr,
  ParentIndexStr,
  WireSubTypesStr,
  WireSubTypesMapsStr,
  WireSubTypeKeyStr,
  WireSubTypeValueStr,
  WireDataSubMapTypeFuncCaseStr,
  TProtoNameStr,
  TNestProtoNameStr,
  FNestProtoNameStr,
  NestProtoNameStr,
  NestedCreateStr,
  NestedFreeStr,
  FFieldNameStr,
  FieldNameStr,
  FieldTypeStr,
  FieldTypeArrayStr,
  NestedFreeVarStr,
  GetNestedArrayObjectByIndexCaseInsertStr,
  s, s2, s3: string;

  Token: PTProtoFieldToken;

  WireDataSubTypeKey,
  WireDataSubTypeValue: TWireDataSubType;

  DefaultsStrList,    // predefined default values to try
  OnCreateDefaultList,  // per class
  PrivateFieldStrList,  // per class
  PrivateSetGetStrList,  // per class
  PropertiesStrList,    // per class
  PublicFuncPropStrList, // per class
  ReadFieldByIndexStrList,  // per class
  ReadFieldByIndexSizeCaseStrList,  // per class
  WriteFieldByIndexStrList, // per class
  ClassBodyStrList,  // per class
  WriteTouchAdjustStrList,
  SetGetMethodsStrList,  // full file
  ForwardsStrList,       // full file
  HeaderStrList,  // unit to implement - full file
  BodyStrList,   // implement down - full file
  WorkStrList,    // anything - temp
  VersionStrList,
  UsesStrList,      // full file
  EnumTypesStrList,  // all file
  EnumTypesNamesStrList,
  ConstNameValueStrList
  : TStringList;

  y, m, d: Word;

  EnumArray: array of string;
  FieldSubTypeArray: array of TWireDataSubType;

  NoProp, Repeated, SubIdx, NoCode: Boolean;
begin
  Result := false;
  ParseError := '';
  ErrorLine := '';
  PrefixStr := ClassPrefix;
  EnumArray := nil;

  DefaultsStrList := TStringList.Create;
  OnCreateDefaultList := TStringList.Create;
  PrivateFieldStrList := TStringList.Create;
  PrivateSetGetStrList := TStringList.Create;
  PropertiesStrList := TStringList.Create;
  PublicFuncPropStrList := TStringList.Create;
  SetGetMethodsStrList := TStringList.Create;
  ForwardsStrList := TStringList.Create;
  ReadFieldByIndexStrList := TStringList.Create;
  ReadFieldByIndexSizeCaseStrList := TStringList.Create;
  WriteFieldByIndexStrList := TStringList.Create;
  WorkStrList := TStringList.Create;
  HeaderStrList := TStringList.Create;
  ClassBodyStrList := TStringList.Create;
  BodyStrList := TStringList.Create;
  WriteTouchAdjustStrList := TStringList.Create;
  VersionStrList := TStringList.Create;
  UsesStrList := TStringList.Create;
  EnumTypesStrList := TStringList.Create;
  EnumTypesNamesStrList := TStringList.Create;
  ConstNameValueStrList := TStringList.Create;

  try

    ForwardsStrList.Sorted := true;
    ForwardsStrList.Duplicates := dupIgnore;
    EnumTypesNamesStrList.Sorted := true;
    EnumTypesNamesStrList.Duplicates := dupIgnore;

    if (Length(DefaultsFile) > 0) and FileExists(DefaultsFile) then
      DefaultsStrList.LoadFromFile(DefaultsFile);


    repeat  // for messages;
      NoProp := false;

      ProtoNameStr := '';
      EnumNameStr := '';
      WireSubTypesStr := '';
      WireSubTypesMapsStr := '';
      TProtoNameStr := '';
      TNestProtoNameStr := '';
      FNestProtoNameStr := '';
      NestProtoNameStr := '';
      NestedCreateStr := '';
      NestedFreeStr := '';
      NestedFreeVarStr := '';
      WireSubTypeKeyStr := '';
      WireSubTypeValueStr := '';
      WireDataSubMapTypeFuncCaseStr := '';
      GetNestedArrayObjectByIndexCaseInsertStr := '';
      ParentIndexStr := '';
      FieldTypeStr := '';

      OnCreateDefaultList.Clear;
      PrivateFieldStrList.Clear;
      PrivateSetGetStrList.Clear;
      PropertiesStrList.Clear;
      PublicFuncPropStrList.Clear;
      ReadFieldByIndexStrList.Clear;
      ReadFieldByIndexSizeCaseStrList.Clear;
      WriteFieldByIndexStrList.Clear;
      WorkStrList.Clear;
      ClassBodyStrList.Clear;
      SetGetMethodsStrList.Clear;
      WriteTouchAdjustStrList.Clear;

      f := -1;
      t := -1;

      // is it an empty proto
      for i := 0 to ProtoTokenList.Count - 1 do
        begin
          Token := PTProtoFieldToken(ProtoTokenList[i]);
          if (f = -1) and Token.HeaderToken then
            begin
              f := i;
              t := f;
              // a zero tag object
              NoProp := (i + 1 <= ProtoTokenList.Count - 1) and (PTProtoFieldToken(ProtoTokenList[i+1]).HeaderToken);
              if NoProp then
                Break;
              Continue;
            end;
          if (f > -1) and (i = ProtoTokenList.Count - 1) then
            t := i
          else if (f > -1) and PTProtoFieldToken(ProtoTokenList[i+1]).HeaderToken then
            begin
              t := i;
              Break;
            end;
        end;
      if f = -1 then
        Break;


      // enum
      if PTProtoFieldToken(ProtoTokenList[f]).HeaderToken and (PTProtoFieldToken(ProtoTokenList[f]).ProtoType = pbwstVarIntEnum) then
        begin
          EnumNameStrRaw := '';
          for i := f to t do
            begin
              Token := PTProtoFieldToken(ProtoTokenList[i]);
              if Token.HeaderToken then
                begin
                  EnumNameStr := Token.ProtoName;
                  EnumNameStrRaw := EnumNameStr;
                  EnumNameStr[1] := Char(Ord(EnumNameStr[1]) and (not $20));
                  // purge class enums to file enums list

                end
              else          // should be the enum const values
                begin
                  SetLength(EnumArray, Length(EnumArray) + 1);
                  EnumArray[High(EnumArray)] := Token.FieldName + '=' + IntToStr(Token.FieldIndex);
                end;
            end;

          if EnumConst then
            begin
              for i := 0 to High(EnumArray) do
                begin
                  s := EnumArray[i];
                  if Length(s) = 0 then
                    Continue;
                  s := StringReplace(s, '=', ' = ', [rfReplaceAll]);
                  if Length(ClassPrefix) > 0 then
                    s := ClassPrefix + '_' + s
                  else
                    s := 'PB_' + s;
                  s := s + ';';
                  s := Uppercase(s);
                  while ConstNameValueStrList.IndexOf(s) > -1 do
                    Insert('a', s, Pos(' ', s));
                  ConstNameValueStrList.Add(s);
                end;
            end
          else
            begin
              if Length(ClassPrefix) > 0 then
                begin
                  s := 'T' + ClassPrefix + EnumNameStr;
                  s2 := ClassPrefix;
                end
              else
                begin
                  s := 'TPB' + EnumNameStr;
                  s2 := 'pb';
                end;
              EnumTypesNamesStrList.Add(EnumNameStrRaw + '=' + s);
              EnumTypesStrList.Add('p' + s + ' = ^' + s + ';');
              s := s + ' = (';
              s2 := s2 + Copy(EnumNameStr, 1, 2);
              s2 := LowerCase(s2);
              for i := 0 to High(EnumArray) do
                begin
                  s3 := EnumArray[i];
                  Delete(s3, Pos('=', s3), 100);
                  if s3 = '' then
                    s3 := 'NoValue' + IntToStr(i)
                  else
                    begin
                      s3 := StringReplace(s3, '__', ' ', [rfReplaceAll]);
                      s3 := StringReplace(s3, '--', ' ', [rfReplaceAll]);
                      s3 := StringReplace(s3, '_', ' ', [rfReplaceAll]);
                      s3 := StringReplace(s3, '-', ' ', [rfReplaceAll]);
                      s3 := Lowercase(s3);
                      s3[1] := Char(Ord(s3[1]) and (not $20));
                      j := Pos(' ', s3);
                      while j > 0 do
                        begin
                          s3[j+1] := Char(Ord(s3[j+1]) and (not $20));
                          j := Pos(' ', s3, j+ 1);
                        end;
                      s3 := StringReplace(s3, ' ', '', [rfReplaceAll]);
                    end;
                  s := s + s2 + s3 + ', ';
                end;
              s := s + s2 + 'ScopeErr);';
              while EnumTypesStrList.IndexOf(s) > -1 do
                Insert('a', s, Length(s) - 10);

              EnumTypesStrList.Add(s);
            end;
          EnumArray := nil;

          for i := t downto f do   // delete tokens for this enum
            begin
              Finalize(PTProtoFieldToken(ProtoTokenList[i])^);
              Dispose(PTProtoFieldToken(ProtoTokenList[i]));
              ProtoTokenList.Delete(i);
            end;

          // go through other tokens and find / adjust for this new Enum type name
          for i := 0 to ProtoTokenList.Count - 1 do
            begin
              Token := PTProtoFieldToken(ProtoTokenList[i]);
              if Token.ProtoName = EnumNameStrRaw then
                Token.ProtoType := pbwstVarIntEnum;
            end;
          Continue;
        end;


      FieldSubTypeArray := nil;
      MaxIDInt := 0;
      for i := f to t do
        MaxIDInt := Max(MaxIDInt, PTProtoFieldToken(ProtoTokenList[i]).FieldIndex);
      SetLength(FieldSubTypeArray, MaxIDInt + 1);
      FillChar(FieldSubTypeArray[0], Length(FieldSubTypeArray), Ord(pbwstUnused));

      for i := f to t do
        begin
          Token := PTProtoFieldToken(ProtoTokenList[i]);

          if Token.HeaderToken then
            begin
              ProtoNameStr := Token.ProtoName;
              ProtoNameStr[1] := Char(Ord(ProtoNameStr[1]) and (not $20));
              TProtoNameStr := 'T' + PrefixStr + ProtoNameStr + 'Proto';

              // .pas unit file name
              if Length(UnitNameStr) = 0  then
                begin
                  if Length(ClassName) > 0 then
                    UnitNameStr := PrefixStr + ClassName    // mergered
                  else
                    UnitNameStr := PrefixStr + ProtoNameStr + 'Proto';  // single files
                end;

              //  add new header
              ln := TemplateStrList.IndexOf(TypeHeaderStart);
              ln2 := TemplateStrList.IndexOf(TypeHeaderEnd);

              WorkStrList.Clear;
              for j := ln + 1 to ln2 -1 do
                WorkStrList.Add(TemplateStrList[j]);
              s := WorkStrList.Text;
              s := StringReplace(s, TProtoName, TProtoNameStr, [rfReplaceAll]);
              WorkStrList.Text := s;
              HeaderStrList.AddStrings(WorkStrList);

              // get new class body
              ln := TemplateStrList.IndexOf(TypeBodyStart);
              ln2 := TemplateStrList.IndexOf(TypeBodyend);
              for j := ln +1 to ln2-1 do
                ClassBodyStrList.Add(TemplateStrList[j]);

              s := 'struct ' + Token.ProtoName;
              for j := 0 to DefaultsStrList.Count - 2 do
                if (s = DefaultsStrList[j]) then
                  begin
                    for ln := j + 2 to DefaultsStrList.Count - 1 do
                      begin
                        s := DefaultsStrList[ln];
                        if s = '};' then
                          Break;
                        Insert('F',s, 3);
                        OnCreateDefaultList.Add(s);
                      end;
                    Break;
                  end;
              Continue;
            end;

          if Token.Cardinality = psOneof then
            begin

              ln := PTProtoFieldToken(ProtoTokenList[i+1]).FieldIndex;
              ln2 := Token.LastOneofIndex;
              WriteTouchAdjustStrList.Add('  if (FieldIdx >= ' + IntToStr(ln) + ') and (FieldIdx <= ' + IntToStr(ln2) + ') then');
              WriteTouchAdjustStrList.Add('    for i := ' + IntToStr(ln) + ' to ' + IntToStr(ln2) + ' do');
              WriteTouchAdjustStrList.Add('      if FieldIdx <> i then');
              WriteTouchAdjustStrList.Add('        FieldTouched[i] := false;');
              PrivateFieldStrList.Add('    // ' + Token.FieldName + ': OneOf - last field = ' + IntToStr(Token.LastOneofIndex));
              Continue;
            end;

          if Token.ProtoType = pbwstNestedMessage then
            begin
              s := Token.ProtoName;
              s[1] := Char(Ord(s[1]) and (not $20));
              TNestProtoNameStr := 'T' + PrefixStr + s + 'Proto';
              s := Token.FieldName;
              s[1] := Char(Ord(s[1]) and (not $20));
              FNestProtoNameStr := 'F' + s;
              NestProtoNameStr := s;
              ForwardsStrList.Add('  ' + TNestProtoNameStr + ' = class;');
              if Length(NestedCreateStr) > 0 then
                begin
                  NestedCreateStr := NestedCreateStr + CRLF;
                  NestedFreeStr := NestedFreeStr + CRLF;
                end;

              if Token.Cardinality = psRepeated then
                begin
                  Token.ProtoType := pbwstRepteadNestedMessage;
                  NestProtoNameStr := NestProtoNameStr + 'Array';
                  FNestProtoNameStr := 'F' + NestProtoNameStr;

                  PrivateFieldStrList.Add('    ' + FNestProtoNameStr + ': array of ' +  TNestProtoNameStr + ';  // ' + IntToStr(Token.FieldIndex) + ' ' + CardinalityShtStr[Token.Cardinality]);
                  PropertiesStrList.Add('    property ' + NestProtoNameStr + '[Index: Integer]: ' + TNestProtoNameStr + ' read Get' + NestProtoNameStr + ';  // ' + IntToStr(Token.FieldIndex));
                  PropertiesStrList.Add('    property ' + NestProtoNameStr + 'Count: Integer read Get' + NestProtoNameStr + 'Count write Set' + NestProtoNameStr + 'Count;');

                  {*GETSETNESTEDPROPARRAYCOUNT*}
                  ln := TemplateStrList.IndexOf(GetSetNestedPropArrayCount);
                  ln2 := TemplateStrList.IndexOf(GetSetNestedPropArrayCountEnd);
                  WorkStrList.Clear;
                  for j := ln + 1 to ln2 - 1 do
                    WorkStrList.Add(TemplateStrList[j]);
                  s := WorkStrList.Text;

                  s2 := 'Get' + NestProtoNameStr + '(Index: Integer): ' + TNestProtoNameStr;
                  PrivateSetGetStrList.Add('    function ' + s2 + ';');
                  s := StringReplace(s, GetNestedItemArrayFuncName, s2, [rfReplaceAll]);
                  s2 := 'Get' + NestProtoNameStr + 'Count: Integer';
                  PrivateSetGetStrList.Add('    function ' + s2 + ';');
                  s := StringReplace(s, GetNestedCountArrayFuncName, s2, [rfReplaceAll]);
                  s2 := 'Set' + NestProtoNameStr + 'Count(const Value: Integer)';
                  PrivateSetGetStrList.Add('    procedure ' + s2 + ';');
                  s := StringReplace(s, SetNestedCountArrayFuncName, s2, [rfReplaceAll]);
                  s := StringReplace(s, SetNestedCountArrayFuncNameNoParam, Copy(s2, 1, Pos('(', s2) -1), [rfReplaceAll]);
                  s := StringReplace(s, IndexNum, IntToStr(Token.FieldIndex), [rfReplaceAll]);
                  s := StringReplace(s, FPropName, FNestProtoNameStr, [rfReplaceAll]);
                  s := StringReplace(s, TProtoName, TProtoNameStr, [rfReplaceAll]);
                  s := StringReplace(s, TNestedProtoName, TNestProtoNameStr, [rfReplaceAll]);
                  WorkStrList.Text := s;
                  SetGetMethodsStrList.AddStrings(WorkStrList);

                  SetGetMethodsStrList.Add('');

                  if Length(NestedFreeVarStr) = 0 then
                    NestedFreeVarStr := 'var i: Integer;';
                  if Length(NestedFreeStr) > 0 then
                    NestedFreeStr := NestedFreeStr + CRLF;
                  s := '  for i := 0 to High(' + FNestProtoNameStr + ') do' + CRLF + '    ' + FNestProtoNameStr + '[i].Free;' + CRLF;
                  NestedFreeStr := NestedFreeStr + s;

                  {*READFIELDBYINDEXARRAY*}
                  ReadFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) + ':begin  // nested - repeated');
                  ln := TemplateStrList.IndexOf(ReadFieldByIndexArray);
                  ln2 := TemplateStrList.IndexOf(ReadFieldByIndexArrayEnd);
                  WorkStrList.Clear;
                  for j := ln + 1 to ln2 - 1 do
                    WorkStrList.Add(TemplateStrList[j]);
                  s := WorkStrList.Text;
                  s := StringReplace(s, FPropName, FNestProtoNameStr, [rfReplaceAll]);
                  s := StringReplace(s, DataType, FieldTypeStr, [rfReplaceAll]);
                  Delete(s, Pos('@', s), 1);
                  WorkStrList.Text := s;
                  ReadFieldByIndexStrList.AddStrings(WorkStrList);
                  ReadFieldByIndexStrList.Add('      end;');
                  WorkStrList.Clear;

                  {*GETNESTEDARRAYOBJECTBYINDEX*}
                  ln := TemplateStrList.IndexOf(GetNestedArrayObjectByIndex);
                  s := TemplateStrList[ln + 1];
                  Delete(s, Pos('{*TPROTONAME*}.', s), Length('{*TPROTONAME*}.'));
                  PublicFuncPropStrList.Insert(0, '    ' + s + ' override;');
                  s := '    ' + IntToStr(Token.FieldIndex) + ':begin' + CRLF;
                  s := s + '      if Repeated >= Get' + NestProtoNameStr + 'Count then' + CRLF;
                  s := s + '        Set' + NestProtoNameStr + 'Count(Repeated + 1);' + CRLF;
                  s := s + '      Result := Get' + NestProtoNameStr + '(Repeated);' + CRLF;
                  s := s + '      inc(Repeated);' + CRLF;
                  s := s + '    end;' + CRLF;
                  if Length(GetNestedArrayObjectByIndexCaseInsertStr) > 0 then
                    s := CRLF + s;
                  GetNestedArrayObjectByIndexCaseInsertStr := GetNestedArrayObjectByIndexCaseInsertStr + s;
                  WriteFieldByIndexStrList.Add('    //' + IntToStr(Token.FieldIndex) + ': ' + FNestProtoNameStr + '; // nested - repeated');

                end
              else
                begin     // single nested
                  NestedCreateStr := NestedCreateStr + '  ' + FNestProtoNameStr + ' := ' + TNestProtoNameStr + '.CreateAsNested(' +
                                     IntToStr(Token.FieldIndex) + ', Self);';
                  NestedFreeStr := NestedFreeStr + '  ' + FNestProtoNameStr + '.Free;';

                  PrivateFieldStrList.Add('    ' + FNestProtoNameStr + ': ' +  TNestProtoNameStr + ';  // ' + IntToStr(Token.FieldIndex) + ' ' + CardinalityShtStr[Token.Cardinality]);
                  PropertiesStrList.Add('    property ' + NestProtoNameStr + ': ' + TNestProtoNameStr + ' read ' + FNestProtoNameStr + ';  // ' + IntToStr(Token.FieldIndex));

                  ReadFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) + ': Result := ' + FNestProtoNameStr +';');
                  WriteFieldByIndexStrList.Add('    //' + IntToStr(Token.FieldIndex) + ': ' + FNestProtoNameStr + '; // nested');
                end;

              FieldSubTypeArray[Token.FieldIndex] := Token.ProtoType;
            end

          else if Token.ProtoType = pbwstMapPair then
            begin
              Token.ProtoType := pbwstMapMessage;
              s := Copy(Token.ProtoName, 1, Pos('_', Token.ProtoName) -1);
              WireDataSubTypeKey := TWireDataSubType(StrToIntDef(s, 0));
              s := Copy(Token.ProtoName, Pos('_', Token.ProtoName) + 1, 100);
              WireDataSubTypeValue := TWireDataSubType(StrToIntDef(s, 0));
              WireSubTypeKeyStr := WireDataSubTypeToPascal[WireDataSubTypeKey];
              WireSubTypeValueStr := WireDataSubTypeToPascal[WireDataSubTypevalue];
              WireSubTypeKeyStr[1] := Char(Ord(WireSubTypeKeyStr[1]) and (not $20));      // affects only "string"
              WireSubTypeValueStr[1] := Char(Ord(WireSubTypeValueStr[1]) and (not $20));
              Token.ProtoName := WireSubTypeKeyStr + WireSubTypeValueStr;
              s := Token.FieldName;
              s[1] := Char(Ord(s[1]) and (not $20));
              FFieldNameStr := 'F' + s;
              FieldNameStr := s;
              FieldTypeStr := 'TProtoBufMap' + Token.ProtoName;
              PrivateFieldStrList.Add('    ' + FFieldNameStr + ': ' + FieldTypeStr + 'Array;  // ' +
                                       IntToStr(Token.FieldIndex) + ' ' + CardinalityShtStr[Token.Cardinality] + ' ' + CardinalityShtStr[psRepeated]);
              {*GETSETCOUNTMETHODSARRAY*}    {*SETPROPARRAYCOUNT*}
              ln := TemplateStrList.IndexOf(GetSetCountMethodsArray);
              ln2 := TemplateStrList.IndexOf(GetSetCountMethodsArrayEnd);
              WorkStrList.Clear;
              for j := ln + 1 to ln2 - 1 do
                WorkStrList.Add(TemplateStrList[j]);
              WorkStrList.Add('');
              ln := TemplateStrList.IndexOf(SetPropArrayCount);
              ln2 := TemplateStrList.IndexOf(SetPropArrayCountEnd);
              for j := ln + 1 to ln2 - 1 do
                WorkStrList.Add(TemplateStrList[j]);
              s := WorkStrList.Text;

              s2 := 'Get' + FieldNameStr + '(Index: Integer): ' + FieldTypeStr;
              PrivateSetGetStrList.Add('    function ' + s2 + ';');
              s := StringReplace(s, GetItemArrayFuncName, s2, [rfReplaceAll]);
              s2 := 'Set' + FieldNameStr + '(Index: Integer; const Value: ' + FieldTypeStr + ')';
              PrivateSetGetStrList.Add('    procedure ' + s2 + ';');
              s := StringReplace(s, SetItemArrayFuncName, s2, [rfReplaceAll]);
              s2 := 'Get' + FieldNameStr + 'Count: Integer';
              PrivateSetGetStrList.Add('    function ' + s2 + ';');
              s := StringReplace(s, GetCountArrayFuncName, s2, [rfReplaceAll]);
              s2 := 'Set' + FieldNameStr + 'Count(const Value: Integer)';
              PrivateSetGetStrList.Add('    procedure ' + s2 + ';');
              s := StringReplace(s, SetCountArrayFuncName, s2, [rfReplaceAll]);
              s := StringReplace(s, FPropName, FFieldNameStr, [rfReplaceAll]);
              s := StringReplace(s, TProtoName, TProtoNameStr, [rfReplaceAll]);
              s := StringReplace(s, IndexNum, IntToStr(Token.FieldIndex), [rfReplaceAll]);
              WorkStrList.Text := s;
              WorkStrList.Add('');
              SetGetMethodsStrList.AddStrings(WorkStrList);

              PropertiesStrList.Add('    property ' + FieldNameStr + '[Index: Integer]: ' + FieldTypeStr + ' read Get' +
                                     FieldNameStr + ' write Set' + FieldNameStr + ';  // ' + IntToStr(Token.FieldIndex));
              PropertiesStrList.Add('    property ' + FieldNameStr + 'Count: Integer read Get' +
                                     FieldNameStr + 'Count write Set' + FieldNameStr + 'Count;  // ' + IntToStr(Token.FieldIndex));

              {*READMAPFIELDBYINDEX*}
              // map specific
              ln := TemplateStrList.IndexOf(ReadMapFieldByIndex);
              ln2 := TemplateStrList.IndexOf(ReadMapFieldByIndexEnd);
              WorkStrList.Clear;
              for j := ln + 1 to ln2 - 1 do
                WorkStrList.Add(TemplateStrList[j]);
              s := WorkStrList.Text;
              s := StringReplace(s, FPropName, FFieldNameStr, [rfReplaceAll]);
              s := StringReplace(s, DataType, FieldTypeStr, [rfReplaceAll]);

              WorkStrList.Text := s;
              ReadFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) + ': begin');
              for j := 0 to WorkStrList.Count -1 do
                ReadFieldByIndexStrList.Add(WorkStrList[j]);
              ReadFieldByIndexStrList.Add('        end;');

              {*WRITEMAPFIELDBYINDEX*}
              ln := TemplateStrList.IndexOf(WriteMapFieldByIndex);
              ln2 := TemplateStrList.IndexOf(WriteMapFieldByIndexEnd);
              WorkStrList.Clear;
              for j := ln + 1 to ln2 - 1 do
                WorkStrList.Add(TemplateStrList[j]);
              s := WorkStrList.Text;
              s := StringReplace(s, FPropName, FFieldNameStr, [rfReplaceAll]);
              s := StringReplace(s, DataType, FieldTypeStr, [rfReplaceAll]);
              s := StringReplace(s, DataTypeKey, WireSubTypeKeyStr, [rfReplaceAll]);
              s := StringReplace(s, DataTypeValue, WireSubTypeValueStr, [rfReplaceAll]);
              WorkStrList.Text := s;
              WriteFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) + ': begin');
              for j := 0 to WorkStrList.Count -1 do
                WriteFieldByIndexStrList.Add(WorkStrList[j]);
              WriteFieldByIndexStrList.Add('        end;');
              FieldSubTypeArray[Token.FieldIndex] := Token.ProtoType;
              if Length(WireDataSubMapTypeFuncCaseStr) > 0  then
                WireDataSubMapTypeFuncCaseStr := WireDataSubMapTypeFuncCaseStr + CRLF;
              s :=  ProtoNameStr + FieldNameStr; // SampleProtoMapStrStr
              WireDataSubMapTypeFuncCaseStr := WireDataSubMapTypeFuncCaseStr +
                  '      ' + IntToStr(Token.FieldIndex) + ': Result := ' + s + 'WireDataSubTypes[FieldSubIdx];';
              if Length(WireSubTypesMapsStr) > 0  then
                WireSubTypesMapsStr := WireSubTypesMapsStr + CRLF;
              WireSubTypesMapsStr := WireSubTypesMapsStr + '  ' + s + 'WireDataSubTypes: array [0..2] of TProtoBufWireDataSubType =' + CRLF;
              WireSubTypesMapsStr := WireSubTypesMapsStr + '    (pbwstUnknown, ' +
                          WireDataSubTypeAsStr[WireDataSubTypeKey] + ', ' + WireDataSubTypeAsStr[WireDataSubTypeValue] + ');';
            end

          else

            begin    // single fields

              s := Token.FieldName;
              s[1] := Char(Ord(s[1]) and (not $20));
              FFieldNameStr := 'F' + s;
              FieldNameStr := s;

              if Token.Cardinality = psRepeated then
                begin
                  s := WireDataSubTypeToPascal[Token.ProtoType];
                  FieldTypeStr := s;
                  s[1] := Char(Ord(s[1]) and (not $20));
                  FieldTypeArrayStr := 'TPBArrayOf' + s;

                  s := '    ' + FFieldNameStr + ': ' + FieldTypeArrayStr + ';  // ' + IntToStr(Token.FieldIndex) + ' ' + CardinalityShtStr[Token.Cardinality];
                  PrivateFieldStrList.Add(s);

                  {*GETSETCOUNTMETHODSARRAY*}   {*SETPROPARRAYCOUNT*}
                  ln := TemplateStrList.IndexOf(GetSetCountMethodsArray);
                  ln2 := TemplateStrList.IndexOf(GetSetCountMethodsArrayEnd);
                  WorkStrList.Clear;
                  for j := ln + 1 to ln2 - 1 do
                    WorkStrList.Add(TemplateStrList[j]);
                  WorkStrList.Add('');
                  ln := TemplateStrList.IndexOf(SetPropArrayCount);
                  ln2 := TemplateStrList.IndexOf(SetPropArrayCountEnd);
                  for j := ln + 1 to ln2 - 1 do
                    WorkStrList.Add(TemplateStrList[j]);
                  s := WorkStrList.Text;
                  s2 := 'Get' + FieldNameStr + '(Index: Integer): ' + FieldTypeStr;
                  PrivateSetGetStrList.Add('    function ' + s2 + ';');
                  s := StringReplace(s, GetItemArrayFuncName, s2, [rfReplaceAll]);
                  s2 := 'Set' + FieldNameStr + '(Index: Integer; const Value: ' + FieldTypeStr + ')';
                  PrivateSetGetStrList.Add('    procedure ' + s2 + ';');
                  s := StringReplace(s, SetItemArrayFuncName, s2, [rfReplaceAll]);
                  s2 := 'Get' + FieldNameStr + 'Count: Integer';
                  PrivateSetGetStrList.Add('    function ' + s2 + ';');
                  s := StringReplace(s, GetCountArrayFuncName, s2, [rfReplaceAll]);
                  s2 := 'Set' + FieldNameStr + 'Count(const Value: Integer)';
                  PrivateSetGetStrList.Add('    procedure ' + s2 + ';');
                  s := StringReplace(s, SetCountArrayFuncName, s2, [rfReplaceAll]);
                  s := StringReplace(s, FPropName, FFieldNameStr, [rfReplaceAll]);
                  s := StringReplace(s, TProtoName, TProtoNameStr, [rfReplaceAll]);
                  s := StringReplace(s, IndexNum, IntToStr(Token.FieldIndex), [rfReplaceAll]);
                  WorkStrList.Text := s;
                  WorkStrList.Add('');
                  SetGetMethodsStrList.AddStrings(WorkStrList);

                  PropertiesStrList.Add('    property ' + FieldNameStr + '[Index: Integer]: ' + FieldTypeStr + ' read Get' +
                                       FieldNameStr + ' write Set' + FieldNameStr + ';  // ' + IntToStr(Token.FieldIndex));
                  PropertiesStrList.Add('    property ' + FieldNameStr + 'Count: Integer read Get' +
                                       FieldNameStr + 'Count write Set' + FieldNameStr + 'Count;  // ' + IntToStr(Token.FieldIndex));

                  {*READFIELDBYINDEXARRAY*}
                  ln := TemplateStrList.IndexOf(ReadFieldByIndexArray);
                  ln2 := TemplateStrList.IndexOf(ReadFieldByIndexArrayEnd);
                  WorkStrList.Clear;
                  for j := ln + 1 to ln2 - 1 do
                    WorkStrList.Add(TemplateStrList[j]);
                  s := WorkStrList.Text;
                  s := StringReplace(s, FPropName, FFieldNameStr, [rfReplaceAll]);
                  s := StringReplace(s, DataType, FieldTypeStr, [rfReplaceAll]);
                  WorkStrList.Text := s;
                  ReadFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) + ': begin');
                  for j := 0 to WorkStrList.Count -1 do
                    ReadFieldByIndexStrList.Add(WorkStrList[j]);
                  ReadFieldByIndexStrList.Add('        end;');

                  {*WRITEFIELDBYINDEXARRAY*}
                  ln := TemplateStrList.IndexOf(WriteFieldByIndexArray);
                  ln2 := TemplateStrList.IndexOf(WriteFieldByIndexArrayEnd);
                  WorkStrList.Clear;
                  for j := ln + 1 to ln2 - 1 do
                    WorkStrList.Add(TemplateStrList[j]);
                  s := WorkStrList.Text;
                  s := StringReplace(s, FPropName, FFieldNameStr, [rfReplaceAll]);
                  s := StringReplace(s, DataType, FieldTypeStr, [rfReplaceAll]);
                  WorkStrList.Text := s;
                  WriteFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) + ': begin');
                  for j := 0 to WorkStrList.Count -1 do
                    WriteFieldByIndexStrList.Add(WorkStrList[j]);
                  WriteFieldByIndexStrList.Add('        end;');
                end

              else if Token.ProtoType = pbwstLenBytes then
                begin
                  PrivateFieldStrList.Add('    ' + FFieldNameStr + ': Pointer;  // ' +
                                           IntToStr(Token.FieldIndex) + ' ' + CardinalityShtStr[Token.Cardinality]);
                  PrivateFieldStrList.Add('    ' + FFieldNameStr + 'Size: Integer;');

                  PropertiesStrList.Add('    property ' + FieldNameStr + ': Pointer read ' +
                                         FFieldNameStr + ';  // ' + IntToStr(Token.FieldIndex));
                  PropertiesStrList.Add('    property ' + FieldNameStr + 'Size: Integer read ' +
                                         FFieldNameStr + 'Size;');

                  ReadFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) +
                                              ': begin Result := ' + FFieldNameStr + '; Size := ' + FFieldNameStr + 'Size; end;');
                  WriteFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) +
                                               ': Set' + FieldNameStr + '(Value, Size);');
                  NestedFreeStr := NestedFreeStr + '  FreeMem(' + FFieldNameStr + ');';

                  s2 := 'Set' + FieldNameStr + '(const Value: Pointer; Size: Integer)';
                  PublicFuncPropStrList.Add('    procedure ' + s2 + ';');

                  {*SETBYTESARRAY*}
                  ln := TemplateStrList.IndexOf(SetByteArray);
                  ln2 := TemplateStrList.IndexOf(SetByteArrayEnd);
                  WorkStrList.Clear;
                  for j := ln + 1 to ln2 - 1 do
                    WorkStrList.Add(TemplateStrList[j]);
                  s := WorkStrList.Text;
                  s := StringReplace(s, SetItemArrayFuncName, s2, [rfReplaceAll]);
                  s := StringReplace(s, FPropName, FFieldNameStr, [rfReplaceAll]);
                  s := StringReplace(s, TProtoName, TProtoNameStr, [rfReplaceAll]);
                  s := StringReplace(s, IndexNum, IntToStr(Token.FieldIndex), [rfReplaceAll]);
                  WorkStrList.Text := s;
                  WorkStrList.Add('');
                  SetGetMethodsStrList.AddStrings(WorkStrList);
                end

              else if Token.ProtoType = pbwstVarIntEnum then
                begin
                  if EnumConst then
                    s2 := 'Integer'
                  else
                    s2 := EnumTypesNamesStrList.Values[Token.ProtoName];
                  PrivateFieldStrList.Add('    ' + FFieldNameStr + ': ' +  s2 + ';  // ' +
                                           IntToStr(Token.FieldIndex) + ' ' + CardinalityShtStr[Token.Cardinality]);
                  s := 'Set' + FieldNameStr + '(const Value: ' + s2 + ');';
                  PrivateSetGetStrList.Add('    procedure ' + s);
                  PropertiesStrList.Add('    property ' + FieldNameStr + ': ' + s2 + ' read ' +
                                         FFieldNameStr + ' write Set' + FieldNameStr + ';  // ' + IntToStr(Token.FieldIndex));
                  if SetGetMethodsStrList.Count > 0 then
                    SetGetMethodsStrList.Add('');
                  SetGetMethodsStrList.Add('procedure ' + TProtoNameStr + '.' + s);
                  SetGetMethodsStrList.Add('begin');
                  if UseIfValuePropTest then
                    begin
                      SetGetMethodsStrList.Add('  if ' + FFieldNameStr + ' = Value then');
                      SetGetMethodsStrList.Add('    Exit;');
                    end;
                  SetGetMethodsStrList.Add('  ' + FFieldNameStr + ' := Value;');
                  SetGetMethodsStrList.Add('  FieldTouched[' + IntToStr(Token.FieldIndex) + '] := true;');
                  SetGetMethodsStrList.Add('end;');
                  SetGetMethodsStrList.Add('');
                  ReadFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) + ': Result := @' + FFieldNameStr +';');
                  if EnumConst then
                    s := FFieldNameStr + ' := p' + s2 + '(Value)^;'
                  else
                    s := 'if pInteger(Value)^ > Ord(High(' + s2 + ')) then ' + FFieldNameStr + ' := High(' + s2 + ') else ' + FFieldNameStr + ' := p' + s2 + '(Value)^;';
                    WriteFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) + ': ' + s);
                end

              else       // normal int / float fields
                begin
                  PrivateFieldStrList.Add('    ' + FFieldNameStr + ': ' +  WireDataSubTypeToPascal[Token.ProtoType] + ';  // ' +
                                           IntToStr(Token.FieldIndex) + ' ' + CardinalityShtStr[Token.Cardinality]);
                  s := 'Set' + FieldNameStr + '(const Value: ' + WireDataSubTypeToPascal[Token.ProtoType] + ');';

                  PrivateSetGetStrList.Add('    procedure ' + s);

                  PropertiesStrList.Add('    property ' + FieldNameStr + ': ' + WireDataSubTypeToPascal[Token.ProtoType] + ' read ' +
                                         FFieldNameStr + ' write Set' + FieldNameStr + ';  // ' + IntToStr(Token.FieldIndex));

                  if SetGetMethodsStrList.Count > 0 then
                    SetGetMethodsStrList.Add('');
                  SetGetMethodsStrList.Add('procedure ' + TProtoNameStr + '.' + s);
                  SetGetMethodsStrList.Add('begin');
                  if UseIfValuePropTest then
                    begin
                      SetGetMethodsStrList.Add('  if ' + FFieldNameStr + ' = Value then');
                      SetGetMethodsStrList.Add('    Exit;');
                    end;
                  SetGetMethodsStrList.Add('  ' + FFieldNameStr + ' := Value;');
                  SetGetMethodsStrList.Add('  FieldTouched[' + IntToStr(Token.FieldIndex) + '] := true;');
                  SetGetMethodsStrList.Add('end;');
                  SetGetMethodsStrList.Add('');

                  ReadFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) + ': Result := @' + FFieldNameStr +';');
                  WriteFieldByIndexStrList.Add('    ' + IntToStr(Token.FieldIndex) + ': ' + FFieldNameStr + ' := p' + WireDataSubTypeToPascal[Token.ProtoType] + '(Value)^;');

                  if Token.DefaultVal <> '' then
                    OnCreateDefaultList.Add('  ' + FFieldNameStr + ' := ' + Token.DefaultVal + ';');
                end;

              if Token.Cardinality = psRequired then
                OnCreateDefaultList.Add('  FieldTouched[' + IntToStr(Token.FieldIndex) + '] := true;');

              FieldSubTypeArray[Token.FieldIndex] := Token.ProtoType;
            end;

          MaxIDInt := Max(MaxIDInt, Token.FieldIndex);

        end; // a token


      for i := t downto f do   // delete used tokens for one message
        begin
          Finalize(PTProtoFieldToken(ProtoTokenList[i])^);
          Dispose(PTProtoFieldToken(ProtoTokenList[i]));
          ProtoTokenList.Delete(i);
        end;

      //Insert private field, GetSet, properties into current header

      ln := HeaderStrList.IndexOf(PrivateField);
      HeaderStrList.Delete(ln);
      for i := 0 to PrivateFieldStrList.Count - 1 do
        HeaderStrList.Insert(ln + i, PrivateFieldStrList[i]);
      PrivateFieldStrList.Clear;

      ln := HeaderStrList.IndexOf(PrivateSetGet);
      HeaderStrList.Delete(ln);
      for i := 0 to PrivateSetGetStrList.Count - 1 do
        HeaderStrList.Insert(ln + i, PrivateSetGetStrList[i]);
      PrivateSetGetStrList.Clear;

      ln := HeaderStrList.IndexOf(PublicFuncProp);
      HeaderStrList.Delete(ln);
      for i := 0 to PublicFuncPropStrList.Count - 1 do
        HeaderStrList.Insert(ln + i, PublicFuncPropStrList[i]);
      PublicFuncPropStrList.Clear;

      ln := HeaderStrList.IndexOf(Properties);
      HeaderStrList.Delete(ln);
      for i := 0 to PropertiesStrList.Count - 1 do
        HeaderStrList.Insert(ln + i, PropertiesStrList[i]);
      PropertiesStrList.Clear;

      s := ClassBodyStrList.Text;
      s := StringReplace(s, MaxID, IntToStr(MaxIdInt), [rfReplaceAll]);


      if MaxIdInt = 0 then
        WireSubTypesStr := '    pbwstUnUsed);'
      else
        begin
          WireSubTypesStr := '    ';
          for i := 0 to High(FieldSubTypeArray) do
            WireSubTypesStr := WireSubTypesStr + WireDataSubTypeAsStr[FieldSubTypeArray[i]] + ', ';
          Delete(WireSubTypesStr, Length(WireSubTypesStr) - 1, 2);
          WireSubTypesStr := WireSubTypesStr + ');';
        end;

      WireSubTypesStr := WrapText(WireSubTypesStr, 120);
      WireSubTypesStr := StringReplace(WireSubTypesStr, #13#10, #13#10'    ', [rfReplaceAll]);
      s := StringReplace(s, WireSubTypes, WireSubTypesStr, [rfReplaceAll]);
      ClassBodyStrList.Text := s;


      if Length(WireSubTypesMapsStr) > 0 then
        begin
          ln := ClassBodyStrList.IndexOf(WireDataSubTypeFunc);
          ln2 := ClassBodyStrList.IndexOf(WireDataSubMapTypeFunc);
          for i := ln to ln2 do
            ClassBodyStrList.Delete(ln);
          ln := ClassBodyStrList.IndexOf(WireDataSubMapTypeFuncEnd);
          ClassBodyStrList.Delete(ln);
          s := ClassBodyStrList.Text;
          s := StringReplace(s, WireDataSubMapTypeFuncCase, WireDataSubMapTypeFuncCaseStr, [rfReplaceAll]);
          s := StringReplace(s, WireSubTypesMaps, WireSubTypesMapsStr, [rfReplaceAll]);
          ClassBodyStrList.Text := s;
        end
      else
        begin
          ln := ClassBodyStrList.IndexOf(WireSubTypesMaps);
          ClassBodyStrList.Delete(ln);
          ln := ClassBodyStrList.IndexOf(WireDataSubTypeFunc);
          ClassBodyStrList.Delete(ln);

          ln := ClassBodyStrList.IndexOf(WireDataSubTypeFuncEnd);
          ln2 := ClassBodyStrList.IndexOf(WireDataSubMapTypeFuncEnd);
          for i := ln to ln2 do
            ClassBodyStrList.Delete(ln);
        end;





      ln := ClassBodyStrList.IndexOf(NestedCreate);
      if Length(NestedCreateStr) > 0 then
        ClassBodyStrList[ln] := NestedCreateStr
      else
        ClassBodyStrList.Delete(ln);
      NestedCreateStr := '';

      ln := ClassBodyStrList.IndexOf(OnCreateDefault);
      ClassBodyStrList.Delete(ln);
      for i := 0 to OnCreateDefaultList.Count - 1 do
        ClassBodyStrList.Insert(ln, OnCreateDefaultList[i]);


      ln := ClassBodyStrList.IndexOf(NestedFree);
      if Length(NestedFreeStr) > 0 then
        ClassBodyStrList[ln] := NestedFreeStr
      else
        ClassBodyStrList.Delete(ln);
      NestedFreeStr := '';

      ln := ClassBodyStrList.IndexOf(NestedFreeVar);
      if Length(NestedFreeVarStr) > 0 then
        ClassBodyStrList[ln] := NestedFreeVarStr
      else
        ClassBodyStrList.Delete(ln);
      NestedFreeVarStr := '';

      s := ClassBodyStrList.Text;
      s := StringReplace(s, TProtoName, TProtoNameStr, [rfReplaceAll]);
      s := StringReplace(s, ProtoName, ProtoNameStr, [rfReplaceAll]);
      s := StringReplace(s, ParentIndex, ParentIndexStr, [rfReplaceAll]);
      ClassBodyStrList.Text := s;

      ln := ClassBodyStrList.IndexOf(ReadFieldByIndex);
      if (ReadFieldByIndexStrList.Count = 0) and (ln > -1) then
        begin
          ClassBodyStrList[ln +1] := '//' + ClassBodyStrList[ln +1];
          ClassBodyStrList[ln -1] := '//' + ClassBodyStrList[ln -1];
          //ClassBodyStrList[ln -3] := '//' + ClassBodyStrList[ln -3];
          ClassBodyStrList[ln -4] := '//' + ClassBodyStrList[ln -4];
          ClassBodyStrList[ln -5] := '//' + ClassBodyStrList[ln -5];
          ClassBodyStrList[ln -6] := '//' + ClassBodyStrList[ln -6] + '  // No properties or fields here';
          ClassBodyStrList[ln -8] := '//' + ClassBodyStrList[ln -8];
        end;
      ln := ClassBodyStrList.IndexOf(ReadFieldByIndex);
      ClassBodyStrList.Delete(ln);
      if NoProp then
        begin
          ClassBodyStrList.Delete(ln);
          ClassBodyStrList.Delete(ln);
          ClassBodyStrList.Delete(ln);
          ClassBodyStrList.Delete(ln-1);
          ClassBodyStrList.Delete(ln-4);
          ClassBodyStrList.Delete(ln-5);
          ClassBodyStrList.Delete(ln-6);
          ClassBodyStrList.Delete(ln-8);
          ClassBodyStrList[ln - 6] := '  //  No properties - empty function';
        end
      else
        begin
          for i := 0 to ReadFieldByIndexStrList.Count - 1 do
            ClassBodyStrList.Insert(ln + i, ReadFieldByIndexStrList[i]);
          Repeated := false;
          for i := 0 to ReadFieldByIndexStrList.Count - 1 do
            Repeated := Repeated or (Pos('Repeated', ReadFieldByIndexStrList[i]) > 0);
          if not Repeated then
            begin
              ClassBodyStrList.Delete(ln-4);
              ClassBodyStrList.Delete(ln-5);
              s := ClassBodyStrList[ln - 8];
              Delete(s, Pos(', FieldSubIdx, Repeated', s), 23);
              ClassBodyStrList[ln - 8] := s;
              ClassBodyStrList.Delete(ln + ReadFieldByIndexStrList.Count -1);
              ClassBodyStrList.Delete(ln + ReadFieldByIndexStrList.Count -1);
            end;

        end;
      ReadFieldByIndexStrList.Clear;

      ln := ClassBodyStrList.IndexOf(ReadFieldByIndexSizeCase);
      ClassBodyStrList.Delete(ln);
      if ReadFieldByIndexSizeCaseStrList.Count = 0 then
        for i := ReadFieldByIndexSizeCaseStrList.Count - 1 downto 0 do
          ClassBodyStrList.Insert(ln, ReadFieldByIndexSizeCaseStrList[i]);


      ln := ClassBodyStrList.IndexOf(WriteTouchAdjust);
      ClassBodyStrList.Delete(ln);
      for i := 0 to WriteTouchAdjustStrList.Count - 1 do
        ClassBodyStrList.Insert(ln + i, WriteTouchAdjustStrList[i]);

      if WriteTouchAdjustStrList.Count > 0 then
        for i := ln downto 0 do
          if Pos('var ', ClassBodyStrList[i]) = 1 then
            begin
              ln := i;
              s := ClassBodyStrList[ln];
              s := StringReplace(s, ': Word', ', i: Word', [rfReplaceAll]);
              ClassBodyStrList[ln] := s;
              Break;
            end;

      NoCode := true;
      ln := ClassBodyStrList.IndexOf(WriteFieldByIndex);
      ClassBodyStrList.Delete(ln);

      for i := 0 to WriteFieldByIndexStrList.Count - 1 do
        begin
          s := WriteFieldByIndexStrList[i];
          NoCode := NoCode and (Pos('//', s) > 0);
        end;
      if NoCode then
        begin
          ClassBodyStrList[ln] := '//' + ClassBodyStrList[ln];
          ClassBodyStrList[ln -1] := '//' + ClassBodyStrList[ln -1];
        end;


      if NoProp then
        begin
          ln := -1;
          s := 'procedure ' + TProtoNameStr + '.WriteFieldByIndex(';
          for i := 0 to ClassBodyStrList.Count - 1 do
            if Pos(s, ClassBodyStrList[i]) > 0 then
              begin
                ln := i;
                Break;
              end;
          i := ln + 1;
          while Pos('end;', ClassBodyStrList[i]) <> 1 do
            ClassBodyStrList.Delete(i);
          ClassBodyStrList.Insert(i, '  //  No properties - empty function');
          ClassBodyStrList.Insert(i, 'begin');
        end
      else
        begin
          for i := 0 to WriteFieldByIndexStrList.Count - 1 do
            ClassBodyStrList.Insert(ln + i, WriteFieldByIndexStrList[i]);
          Repeated := false;
          for i := 0 to WriteFieldByIndexStrList.Count - 1 do
            Repeated := Repeated or (Pos('Repeated', WriteFieldByIndexStrList[i]) > 0);
          SubIdx := false;
          for i := 0 to WriteFieldByIndexStrList.Count - 1 do
            SubIdx := SubIdx or (Pos('FieldSubIdx', WriteFieldByIndexStrList[i]) > 0);
          if not Repeated or not Subidx then
            begin
              ln := -1;
              s := 'procedure ' + TProtoNameStr + '.WriteFieldByIndex(';
              for i := 0 to ClassBodyStrList.Count - 1 do
                if Pos(s, ClassBodyStrList[i]) > 0 then
                  begin
                    ln := i;
                    Break;
                  end;
              if ln > -1 then
                begin
                  s := ClassBodyStrList[ln + 1];
                  if not Repeated then
                    Delete(s, Pos(', FieldSubIdx, Repeated', s), 23);
                  if not SubIdx then
                    Delete(s, Pos(', FieldSubIdx', s), 13);
                  ClassBodyStrList[ln + 1] := s;
                  if not Repeated then
                    begin
                      ClassBodyStrList.Delete(ln + 4);
                      ClassBodyStrList.Delete(ln + 4);
                    end
                  else if not Subidx then
                    ClassBodyStrList.Delete(ln + 4);
                end;
            end;

        end;
      WriteFieldByIndexStrList.Clear;


      if Length(GetNestedArrayObjectByIndexCaseInsertStr) > 0 then
        begin
          ln := TemplateStrList.IndexOf(GetNestedArrayObjectByIndex);
          ln2 := TemplateStrList.IndexOf(GetNestedArrayObjectByIndexEnd);
          WorkStrList.Clear;
          for j := ln + 1 to ln2 - 1 do
            WorkStrList.Add(TemplateStrList[j]);

          s := WorkStrList.Text;
          s := StringReplace(s, GetNestedArrayObjectByIndexCaseInsert, GetNestedArrayObjectByIndexCaseInsertStr, [rfReplaceAll]);
          s := StringReplace(s, TProtoName, TProtoNameStr, [rfReplaceAll]);
          WorkStrList.Text := s;
          SetGetMethodsStrList.AddStrings(WorkStrList);
          SetGetMethodsStrList.Add('');
          WorkStrList.Clear;
        end;


      ln := ClassBodyStrList.IndexOf(SetGetMethods);
      ClassBodyStrList.Delete(ln);
      for i := 0 to SetGetMethodsStrList.Count - 1 do
        ClassBodyStrList.Insert(ln + i, SetGetMethodsStrList[i]);
      SetGetMethodsStrList.Clear;

      BodyStrList.AddStrings(ClassBodyStrList);
      ClassBodyStrList.Clear;

    until false;  // end a message .. might be two messages in one file... or is merged.



    // build unit uses
    for i := 0 to High(PasHeaderStr) do
      WorkStrList.Add(PasHeaderStr[i]);
    ln := WorkStrList.IndexOf(VersionStr);

    if FileExists(SourceVersionFile) then
      begin
        VersionStrList.LoadFromFile(SourceVersionFile);
        WorkStrList[ln] := '      ' + Prefix + ' ' + Trim(VersionStrList.Text);
      end
    else
      WorkStrList.Delete(ln);

    DecodeDate(Date, y, m, d);
    s := WorkStrList[2];
    s := s + '   Version ' + BuildVersionStr + '  (' + IntToStr(y) + ')';
    WorkStrList[2] := s;

    WorkStrList.Add('unit ' + UnitNameStr + ';');
    ln := TemplateStrList.IndexOf(IntroEnd);
    for i := 1 to ln - 1 do
      WorkStrList.Add(TemplateStrList[i]);


    // convert to uses
    for i := ForwardsStrList.Count - 1 downto 0 do
      begin
        s := ForwardsStrList[i];
        Delete(s, Length(s), 1);
        s := s + '(TProtocolBuffer)';
        ln := HeaderStrList.IndexOf(s);
        if ln = -1 then
          begin
            s := ForwardsStrList[i];
            Delete(s, 1, 3);
            Delete(s, Pos(' ', s), 100);
            UsesStrList.Add(s);
            ForwardsStrList.Delete(i);
          end;
      end;

    ln := WorkStrList.IndexOf(Forwards);
    WorkStrList.Delete(ln);
    if ForwardsStrList.Count > 0 then
      begin
        for i := ForwardsStrList.Count - 1 downto 0 do
          WorkStrList.Insert(ln, ForwardsStrList[i]);
        WorkStrList.Insert(ln, '  // forward declarations');
      end;

    UsesStr := '';
    if UsesStrList.Count > 0 then
      begin
        for i := UsesStrList.Count - 1 downto 0 do
          begin
            s := '  T' + UsesStrList[i] + ' = class(TProtocolBuffer)';
            ln := HeaderStrList.IndexOf(s);
            if ln > -1 then
              UsesStrList.Delete(i);
          end;
        for i := UsesStrList.Count - 1 downto 0 do
          UsesStr := UsesStr + ' ' + UsesStrList[i] + ',';
      end;

    ln := WorkStrList.IndexOf(Usess);
    if Length(UsesStr) > 0 then
      begin
        UsesStr := WrapText(UsesStr, 120);
        UsesStr := StringReplace(UsesStr, #13#10, #13#10'    ', [rfReplaceAll]);
        WorkStrList[ln] := UsesStr;
      end
    else
      WorkStrList.Delete(ln);

    ln := WorkStrList.IndexOf(EnumType);
    WorkStrList.Delete(ln);
    if EnumTypesStrList.Count > 0 then
      begin
        EnumTypesStrList.Sorted := false;
        for i := EnumTypesStrList.Count -2 downto 0 do
          if Pos('^', EnumTypesStrList[i]) > 0 then
            begin
              s := EnumTypesStrList[i];
              EnumTypesStrList.Delete(i);
              EnumTypesStrList.Add(s);
            end;

        for i := EnumTypesStrList.Count -1 downto 0 do
          begin
            s := '  ' + EnumTypesStrList[i];
            s := WrapText(s, 120);
            s := StringReplace(s, #13#10, #13#10'    ', [rfReplaceAll]);
            WorkStrList.Insert(ln, s);
          end;
        WorkStrList.Insert(ln, '');
      end;

    WorkStrList.Add('');

    // build the file
    DelphiFileStrList.AddStrings(WorkStrList);    // unit uses
    DelphiFileStrList.AddStrings(HeaderStrList);  // head
    if ConstNameValueStrList.Count > 0 then
      begin
        DelphiFileStrList.Add('const');
        for i := 0 to ConstNameValueStrList.Count -1 do
          DelphiFileStrList.Add('  ' + ConstNameValueStrList[i]);
        DelphiFileStrList.Add('');
      end;
    DelphiFileStrList.Add('implementation');
    DelphiFileStrList.Add('');
    DelphiFileStrList.AddStrings(BodyStrList);  // body
    DelphiFileStrList.Add('end.');
    for i := DelphiFileStrList.Count - 2 downto 1 do
      if (DelphiFileStrList[i] = '') and (DelphiFileStrList[i+1] = '') then
        DelphiFileStrList.Delete(i + 1);

    Result := true;

  finally
    DefaultsStrList.Free;
    OnCreateDefaultList.Free;
    PrivateFieldStrList.Free;
    PrivateSetGetStrList.Free;
    PropertiesStrList.Free;
    PublicFuncPropStrList.Free;
    SetGetMethodsStrList.Free;
    ForwardsStrList.Free;
    ReadFieldByIndexStrList.Free;
    ReadFieldByIndexSizeCaseStrList.Free;
    WriteFieldByIndexStrList.Free;
    WorkStrList.Free;
    HeaderStrList.Free;
    BodyStrList.Free;
    ClassBodyStrList.Free;
    WriteTouchAdjustStrList.Free;
    VersionStrList.Free;
    UsesStrList.Free;
    EnumTypesStrList.Free;
    EnumTypesNamesStrList.Free;
    ConstNameValueStrList.Free;
  end;
end;

initialization
  TemplateStrList := TStringList.Create;

finalization
  TemplateStrList.Free;

end.

