unit UprotocParser;

interface

{$IFDEF FPC}
{$mode delphi}{$H+}
uses SysUtils, Classes,
{$ELSE}
uses System.Classes, System.SysUtils,
{$ENDIF}
  UprotocConst;

  function ParseProtoFile(ProtoFile: string;  ProtoFileTokenList: TList): Boolean;
  function ReservedWordCheck(s: string): string;



implementation

uses Uprotoc;

type
  TDecodeItem = (decMsgType, decTypeName, decCardinality, decDataType, decFieldName, decIndex, decDefValue, decOptionName, decOptionValue,
                 decMapKey, decMapValue, decProtoVersion);


function PosCharRev(Chr: Char; const Str: string; Offset: Integer): Integer;
begin
  Result := 0;
  if Offset > Length(Str) then
    Exit;
  while (Offset > 0) do
    begin
      if Str[Offset] = Chr then
        begin
          Result := Offset;
          Break;
        end
      else
        dec(Offset);
    end;
end;

function ParseLine(LineStr: string; DataType: TDecodeItem; Cardinality: TCardinalityParam): string;
var i, p: Integer;
begin
  Result := '';
  LineStr := Trim(LineStr);
  case DataType of
    decMsgType: if Pos('message', LineStr) = 1 then
                  Result := 'message'
                else if Pos('enum', LineStr) = 1 then
                  Result := 'enum';
    decDataType:begin
                  Result := '0';
                  LineStr := LowerCase(LineStr);
                  if Cardinality > psNone then
                    begin
                      p := Pos(' ', LineStr);
                      Delete(LineStr, 1, p);
                    end;
                  Delete(LineStr, Pos(' ', LineStr), 100);
                  for i := 0 to Ord(High(TWireDataSubType)) do
                    if WireDataSubTypeStr[TWireDataSubType(i)] = LineStr then
                      begin
                        Result := IntToStr(i);
                        Break;
                      end;
                end;
    decTypeName:begin
                  if (Pos('message', LineStr) = 1) or (Pos('enum', LineStr) = 1) then
                    begin
                      p := Pos(' ', LineStr);
                      Delete(LineStr, 1, p);
                    end
                  else
                    for i := 0 to Ord(High(TCardinalityParam)) do
                      if Pos(CardinalityStr[TCardinalityParam(i)], LineStr) = 1 then
                        begin
                          p := Pos(' ', LineStr);
                          Delete(LineStr, 1, p);
                          Break;
                        end;
                  Result := Copy(LineStr, 1, Pos(' ', LineStr) - 1);
                end;
    decCardinality:begin
                LineStr := LowerCase(LineStr);
                Result := '0';
                for i := 0 to Ord(High(TCardinalityParam)) do
                  if Pos(CardinalityStr[TCardinalityParam(i)], LineStr) = 1 then
                    begin
                      Result := IntToStr(i);
                      Break;
                    end;
              end;
    decFieldName: begin
                    p := Pos('=', LineStr);
                    if p = 0 then
                      Exit;
                    Delete(LineStr, p, 100);
                    if LineStr[Length(LineStr)] = ' ' then
                      Delete(LineStr, Length(LineStr), 1);
                    p := PosCharRev(' ', LineStr, Length(LineStr));
                    if p > 0 then
                      Result := Copy(LineStr, p + 1, 100)
                    else
                      Result := LineStr;
                  end;
    decIndex: begin
                LineStr := StringReplace(LineStr, ' ', '', [rfReplaceAll]);
                p := Pos('=', LineStr); Delete(LineStr, 1, p); p := Pos(';', LineStr); Delete(LineStr, p, 100);
                p := Pos('[', LineStr); if p > 0 then Delete(LineStr, p, 100);
                Result := LineStr;
              end;
    decDefValue:begin
                  LineStr := StringReplace(LineStr, ' ', '', [rfReplaceAll]);
                  p := Pos('[default', LineStr); if p = 0 then Exit;
                  Delete(LineStr, 1, p);
                  p := Pos('=', LineStr);
                  Delete(LineStr, 1, p);
                  Delete(LineStr, Pos(']', LineStr), 100);
                  Result := LineStr;
                end;
    decOptionName,
    decOptionValue: begin
                      p := Pos('option', LineStr);
                      if p = 0 then
                        Exit;
                      Delete(LineStr, 1, 6);
                      LineStr := StringReplace(LineStr, ' ', '', [rfReplaceAll]);
                      if DataType = decOptionName then
                        Result := Copy(LineStr, 1, Pos('=', LineStr) - 1)
                      else
                        begin
                          p := Pos(';', LineStr); Delete(LineStr, p, 100);
                          Result := Copy(LineStr, Pos('=', LineStr) + 1, 100);
                        end;
                    end;
    decMapKey, decMapValue:
                begin
                  LineStr := StringReplace(LineStr, ' ', '', [rfReplaceAll]);
                  p := Pos('<', LineStr);
                  if p = 0 then
                    Exit;
                  Delete(LineStr, 1, p);
                  p := Pos('>', LineStr);
                  if p = 0 then
                    Exit;
                  Delete(LineStr, p, 100);
                  p := Pos(',', LineStr);
                  if p = 0 then
                    Exit;
                  if DataType = decMapValue then // value
                    Result := Copy(LineStr, p + 1, 100)
                  else
                    Result := Copy(LineStr, 1, p -1);
                  for i := 0 to Ord(High(TWireDataSubType)) do
                    if WireDataSubTypeStr[TWireDataSubType(i)] = Result then
                      begin
                        Result := IntToStr(i);
                        Break;
                      end;
                end;
    decProtoVersion:begin
                      LineStr := LowerCase(LineStr);
                      if Pos('syntax', LineStr) = 1 then
                        begin
                          if Pos('proto2', LineStr) > 0 then
                            Result := '2'
                          else if Pos('proto3', LineStr) > 0 then
                            Result := '3';
                        end
                      else if Pos('edition', LineStr) = 1 then
                        begin
                          if Pos('2023', LineStr) > 0 then
                            Result := '2023'
                          else if Pos('2024', LineStr) > 0 then
                            Result := '2024';
                        end;
                    end;
  end;
end;

function ParseProtoFile(ProtoFile: string; ProtoFileTokenList: TList): Boolean;
var ProtoStrList, WorkStrList: TStringList; e, i, m, f, t, ln, p: Integer; s, s2, MsgEnumName, NewMsgEnumName: string; Token: PTProtoFieldToken;
    MsgFound: Boolean; MsgHeaderLine: array of Integer;
begin
  Result := false;
  ParseError := '';
  ErrorLine := '';
  MsgFound := false;
  MsgHeaderLine := nil;

  if not FileExists(ProtoFile) then
    begin
      ParseError := 'Unable to locate file';
      Exit;
    end;

  ProtoStrList := TStringList.Create;
  WorkStrList := TStringList.Create;
  try
    ProtoStrList.LoadFromFile(ProtoFile);
    ProtoVersion := 0;
    m := 0;

    // unwrap lines, clean up some formatting.
    for i := ProtoStrList.Count -1 downto 0 do
      begin
        s := Trim(ProtoStrList[i]);
        if Length(s) = 0 then
          Continue;
        p := Pos('//', s);
        if p = 1 then       // comment lines
          begin
            ProtoStrList.Delete(i);
            Continue;
          end;
        if p > 0 then   // comments on line end
          begin
            s := ProtoStrList[i];
            Delete(s, Pos('//', s), 1000);
            ProtoStrList[i] := s;
            s := Trim(s);
          end;
        if Pos('/*', s) > 0 then  // comment lines
          begin
            m := 0;
            ProtoStrList.Delete(i);
            Continue;
          end;
        if (m > 0) or (Pos('*/', s) > 0) then
          begin
            m := 1;
            ProtoStrList.Delete(i);
            Continue;
          end;

        f := Pos('{', s);
        t := Pos('}', s);
        if ((f > 0) and (t > 0)) or (t > 1) then
          begin
            ProtoStrList.Insert(i + 1, Copy(s, t, 100));  // message open and close brackets on same line
            s := Trim(ProtoStrList[i]);       // fields on same line as close bracket
            Delete(s, t, 100);
            ProtoStrList[i] := s;
            t := Pos('}', s);
          end;

        p := Pos(';', s);
        while (p > 0) and (p < Length(s)) do  // multiple fileds on same line
          begin
            ProtoStrList.Insert(i + 1, Copy(s, 1, p));
            Delete(s, 1, p);
            s := Trim(s);
            ProtoStrList[i] := s;
            p := Pos(';', s);
          end;

        if f > 0 then
          Continue;
        if t > 0 then
          Continue;
        if (Pos(';', s) = 0) and (i < ProtoStrList.Count -1) then  // wrapped line
          begin
            s := ProtoStrList[i + 1];
            ProtoStrList.Delete(i + 1);
            ProtoStrList[i] := ProtoStrList[i] + s;
          end;

        if Pos('google.protobuf.Any', s) > 0 then
          begin
            for e := i downto 0 do
              begin
                s2 := StringReplace(ProtoStrList[e], 'google.protobuf.Any', 'GoogleAny', [rfReplaceAll, rfIgnoreCase]);
                ProtoStrList[e] := s2;
              end;
            ProtoStrList.Add('');
            for e := 0 to High(GoogleAnyProto) do
              ProtoStrList.Add(GoogleAnyProto[e]);
          end;
      end;

    // reformat indenting, white space, comments
    t := 0;
    f := 0;
    for i := 0 to ProtoStrList.Count -1 do
      begin
        inc(t, f);
        f := 0;
        s := Trim(ProtoStrList[i]);
        if Length(s) = 0 then
          begin
            if t > 0 then
              Continue;
            if (i > 0) and (Length(WorkStrList[WorkStrList.Count -1]) > 0) then
              WorkStrList.Add('');
            Continue;
          end;
        if Pos('//', s) = 1 then
          Continue;
        if Pos('reserved', s) = 1 then // these are comments for proto writers
          Continue;
        if Pos('extensions', s) = 1 then // in spec 2023 - these are comments for proto writers
          Continue;
        if Pos('{', s) > 0 then
          f := 2;
        if Pos('}', s) > 0 then
          dec(t, 2);
        if t < 0 then
          t := 0;
        p := Pos('.', s);
        while (p > 0) and (p < Pos(' ', s)) do
          begin
            Delete(s, p, 1);    // for fields referenced to messages embedded in other top level messages
            p := Pos('.', s);   // Remove nested class qualifiers (.) as the FQN below replicates the full name here.
          end;
        WorkStrList.Add(StringOfChar(' ', t) + s);
      end;
    ProtoStrList.Clear;
    ProtoStrList.AddStrings(WorkStrList);

    // unwind embedded message / enum declarations - shift to top level
    SetLength(MsgHeaderLine, ProtoStrList.Count);
    for i := 0 to ProtoStrList.Count -1 do
      begin
        // indenting index map
        s := ProtoStrList[i];
        p := Pos('message ', s);
        if p = 0 then
          p := Pos('enum ', s);
        if (p > 1) and (s[p-1] <> ' ') then
          p := 0;
        MsgHeaderLine[i] := p;
        if MsgHeaderLine[i] = 0 then
          MsgHeaderLine[i] := 0 - Pos('}', s);
      end;

    f := -1;
    t := -1;
    while t < High(MsgHeaderLine) do
      begin
        for i := t + 1 to High(MsgHeaderLine) do
          begin
            if MsgHeaderLine[i] = 1 then
              f := i;                   // start this top level msg
            if MsgHeaderLine[i] = -1 then
              begin
                t := i;     // end this top level msg
                Break;
                // markers for this message set;  now find any inner message definitions
              end;
          end;
        if (f = -1) or (t = -1) then
          Break;

        NewMsgEnumName := '';
        repeat
          e := 0;
          m := -1;
          for i := f + 1 to t -1 do
            if MsgHeaderLine[i] > e then
              begin
                m := i;            //  deepest nested msg/enum open line
                e := MsgHeaderLine[i];
              end;
          if m = -1 then
            Break;
          e := 0;
          for i := m + 1 to t -1 do
            if (MsgHeaderLine[m] = -MsgHeaderLine[i]) and (e = 0) then
              e := i;              // matching closing line for deepest nested msg.
          if e = 0 then
            Break;

          MsgEnumName := ParseLine(ProtoStrList[m], decTypeName, psNone);
          NewMsgEnumName := MsgEnumName;
          NewMsgEnumName[1] := Char(Ord(NewMsgEnumName[1]) and (not $20));
          p := MsgHeaderLine[m];
          for i := m -1 downto f do
            if (MsgHeaderLine[i] > 0) and (MsgHeaderLine[i] < p) then
              begin
                p := MsgHeaderLine[i];  // the parent message line
                NewMsgEnumName := ParseLine(ProtoStrList[i], decTypeName, psNone) + NewMsgEnumName;
                NewMsgEnumName[1] := Char(Ord(NewMsgEnumName[1]) and (not $20));
              end;
          if Length(NewMsgEnumName) > 128 then
            for i := Length(NewMsgEnumName) downto 2 do
              if CharInSet(NewMsgEnumName[i], VowelChars) then
                Delete(NewMsgEnumName, i, 1);
          if Length(NewMsgEnumName) > 128 then
            for i := Length(NewMsgEnumName) downto 2 do
              if CharInSet(NewMsgEnumName[i], NumberChars) then
                Delete(NewMsgEnumName, i, 1);
          if Length(NewMsgEnumName) > 128 then
            for i := Length(NewMsgEnumName) downto 2 do
              if CharInSet(NewMsgEnumName[i], MoreChars) then
                Delete(NewMsgEnumName, i, 1);

          // now newMsg holds new FQN.  rename all affected fields.
          for i := f + 1 to t -1 do
            begin
              if ProtoStrList.Objects[i] <> nil then
                Continue;
              s := ProtoStrList[i];
              s2 := s;
              s := StringReplace(s, MsgEnumName, NewMsgEnumName, []);
              ProtoStrList[i] := s;
              ProtoStrList.Objects[i] := TObject(s <> s2);    //  avoid double renaming
            end;
          // move this inner msg to new outer location;
          for i := e downto m do
            begin
              s := ProtoStrList[e];
              Delete(s, 1, MsgHeaderLine[m] - 1);
              ProtoStrList.Delete(e);
              ProtoStrList.Insert(f, s);
            end;
          // adjust the indent index to match shifted strings
          for i := m to e do
            MsgHeaderLine[i] := 0;
          for i := f to m do
            MsgHeaderLine[i + (e-m) + 1] := MsgHeaderLine[i];

          inc(f, e - m + 1);
        until m = -1;

        inc(t);
        f := -1;
      end;

    ln := 0;
    while (ln < ProtoStrList.Count) and (ProtoVersion = 0) do
      begin
        s := ParseLine(ProtoStrList[ln], decProtoVersion, psNone);
        if s <> '' then
          ProtoVersion := StrToIntDef(s, -1);
        inc(ln);
      end;

    // Now ... parse the messages and enums into tokens
    repeat
      begin
        m := -1;
        e := -1;
        t := -1;
        f := -1;
        for i := 0 to ProtoStrList.Count -1 do
          begin
            s := ProtoStrList[i];
            if (Pos('message ', s) = 1) and (m = -1) and (e = -1) then
              m := i;
            if (Pos('enum ', s) = 1) and (m = -1) and (e = -1) then
              e := i;
            if (Pos('{', s) > 0) and (f = -1) then
              f := i;
            if (Pos('}', s) = 1) and (t = -1) then
              t := i;
          end;

        if (m = -1) and (e = -1) then
          begin
            if not MsgFound then
              begin
                ParseError := 'Unable to locate proto message header.';
                Result := false;
              end;
            Exit;
          end;
        MsgFound := true;


        if ProtoFileTokenList.Count >= 1 then
          PTProtoFieldToken(ProtoFileTokenList[ProtoFileTokenList.Count -1]).LastField := true;

        New(Token);
        FillChar(Token^, SizeOf(TProtoFieldToken), 0);
        Token.HeaderToken := true;
        ProtoFileTokenList.Add(Token);

        // message or enum name
        if (m > -1) then
          s := ProtoStrList[m]
        else
          s := ProtoStrList[e];
        Token.ProtoName := ParseLine(s, decTypeName, Token.Cardinality);
        Token.ProtoName := ReservedWordCheck(Token.ProtoName);

        inc(f);
        dec(t);
        // enum
        if e > - 1 then
          begin
            Token.ProtoType := pbwstVarIntEnum;
            for ln := f to t do
              begin
                s := Trim(ProtoStrList[ln]);
                if s = '' then
                  Continue;
                if Pos('{',s) = 1 then
                  Continue;
                if Pos('}',s) = 1 then
                  Continue;

                // enum options:
                if Pos('option', s) > 0 then
                  begin
                    s := ParseLine(ProtoStrList[ln], decOptionName, Token.Cardinality);
                    if s = 'allow_alias' then
                      EnumConst := 'true' = ParseLine(ProtoStrList[ln], decOptionValue, Token.Cardinality);
                    Continue;
                  end;

                New(Token);
                FillChar(Token^, SizeOf(TProtoFieldToken), 0);
                ProtoFileTokenList.Add(Token);
                Token.Cardinality := Token.Cardinality;
                Token.ProtoType := pbwstVarIntEnum;
                Token.FieldName := ParseLine(ProtoStrList[ln], decFieldName, Token.Cardinality);
                Token.FieldIndex := StrToIntDef(ParseLine(ProtoStrList[ln], decIndex, Token.Cardinality), 0);
              end;
            if t < ProtoStrList.Count - 1 then
              inc(t);
            for ln := t downto f do
              ProtoStrList.Delete(ln);
            ProtoStrList.Delete(e);
            Continue;
          end;

        // message and field lines
        for ln := f to t do
          begin
            s := Trim(ProtoStrList[ln]);
            if s = '' then
              Continue;
            if Pos('{',s) = 1 then
              Continue;
            if Pos('}',s) = 1 then
              Continue;
            New(Token);
            FillChar(Token^, SizeOf(TProtoFieldToken), 0);
            ProtoFileTokenList.Add(Token);

            // [default = ] field values
            p := Pos('[', s);
            if p > 0 then
              begin
                Token.DefaultVal := ParseLine(ProtoStrList[ln], decDefValue, Token.Cardinality);
                Token.DefaultVal := StringReplace(Token.DefaultVal, PathDelim, '', [rfReplaceAll]);
                Token.DefaultVal := StringReplace(Token.DefaultVal, '''', '''', [rfReplaceAll]);
                Token.DefaultVal := StringReplace(Token.DefaultVal, '"', '''', [rfReplaceAll]);
              end;

            // Cardinality state - the first param of a field line
            Token.Cardinality := TCardinalityParam(StrToIntDef(ParseLine(ProtoStrList[ln], decCardinality, Token.Cardinality), 0));

            // map
            if Token.Cardinality = psMap then
              begin
                Token.ProtoType := pbwstMapPair;
                s := ParseLine(ProtoStrList[ln], decMapKey, Token.Cardinality);   // WireDataSubType Ord #.
                s2 := s + '_';
                s := ParseLine(ProtoStrList[ln], decMapValue, Token.Cardinality);
                Token.ProtoName := s2 + s;
              end

            // OneOf
            else if Token.Cardinality = psOneof then
              begin
                s := ProtoStrList[ln];
                Delete(s, 1, Pos('oneof', s) + 4);
                Delete(s, Pos('{', s), 100);
                Token.FieldName := Trim(s);
                Token.FieldName := ReservedWordCheck(Token.FieldName);
                Token.LastOneofIndex := 1;
                if ProtoFileTokenList.Count > 1 then
                  Token.LastOneofIndex := PTProtoFieldToken(ProtoFileTokenList[ProtoFileTokenList.Count - 2]).FieldIndex;
                i := ln + 1;
                while (i <= t) and (Pos('}', (Trim(ProtoStrList[i]))) = 0) do
                  begin
                    inc(Token.LastOneofIndex);
                    inc(i);
                  end;
                Continue;
              end

            else
              begin
                // normal field data types
                Token.ProtoType := TWireDataSubType(StrToIntDef(ParseLine(ProtoStrList[ln], decDataType, Token.Cardinality), 0));
              end;

            if Token.ProtoType = pbwstUnknown then  // assume a nested message
              begin
                Token.ProtoType := pbwstNestedMessage;
                Token.ProtoName := ParseLine(ProtoStrList[ln], decTypeName, Token.Cardinality);
                Token.ProtoName := ReservedWordCheck(Token.ProtoName);
              end;

            // field name
            Token.FieldName := ParseLine(ProtoStrList[ln], decFieldName, Token.Cardinality);
            Token.FieldName := ReservedWordCheck(Token.FieldName);

            // field index
            Token.FieldIndex := StrToIntDef(ParseLine(ProtoStrList[ln], decIndex, Token.Cardinality), 0);

            if (Token.HeaderToken = false) and
              ((Token.ProtoType = pbwstUnknown) or (Token.FieldName = '') or (Token.FieldIndex = 0)) then
              begin
                ParseError := 'Error reading line #' + IntToStr(ln + 1) + ':';
                ErrorLine := '  ' + StringReplace(ProtoStrList[ln], #9, '    ', [rfReplaceAll]);
                Result := false;
                Exit;
              end;
          end;

        if t < ProtoStrList.Count - 1 then
          inc(t);
        for ln := t downto f do
          ProtoStrList.Delete(ln);
        ProtoStrList.Delete(m);

        Result := true;
      end;
    until false;

  finally
    ProtoStrList.Free;
    WorkStrList.Free;
  end;
end;


function ReservedWordCheck(s: string): string;
var i: Integer;
begin
  if Length(s) > 0 then
    for i := 0 to High(ReservedWords) do
      if ReservedWords[i] = s then
        s := 'a' + s;

  for i := High(s) downto 1 do
    if CharInSet(s[i], ProhibitedNameCharsSet) then
      Delete(s, i, 1);
  Result := s;
end;



end.
