unit Uprotoc;

interface

{$IFDEF FPC}
{$mode delphi}{$H+}
uses SysUtils, Classes,
{$ELSE}
uses System.Classes, System.SysUtils, Winapi.Windows,
{$ENDIF}
  UprotocConst, UprotocParser, UprotocGenerator;

  function ParseCommandLine: Boolean;
  procedure WriteHeader;
  procedure WriteSucess;
  procedure WriteBlurb;
  procedure WriteError(s: string);
  function CreateDelphiFiles: Boolean;
  function GetTemplateFilefromBinary: Boolean;

var
  ParseError: string;
  ErrorLine: string;
  TemplateFile: string;
  WriteBlurbOut: Boolean = true;
  EnumConst: Boolean;
  ProtoVersion: Integer;

implementation

var
  InPathList: TStringList;
  ProtoFiles: TStringList;
  Merge: Boolean = false;
  OutPath: string = '';
  MergeFileName: string = '';
  FileNameSucess: string = '';




function ParseCommandLine: Boolean;
var i, p: Integer; s: string; SR: TSearchRec;
begin
  Result := false;

  if FindCmdLineSwitch('h') or FindCmdLineSwitch('?') or FindCmdLineSwitch('help') then
    Exit;
  if FindCmdLineSwitch('v') or FindCmdLineSwitch('version') then
    begin
      WriteLn('version=' + BuildVersionStr);
      WriteLn('');
      Exit;
    end;
  Merge := FindCmdLineSwitch('m') or FindCmdLineSwitch('merge');
  EnumConst := FindCmdLineSwitch('enmcnst');

  if FindCmdLineSwitch('prntmplt') then
    begin
      if not GetTemplateFilefromBinary then
        begin
          ErrorLine := 'Unable to find Template file: ' + Templatefile;
          WriteError(ErrorLine);
        end
      else
        begin
          WriteLn('  This is the included template file used to create the TSomeProto classes.');
          WriteLn('  It''s also saved to disk as TemplateProtoBuffer.pas');
          WriteLn('  It can be modified slightly and used with the -tmplt= option.');
          WriteLn('');
          WriteLn('    ********************************');
          WriteLn('');
          WriteLn(TemplateStrList.Text);
          {$IFNDEF FPC}System.SysUtils.{$ENDIF}DeleteFile('TemplateProtoBuffer.pas');;
          TemplateStrList.SaveToFile('TemplateProtoBuffer.pas');
          WriteBlurbOut := false;
        end;
      Result := false;
      Exit;
    end;

  i := 1;
  while i <= ParamCount do
    begin
      s := ParamStr(i);

      // include search paths for .proto files
      if (s = '-ip') and (i < ParamCount) then
        begin
          InPathList.Add(ParamStr(i + 1));
          inc(i,2);
          Continue;
        end;
      p := Pos('-inpath=', s);
      if p > 0 then
        InPathList.Add(Copy(s,9));

      // output folder for result
      if (s = '-op') and (i < ParamCount) then
        begin
          OutPath := ParamStr(i + 1);
          inc(i,2);
          Continue;
        end;
      p := Pos('-outpath=', s);
      if p > 0 then
        OutPath := Copy(s,10);

      // an input pife that hold default values to insert in the OnCreate.  see sample file.
      if (s = '-iv') and (i < ParamCount) then
        begin
          DefaultsFile := ParamStr(i + 1);
          inc(i,2);
          Continue;
        end;
      p := Pos('-initval=', s);
      if p > 0 then
        DefaultsFile := Copy(s,10);

      // a prefix string to use with all files and types created. eg.  XYZ (TXYZSomeProto).
      if (s = '-pr') and (i < ParamCount) then
        begin
          Prefix := ParamStr(i + 1);
          inc(i,2);
          Continue;
        end;
      p := Pos('-prefix=', s);
      if p > 0 then
        Prefix := Copy(s,9);

      if (s = '-mn') and (i < ParamCount) then
        begin
          MergeFileName := ParamStr(i + 1);
          inc(i,2);
          Continue;
        end;
      p := Pos('-mname=', s);
      if p > 0 then
        MergeFileName := Copy(s,8);

      // a text file to insert in the header, with a one line versioning number from the .proto
      if (s = '-sv') and (i < ParamCount) then
        begin
          SourceVersionFile := ParamStr(i + 1);
          inc(i,2);
          Continue;
        end;
      p := Pos('-srcver=', s);
      if p > 0 then
        SourceVersionFile := Copy(s,9);

      //  In properties, include the test "if Value <> F<storedvalue>".  Otherwise all write to properties are written
      //  and the touched flag is set
      if '-valeq' = s then
        UseIfValueProptest := true;

      // location of a template to use as basis to create each TXYZProtoClass file.
      p := Pos('-tmplt=', s);
      if p > 0 then
        TemplateFile := Copy(s,8);

      p := Pos('.proto', s);
      if p > 0 then
        ProtoFiles.Add(s)
      else if DirectoryExists(s) then
        InPathList.Add(s);

      inc(i);
    end;


  p := Pos('.pas', MergeFileName);
  if (p > 0) and (p = Length(MergeFileName) - 3) then
    Delete(MergeFileName, p, 100);

  if (OutPath <> '') and (OutPath[Length(OutPath)] <> PathDelim) then
    OutPath := OutPath + PathDelim;

  for i := 0 to InPathList.Count -1 do
    begin
      s := InPathList[i];
      if s[Length(s)] <> PathDelim then
        s := s + PathDelim;
      InPathList[i] := s;
      if FindFirst(s + '*.proto', faNormal, SR) = 0 then
        begin
          repeat
            ProtoFiles.Add(s + SR.Name); //Fill the list
          until FindNext(SR) <> 0;
          {$IFNDEF FPC}System.SysUtils.{$ENDIF}FindClose(SR);
        end;
    end;

  if ProtoFiles.Count = 0 then
    Exit;

  Result := true;
end;

procedure WriteHeader;
var i: Integer;
begin
  for i := 0 to High(HeaderStr) do
    WriteLn(HeaderStr[i]);
end;

procedure WriteSucess;
var i: Integer; Dir: String;
begin
  for i := 0 to High(SucessStr) do
    WriteLn(SucessStr[i]);
  if Length(OutPath) > 0 then
    Dir := OutPath
  else
    Dir := GetCurrentDir;
  if Merge then
    WriteLn('  ' + FileNameSucess)
  else
    WriteLn('  ' + Dir);
end;

procedure WriteBlurb;
var i: Integer;
begin
  for i := 0 to High(BlurbStr) do
    WriteLn(BlurbStr[i]);
end;

procedure WriteError(s: string);
begin
  WriteLn('ERROR: ' + s);
end;


function GetTemplateFilefromBinary: Boolean;
var  MemStream: TMemoryStream; pb, pend: pAnsiChar; unitstr: AnsiString; i, c: Integer;
begin
  // If this binary was made with the TemplateProtoBuffer.pas appended (copy /b f1 + f2  F3).
  TemplateStrList.Clear;
  unitstr := '{*UNITNAME*}unit;';
  i := Length(unitstr);
  MemStream := TMemoryStream.Create;
  MemStream.LoadFromFile(ParamStr(0));
  MemStream.Position := MemStream.Size - TEMPLATE_FILE_SIZE;  // an oversized guess at the size of TemplatProtoBuffer.pas
  pb := MemStream.Memory;
  inc(pb, MemStream.Position);
  pend := pb;
  inc(pend, TEMPLATE_FILE_SIZE);
  repeat
    while (NativeUInt(pb) < NativeUInt(pend)) and (pb^ <> '{') do
      inc(pb);
    {$IFDEF FPC}
    c := strlcomp(pb, @UnitStr[1], i);
    {$ELSE}
    c := CompareStringA(LOCALE_USER_DEFAULT, 0, pb, i, @UnitStr[1], i) - CSTR_EQUAL;
    {$ENDIF}
    if c = 0 then
      begin
        MemStream.Position := Int64(NativeUInt(pb) - NativeUInt(MemStream.Memory));
        TemplateStrList.LoadFromStream(MemStream);
        Break;
      end;
      inc(pb);
   until (NativeUInt(pb) >= NativeUInt(pend));
  MemStream.Free;
  Result := TemplateStrList.Count > 0;
end;


function CreateDelphiFiles: Boolean;
var i: Integer; s, FileName, ClassName, UnitNameStr: string;  ProtoTokenList: TList; DelphiFile: TStringList;
    Status: Boolean;

begin
  Result := true;
  ProtoTokenList := TList.Create;
  DelphiFile := TStringlist.Create;

  if (Length(TemplateFile) > 0) and not FileExists(TemplateFile) then
    begin
      ErrorLine := 'Unable to find Template file: ' + Templatefile;
      WriteError(ErrorLine);
      Result := false;
      Exit;
    end;

  if (Length(TemplateFile) > 0) then
    begin
      TemplateStrList.LoadFromFile(TemplateFile);
      i := TemplateStrList.IndexOf('{*UNITNAME*}unit;');
      while i > 0 do
        begin
          TemplateStrList.Delete(0);
          dec(i);
        end;
    end
  else
    if not GetTemplateFilefromBinary then
      begin
        ErrorLine := 'Unable to find Template file: ' + Templatefile;
        WriteError(ErrorLine);
        Result := false;
        Exit;
      end;


  WriteLn('Processing proto message files...');
  WriteLn('');

  for i := 0 to ProtoFiles.Count -1 do
    begin
      s := ProtoFiles[i];
      WriteLn(s);

      Status := ParseProtoFile(s, ProtoTokenList);
      if not Status then
        begin
          WriteError(ParseError);
          WriteError(ErrorLine);
          Result := false;
          Break;
        end;

      if ProtoVersion >= 2023 then
        WriteError('WARNING:  Proto version ' + IntToStr(ProtoVersion) + ' is not fully supported.');

      if not Merge or (i = ProtoFiles.Count -1) then
        begin
          ClassName := '';

          if Merge then
            begin
              if MergeFileName <> '' then
                begin
                  MergeFileName := ExtractFileName(MergeFileName);
                  if Pos(Prefix, MergeFileName) = 1 then
                    Delete(MergeFileName, 1, Length(Prefix));
                  ClassName := MergeFileName;
                  FileName := OutPath + Prefix + MergeFileName + '.pas';
                end
              else
                begin
                  MergeFileName := 'ProtoBufPas';
                  ClassName := MergeFileName;
                  FileName := OutPath + Prefix + MergeFileName + '.pas';
                end;
            end;
          UnitNameStr := '';
          Status := TokenToPascal(ProtoTokenList, DelphiFile, Prefix, ClassName, UnitNameStr);
          if not Status then
            begin
              WriteError(ParseError);
              WriteError(ErrorLine);
              Result := false;
              Break;
            end;

          if not Merge then
            begin
              FileName := OutPath + UnitNameStr + '.pas';
            end;

          {$IFNDEF FPC}System.SysUtils.{$ENDIF}DeleteFile(FileName);
          FileNameSucess := FileName;
          if DelphiFile.Count > 0 then
            DelphiFile.SaveToFile(FileName)
          else
            Result := false;
          DelphiFile.Clear;
        end;
    end;

  for i := 0 to ProtoTokenList.Count - 1 do
    begin
      Finalize(PTProtoFieldToken(ProtoTokenList[i])^);
      Dispose(PTProtoFieldToken(ProtoTokenList[i]));
    end;
  ProtoTokenList.Free;
  DelphiFile.Free;
end;



initialization
  InPathList := TStringList.Create;
  ProtoFiles := TStringList.Create;

finalization
  InPathList.Free;
  ProtoFiles.Free;

end.
