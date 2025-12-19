program protoc_pascal;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Uprotoc in 'Uprotoc.pas',
  UprotocParser in 'UprotocParser.pas',
  UprotocGenerator in 'UprotocGenerator.pas',
  UprotocConst in 'UprotocConst.pas';

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  try
    WriteHeader;
    WriteLn('');
    if not ParseCommandLine then
      begin
        if WriteBlurbOut then
          WriteBlurb;
      end
    else
      begin
        WriteLn('');
        if not CreateDelphiFiles then
          begin
            WriteLn('');
            WriteBlurb;
          end
        else
          WriteSucess;
        WriteLn('');
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
