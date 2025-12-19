# Protocol-Buffer_Delphi_FPC
An Object-Pascal impementation for Google Protocol Buffer system.  For use in Delphi and FPC projects.  Includes a code generator (protoc3-pascal) to parse .proto message files into .pas files.

# How to use?
- Add the file ProtocolBuffer.pas to your project,
- Use the protoc-pascal.exe to convert your .proto message files to .pas files.  Add these to your project.
- Add code to you project like so:
```
procedure TForm1.Button1Click(Sender: TObject);
var SampleProto: TSampleProto;
begin
  SampleProto := TSampleProto.Create;
  SampleProto.SomeField := 123;
    // set other properties / fields
  Size := SampleProto.EncodeToStream(OutboundStream);
  SampleProto.Free;
```
Now the OutboundStream holds the encoded ProtocolBuffer data stream, ready to be sent to its destination.
\
To decode incoming data:
```
  Size := SampleProto.DecodeToProto(InboundStream, ByteSizeOfData);
```
The SampleProto properties now hold the decoded data from the stream.\
That's it - simple.
