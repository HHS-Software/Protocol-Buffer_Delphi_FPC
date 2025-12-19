# Protocol-Buffer_Delphi_FPC
An Object-Pascal implementation for Google Protocol Buffer system.  For use in Delphi and FPC projects.  Includes a code generator (protoc-pascal) to parse .proto message files into .pas files.

## How to use?
- Add the file ProtocolBuffer.pas to your project,
- Use the protoc-pascal.exe utility to convert your .proto message files into .pas object files.  Add these to your project.
- Add code to the project like so:
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
The `EncodeToStream` function serializes the properties of SampleProto into the TStream Object, ready to be sent to its destination.\
 \
To decode incoming data:
```
  SampleProto := TSampleProto.Create;
  Size := SampleProto.DecodeToProto(InboundStream, ByteSizeOfData);
```
The `DecodeToProto` function takes the encoded stream data and de-serializes it to the propeties of SampleProto.

### Features
- Automatically creates nested message (object) types.  Access these like so: `SampleProto.NestedProto.AString := 'xyz';`
- Automatically allocates repeated (array) data types.  Access these like so: `SampleProto.SomeIntArray[2] := 321;`
- Automatically creates repeated nested (array) data types.  Access these like so: `SampleProto.NestedArray[3].x := 789;`
- Converts enum message types into Pascal Enumeration types.
- Does all proto v3 specification messages, options, defaults, etc.  Handles most v2023, v2024 code too.




