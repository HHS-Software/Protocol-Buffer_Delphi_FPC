## Some coding notes:

- To compile the TemplateProtoBuffer.pas file into the binary, in Delphi, set Project options -> Build events -> Post build events -> Commands  and add the following lines:\
```copy /b .\$(Platform)\$(Config)\$(OUTPUTNAME).exe + .\TemplateProtoBuffer.pas  .\$(Platform)\$(Config)\$(OUTPUTNAME).exe.both``` ...and...\
```move /y .\$(Platform)\$(Config)\$(OUTPUTNAME).exe.both .\$(Platform)\$(Config)\$(OUTPUTNAME).exe```
