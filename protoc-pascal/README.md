# protoc-pascal
This utility will take ProtocolBuffer message .proto formatted files as input, and create matching Delphi / FPC object pascal files. These work in conjuction with the [ProtocolBuffer.pas](ProtocolBuffer.pas) file, to recv and send data in ProtocolBuffer wire format.

**See the command line parameters to improve the use of this utility.**  These options are needed to make life a lot easier when working with multiple .proto files.

### Command line paramaters and options
```
     Usage :: protoc-pascal  source  [file [file]...]  [options]

   source           ::   Specify either a single .proto file, or
                           a space seperated list of .proto files, or
                           a directory to search for all .proto files.

  -tmplt=NAME       ::   File that is the template to create the TSomeProto class
                           files from.  Included default is attached to this binary.
                           Use option -prntmplt to retrieve it.
  -prntmplt         ::   Show / dump the included TemplateProtoBuffer.pas file.

  -ip, -inpath=PATH ::   Specify the directory in which to search for .proto files.
                           May be specified multiple times.  Directories will be searched
                           in order.  Otherwise the current working directory is used.
  -op, -outpath=PATH::   Specify the directory to place the output files.
  -v, -version      ::   Show version info and exit.
  -h,? -help        ::   Show this text and exit.
  -m, -merge        ::   Merges all output files into one .pas file. The default
                           result is one .pas file per .proto input file.
  -mn, -mname=NAME  ::   Name for the merged file (name.pas) - only if -m option used.
  -pr, -prefix=ID   ::   Apply a prefix to all Class and Type names T<prefix>SomeProto
  -sv, -srcver=NAME ::   A file with the source version text - inserted to output header.
  -iv, -initval=NAME::   A file listing the default values to add to the class Create.
  -valeq            ::   Include "if Value <> Fxxx" test code in Setproperty procedures,
                           otherwise all prop assignments are written and touched flag set.
  -enmcnst          ::   Save enum to const values, or pascal enumeration type (default).
```

###  Notes:
- The utility can process hundreds of input .proto files in the same run.  It searches directories for .proto files.  With default settings, it outputs one .pas file for every .proto file.
- For better results, use the ```-m, -merge``` param, as it merges hundreds of single .pas files into one large .pas file.  All the required ```uses, forwards, etc``` statements are added automatically.
- The default template file is attached to the binary, and no extra file is needed.  If you want to modify the template, then dump the original ```-prntmplt```, modify it and reload it with ```-tmplt=NAME```.
- The ```-pr, -prefix=ID``` adds a short name to the front of all proto classes. i.e. ```prefix=ABC``` gives ```TABC<protoname> = class```
- Enum types:  Normally these are converted into pascal Enumeration type.  But if the option ```allow_alias``` is in the enum definition, indicating C style enum value integer re-use, then this utility will force all enum values to ```const``` values.  The ```-enmcnst``` param does this too.
- The option ```-sv, -srcver=NAME``` gives a single line import of a version number - usefull to stamp proto file revisions # into the resulting .pas files.  Its printed on about line 6.
- The ```-valeq``` controls how ```property write``` code is created.  In default, each property value assignment will write the value to the private FSomeprop: integer;  This in not normal pascal.  With the ```-valeq``` param added, the write code will include the usual ```if Value <> FSomeprop  then  FSomeprop := Value;```.\
This setting is important to the way you send data.  Writing a value will set the touched flag, and cause the value to be pushed out the data stream, including empty strings and zeros. However, depending on the recieving end code, they might follow the proto rules and know zero and null strings are valid and do not need them sent (use ```-valeq```).  But with badly designed reciever code it will expect a valid field to be ```set``` including the zeroes and nulls (```-valeq``` required).
- 

