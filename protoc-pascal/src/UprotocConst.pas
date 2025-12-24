unit UprotocConst;

interface

uses System.SysUtils;

type
  TCardinalityParam = (psNone, psRequired, psOptional, psOneOf, psReserved, psRepeated, psMap);
  TWireDataSubType = (pbwstUnknown,
    pbwstVarIntInt32, pbwstVarIntInt64, pbwstVarIntUInt32, pbwstVarIntUInt64, pbwstVarIntSInt32, pbwstVarIntSInt64, pbwstVarIntBool, pbwstVarIntEnum,
    pbwstI64Fixed64, pbwstI64SFixed64, pbwstI64Double,
    pbwstLenString, pbwstLenBytes, pbwstNestedMessage, pbwstRepteadNestedMessage, pbwstMapMessage,
    pbwstI32Fixed32, pbwstI32SFixed32, pbwstI32Float,
    pbwstMapPair,
    pbwstUnused
    );

  PTProtoFieldToken = ^TProtoFieldToken;
  TProtoFieldToken = record
    HeaderToken: Boolean;
    ProtoName: string;
    Cardinality: TCardinalityParam;
    ProtoType: TWireDataSubType;
    FieldName: string;
    FieldIndex: Integer;
    LastOneofIndex: Integer;
    LastField: Boolean;
    DefaultVal: string;
  end;

const BuildVersionStr: string = 'v1.03';

const TEMPLATE_FILE_SIZE  = 10000; // MUST be equal or bigger that actual file size;

const CardinalityStr: array [TCardinalityParam] of string = ('*missing*', 'required', 'optional', 'oneof', 'reserved', 'repeated', 'map');
const CardinalityShtStr: array [TCardinalityParam] of string = ('', 'Req', 'Opt', 'One', 'Res', 'Rep', 'Map');

const WireDataSubTypeAsStr: array [TWireDataSubType] of string = ('pbwstUnknown',
    'pbwstVarIntInt32', 'pbwstVarIntInt64', 'pbwstVarIntUInt32', 'pbwstVarIntUInt64', 'pbwstVarIntSInt32', 'pbwstVarIntSInt64', 'pbwstVarIntBool', 'pbwstVarIntEnum',
    'pbwstI64Fixed64', 'pbwstI64SFixed64', 'pbwstI64Double',
    'pbwstLenString', 'pbwstLenBytes', 'pbwstNestedMessage', 'pbwstRepteadNestedMessage', 'pbwstMapMessage',
    'pbwstI32Fixed32', 'pbwstI32SFixed32', 'pbwstI32Float',
    'pbwstMapPair',
    'pbwstUnused'
    );

const WireDataSubTypeStr: array [TWireDataSubType] of string = ('*missing*',
    'int32', 'int64', 'uint32', 'uint64', 'sint32', 'sint64', 'bool', 'enum',
    'fixed64', 'sfixed64', 'double',
    'string', 'bytes', 'nestedmessage', 'repteadnestedmessage', 'mapmessage',
    'fixed32', 'sfixed32', 'float',
    'mappair',
    'notused'
    );

const WireDataSubTypeToPascal: array [TWireDataSubType] of string = ('*missing*',
    'Integer', 'Int64', 'Cardinal', 'UInt64', 'Integer', 'Int64', 'Boolean', 'Word',
    'UInt64', 'Int64', 'Double',
    'string', 'byte', 'nestedmessage', 'repteadnestedmessage', 'mapmessage',
    'Cardinal', 'Integer', 'Single',
    'mappair',
    'notused'
);

const HeaderStr: array [0..2] of string = (
'  protoc-pascal.exe',
'    A tool to create Delphi files from Protocol Buffer v3 message files.',
'    by HHS Software Corp (c) 2025.'
);

const SucessStr: array [0..1] of string = (
'',
'Done.  Output file(s) located at: '
);

const PasHeaderStr: array [0..7] of string = (
'{',
'Subject to the terms of the MIT license: Copyright (c) 2025 HHS Software Corp.',
'  Created with protoc-pascal.exe',
'    https://github.com/HHS-Software/Protocol-Buffer_Delphi_FPC',
'    A utility to read and convert the Google Protocol Buffer v3 system into pascal code.',
'    The code is designed for the v3 proto.  Later proto versions possibly too.',
'{*VERSIONSTR*}',
'}'
);


const BlurbStr: array [0..27] of string = (
'  Parse PROTO_FILES and generate output based on the options given:',
'',
'     Usage :: protoc-pascal  source  [file [file]...]  [options]',
'',
'   source           ::   Specify either a single .proto file, or',
'                           a space seperated list of .proto files, or',
'                           a directory to search for all .proto files.',
'',
'  -tmplt=NAME       ::   File that is the template to create the TSomeProto class',
'                           files from.  Included default is attached to this binary.',
'                           Use option -prntmplt to retrieve it.',
'  -prntmplt         ::   Show / dump the included TemplateProtoBuffer.pas file.',
'',
'  -ip, -inpath=PATH ::   Specify the directory in which to search for .proto files.',
'                           May be specified multiple times.  Directories will be searched',
'                           in order.  Otherwise the current working directory is used.',
'  -op, -outpath=PATH::   Specify the directory to place the output files.',
'  -v, -version      ::   Show version info and exit.',
'  -h,? -help        ::   Show this text and exit.',
'  -m, -merge        ::   Merges all output files into one .pas file. The default',
'                           result is one .pas file per .proto input file.',
'  -mn, -mname=NAME  ::   Name for the merged file (name.pas) - only if -m option used.',
'  -pr, -prefix=ID   ::   Apply a prefix to all Class and Type names T<prefix>SomeProto',
'  -sv, -srcver=NAME ::   A file with the source version text - inserted to output header.',
'  -iv, -initval=NAME::   A file listing the default values to add to the class Create.',
'  -valeq            ::   Include "if Value <> Fxxx" test code in Setproperty procedures,',
'                           otherwise all prop assignments are written and touched flag set.',
'  -enmcnst          ::   Save enum to const values, or pascal enumeration type (default).'
);


const ReservedWords: array [0..82] of string = (
  'absolute', 'and', 'array', 'asm', 'begin', 'case', 'const', 'constructor', 'destructor', 'div', 'do', 'downto', 'else',
  'end', 'file', 'for', 'function', 'goto', 'if', 'implementation', 'in', 'inherited', 'inline', 'interface', 'label', 'mod',
  'nil', 'not', 'object', 'of', 'operator', 'or', 'packed', 'procedure', 'program', 'record', 'reintroduce', 'repeat', 'self',
  'set', 'shl', 'shr', 'string', 'then', 'to', 'type', 'unit', 'until', 'uses', 'var', 'while', 'with', 'xor', 'as', 'class',
  'dispinterface', 'except', 'exports', 'finalization', 'finally', 'initialization', 'inline', 'is', 'library', 'on', 'out',
  'packed', 'property', 'raise', 'resourcestring', 'threadvar', 'try', 'default',
  'override', 'virtual', 'dynamic', 'overload', 'forward', 'uses', 'abstract', 'initialization', 'finalization', 'message'
  );

const ProhibitedNameCharsSet: TSysCharSet = [#0..#21, #58..#64, #91..#96, #123..#255];
const VowelChars: TSysCharSet = ['a','e','i','o','u'];
const NumberChars: TSysCharSet = ['0'..'9'];
const MoreChars: TSysCharSet = ['b','g','h','k','q','v','x'];

const GoogleAnyProto: array [0..3] of string = (
'message GoogleAny {',
'  string TypeURL = 1;',
'  bytes Value = 2;',
'}'
);



const  // in the template file
  CRLF = #13#10;
  UnitName: string = '{*UNITNAME*}unit;';
  Forwards: string = '{*FORWARDS*}';
  EnumType: string = '{*ENUMTYPE*}';
  IntroEnd: string = '{*INTROEND*}';
  ProtoName: string = '{*PROTONAME*}';
  TProtoName: string = '{*TPROTONAME*}';
  TNestedProtoName: string = '{*TNESTEDPROTONAME*}';
  PrivateField: string = '{*PRIVATEFIELD*}';
  PrivateSetGet: string = '{*PRIVATESETGET*}';
  PublicFuncProp: string = '{*PUBLICFUNCPROC*}';
  Properties: string = '{*PROPERTIES*}';
  FPropName: string = '{*FPROPNAME*}';
  DataType: string = '{*DATATYPE*}';
  DataTypeKey: string = '{*DATATYPEKEY*}';
  DataTypeValue: string = '{*DATATYPEVALUE*}';
  Usess: string = '{*USES*}';
  MaxID: string = '{*MAXID*}';
  WireSubTypes: string = '{*WIRESUBTYPES*}';
  WireSubTypesMaps: string = '{*WIRESUBTYPESMAPS*}';
  NestedCreate: string = '{*NESTEDCREATE*}';
  OnCreateDefault: string = '{*ONCREATEDEFAULT*}';
  ParentIndex: string = '{*PARENTINDEX*}';
  NestedFree: string = '{*NESTEDFREE*}';
  ReadFieldByIndex: string = '{*READFIELDBYINDEX*}';
  ReadFieldByIndexSizeCase: string = '{*READFIELDBYINDEXSIZECASE*}';
  WriteFieldByIndex: string = '{*WRITEFIELDBYINDEX*}';
  SetGetMethods: string = '{*SETGETMETHODS*}';
  TypeHeaderStart: string = '{*TYPEHEADERSTART*}';
  TypeHeaderEnd: string = '{*TYPEHEADEREND*}';
  TypeBodyStart: string = '{*TYPEBODYSTART*}';
  TypeBodyEnd: string = '{*TYPEBODYEND*}';
  ReadFieldByIndexArray: string = '{*READFIELDBYINDEXARRAY*}';
  ReadFieldByIndexArrayEnd: string = '{*READFIELDBYINDEXARRAYEND*}';
  WriteFieldByIndexArray: string = '{*WRITEFIELDBYINDEXARRAY*}';
  WriteFieldByIndexArrayEnd: string = '{*WRITEFIELDBYINDEXARRAYEND*}';
  ReadMapFieldByIndex: string = '{*READMAPFIELDBYINDEX*}';
  ReadMapFieldByIndexEnd: string = '{*READMAPFIELDBYINDEXEND*}';
  WriteMapFieldByIndex: string = '{*WRITEMAPFIELDBYINDEX*}';
  WriteMapFieldByIndexEnd: string = '{*WRITEMAPFIELDBYINDEXEND*}';
  GetSetNestedPropArrayCount: string = '{*GETSETNESTEDPROPARRAYCOUNT*}';
  GetSetNestedPropArrayCountEnd: string = '{*GETSETNESTEDPROPARRAYCOUNTEND*}';
  NestedFreeVar: string = ('{*NESTEDFREEVAR*}');
  WriteTouchAdjust: string = ('{*WRITEFIELDBYINDEXTOUCHDADJUST*}');
  WireDataSubMapTypeFuncCase: string = ('{*WIREDATASUBMAPTYPEFUNCCASE*}');
  WireDataSubTypeFunc: string = ('{*WIREDATASUBTYPEFUNC*}');
  WireDataSubTypeFuncEnd: string = ('{*WIREDATASUBTYPEFUNCEND*}');
  WireDataSubMapTypeFunc: string = ('{*WIREDATASUBMAPTYPEFUNC*}');
  WireDataSubMapTypeFuncEnd: string = ('{*WIREDATASUBMAPTYPEFUNCEND*}');
  GetNestedArrayObjectByIndex: string = ('{*GETNESTEDARRAYOBJECTBYINDEX*}');
  GetNestedArrayObjectByIndexEnd: string = ('{*GETNESTEDARRAYOBJECTBYINDEXEND*}');
  GetNestedArrayObjectByIndexCaseInsert: string = ('{*GETNESTEDARRAYOBJECTBYINDEXCASEINSERT*}');
  VersionStr: string = ('{*VERSIONSTR*}');
  IndexNum: string = ('{*INDEXNUM*}');
  GetSetCountMethodsArray: string = ('{*GETSETCOUNTMETHODSARRAY*}');
  GetSetCountMethodsArrayEnd: string = ('{*GETSETCOUNTMETHODSARRAYEND*}');
  SetPropArrayCount: string = ('{*SETPROPARRAYCOUNT*}');
  SetPropArrayCountEnd: string = ('{*SETPROPARRAYCOUNTEND*}');
  GetItemArrayFuncName: string = ('{*GETITEMARRAYFUNCNAME*}');
  SetItemArrayFuncName: string = ('{*SETITEMARRAYFUNCNAME*}');
  GetCountArrayFuncName: string = ('{*GETCOUNTARRAYFUNCNAME*}');
  SetCountArrayFuncName: string = ('{*SETCOUNTARRAYFUNCNAME*}');
  GetNestedCountArrayFuncName: string = ('{*GETNESTEDCOUNTARRAYFUNCNAME*}');
  SetNestedCountArrayFuncName: string = ('{*SETNESTEDCOUNTARRAYFUNCNAME*}');
  GetNestedItemArrayFuncName: string = ('{*GETNESTEDITEMARRAYFUNCNAME*}');
  SetNestedCountArrayFuncNameNoParam: string = ('{*SETNESTEDCOUNTARRAYFUNCNAMENOPARAM*}');
  SetByteArray: string = ('{*SETBYTESARRAY*}');
  SetByteArrayEnd: string = ('{*SETBYTESARRAYEND*}');

implementation

end.
