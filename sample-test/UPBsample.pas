unit UPBsample;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ProtocolBuffer, SimpleProto, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    EditReqId: TEdit;
    EditField2: TEdit;
    EditSomeString: TEdit;
    ComboBoxManyInts: TComboBox;
    ListBoxMapStrStr: TListBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    EditWireData: TEdit;
    Label8: TLabel;
    ButtonEncode: TButton;
    ButtonManyIntsAdd: TButton;
    ButtonMapStrStrAdd: TButton;
    ButtonDecode: TButton;
    MemoResult: TMemo;
    procedure ButtonManyIntsAddClick(Sender: TObject);
    procedure ButtonMapStrStrAddClick(Sender: TObject);
    procedure ButtonEncodeClick(Sender: TObject);
    procedure ButtonDecodeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonManyIntsAddClick(Sender: TObject);
var i: Integer;
begin
  i := StrToIntDef(ComboBoxManyInts.Text, -12345);
  if i <> -12345 then
    ComboBoxManyInts.Items.Add(ComboBoxManyInts.Text);
end;

procedure TForm1.ButtonMapStrStrAddClick(Sender: TObject);
var s: string; p: Integer;
begin
  if InputQuery('', 'Format is abc=xyz', s) then
    begin
      p := Pos('=', s);
      if (p > 1) and (p < Length(s)) then
        ListBoxMapStrStr.Items.Add(s);
    end;
end;

procedure TForm1.ButtonDecodeClick(Sender: TObject);
var MemStream: TMemoryStream; SimpleProto: TSimpleProto; s: string;
    i, p: Integer; PBMapStrStr: TProtoBufMapStringString;
begin
  MemoResult.Clear;
  s := EditWireData.Text;
  p := Length(s);
  if p < 2 then
    Exit;
  MemStream := TMemoryStream.Create;
  SimpleProto := TSimpleProto.Create;
  try
    MemStream.Size := p div 2;
    HexToBin(pChar(@s[1]), MemStream.Memory, p div 2);

    SimpleProto.DecodeToProto(MemStream, MemStream.Size);

    MemoResult.Lines.Add('ReqId = ' + IntToStr(SimpleProto.ReqId));
    MemoResult.Lines.Add('Field2 = ' + FloatToStr(SimpleProto.Field2));
    MemoResult.Lines.Add('SomeString = ' + SimpleProto.SomeString);
    s := '';
    for i := 0 to SimpleProto.ManyIntsCount - 1 do
      s := s + IntToStr(SimpleProto.ManyInts[i]) + ', ';
    MemoResult.Lines.Add('ManyInts = ' + s);
    MemoResult.Lines.Add('MapStrStr =');
    for i := 0 to SimpleProto.MapStrStrCount - 1 do
      begin
        PBMapStrStr := SimpleProto.MapStrStr[i];
        MemoResult.Lines.Add('  ' + IntToStr(i) + ': '  + PBMapStrStr.Key + '=' + PBMapStrStr.Value);
      end;
  finally
    MemStream.Free;
    SimpleProto.Free;
  end;
end;

procedure TForm1.ButtonEncodeClick(Sender: TObject);
var MemStream: TMemoryStream; SimpleProto: TSimpleProto; s: string;
    i, p: Integer; PBMapStrStr: TProtoBufMapStringString;
begin
  MemStream := TMemoryStream.Create;
  SimpleProto := TSimpleProto.Create;
  try
    if EditReqId.Text <> '0' then
      SimpleProto.ReqId := StrToIntDef(EditReqId.Text, 0);

    if EditField2.Text <> '0.0' then
      SimpleProto.Field2 := StrToFloatDef(EditField2.Text, 0.0);

    SimpleProto.SomeString := EditSomeString.Text;

    SimpleProto.ManyIntsCount := ComboBoxManyInts.Items.Count;
    for i := 0 to ComboBoxManyInts.Items.Count -1 do
      SimpleProto.ManyInts[i] := StrToIntDef(ComboBoxManyInts.Items[i], 0);

    for i := 0 to ListBoxMapStrStr.Items.Count -1 do
      begin
        s := ListBoxMapStrStr.Items[i];
        p := Pos('=', s);
        PBMapStrStr.Key := Copy(s, 1, p -1);
        PBMapStrStr.Value := Copy(s, p + 1, 100);
        SimpleProto.MapStrStr[i] := PBMapStrStr;
      end;

    SimpleProto.EncodeToStream(MemStream);

    SetLength(s, MemStream.Size * 2);
    BinToHex(MemStream.Memory, pChar(s), MemStream.Size);
    EditWireData.Text := s;

  finally
    MemStream.Free;
    SimpleProto.Free;
  end;
end;

end.
