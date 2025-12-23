object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'ProtoBuf sample'
  ClientHeight = 338
  ClientWidth = 614
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 36
    Top = 19
    Width = 542
    Height = 26
    Caption = 
      'This sample/test program, asks you to set some parameters, and t' +
      'hen encodes the data.  This wire data is shown in hex.  Then the' +
      ' decode turns the wire data back into regular parameters.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 278
    Top = 65
    Width = 62
    Height = 13
    Caption = 'TSimpleProto'
  end
  object Label3: TLabel
    Left = 17
    Top = 109
    Width = 65
    Height = 13
    Caption = 'ReqID (int32)'
  end
  object Label4: TLabel
    Left = 12
    Top = 136
    Width = 71
    Height = 13
    Caption = 'Field2 (double)'
  end
  object Label5: TLabel
    Left = 17
    Top = 164
    Width = 54
    Height = 13
    Caption = 'SomeString'
  end
  object Label6: TLabel
    Left = 8
    Top = 187
    Width = 82
    Height = 13
    Caption = 'ManyInts (array)'
  end
  object Label7: TLabel
    Left = 25
    Top = 211
    Width = 56
    Height = 31
    AutoSize = False
    Caption = 'MapStrStr (abc=xyz)'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 234
    Top = 274
    Width = 106
    Height = 13
    Caption = 'The wire data (in hex)'
  end
  object EditReqId: TEdit
    Left = 95
    Top = 103
    Width = 43
    Height = 21
    TabOrder = 0
    Text = '0'
  end
  object EditField2: TEdit
    Left = 96
    Top = 130
    Width = 58
    Height = 21
    TabOrder = 1
    Text = '0.0'
  end
  object EditSomeString: TEdit
    Left = 96
    Top = 157
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'abc'
  end
  object ComboBoxManyInts: TComboBox
    Left = 96
    Top = 184
    Width = 58
    Height = 21
    TabOrder = 3
  end
  object ListBoxMapStrStr: TListBox
    Left = 95
    Top = 211
    Width = 122
    Height = 58
    ItemHeight = 13
    TabOrder = 5
  end
  object EditWireData: TEdit
    Left = 25
    Top = 293
    Width = 553
    Height = 21
    TabStop = False
    TabOrder = 10
  end
  object ButtonEncode: TButton
    Left = 187
    Top = 101
    Width = 101
    Height = 25
    Caption = 'Encode to wire'
    TabOrder = 7
    OnClick = ButtonEncodeClick
  end
  object ButtonManyIntsAdd: TButton
    Left = 160
    Top = 184
    Width = 57
    Height = 21
    Caption = 'Add'
    TabOrder = 4
    OnClick = ButtonManyIntsAddClick
  end
  object ButtonMapStrStrAdd: TButton
    Left = 25
    Top = 248
    Width = 57
    Height = 21
    Caption = 'Add'
    TabOrder = 6
    OnClick = ButtonMapStrStrAddClick
  end
  object ButtonDecode: TButton
    Left = 441
    Top = 102
    Width = 101
    Height = 25
    Caption = 'Decode from wire'
    TabOrder = 8
    OnClick = ButtonDecodeClick
  end
  object MemoResult: TMemo
    Left = 393
    Top = 133
    Width = 185
    Height = 136
    TabStop = False
    TabOrder = 9
  end
end
