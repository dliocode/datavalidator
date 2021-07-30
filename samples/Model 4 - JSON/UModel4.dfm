object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form '
  ClientHeight = 666
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object MemoJSON: TMemo
    Left = 0
    Top = 44
    Width = 388
    Height = 161
    Align = alTop
    Lines.Strings = (
      '{'
      '    "nome": "DLIO Code",'
      '    "base64":"QWNlc3NlOiBodHRwczovL2dpdGh1Yi5jb20vZGxpb2NvZGU=",'
      '    "emaill": "developer.dlio@gmail.com",'
      '    "data_cadastro": "2021-07-20T03:00:00.0Z"'
      '}')
    TabOrder = 0
  end
  object MemoResult: TMemo
    Left = 0
    Top = 205
    Width = 388
    Height = 218
    Align = alTop
    ReadOnly = True
    TabOrder = 1
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 388
    Height = 44
    Align = alTop
    TabOrder = 2
    object btnValidate: TButton
      Left = 1
      Top = 1
      Width = 386
      Height = 41
      Align = alTop
      Caption = 'Validate'
      TabOrder = 0
      OnClick = btnValidateClick
    end
  end
  object MemoJSONResult: TMemo
    Left = 0
    Top = 423
    Width = 388
    Height = 243
    Align = alClient
    Lines.Strings = (
      'MemoJSONResult')
    TabOrder = 3
  end
end
