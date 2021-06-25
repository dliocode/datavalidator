object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form '
  ClientHeight = 527
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
  object LabeledEditEmail: TLabeledEdit
    Left = 8
    Top = 24
    Width = 372
    Height = 21
    CharCase = ecUpperCase
    EditLabel.Width = 28
    EditLabel.Height = 13
    EditLabel.Caption = 'E-mail'
    TabOrder = 0
    Text = 'DEVELOPER.DLIO@GMAIL.COM'
  end
  object btnSanitize: TButton
    Left = 8
    Top = 155
    Width = 372
    Height = 41
    Caption = 'Sanitize'
    TabOrder = 1
    OnClick = btnSanitizeClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 202
    Width = 388
    Height = 325
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    TabOrder = 2
  end
  object LabeledEditBase64ToDecode: TLabeledEdit
    Left = 8
    Top = 80
    Width = 372
    Height = 21
    EditLabel.Width = 89
    EditLabel.Height = 13
    EditLabel.Caption = 'Base64 To Decode'
    TabOrder = 3
    Text = 'QWNlc3NlOiBodHRwczovL2dpdGh1Yi5jb20vZGxpb2NvZGU='
  end
  object LabeledEditBase64ToEncode: TLabeledEdit
    Left = 8
    Top = 128
    Width = 372
    Height = 21
    EditLabel.Width = 91
    EditLabel.Height = 13
    EditLabel.Caption = 'Base64 To Encode '
    TabOrder = 4
    Text = 'Minha Mensagem'
  end
end
