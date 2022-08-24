object Form2: TForm2
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'DataValidator - Sanitize'
  ClientHeight = 297
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 105
    Height = 297
    Align = alLeft
    BevelOuter = bvNone
    Color = 2565927
    ParentBackground = False
    TabOrder = 0
    object btnSanitize: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 99
      Height = 25
      Align = alTop
      Caption = 'Sanitize'
      TabOrder = 0
      OnClick = btnSanitizeClick
    end
  end
  object Panel2: TPanel
    Left = 105
    Top = 0
    Width = 545
    Height = 297
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object EditEmail: TLabeledEdit
      Left = 102
      Top = 6
      Width = 437
      Height = 23
      EditLabel.Width = 29
      EditLabel.Height = 15
      EditLabel.Caption = 'Email'
      LabelPosition = lpLeft
      TabOrder = 0
      Text = 'DEVELOPER.DLIO@GMAIL.COM'
    end
    object EditBase64ToDecode: TLabeledEdit
      Left = 102
      Top = 30
      Width = 437
      Height = 23
      EditLabel.Width = 88
      EditLabel.Height = 15
      EditLabel.Caption = 'Base64ToDecode'
      LabelPosition = lpLeft
      TabOrder = 1
      Text = 'QWNlc3NlOiBodHRwczovL2dpdGh1Yi5jb20vZGxpb2NvZGU='
    end
    object EditBase64ToEncode: TLabeledEdit
      Left = 102
      Top = 54
      Width = 437
      Height = 23
      EditLabel.Width = 87
      EditLabel.Height = 15
      EditLabel.Caption = 'Base64ToEncode'
      LabelPosition = lpLeft
      TabOrder = 2
      Text = 'Minha Mensagem'
    end
    object MemoInfo: TMemo
      Left = 0
      Top = 83
      Width = 545
      Height = 214
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 3
    end
  end
end
