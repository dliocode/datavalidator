object Form2: TForm2
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'DataValidator - Basic'
  ClientHeight = 117
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
    Height = 117
    Align = alLeft
    BevelOuter = bvNone
    Color = 2565927
    ParentBackground = False
    TabOrder = 0
    object btnLimpar: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 99
      Height = 25
      Align = alTop
      Caption = 'Limpar'
      TabOrder = 0
      OnClick = btnLimparClick
    end
    object btnCheck: TButton
      AlignWithMargins = True
      Left = 3
      Top = 31
      Width = 99
      Height = 25
      Margins.Top = 0
      Align = alTop
      Caption = 'Check'
      TabOrder = 1
      OnClick = btnCheckClick
    end
    object btnCheckAllFirst: TButton
      AlignWithMargins = True
      Left = 3
      Top = 87
      Width = 99
      Height = 25
      Margins.Top = 0
      Align = alTop
      Caption = 'CheckAll (First)'
      TabOrder = 2
      OnClick = btnCheckAllFirstClick
    end
    object btnCheckAll: TButton
      AlignWithMargins = True
      Left = 3
      Top = 59
      Width = 99
      Height = 25
      Margins.Top = 0
      Align = alTop
      Caption = 'CheckAll'
      TabOrder = 3
      OnClick = btnCheckAllClick
    end
  end
  object Panel2: TPanel
    Left = 105
    Top = 0
    Width = 545
    Height = 117
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object EditCodigo: TLabeledEdit
      Left = 62
      Top = 11
      Width = 59
      Height = 23
      EditLabel.Width = 39
      EditLabel.Height = 15
      EditLabel.Caption = 'C'#243'digo'
      LabelPosition = lpLeft
      TabOrder = 0
      Text = ''
    end
    object EditNome: TLabeledEdit
      Left = 62
      Top = 35
      Width = 275
      Height = 23
      EditLabel.Width = 33
      EditLabel.Height = 15
      EditLabel.Caption = 'Nome'
      LabelPosition = lpLeft
      TabOrder = 1
      Text = ''
    end
    object EditEmail: TLabeledEdit
      Left = 62
      Top = 59
      Width = 467
      Height = 23
      EditLabel.Width = 34
      EditLabel.Height = 15
      EditLabel.Caption = 'E-mail'
      LabelPosition = lpLeft
      TabOrder = 2
      Text = ''
    end
    object EditFone: TLabeledEdit
      Left = 62
      Top = 83
      Width = 99
      Height = 23
      EditLabel.Width = 35
      EditLabel.Height = 15
      EditLabel.Caption = 'Fone 1'
      LabelPosition = lpLeft
      TabOrder = 3
      Text = ''
    end
    object EditFone2: TLabeledEdit
      Left = 238
      Top = 83
      Width = 99
      Height = 23
      EditLabel.Width = 35
      EditLabel.Height = 15
      EditLabel.Caption = 'Fone 2'
      LabelPosition = lpLeft
      TabOrder = 4
      Text = ''
    end
    object EditFone3: TLabeledEdit
      Left = 430
      Top = 83
      Width = 99
      Height = 23
      EditLabel.Width = 35
      EditLabel.Height = 15
      EditLabel.Caption = 'Fone 3'
      LabelPosition = lpLeft
      TabOrder = 5
      Text = ''
    end
    object EditDataNascimento: TLabeledEdit
      Left = 448
      Top = 35
      Width = 81
      Height = 23
      EditLabel.Width = 91
      EditLabel.Height = 15
      EditLabel.Caption = 'Data Nascimento'
      LabelPosition = lpLeft
      TabOrder = 6
      Text = ''
    end
  end
end
