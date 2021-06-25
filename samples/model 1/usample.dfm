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
  object LabelMatematica: TLabel
    Left = 93
    Top = 75
    Width = 36
    Height = 13
    Caption = '3 / 3 = '
  end
  object LabeledEditNome: TLabeledEdit
    Left = 8
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 27
    EditLabel.Height = 13
    EditLabel.Caption = 'Nome'
    TabOrder = 0
  end
  object LabeledEditIdade: TLabeledEdit
    Left = 135
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 28
    EditLabel.Height = 13
    EditLabel.Caption = 'Idade'
    TabOrder = 1
  end
  object LabeledEditDataNascimento: TLabeledEdit
    Left = 262
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 96
    EditLabel.Height = 13
    EditLabel.Caption = 'Data de Nascimento'
    TabOrder = 2
  end
  object LabeledEditResultado: TLabeledEdit
    Left = 135
    Top = 72
    Width = 121
    Height = 21
    EditLabel.Width = 48
    EditLabel.Height = 13
    EditLabel.Caption = 'Resultado'
    TabOrder = 3
  end
  object btnValidarTodos: TButton
    Left = 8
    Top = 195
    Width = 177
    Height = 41
    Caption = 'Validar todos os campos'
    TabOrder = 4
    OnClick = btnValidarTodosClick
  end
  object LabeledEditCNPJ: TLabeledEdit
    Left = 8
    Top = 120
    Width = 121
    Height = 21
    EditLabel.Width = 25
    EditLabel.Height = 13
    EditLabel.Caption = 'CNPJ'
    TabOrder = 5
  end
  object LabeledEditCPF: TLabeledEdit
    Left = 135
    Top = 120
    Width = 121
    Height = 21
    EditLabel.Width = 19
    EditLabel.Height = 13
    EditLabel.Caption = 'CPF'
    TabOrder = 6
  end
  object LabeledEditEmail: TLabeledEdit
    Left = 8
    Top = 168
    Width = 375
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = 'Email'
    TabOrder = 7
  end
  object LabeledEditCPFCNPJ: TLabeledEdit
    Left = 262
    Top = 120
    Width = 121
    Height = 21
    EditLabel.Width = 48
    EditLabel.Height = 13
    EditLabel.Caption = 'CPF/CNPJ'
    TabOrder = 8
  end
  object Memo1: TMemo
    Left = 0
    Top = 242
    Width = 388
    Height = 285
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    TabOrder = 9
  end
  object btnValidar: TButton
    Left = 206
    Top = 195
    Width = 177
    Height = 41
    Caption = 'Validar '
    TabOrder = 10
    OnClick = btnValidarClick
  end
end
