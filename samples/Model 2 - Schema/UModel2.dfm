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
  object btnValidarTodos: TButton
    Left = 8
    Top = 51
    Width = 177
    Height = 41
    Caption = 'Validar todos os campos'
    TabOrder = 3
    OnClick = btnValidarTodosClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 98
    Width = 388
    Height = 429
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    TabOrder = 5
  end
  object btnValidar: TButton
    Left = 206
    Top = 51
    Width = 177
    Height = 41
    Caption = 'Validar '
    TabOrder = 4
    OnClick = btnValidarClick
  end
  object LabeledEditApelido: TLabeledEdit
    Left = 135
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'Apelido'
    TabOrder = 1
  end
  object LabeledEditRazaoSocial: TLabeledEdit
    Left = 262
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 60
    EditLabel.Height = 13
    EditLabel.Caption = 'Razao Social'
    TabOrder = 2
  end
end
