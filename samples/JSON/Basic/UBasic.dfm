object Form2: TForm2
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'DataValidator - Basic'
  ClientHeight = 598
  ClientWidth = 935
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
    Height = 598
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
    Width = 830
    Height = 598
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 830
      Height = 598
      ActivePage = TabSample2
      Align = alClient
      TabOrder = 0
      object TabSample1: TTabSheet
        Caption = 'Sample 1'
        object Memo1: TMemo
          Left = 0
          Top = 0
          Width = 417
          Height = 568
          Align = alLeft
          Lines.Strings = (
            '{'
            '    "id": 1,'
            '    "name": "Cabo USB",'
            '    "description": "Cabo USB 30 metros",'
            '    "price": 159.50'
            '}')
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object Memo11: TMemo
          Left = 417
          Top = 0
          Width = 405
          Height = 568
          Align = alClient
          Color = clCream
          ScrollBars = ssBoth
          TabOrder = 1
        end
      end
      object TabSample2: TTabSheet
        Caption = 'Sample 2'
        ImageIndex = 1
        object Memo2: TMemo
          Left = 0
          Top = 0
          Width = 313
          Height = 568
          Align = alLeft
          Lines.Strings = (
            '{'
            '    "id": 1,'
            '    "customers": {'
            '        "id": 555,'
            '        "name": "Fulano da Silva",'
            '        "gender": "f",'
            '        "birthDate": "1980-01-20",'
            '        "documents": ['
            '            {'
            '                "type": "cpf",'
            '                "number": "01234567890"'
            '            }'
            '        ],'
            '        "phones": ['
            '            {'
            '                "type": "primary",'
            '                "number": "554899999999"'
            '            }'
            '        ],'
            '        "email": "developer.dlio@gmail.com"'
            '    },'
            '    "items": ['
            '        {'
            '            "product_id": 111,'
            '            "product_name": "Cabo USB 2m",'
            '            "price": 100,'
            '            "quantity": 20'
            '        },'
            '        {'
            '            "product_id": 16,'
            '            "product_name": "PenDriver USB 8GB",'
            '            "price": 56.90,'
            '            "quantity": 12.98'
            '        }'
            '    ]'
            '}')
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object Memo22: TMemo
          Left = 313
          Top = 0
          Width = 509
          Height = 568
          Align = alClient
          Color = clCream
          ScrollBars = ssBoth
          TabOrder = 1
        end
      end
    end
  end
end
