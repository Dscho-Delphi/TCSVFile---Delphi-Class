object frmMani: TfrmMani
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'TCSVFile Class Example'
  ClientHeight = 160
  ClientWidth = 379
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 21
  object Label1: TLabel
    Left = 10
    Top = 13
    Width = 45
    Height = 21
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 10
    Top = 50
    Width = 81
    Height = 21
    Caption = 'Postleitzahl:'
  end
  object Label3: TLabel
    Left = 10
    Top = 86
    Width = 30
    Height = 21
    Caption = 'City:'
  end
  object Label4: TLabel
    Left = 10
    Top = 121
    Width = 37
    Height = 21
    Caption = 'State:'
  end
  object Edit1: TEdit
    Left = 100
    Top = 10
    Width = 265
    Height = 29
    TabOrder = 0
    TextHint = 'Please insert your Name'
  end
  object Edit2: TEdit
    Left = 100
    Top = 47
    Width = 265
    Height = 29
    TabOrder = 1
    TextHint = 'Insert your Zip'
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 100
    Top = 83
    Width = 265
    Height = 29
    TabOrder = 2
    TextHint = 'Insert your Zip'
  end
  object Edit4: TEdit
    Left = 100
    Top = 118
    Width = 265
    Height = 29
    TabOrder = 3
    TextHint = 'Insert your Zip'
  end
end
