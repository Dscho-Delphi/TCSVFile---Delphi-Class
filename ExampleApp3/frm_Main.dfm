object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'CSVFile Class Demo: Read and calculate Data...'
  ClientHeight = 479
  ClientWidth = 615
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 17
  object Label1: TLabel
    Left = 13
    Top = 412
    Width = 38
    Height = 17
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 13
    Top = 439
    Width = 38
    Height = 17
    Caption = 'Label1'
  end
  object RelativePanel1: TRelativePanel
    Left = 0
    Top = 0
    Width = 615
    Height = 9
    ControlCollection = <>
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 597
  end
  object RelativePanel2: TRelativePanel
    Left = 0
    Top = 470
    Width = 615
    Height = 9
    ControlCollection = <>
    Align = alBottom
    TabOrder = 1
    ExplicitWidth = 597
  end
  object CSVGrid: TStringGrid
    Left = 0
    Top = 9
    Width = 615
    Height = 392
    Align = alTop
    TabOrder = 2
    OnDrawCell = CSVGridDrawCell
    ExplicitWidth = 597
  end
end
