object Form9: TForm9
  Left = 0
  Top = 0
  Hint = 'CSVFile Class Demo'
  BorderStyle = bsDialog
  Caption = 'CSVFile Class Demo'
  ClientHeight = 587
  ClientWidth = 967
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 967
    Height = 120
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = -6
    object Label1: TLabel
      Left = 14
      Top = 61
      Width = 50
      Height = 15
      Caption = 'Sortieren:'
    end
    object Label6: TLabel
      Left = 15
      Top = 92
      Width = 42
      Height = 15
      Caption = 'Suchen:'
    end
    object Label11: TLabel
      Left = 557
      Top = 92
      Width = 35
      Height = 15
      Caption = 'Zeilen:'
    end
    object Label12: TLabel
      Left = 690
      Top = 92
      Width = 42
      Height = 15
      Caption = 'Spalten:'
    end
    object ProgressBar1: TProgressBar
      Left = 431
      Top = 6
      Width = 529
      Height = 29
      TabOrder = 7
      Visible = False
    end
    object ComboBox1: TComboBox
      Left = 74
      Top = 58
      Width = 161
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      TextHint = 'Bitte ausw'#228'hlen...'
      OnChange = ComboBox1Change
    end
    object Button3: TButton
      Left = 12
      Top = 10
      Width = 125
      Height = 25
      Action = ac_open
      Caption = 'CSV-Datei '#246'ffnen...'
      TabOrder = 1
    end
    object Aufsteigend: TRadioButton
      Left = 240
      Top = 61
      Width = 97
      Height = 17
      Caption = 'Aufsteigend'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = AufsteigendClick
    end
    object RadioButton1: TRadioButton
      Left = 337
      Top = 62
      Width = 97
      Height = 16
      Caption = 'Absteigend'
      TabOrder = 3
      OnClick = RadioButton1Click
    end
    object Panel1: TPanel
      Left = -91
      Top = 45
      Width = 1198
      Height = 2
      TabOrder = 4
    end
    object Button1: TButton
      Left = 149
      Top = 10
      Width = 138
      Height = 25
      Action = ac_save
      TabOrder = 5
    end
    object Button2: TButton
      Left = 320
      Top = 10
      Width = 65
      Height = 25
      Caption = 'Clear'
      TabOrder = 6
      OnClick = Button2Click
    end
    object Edit1: TEdit
      Left = 74
      Top = 89
      Width = 161
      Height = 23
      TabOrder = 8
      TextHint = 'Suchbegriff, auch mit *'
    end
    object Button7: TButton
      Left = 245
      Top = 88
      Width = 75
      Height = 25
      Action = ac_search
      TabOrder = 9
    end
    object Button8: TButton
      Left = 330
      Top = 88
      Width = 75
      Height = 25
      Action = ac_next
      TabOrder = 10
    end
    object Button4: TButton
      Left = 833
      Top = 87
      Width = 128
      Height = 25
      Caption = 'Random Content'
      TabOrder = 11
      OnClick = Button4Click
    end
    object NumberBox1: TNumberBox
      Left = 598
      Top = 89
      Width = 81
      Height = 23
      TabOrder = 12
      Value = 1500.000000000000000000
    end
    object NumberBox2: TNumberBox
      Left = 741
      Top = 89
      Width = 81
      Height = 23
      TabOrder = 13
      Value = 6.000000000000000000
    end
  end
  object CSVGrid: TStringGrid
    Left = 0
    Top = 120
    Width = 967
    Height = 292
    Align = alClient
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goFixedRowDefAlign]
    TabOrder = 1
    OnClick = CSVGridClick
    ExplicitTop = 119
    ExplicitHeight = 273
  end
  object Panel3: TPanel
    Left = 0
    Top = 453
    Width = 967
    Height = 134
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Label4: TLabel
      Left = 15
      Top = 12
      Width = 65
      Height = 15
      Caption = 'Zelleninhalt:'
    end
    object Label5: TLabel
      Left = 91
      Top = 76
      Width = 3
      Height = 15
    end
    object Label3: TLabel
      Left = 15
      Top = 112
      Width = 563
      Height = 15
      Caption = 
        'Viele weitere Methoden und Properties um die Daten zu manipulier' +
        'en entnehmen Sie bitte der Klassenhilfe.'
    end
    object Label2: TLabel
      Left = 15
      Top = 94
      Width = 425
      Height = 15
      Caption = 
        'F'#252'r diese Demo gehen wir davon aus, das die erste Zeile einen He' +
        'ader beinhaltet!'
    end
    object Label7: TLabel
      Left = 781
      Top = 13
      Width = 101
      Height = 15
      Caption = 'Zeilen verschieben:'
    end
    object Label8: TLabel
      Left = 781
      Top = 43
      Width = 101
      Height = 15
      Caption = 'Spalte verschieben:'
    end
    object Memo1: TMemo
      Left = 86
      Top = 8
      Width = 185
      Height = 65
      TabOrder = 0
    end
    object Button5: TButton
      Left = 280
      Top = 11
      Width = 153
      Height = 25
      Caption = #196'nderung '#252'bernehmen'
      TabOrder = 1
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 815
      Top = 70
      Width = 145
      Height = 25
      Caption = 'Testbutton'
      TabOrder = 2
      Visible = False
      OnClick = Button6Click
    end
    object Button11: TButton
      Left = 930
      Top = 8
      Width = 33
      Height = 25
      Caption = #55358#56451
      TabOrder = 3
      OnClick = Button11Click
    end
    object Button12: TButton
      Left = 891
      Top = 39
      Width = 33
      Height = 25
      Caption = #55358#56448
      TabOrder = 4
      OnClick = Button12Click
    end
    object Button13: TButton
      Left = 930
      Top = 39
      Width = 33
      Height = 25
      Caption = #55358#56450
      TabOrder = 5
      OnClick = Button13Click
    end
    object CheckBox1: TCheckBox
      Left = 733
      Top = 110
      Width = 240
      Height = 17
      Caption = 'Multiline Parser verwenden (langsamer)'
      TabOrder = 6
    end
  end
  object Button9: TButton
    Left = 891
    Top = 461
    Width = 33
    Height = 25
    Caption = #55358#56449
    TabOrder = 3
    OnClick = Button9Click
  end
  object Panel4: TPanel
    Left = 0
    Top = 412
    Width = 967
    Height = 41
    Align = alBottom
    TabOrder = 4
    ExplicitLeft = 255
    ExplicitTop = 392
    ExplicitWidth = 185
    object Label9: TLabel
      Left = 9
      Top = 4
      Width = 65
      Height = 15
      Caption = 'Zelleninhalt:'
    end
    object Label10: TLabel
      Left = 9
      Top = 20
      Width = 77
      Height = 15
      Caption = 'Ben'#246'tigte Zeit:'
    end
  end
  object ActionList1: TActionList
    Left = 760
    Top = 160
    object ac_open: TAction
      Caption = #214'ffnen...'
      OnExecute = ac_openExecute
    end
    object ac_save: TAction
      Caption = 'CSV-Datei speichern...'
      OnExecute = ac_saveExecute
    end
    object ac_search: TAction
      Caption = 'Suchen'
      OnExecute = ac_searchExecute
      OnUpdate = ac_searchUpdate
    end
    object ac_next: TAction
      Caption = 'Weiter'
      OnExecute = ac_nextExecute
      OnUpdate = ac_nextUpdate
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'csv'
    Filter = 'CSV-Datei (*.csv)|*.csv'
    FilterIndex = 0
    InitialDir = 'Desktop'
    Left = 896
    Top = 144
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'csv'
    Filter = 'CSV-Datei (*.csv)|*.csv'
    InitialDir = 'desktop'
    Left = 808
    Top = 144
  end
end
