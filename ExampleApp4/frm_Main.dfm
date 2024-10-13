object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'TCSVClass Demoapp'
  ClientHeight = 630
  ClientWidth = 918
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 20
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 918
    Height = 630
    ActivePage = TabSheet5
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Style = tsButtons
    TabHeight = 50
    TabOrder = 0
    object TabSheet5: TTabSheet
      Caption = 'TCSVClass-Einleitung'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 910
        Height = 570
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 8
        TabOrder = 0
        object RichEdit1: TRichEdit
          Left = 8
          Top = 8
          Width = 894
          Height = 554
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          Lines.Strings = (
            'RichEdit1')
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
          Transparent = True
          WantTabs = True
          OnLinkClick = RichEdit1LinkClick
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Stringlist Output'
      object ListBox1: TListBox
        Left = 3
        Top = 3
        Width = 233
        Height = 589
        ItemHeight = 20
        TabOrder = 0
        OnClick = ListBox1Click
      end
      object Panel1: TPanel
        Left = 256
        Top = 3
        Width = 529
        Height = 589
        TabOrder = 1
        object Label1: TLabel
          Left = 85
          Top = 24
          Width = 27
          Height = 20
          Caption = 'PLZ:'
        end
        object Label2: TLabel
          Left = 88
          Top = 64
          Width = 24
          Height = 20
          Caption = 'Ort:'
        end
        object Label3: TLabel
          Left = 32
          Top = 105
          Width = 80
          Height = 20
          Caption = 'Bundesland:'
        end
        object Label4: TLabel
          Left = 27
          Top = 145
          Width = 85
          Height = 20
          Caption = 'Bev'#246'lkerung:'
        end
        object Label5: TLabel
          Left = 23
          Top = 186
          Width = 89
          Height = 20
          Caption = 'CO2-Aussto'#223':'
        end
        object Label6: TLabel
          Left = 342
          Top = 145
          Width = 69
          Height = 20
          Caption = 'Einwohner'
        end
        object Label7: TLabel
          Left = 342
          Top = 185
          Width = 102
          Height = 20
          Caption = 'Tonnen/j'#228'hrlich'
        end
        object Label8: TLabel
          Left = 45
          Top = 228
          Width = 67
          Height = 20
          Caption = 'Erfa'#223't am:'
        end
        object Edit1: TEdit
          Left = 127
          Top = 21
          Width = 201
          Height = 28
          TabOrder = 0
        end
        object Edit2: TEdit
          Left = 127
          Top = 61
          Width = 201
          Height = 28
          TabOrder = 1
        end
        object Edit3: TEdit
          Left = 127
          Top = 101
          Width = 201
          Height = 28
          TabOrder = 2
        end
        object NumberBox1: TNumberBox
          Left = 127
          Top = 142
          Width = 202
          Height = 28
          TabOrder = 3
        end
        object NumberBox2: TNumberBox
          Left = 127
          Top = 182
          Width = 202
          Height = 28
          TabOrder = 4
        end
        object Button1: TButton
          Left = 23
          Top = 515
          Width = 202
          Height = 33
          Caption = 'In CSV speichern...'
          TabOrder = 5
          OnClick = Button1Click
        end
        object DatePicker1: TDatePicker
          Left = 127
          Top = 222
          Date = 45574.000000000000000000
          DateFormat = 'dd/mm/yyyy'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Segoe UI'
          Font.Style = []
          TabOrder = 6
        end
        object Button2: TButton
          Left = 247
          Top = 515
          Width = 234
          Height = 33
          Caption = 'CSV in die Zwischanablage...'
          TabOrder = 7
          OnClick = Button2Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Virtual Treeview Output'
      ImageIndex = 1
      object Treeview: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 910
        Height = 570
        Align = alClient
        Colors.BorderColor = 15987699
        Colors.DisabledColor = clSilver
        Colors.DropMarkColor = 15385233
        Colors.DropTargetColor = 15385233
        Colors.DropTargetBorderColor = 15385233
        Colors.FocusedSelectionColor = 15385233
        Colors.FocusedSelectionBorderColor = 15385233
        Colors.GridLineColor = 15987699
        Colors.HeaderHotColor = 9663826
        Colors.HotColor = clBlack
        Colors.SelectionRectangleBlendColor = 15385233
        Colors.SelectionRectangleBorderColor = 15385233
        Colors.SelectionTextColor = 542658373
        Colors.TreeLineColor = 9471874
        Colors.UnfocusedColor = clGray
        Colors.UnfocusedSelectionColor = clWhite
        Colors.UnfocusedSelectionBorderColor = clWhite
        DefaultNodeHeight = 22
        Header.AutoSizeIndex = 0
        Header.Background = clGray
        Header.DefaultHeight = 24
        Header.Height = 24
        Header.MainColumn = -1
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoOwnerDraw, hoShowSortGlyphs, hoVisible, hoFullRepaintOnResize]
        Header.Style = hsFlatButtons
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoHideButtons, toAutoChangeScale]
        TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toShowBackground, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
        TreeOptions.StringOptions = [toSaveCaptions]
        OnBeforeCellPaint = TreeviewBeforeCellPaint
        OnCompareNodes = TreeviewCompareNodes
        OnGetText = TreeviewGetText
        OnHeaderClick = TreeviewHeaderClick
        OnHeaderDraw = TreeviewHeaderDraw
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <>
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'StringGrid Output'
      ImageIndex = 2
      object StringGrid: TStringGrid
        Left = 0
        Top = 81
        Width = 910
        Height = 489
        Align = alClient
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goFixedRowDefAlign]
        TabOrder = 0
        OnClick = StringGridClick
        OnDrawCell = StringGridDrawCell
        OnKeyDown = StringGridKeyDown
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 910
        Height = 81
        Align = alTop
        TabOrder = 1
        object Label9: TLabel
          Left = 10
          Top = 9
          Width = 84
          Height = 20
          Caption = #196'nderungen:'
        end
        object Label10: TLabel
          Left = 584
          Top = 9
          Width = 116
          Height = 20
          Caption = '                             '
        end
        object Label11: TLabel
          Left = 249
          Top = 9
          Width = 50
          Height = 20
          Caption = 'History:'
        end
        object Button3: TButton
          Left = 10
          Top = 41
          Width = 223
          Height = 30
          Caption = #196'nderungen zur'#252'cksetzen'
          TabOrder = 0
          OnClick = Button3Click
        end
        object Button4: TButton
          Left = 309
          Top = 41
          Width = 233
          Height = 31
          Caption = #196'nderungen R'#252'ckg'#228'ngig'
          TabOrder = 1
          OnClick = Button4Click
        end
        object ComboBox1: TComboBox
          Left = 309
          Top = 7
          Width = 233
          Height = 28
          TabOrder = 2
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'ListView Output'
      ImageIndex = 3
      object ListView: TListView
        Left = 0
        Top = 0
        Width = 910
        Height = 570
        Align = alClient
        Columns = <>
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
end
