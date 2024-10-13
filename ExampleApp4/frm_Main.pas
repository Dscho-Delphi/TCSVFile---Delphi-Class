unit frm_Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.DateUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Math, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, VirtualTrees, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.NumberBox,  Vcl.WinXPickers,
  Vcl.Clipbrd, RzGrids, Vcl.Grids,

  CSVFile,
  class_Frontend_Output_TStringlist,
  class_Frontend_Output_TVirtualTreeview,
  class_Frontend_Output_TStringGrid,
  class_Frontend_Output_TListView, Winapi.ShellAPI;

type
  TfrmMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    ListBox1: TListBox;
    TabSheet2: TTabSheet;
    Treeview: TVirtualStringTree;
    Panel1: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    NumberBox1: TNumberBox;
    Label4: TLabel;
    Label5: TLabel;
    NumberBox2: TNumberBox;
    Label6: TLabel;
    Label7: TLabel;
    Button1: TButton;
    DatePicker1: TDatePicker;
    Label8: TLabel;
    Button2: TButton;
    TabSheet3: TTabSheet;
    StringGrid: TStringGrid;
    TabSheet4: TTabSheet;
    ListView: TListView;
    TabSheet5: TTabSheet;
    Panel2: TPanel;
    RichEdit1: TRichEdit;
    Panel3: TPanel;
    Label9: TLabel;
    Button3: TButton;
    Button4: TButton;
    ComboBox1: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TreeviewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeviewHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure TreeviewCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure ListBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TreeviewHeaderDraw(Sender: TVTHeader; HeaderCanvas: TCanvas;
      Column: TVirtualTreeColumn; R: TRect; Hover, Pressed: Boolean;
      DropMark: TVTDropMarkMode);
    procedure TreeviewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure TreeviewAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure Button2Click(Sender: TObject);
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: LongInt;
      Rect: TRect; State: TGridDrawState);
    procedure RichEdit1LinkClick(Sender: TCustomRichEdit; const URL: string;
      Button: TMouseButton);
    procedure StringGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure StringGridClick(Sender: TObject);
  private
    FCSVFile1,FCSVFile2,FCSVFile3,FCSVFile4: TCSVFile;
    FFrontendOutput: TFrontend_Output_TStringlist;
    FFrontendTreeView: TFrontend_Output_TVirtualTreeview;
    FFrontendStringGrid: TFrontend_Output_TStringGrid;
    FFrontendListView: TFrontend_Output_TListView;
    procedure AdjustColumnWidths;
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SortColumn(Column: Integer);
    Procedure UpdateGridInfo();
  public
    { Public-Deklarationen }
    FormatSettings: TFormatSettings;
    FSortColumn: Integer; // Speichert die aktuell sortierte Spalte
    FSortDescending: Boolean; // Speichert die Sortierrichtung (aufsteigend/absteigend)

    procedure LoadDemoData(CSV:TCSVFile);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}


procedure GotoUrl(const Url: string);
begin
  if Url <> '' then
  begin
    // Die URL mit dem Standardbrowser öffnen
    ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOWNORMAL);
  end
  else
  begin
    raise Exception.Create('Die angegebene URL ist leer.');
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
Var
  DTyp : TDataType;
  Filename : String;
begin

  Filename := ExtractFileDir( Application.ExeName ) + '\Info.rtf';
  Richedit1.Lines.LoadFromFile( Filename );

  FormatSettings := TFormatSettings.Create;
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';

  try
    FCSVFile1 := TCSVFile.Create;  // Instanz von TCSVFile erstellen
    FFrontendOutput := TFrontend_Output_TStringlist.Create(FCSVFile1);  // Erstellen der Ausgabe-Klasse und Übergabe von CSVFile
    FFrontendOutput.OutputColumn := 1;
    FFrontendOutput.Strings := TStringlist(Listbox1.Items);
    FFRontendOutput.SortDirection := sdCSVAscending;

  except
    ON E:Exception do begin
      ShowMessage(Format('Fehler: %s', [E.Message]));
    end;
  end;

  if assigned(FCSVFile1) then begin
    LoadDemoData(FCSVFile1);  // Laden der Demo-Daten
    FCSVFile1.Output := FFrontendOutput;
    if assigned(FCSVFile1.Output) then begin
      FCSVFile1.Output.Update;  // Aktualisierung des Grids
    end;
  end;


  try
    FCSVFile2 := TCSVFile.Create;  // Instanz von TCSVFile erstellen
    FFrontendTreeView := TFrontend_Output_TVirtualTreeview.Create(Self.FCSVFile2, self.Treeview);
  except
    ON E:Exception do begin
      ShowMessage(Format('Fehler: %s', [E.Message]));
    end;
  end;

  if assigned(FCSVFile2) then begin
    LoadDemoData(FCSVFile2);  // Laden der Demo-Daten
    FCSVFile2.Output := FFrontendTreeView;
    if assigned(FCSVFile2.Output) then begin
      FCSVFile2.Output.Update;  // Aktualisierung des Grids
      AdjustColumnWidths;
    end;
  end;


  try
    // Initialisierung von FCSVFile3 und FFrontendStringGrid (TStringGrid)
    FCSVFile3 := TCSVFile.Create;  // Instanz von TCSVFile erstellen
    FFrontendStringGrid := TFrontend_Output_TStringGrid.Create(Self.FCSVFile3, Self.StringGrid);  // Erstellen der Ausgabe-Klasse und Übergabe von CSVFile und StringGrid
  except
    ON E:Exception do begin
      ShowMessage(Format('Fehler: %s', [E.Message]));
    end;
  end;

  if assigned(FCSVFile3) then begin
  // Festlegen, wie groß die Abständer in den Zellen sein sollen
    FFrontendStringGrid.AutoSizeSpan := 6;

    LoadDemoData(FCSVFile3);  // Laden der Demo-Daten

    FCSVFile3.Output := FFrontendStringGrid;
    if assigned(FCSVFile3.Output) then begin

      FSortColumn := 0;  // Setze die Sortierspalte auf 0
      FSortDescending := False; // Standardmäßig aufsteigend

      // Event-Handler zuweisen
      StringGrid.OnMouseDown := GridMouseDown;
      DTyp := FCSVFile3.DetectDataType( FCSVFile3.Cell[1,FSortColumn] );
      FCSVFile3.SortExt(FSortColumn, FSortDescending, True, DTyp);

      FCSVFile3.Output.Update;  // Aktualisierung des StringGrid
    end;

  end;


  try
    // Initialisierung von FCSVFile3 und FFrontendStringGrid (TStringGrid)
    FCSVFile4 := TCSVFile.Create;  // Instanz von TCSVFile erstellen
    FFrontendListView := TFrontend_Output_TListView.Create( FCSVFile3, ListView);  // Erstellen der Ausgabe-Klasse und Übergabe von CSVFile und StringGrid
  except
    ON E:Exception do begin
      ShowMessage(Format('Fehler: %s', [E.Message]));
    end;
  end;

  if assigned(FCSVFile4) then begin
    LoadDemoData(FCSVFile4);  // Laden der Demo-Daten
    FCSVFile4.Output := FFrontendListView;
    if assigned(FCSVFile4.Output) then begin

      FSortColumn := 0;  // Setze die Sortierspalte auf 0
      FSortDescending := False; // Standardmäßig aufsteigend


      FCSVFile4.SortExt(FSortColumn, FSortDescending, True, DTyp);

      FCSVFile4.Output.Update;  // Aktualisierung des StringGrid
    end;
  end;



end;

procedure TfrmMain.GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  // Koordinaten in Spalte und Zeile umrechnen
  StringGrid.MouseToCell(X, Y, Col, Row);

  // Prüfen, ob die Kopfzeile angeklickt wurde (Zeile 0)
  if (Row = 0) and (Col >= 0) then
  begin
    SortColumn(Col); // Sortiere nach der angeklickten Spalte
  end;

  // Neu zeichnen, damit der Pfeil korrekt dargestellt wird
  StringGrid.Invalidate;
end;


Procedure TfrmMain.UpdateGridInfo();
Var
  ARow,ACol : Integer;
  History : TStringlist;
  i : Integer;
  Value : String;
begin
  ARow := StringGrid.row;
  ACol := StringGrid.Col;

  Value := FCSVFile3.Cell[ARow,ACol];
  Label10.Caption := Value;

  History := FCSVFile3.GetHistory(ARow,ACol);

  if assigned(History) then begin
    Combobox1.Items.Assign(History);
    label9.Caption := 'Anzahl der Änderungen: ' + inttostr(History.Count);
  end
  else begin
    Combobox1.Items.Clear;
    label9.Caption := 'Anzahl der Änderungen: keine';
  end;



end;


procedure TfrmMain.SortColumn(Column: Integer);
Var
  DTyp : TDataType;
begin
  if FSortColumn = Column then
    FSortDescending := not FSortDescending // Sortierrichtung umkehren
  else
  begin
    FSortColumn := Column; // Neue Spalte zum Sortieren
    FSortDescending := False; // Standardmäßig aufsteigend sortieren
  end;

  // Sortiere die Daten in der CSV-Datei
  DTyp := FCSVFile3.DetectDataType( FCSVFile3.Cell[1,Column] );
  FCSVFile3.SortExt(Column, FSortDescending, True, DTyp);
  FCSVFile3.Output.Update;  // Aktualisierung des StringGrid
end;


procedure TfrmMain.Button1Click(Sender: TObject);
Var
  Index,IntValue : Integer;
  DateValue : TDateTime;
  Value : String;
begin
  Index := ListBox1.ItemIndex;
  if Index >= 0 then begin

     Value := Edit1.Text;
     FCSVFile1.Cell[Index+1, 0] := Value;

     Value := Edit2.Text;
     FCSVFile1.Cell[Index+1, 1] := Value;

     Value := Edit2.Text;
     FCSVFile1.Cell[Index+1, 2] := Value;

     IntValue := Round(Numberbox1.Value);
     Value := IntToStr(IntValue);
     FCSVFile1.Cell[Index+1, 3] := Value;

     IntValue := Round(Numberbox2.Value);
     Value := IntToStr(IntValue);
     FCSVFile1.Cell[Index+1, 4] := Value;

     DateValue := DatePicker1.Date;
     Value := DateToStr( DateValue, FormatSettings);
     FCSVFile1.Cell[Index+1, 5] := Value;

  end;

end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  Clipboard.AsText := FCSVFile1.GetasText;
end;


procedure TfrmMain.Button3Click(Sender: TObject);
begin
  FCSVFile3.ClearChanges;
  StringGrid.Invalidate;
end;

procedure TfrmMain.Button4Click(Sender: TObject);
Var
  ARow,ACol : Integer;
  History : TStringlist;
begin
  ARow := StringGrid.row;
  ACol := StringGrid.Col;
  History := FCSVFile3.GetHistory(ARow,ACol);

  if assigned(History) then begin
    StringGrid.Cells[ACol,ARow] := History[0];
    FCSVFile3.Cell[ARow,ACol] := History[0];
    UpdateGridInfo();
  end;

end;

procedure TfrmMain.ListBox1Click(Sender: TObject);
Var
  Index,IntValue : Integer;
  DateValue : TDateTime;
  Value : String;
begin
  Index := ListBox1.ItemIndex;
  if Index >= 0 then begin

     Value := FCSVFile1.Cell[Index+1, 0];
     Edit1.Text := Value;

     Value := FCSVFile1.Cell[Index+1, 1];
     Edit2.Text := Value;

     Value := FCSVFile1.Cell[Index+1, 2];
     Edit3.Text := Value;

     Value := FCSVFile1.Cell[Index+1, 3];
     TryStrToInt(Value,IntValue);
     Numberbox1.Value := IntValue;

     Value := FCSVFile1.Cell[Index+1, 4];
     TryStrToInt(Value,IntValue);
     Numberbox2.Value := IntValue;

     Value := FCSVFile1.Cell[Index+1, 5];
     TryStrToDate( Value, DateValue, FormatSettings);
     DatePicker1.Date := DateValue;


  end
  else begin


  end;
end;

procedure TfrmMain.LoadDemoData(CSV:TCSVFile);
const
  DemoCSVData =
    'PLZ;Stadt;Bundesland;Bevölkerung;CO2-Ausstoß (t);Erfasst am' + sLineBreak +
    '10115;Berlin;Berlin;3769495;23000000;2021-03-15' + sLineBreak +
    '80331;München;Bayern;1488200;11000000;2021-06-21' + sLineBreak +
    '20095;Hamburg;Hamburg;1857253;10200000;2021-07-10' + sLineBreak +
    '04109;Leipzig;Sachsen;605258;3650000;2021-09-05' + sLineBreak +
    '01067;Dresden;Sachsen;560000;2650000;2021-10-01' + sLineBreak +
    '90402;Nürnberg;Bayern;515000;3200000;2021-11-13' + sLineBreak +
    '70173;Stuttgart;Baden-Württemberg;635000;4200000;2022-01-15' + sLineBreak +
    '50667;Köln;Nordrhein-Westfalen;1084000;6150000;2022-02-20' + sLineBreak +
    '60549;Frankfurt am Main;Hessen;763000;4550000;2022-04-14' + sLineBreak +
    '28195;Bremen;Bremen;570000;2100000;2022-05-30' + sLineBreak +
    '04103;Chemnitz;Sachsen;246000;1300000;2022-06-25' + sLineBreak +
    '01069;Zwickau;Sachsen;91000;520000;2022-08-11' + sLineBreak +
    '30159;Hannover;Niedersachsen;537000;3100000;2022-09-19' + sLineBreak +
    '99084;Erfurt;Thüringen;214000;1150000;2022-10-30' + sLineBreak +
    '45000;Gelsenkirchen;Nordrhein-Westfalen;262000;1450000;2022-11-15' + sLineBreak +
    '48143;Münster;Nordrhein-Westfalen;315000;1900000;2023-01-12' + sLineBreak +
    '09111;Chemnitz;Sachsen;246000;1250000;2023-02-14' + sLineBreak +
    '04229;Halle (Saale);Sachsen-Anhalt;239000;1350000;2023-03-22' + sLineBreak +
    '39104;Magdeburg;Sachsen-Anhalt;240000;1250000;2023-05-05' + sLineBreak +
    '06108;Halle;Sachsen-Anhalt;239000;1350000;2023-06-17' + sLineBreak +
    '52062;Aachen;Nordrhein-Westfalen;251000;1550000;2023-07-21' + sLineBreak +
    '85049;Ingolstadt;Bayern;135000;800000;2023-08-16' + sLineBreak +
    '60528;Offenbach am Main;Hessen;130000;700000;2023-09-30' + sLineBreak +
    '70176;Heilbronn;Baden-Württemberg;126000;650000;2023-10-10' + sLineBreak +
    '96450;Coburg;Bayern;41000;200000;2023-01-28' + sLineBreak +
    '89073;Ulm;Baden-Württemberg;126000;650000;2022-12-15' + sLineBreak +
    '24103;Kiel;Schleswig-Holstein;247000;1300000;2022-11-01' + sLineBreak +
    '66111;Saarbrücken;Saarland;180000;1100000;2023-02-20' + sLineBreak +
    '06844;Dessau-Roßlau;Sachsen-Anhalt;82000;400000;2022-08-07' + sLineBreak +
    '14467;Potsdam;Brandenburg;183000;1000000;2023-04-14' + sLineBreak +
    '86150;Augsburg;Bayern;295000;1600000;2023-05-19' + sLineBreak +
    '54290;Trier;Rheinland-Pfalz;110000;650000;2023-03-16' + sLineBreak +
    '56068;Koblenz;Rheinland-Pfalz;114000;700000;2023-01-11' + sLineBreak +
    '99423;Weimar;Thüringen;65000;350000;2022-05-25' + sLineBreak +
    '92637;Weiden in der Oberpfalz;Bayern;42000;230000;2023-06-30' + sLineBreak +
    '72070;Tübingen;Baden-Württemberg;91000;500000;2022-09-09' + sLineBreak +
    '54292;Trier-Ehrang;Rheinland-Pfalz;22000;120000;2023-04-01' + sLineBreak +
    '07743;Jena;Thüringen;110000;550000;2023-07-23' + sLineBreak +
    '39112;Magdeburg-Neustadt;Sachsen-Anhalt;35000;175000;2023-10-05' + sLineBreak +
    '04129;Leipzig-Eutritzsch;Sachsen;18000;90000;2023-08-14' + sLineBreak +
    '17489;Greifswald;Mecklenburg-Vorpommern;59000;300000;2022-12-03' + sLineBreak +
    '99089;Erfurt-Nord;Thüringen;42000;200000;2022-11-20' + sLineBreak +
    '02625;Bautzen;Sachsen;38000;200000;2023-05-02' + sLineBreak +
    '06110;Halle-Neustadt;Sachsen-Anhalt;45000;230000;2023-02-05' + sLineBreak +
    '93047;Regensburg;Bayern;153000;850000;2023-09-12' + sLineBreak +
    '49074;Osnabrück;Niedersachsen;165000;920000;2022-10-28' + sLineBreak +
    '68159;Mannheim;Baden-Württemberg;309000;1700000;2023-06-22';


begin

  // Einfügen der Demo-CSV-Daten in das CSVFile-Objekt
  CSV.InsertFromText(DemoCSVData);

end;

procedure TfrmMain.RichEdit1LinkClick(Sender: TCustomRichEdit;
  const URL: string; Button: TMouseButton);
begin
  if trim(URL) = 'TFrontend_Output_TStringlist' then
    PageControl1.ActivePageIndex := 1
  else if trim(URL) = 'TFrontend_Output_TVirtualTreeview' then
    PageControl1.ActivePageIndex := 2
  else if trim(URL) = 'TFrontend_Output_TStringGrid' then
    PageControl1.ActivePageIndex := 3
  else if trim(URL) = 'TFrontend_Output_TListView' then
    PageControl1.ActivePageIndex := 4
  else
    GotoUrl( trim(URL) );
end;

function DarkenColor(MyColor: TColor; Percent: Byte): TColor;
var
  r, g, b: Byte;
begin
  MyColor := ColorToRGB(MyColor);
  r := GetRValue(MyColor);
  g := GetGValue(MyColor);
  b := GetBValue(MyColor);

  // Abdunkeln um den angegebenen Prozentsatz
  r := r - MulDiv(r, Percent, 100);  // Percent% näher zu schwarz
  g := g - MulDiv(g, Percent, 100);
  b := b - MulDiv(b, Percent, 100);

  Result := RGB(r, g, b); // Ergebnisfarbe zurückgeben
end;

procedure TfrmMain.StringGridClick(Sender: TObject);
begin
  UpdateGridInfo();
end;

procedure TfrmMain.StringGridDrawCell(Sender: TObject; ACol, ARow: LongInt;
  Rect: TRect; State: TGridDrawState);
const
  ArrowUp = '▲';   // Unicode für Pfeil nach oben
  ArrowDown = '▼'; // Unicode für Pfeil nach unten
  PaddingRight = 16; // Platz für den Pfeil in der Kopfzeile
  ColorOdd = clInfoBk; // Farbe für ungerade Zeilen
  ColorEven = clWhite;  // Farbe für gerade Zeilen
  DarkenFactor = 5; // Faktor zum Abdunkeln der Farbe (10%)
var
  Arrow: String;
  Text: String;
  TextWidth, ArrowWidth, TextHeight: Integer;
  TextRect: TRect;
  BackgroundColor: TColor;
  TextColor: TColor;
  R, G, B: Byte; // RGB-Werte als Byte-Variablen
begin
  // Kopfzellen (Zeile 0) individuell behandeln
  if ARow = 0 then
  begin

    // Hintergrund der Zelle füllen
    StringGrid.Canvas.FillRect(Rect);

    // Text der Kopfzeile ermitteln
    Text := StringGrid.Cells[ACol, ARow];

    // Berechne die Breite des Pfeils
    if ACol = FSortColumn then
    begin
      if FSortDescending then
        Arrow := ArrowDown
      else
        Arrow := ArrowUp;
    end
    else
      Arrow := '';

    // Berechne Text- und Pfeilgrößen
    TextWidth := StringGrid.Canvas.TextWidth(Text);
    ArrowWidth := StringGrid.Canvas.TextWidth(Arrow);
    TextHeight := StringGrid.Canvas.TextHeight(Text);

    // Berechne das rechteckige Gebiet für den Text (Platz für Pfeil reservieren)
    TextRect := Rect;
    TextRect.Right := TextRect.Right - ArrowWidth - 4; // Platz für Pfeil

    // Zeichne den Text der Kopfzeile (leicht nach rechts eingerückt)
//    DrawText(StringGrid.Canvas.Handle, PChar(Text), -1, TextRect, DT_SINGLELINE or DT_VCENTER or DT_LEFT);

    // Zeichne den Pfeil, falls vorhanden
    if Arrow <> '' then
      StringGrid.Canvas.TextOut(Rect.Right - ArrowWidth - 4, Rect.Top + (Rect.Height - TextHeight) div 2, Arrow);
  end
  else begin
    // Alternierende Farben für die Zeilen
    if ARow mod 2 = 0 then
      BackgroundColor := ColorEven  // Gerade Zeilen
    else
      BackgroundColor := ColorOdd;   // Ungerade Zeilen


    // Wenn die aktuelle Spalte die sortierte Spalte ist, berechne die neue Farbe
    // Überprüfe, ob die aktuelle Spalte die sortierte Spalte ist
    if ACol = FSortColumn then
    begin
      // Verdunkle die Hintergrundfarbe um 10%
      try
        BackgroundColor := DarkenColor(BackgroundColor, DarkenFactor);
      except
        On E:Exception do begin
          ShowMessage('Error: ' + E.Message);
        end;
      end;
      StringGrid.Canvas.Font.Style := StringGrid.Canvas.Font.Style + [fsBold];
    end
    else
      StringGrid.Canvas.Font.Style := StringGrid.Canvas.Font.Style - [fsBold];


    // Hintergrund der Zelle füllen
    StringGrid.Canvas.Brush.Color := BackgroundColor;
    StringGrid.Canvas.FillRect(Rect);


      // Überprüfe, ob die Zelle geändert wurde
      if FCSVFile3.Changed[ARow, ACol] then
        StringGrid.Canvas.Font.Color := clRed; // Setze die Textfarbe auf Rot



    // Text der Zelle ermitteln
    Text := StringGrid.Cells[ACol, ARow];

    // Text in die Zelle zeichnen
    StringGrid.Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, Text);

  end;

end;

procedure TfrmMain.StringGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
Var
  i : integer;
  ARow,ACol : Integer;
  AValue : String;
begin
  if Key = VK_RETURN then begin


    if assigned(FCSVFile3) then begin
      ARow := StringGrid.Row;
      ACol := StringGrid.Col;
      AValue := StringGrid.Cells[ACol,ARow];
      FCSVFile3.Cell[ARow,ACol] := AValue;
      FFrontendStringGrid.AutoSizeColumnsAndRows();
    end;

    UpdateGridInfo();

  end;
end;

procedure TfrmMain.AdjustColumnWidths;
var
  Column: Integer;
  Node: PVirtualNode;
  MaxWidth: Integer;
  TextWidth: Integer;
  HeaderTextWidth: Integer;
  Text: String;
begin
  for Column := 0 to Treeview.Header.Columns.Count - 1 do
  begin
    MaxWidth := 0;

    // 1. Berechne die Breite des Header-Textes
    HeaderTextWidth := Treeview.Canvas.TextWidth(Treeview.Header.Columns[Column].Text);
    MaxWidth := HeaderTextWidth;  // Setze MaxWidth initial auf die Header-Breite

    // 2. Berechne die maximale Breite des Inhalts in der Spalte
    Node := Treeview.GetFirst;
    while Assigned(Node) do
    begin
      // Hole den Text der Zelle
      Text := Treeview.Text[Node, Column];

      // Berechne die Breite des Textes
      TextWidth := Treeview.Canvas.TextWidth(Text);

      // Finde die maximale Breite (Header-Text vs. Zellen-Text)
      if TextWidth > MaxWidth then
        MaxWidth := TextWidth;

      Node := Treeview.GetNext(Node);
    end;

    // 3. Setze die Spaltenbreite auf die maximale gefundene Breite + Puffer
    Treeview.Header.Columns[Column].Width := MaxWidth + 60; // +20 für etwas Puffer
  end;
end;





procedure TfrmMain.TreeviewAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
  AdjustColumnWidths;
end;

procedure TfrmMain.TreeviewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  // Überprüfe, ob die Zeile ungerade oder gerade ist
  if Odd(Node.Index) then
  begin
    // Ungerade Zeilen: Leicht dunklere Hintergrundfarbe
    TargetCanvas.Brush.Color := RGB(220, 220, 220); // Hellgrau für ungerade Zeilen
  end
  else
  begin
    // Gerade Zeilen: Standardfarbe oder hellere Farbe
    TargetCanvas.Brush.Color := clWhite; // Weiß für gerade Zeilen
  end;

  // Fülle den Zellbereich mit der gewählten Farbe
  TargetCanvas.FillRect(CellRect);
end;

procedure TfrmMain.TreeviewCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PNodeData;
  Value1, Value2: string;
  Num1, Num2: Double;
  Date1, Date2: TDateTime;
  IsNum1, IsNum2: Boolean;
  IsDate1, IsDate2: Boolean;
begin
  Data1 := Treeview.GetNodeData(Node1);
  Data2 := Treeview.GetNodeData(Node2);

  if Assigned(Data1) and Assigned(Data2) then
  begin
    // Vergleiche die Daten der jeweiligen Spalte (Column)
    if Column >= 0 then
    begin
      Value1 := Data1.Data[Column];
      Value2 := Data2.Data[Column];

      // Überprüfen, ob es sich um ein Datum handelt (angenommen, das Datum befindet sich in der letzten Spalte)
      if Column = 5 then // Index der Datumsspalte
      begin
        IsDate1 := TryStrToDate(Value1, Date1, FormatSettings);
        IsDate2 := TryStrToDate(Value2, Date2, FormatSettings);
        if IsDate1 and IsDate2 then
        begin
          // Datum vergleichen
          Result := CompareDate(Date1, Date2);
          Exit; // Beende die Methode hier, da das Datum bereits verglichen wurde
        end;
      end;

      // Versuche, beide Werte in Gleitkommazahlen umzuwandeln
      IsNum1 := TryStrToFloat(Value1, Num1);
      IsNum2 := TryStrToFloat(Value2, Num2);

      if IsNum1 and IsNum2 then
      begin
        // Numerischen Vergleich durchführen, wenn es sich um Zahlen handelt
        Result := CompareValue(Num1, Num2);
      end
      else
      begin
        // Textvergleich durchführen
        Result := CompareText(Value1, Value2);
      end;
    end;
  end;
end;




procedure TfrmMain.TreeviewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData: PNodeData; // Pointer auf die Knotendaten
begin
  // Überprüfen, ob der Knoten gültig ist
  if Assigned(Node) then
  begin
    NodeData := Sender.GetNodeData(Node); // Hole die Knotendaten
    if Assigned(NodeData) then
    begin
      // Setze den Text basierend auf der aktuellen Spalte
      // Hier wird auch die Länge des Data-Arrays überprüft
      if (Column < Length(NodeData.Data)) and (Column >= 0) then // Überprüfe, ob die Spalte im Bereich liegt
        CellText := NodeData.Data[Column] // Setze den Text der Zelle
      else
        CellText := ''; // Leere den Text, wenn die Spalte nicht existiert
    end;
  end;
end;


procedure TfrmMain.TreeviewHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);

begin
  if HitInfo.Column >= 0 then
  begin
    // Wechsel zwischen aufsteigender und absteigender Sortierung
    if Treeview.Header.SortColumn = HitInfo.Column then
    begin
      if Treeview.Header.SortDirection = sdAscending then
        Treeview.Header.SortDirection := sdDescending
      else
        Treeview.Header.SortDirection := sdAscending;
    end
    else
    begin
      // Neue Spalte ausgewählt, setze Sortierreihenfolge auf aufsteigend
      Treeview.Header.SortDirection := sdAscending;
    end;

    // Setze die Spalte, die sortiert werden soll
    Treeview.Header.SortColumn := HitInfo.Column;

    // Starte die Sortierung
    Treeview.SortTree(HitInfo.Column, Treeview.Header.SortDirection, False);
  end;

end;

procedure TfrmMain.TreeviewHeaderDraw(Sender: TVTHeader; HeaderCanvas: TCanvas;
  Column: TVirtualTreeColumn; R: TRect; Hover, Pressed: Boolean;
  DropMark: TVTDropMarkMode);
begin
  // Hintergrundfarbe auf Grau setzen
  HeaderCanvas.Brush.Color := clGray;
  HeaderCanvas.FillRect(R);

  // Optional: Textfarbe und Schriftstil für den Header-Text setzen
  HeaderCanvas.Font.Color := clWhite;
  HeaderCanvas.Font.Style := [fsBold];

  // Den Text des Headers zeichnen
  HeaderCanvas.TextOut(R.Left + 10, R.Top + 5, Column.Text);
end;

end.

