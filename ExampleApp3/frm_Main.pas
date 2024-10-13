unit frm_Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.Menus, Vcl.ExtCtrls, System.Math,
  Vcl.WinXCtrls, CSVFile, Vcl.StdCtrls,

  class_Frontend_Output_TStringGrid;

type
  TfrmMain = class(TForm)
    RelativePanel1: TRelativePanel;
    RelativePanel2: TRelativePanel;
    CSVGrid: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CSVGridDrawCell(Sender: TObject; ACol, ARow: LongInt; Rect: TRect;
      State: TGridDrawState);
  private
    { Private-Deklarationen }
    FSortColumn: Integer; // Speichert die aktuell sortierte Spalte
    FSortDescending: Boolean; // Speichert die Sortierrichtung (aufsteigend/absteigend)
    CSVFile: TCSVFile;
    Output: TFrontend_Output_TStringGrid;

    procedure GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SortColumn(Column: Integer);
  public
    { Public-Deklarationen }
    procedure LoadDemoData;
    procedure CalcData();
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
Var
  DTyp : TDataType;
begin
  CSVFile := TCSVFile.Create(TEncoding.UTF8, ';');
  Output := TFrontend_Output_TStringGrid.Create(CSVFile,CSVGrid);
  Output.AutosizeSpan := 6;
  LoadDemoData;
  CSVFile.Output := Output;


  // Sortiere nach der ersten Spalte (Spalte 0) aufsteigend
  FSortColumn := 0;  // Setze die Sortierspalte auf 0
  FSortDescending := False; // Standardmäßig aufsteigend
//  CSVFile.Sort(FSortColumn, FSortDescending); // Sortiere die Daten

  // Event-Handler zuweisen
  CSVGrid.OnMouseDown := GridMouseDown;

  DTyp := CSVFile.DetectDataType( CSVFile.Cell[1,FSortColumn] );
  CSVFile.SortExt(FSortColumn, FSortDescending, True, DTyp);

  CSVFile.Output.Update;
end;

procedure TfrmMain.LoadDemoData;
const
  DemoCSVData =
    'PLZ;Stadt;Bundesland;Bevölkerung;CO2-Ausstoß (t)' + sLineBreak +
    '10115;Berlin;Berlin;3769495;22948000' + sLineBreak +
    '80331;München;Bayern;1578600;10882000' + sLineBreak +
    '20095;Hamburg;Hamburg;1847253;10000000' + sLineBreak +
    '04109;Leipzig;Sachsen;605258;3500000' + sLineBreak +
    '01067;Dresden;Sachsen;550000;2500000' + sLineBreak +
    '90402;Nürnberg;Bayern;518000;3000000' + sLineBreak +
    '70173;Stuttgart;Baden-Württemberg;635000;4000000' + sLineBreak +
    '50667;Köln;Nordrhein-Westfalen;1084000;6000000' + sLineBreak +
    '60549;Frankfurt am Main;Hessen;763000;4500000' + sLineBreak +
    '28195;Bremen;Bremen;569000;2000000' + sLineBreak +
    '04103;Chemnitz;Sachsen;246000;1200000' + sLineBreak +
    '01069;Zwickau;Sachsen;90000;500000' + sLineBreak +
    '30159;Hannover;Niedersachsen;535000;3000000' + sLineBreak +
    '99084;Erfurt;Thüringen;213000;1100000' + sLineBreak +
    '45000;Gelsenkirchen;Nordrhein-Westfalen;260000;1400000' + sLineBreak +
    '48143;Münster;Nordrhein-Westfalen;313000;1800000' + sLineBreak +
    '09111;Chemnitz;Sachsen;246000;1200000' + sLineBreak +
    '04229;Halle (Saale);Sachsen-Anhalt;238000;1300000' + sLineBreak +
    '39104;Magdeburg;Sachsen-Anhalt;239000;1200000' + sLineBreak +
    '06108;Halle;Sachsen-Anhalt;238000;1300000' + sLineBreak +
    '52062;Aachen;Nordrhein-Westfalen;250000;1500000';
begin
  // Festlegen, wie groß die Abständer in den Zellen sein sollen
  Output.AutosizeSpan := 6;

  // Einfügen der Demo-CSV-Daten in das CSVFile-Objekt
  CSVFile.InsertFromText(DemoCSVData);

  // Aktualisiert das StringGrid, damit die eingefügten Daten angezeigt werden
  Output.Update;

  // Setzt die Anzahl der fixierten (festen) Zeilen auf 1, um die Kopfzeile zu fixieren
  CSVGrid.FixedRows := 1;

  // Ruft die Methode zur Berechnung von Minimal- und Maximalwerten auf und aktualisiert die Labels
  CalcData();
end;

procedure TfrmMain.GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  // Koordinaten in Spalte und Zeile umrechnen
  CSVGrid.MouseToCell(X, Y, Col, Row);

  // Prüfen, ob die Kopfzeile angeklickt wurde (Zeile 0)
  if (Row = 0) and (Col >= 0) then
  begin
    SortColumn(Col); // Sortiere nach der angeklickten Spalte
  end;

  // Neu zeichnen, damit der Pfeil korrekt dargestellt wird
  CSVGrid.Invalidate;
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
  DTyp := CSVFile.DetectDataType( CSVFile.Cell[1,Column] );
  CSVFile.SortExt(Column, FSortDescending, True, DTyp);
  Output.Update; // Aktualisiere das Grid mit den sortierten Daten
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

procedure TfrmMain.CalcData();
var
  D: Double;
  CSVPos: TCSVPos;
  City, Text: String;
begin
  // Berechnung des maximalen Wertes in der 4. Spalte (CO₂-Ausstoß)
  D := CSVFile.Calculate(False, 4, [], CSVPos, ctMax);

  // Ermittlung der Stadt, die den maximalen CO₂-Ausstoß hat
  City := CSVFile[CSVPos.Y+1, 1]; // Annahme: Spalte 2 ist die Stadtspalte

  // Formatierung des Anzeigetextes
  Text := 'Maximaler CO²-Ausstoß hat %s mit %d Tonnen pro Jahr.'; // 'ddd' kann durch eine geeignete Beschreibung ersetzt werden

  // Anzeige im Label
  Label1.Caption := Format(Text, [City, Round(D)]);


  // Berechnung des maximalen Wertes in der 4. Spalte (CO₂-Ausstoß)
  D := CSVFile.Calculate(False, 4, [], CSVPos, ctMin);

  // Ermittlung der Stadt, die den maximalen CO₂-Ausstoß hat
  City := CSVFile[CSVPos.Y+1, 1]; // Annahme: Spalte 2 ist die Stadtspalte

  // Formatierung des Anzeigetextes
  Text := 'Minimaler CO²-Ausstoß hat %s mit %d Tonnen pro Jahr.'; // 'ddd' kann durch eine geeignete Beschreibung ersetzt werden

  // Anzeige im Label
  Label2.Caption := Format(Text, [City, Round(D)]);
end;

procedure TfrmMain.CSVGridDrawCell(Sender: TObject; ACol, ARow: LongInt;
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
    CSVGrid.Canvas.FillRect(Rect);

    // Text der Kopfzeile ermitteln
    Text := CSVGrid.Cells[ACol, ARow];

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
    TextWidth := CSVGrid.Canvas.TextWidth(Text);
    ArrowWidth := CSVGrid.Canvas.TextWidth(Arrow);
    TextHeight := CSVGrid.Canvas.TextHeight(Text);

    // Berechne das rechteckige Gebiet für den Text (Platz für Pfeil reservieren)
    TextRect := Rect;
    TextRect.Right := TextRect.Right - ArrowWidth - 4; // Platz für Pfeil

    // Zeichne den Text der Kopfzeile (leicht nach rechts eingerückt)
//    DrawText(CSVGrid.Canvas.Handle, PChar(Text), -1, TextRect, DT_SINGLELINE or DT_VCENTER or DT_LEFT);

    // Zeichne den Pfeil, falls vorhanden
    if Arrow <> '' then
      CSVGrid.Canvas.TextOut(Rect.Right - ArrowWidth - 4, Rect.Top + (Rect.Height - TextHeight) div 2, Arrow);
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
      CSVGrid.Canvas.Font.Style := CSVGrid.Canvas.Font.Style + [fsBold];
    end
    else
      CSVGrid.Canvas.Font.Style := CSVGrid.Canvas.Font.Style - [fsBold];


    // Hintergrund der Zelle füllen
    CSVGrid.Canvas.Brush.Color := BackgroundColor;
    CSVGrid.Canvas.FillRect(Rect);


    // Text der Zelle ermitteln
    Text := CSVGrid.Cells[ACol, ARow];

    // Text in die Zelle zeichnen
    CSVGrid.Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, Text);

  end;
end;

end.
