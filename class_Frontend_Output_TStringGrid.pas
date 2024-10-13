unit class_Frontend_Output_TStringGrid;

{**
 * class_Frontend_Output_TStringGrid.pas
 *
 * Eine Delphi-Klasse zur Verwaltung der Ausgabe von CSV-Daten in einem TStringGrid.
 * Sie bietet Funktionen zum Aktualisieren von Zellen und zur Sortierung von Ausgaben.
 *
 * @author     Johannes Teitge
 * @version    1.0, BETA
 * @date       08.10.2024
 *
 * Copyright: Die Urheberrechtsbestimmungen dieser Datei richten sich nach den
 * Bestimmungen in der Datei CSVFile.pas.
 *}

interface

uses
  CSVFile, Vcl.Grids, System.Variants, System.Classes, System.SysUtils, VCL.Dialogs;

type

  {
    TFrontend_Output_TStringGrid ist eine abgeleitete Klasse von TFrontend_Output_Base, die speziell dafür entwickelt wurde,
    CSV-Daten in einem TStringGrid anzuzeigen und zu bearbeiten. Diese Klasse stellt Methoden zur Verfügung, um das
    TStringGrid zu initialisieren, zu aktualisieren, Zellen zu bearbeiten und den Fokus auf eine bestimmte Zelle zu setzen.

    Sie ermöglicht eine flexible und benutzerfreundliche Integration von CSV-Daten in eine visuelle Oberfläche,
    wobei spezifische Methoden zur Größenanpassung von Spalten und Zeilen sowie zum Ignorieren des Headers angeboten werden.

    Abgeleitete Klassen, die diese Basisklasse verwenden, müssen die abstrakten Methoden `Initialize`, `Clear`,
    `Update`, `UpdateCell` und `SelectFocus` implementieren, um eine spezifische Ausgabeform zu realisieren.

    @author Johannes Teitge
    @version 1.0
    @created 12.10.2024
  }
  TFrontend_Output_TStringGrid = class(TFrontend_Output_Base)
  private
    FAutoSize: Boolean;  //< Flag zum automatischen Anpassen der Größe
    FAutosizeSpan: Integer;  //< Span für die automatische Größenanpassung
    FIgnoreHeader: Boolean;  //< Flag zum Ignorieren des Headers
    FStringGrid: TStringGrid;  //< Referenz auf das TStringGrid
  public
    constructor Create(ACSVFile: TCSVFile; AStringGrid: TStringGrid); reintroduce;  //< Konstruktor zur Initialisierung der Klasse mit einer CSV-Datei und einem StringGrid
    procedure Clear; override;  //< Methode zum Zurücksetzen der Ausgabe
    procedure Initialize; override;  //< Methode zur Initialisierung der Ausgabe
    procedure Update; override;  //< Methode zur Aktualisierung der gesamten Ausgabe
    procedure UpdateCell(ARow, ACol: Integer); override;  //< Aktualisierungsmethode für eine bestimmte Zelle
    procedure SelectFocus(ARow, ACol: Integer); override;  //< Methode zum Setzen des Fokus auf eine bestimmte Zelle
    procedure AutoSizeColumnsAndRows(Span: Integer = -1);  //< Methode zum automatischen Anpassen der Spalten- und Zeilengröße

    property AutoSize: Boolean read FAutoSize write FAutoSize;  //< Flag zum automatischen Anpassen der Größe
    property AutosizeSpan: Integer read FAutosizeSpan write FAutosizeSpan;  //< Span für die automatische Größenanpassung
    property IgnoreHeader: Boolean read FIgnoreHeader write FIgnoreHeader;  //< Flag zum Ignorieren des Headers
    property StringGrid: TStringGrid read FStringGrid;  //< Referenz auf das TStringGrid
  end;

implementation

{ TFrontend_Output_TStringGrid }

// Konstruktor der TFrontend_Output_TStringGrid-Klasse
constructor TFrontend_Output_TStringGrid.Create(ACSVFile: TCSVFile; AStringGrid: TStringGrid);
begin
  inherited Create(ACSVFile); // Aufruf des Konstruktors der Basisklasse
  FStringGrid := AStringGrid; // Speichere die Referenz auf das TStringGrid
  FIgnoreHeader := true;      // Standardwert für Header-Option
  FAutosizeSpan := 6;
  FAutoSize := True;
end;

// Clearmethode
procedure TFrontend_Output_TStringGrid.Clear;
Var
  i,j : integer;
begin
  if assigned( FStringGrid ) then begin
    FStringGrid.BeginUpdate;

    try

      for I := 0 to FStringgrid.RowCount-1 do begin
        for j := 0 to FStringgrid.ColCount-1 do begin
          FStringgrid.Cells[j,i] := '';
        end;
      end;

      FStringGrid.RowCount := 1;   // Setzt die Anzahl der Zeilen auf 1
      FStringGrid.ColCount := 1;   // Setzt die Anzahl der Spalten auf 1
      FStringGrid.Rows[0].Clear;   // Leert die einzige verbleibende Zeile

    except

    end;

    FStringGrid.EndUpdate;

  end;
end;


// Initialisierungsmethode
procedure TFrontend_Output_TStringGrid.Initialize;
begin
  if self.CSVFile.Size = 0 then exit;  

  FStringGrid.RowCount := 1;  // Leere das StringGrid vor der neuen Befüllung, setze mindestens eine Zeile
  FStringGrid.ColCount := FCSVFile.ColCount[0]; // Setze die Anzahl der Spalten

  // Füge Zeilen hinzu, falls bereits Daten vorhanden sind
  Update;  // Rufe die Methode zur Befüllung des StringGrids auf
end;

// Methode zur Aktualisierung einer bestimmten Zelle
procedure TFrontend_Output_TStringGrid.UpdateCell(ARow, ACol: Integer);
begin
  // Überprüfe, ob das StringGrid und die CSV-Datei gültig sind
  if Assigned(FStringGrid) and Assigned(FCSVFile) then begin
    // Stelle sicher, dass die Zeilen- und Spaltenanzahl ausreichend ist
    if (ARow >= 0) and (ARow < FStringGrid.RowCount) and
       (ACol >= 0) and (ACol < FStringGrid.ColCount) then begin
      // Aktualisiere die Zelle mit dem neuen Wert
      FStringGrid.Cells[ACol, ARow] := self.FCSVFile.Cell[ARow,ACol];
    end;
  end;
end;

// Methode eine bestimmten Zelle auszuwählen und den Fokus zu setzen
procedure TFrontend_Output_TStringGrid.SelectFocus(ARow, ACol: Integer);
begin
  if Assigned(FStringGrid) and Assigned(FCSVFile) then begin
    if (ARow >= 0) and (ARow < FCSVFile.Data.Count) and (ACol >= 0) and (ACol < FCSVFile.Data[ARow].Count) then begin

      // Versuche, die Zelle ARow, ACol zu selektieren
      try
        // Setze die aktive Zelle auf die gewünschte Zeile und Spalte
        FStringGrid.Row := ARow;
        FStringGrid.Col := ACol;

        // Versuche, den Fokus auf das TStringGrid zu setzen
        FStringGrid.SetFocus;
      except
        on E: Exception do
        begin
          // Wenn der Fokus nicht gesetzt werden kann, eine Fehlermeldung ausgeben oder ignorieren
          ShowMessage('Fokus konnte nicht gesetzt werden: ' + E.Message);
        end;
      end;
    end;
  end;
end;


// Methode zur Aktualisierung der gesamten Ausgabe
procedure TFrontend_Output_TStringGrid.Update;
var
  I, J: Integer;
begin
  // Überprüfe, ob die CSV-Datei und das StringGrid gültig sind
  if (not Assigned(FCSVFile)) or (not Assigned(FStringGrid)) then Exit;

  if self.CSVFile.Size = 0 then begin
    Clear;
    AutoSizeColumnsAndRows();
    exit;
  end;


  // Setze die Anzahl der Zeilen und Spalten (inkl. Header falls useHeader gesetzt ist)
  if FCSVFile.useHeader then
    FStringGrid.RowCount := FCSVFile.RowCount
  else
    FStringGrid.RowCount := FCSVFile.RowCount - 1;

  FStringGrid.ColCount := FCSVFile.ColCount[0];  // Setze die Anzahl der Spalten

  // Falls der Header verwendet wird, setze die erste Zeile als Header
  if FCSVFile.useHeader then
  begin
    for J := 0 to FCSVFile.ColCount[0] - 1 do
    begin
      FStringGrid.Cells[J, 0] := FCSVFile.Cell[0, J];  // Setze den Header in der ersten Zeile
    end;
  end;

  // Iteriere über alle Datenzeilen und Spalten der CSV-Datei (Daten füllen)

  FStringGrid.BeginUpdate;

  try
    for I := 1 to FCSVFile.RowCount - 1 do
    begin
      for J := 0 to FCSVFile.ColCount[0] - 1 do
      begin
        // Fülle die Daten ab der zweiten Zeile (erste Zeile ist Header, falls useHeader gesetzt ist)
        if FCSVFile.useHeader then
          FStringGrid.Cells[J, I] := FCSVFile.Cell[I, J]
        else
          FStringGrid.Cells[J, I-1] := FCSVFile.Cell[I, J];
      end;
    end;

    if FCSVFile.useHeader then
      FStringGrid.FixedRows := 1;

    if self.FAutoSize then
      self.AutoSizeColumnsAndRows();

  except

  end;

  FStringGrid.EndUpdate;
  FStringGrid.Invalidate;  // Aktualisiere das StringGrid, um die Änderungen anzuzeigen
end;


procedure TFrontend_Output_TStringGrid.AutoSizeColumnsAndRows(Span: Integer = -1);
var
  Col, Row: Integer;
  MaxWidth, MaxHeight, CellWidth, CellHeight: Integer;
  Grid : TStringgrid;
begin
  if not assigned( FStringGrid ) then exit;
  Grid := FStringGrid;

  // Setze die Schriftart des Canvas auf die aktuelle Font des Grids
  Grid.Canvas.Font := Grid.Font;

  if Span = -1 then
    Span := FAutosizeSpan;

  // Zuerst die Breite der Spalten anpassen
  for Col := 0 to Grid.ColCount - 1 do
  begin
    MaxWidth := 0;
    for Row := 0 to Grid.RowCount - 1 do
    begin
      // Berechne die Breite des Zellinhalts
      CellWidth := Grid.Canvas.TextWidth(Grid.Cells[Col, Row]);
      // Füge den Puffer hinzu, der durch Span bestimmt wird
      CellWidth := CellWidth + (Span * 5); // Beispiel: 5 Pixel pro Span
      // Aktualisiere MaxWidth, wenn die aktuelle Zellenbreite größer ist
      if CellWidth > MaxWidth then
        MaxWidth := CellWidth;
    end;
    // Setze die Spaltenbreite auf die maximale berechnete Breite
    Grid.ColWidths[Col] := MaxWidth;
  end;

  // Nun die Höhe der Zeilen anpassen
  for Row := 0 to Grid.RowCount - 1 do
  begin
    MaxHeight := 0;
    for Col := 0 to Grid.ColCount - 1 do
    begin
      // Berechne die Höhe des Zellinhalts
      CellHeight := Grid.Canvas.TextHeight(Grid.Cells[Col, Row]);
      // Füge den Puffer hinzu, der durch Span bestimmt wird
      CellHeight := CellHeight + (Span * 2); // Beispiel: 2 Pixel pro Span
      // Aktualisiere MaxHeight, wenn die aktuelle Zellenhöhe größer ist
      if CellHeight > MaxHeight then
        MaxHeight := CellHeight;
    end;
    // Setze die Zeilenhöhe auf die maximale berechnete Höhe
    Grid.RowHeights[Row] := MaxHeight;
  end;
end;


end.

