unit class_Frontend_Output_PDF;

{**
 * class_Frontend_Output_PDF.pas
 *
 * Eine Delphi-Klasse zur Ausgabe von CSV-Daten in eine PDF-Datei.
 * Diese Klasse erstellt eine PDF-Tabelle, die CSV-Daten anzeigt.
 *
 * @author     Johannes Teitge
 * @version    1.0
 * @date       12.10.2024
 *
 * Copyright: Die Urheberrechtsbestimmungen dieser Datei richten sich nach den
 * Bestimmungen in der Datei CSVFile.pas.
 *}

interface

uses
  CSVFile, fpdf, System.Classes, System.SysUtils;

type
  TFrontend_Output_PDF = class(TFrontend_Output_Base)
  private
    FPDF: TPDFDocument;  //< Referenz auf das PDF-Dokument
  public
    constructor Create(ACSVFile: TCSVFile); reintroduce;  //< Konstruktor, um die CSV-Datei zu initialisieren
    destructor Destroy; override;  //< Destruktor zur Bereinigung

    procedure Initialize; override;  //< Initialisierung der PDF-Datei
    procedure Clear; override;  //< Löschen der PDF-Inhalte
    procedure Update; override;  //< Erstellen der PDF mit den CSV-Daten
    procedure UpdateCell(ARow, ACol: Integer); override;  //< Optional: Update für einzelne Zellen
    procedure SelectFocus(ARow, ACol: Integer); override;  //< Optional: Fokus auf bestimmte Zelle setzen
  end;

implementation

{ TFrontend_Output_PDF }

constructor TFrontend_Output_PDF.Create(ACSVFile: TCSVFile);
begin
  inherited Create(ACSVFile);
  FPDF := TPDFDocument.Create;
end;

destructor TFrontend_Output_PDF.Destroy;
begin
  FPDF.Free;
  inherited Destroy;
end;

procedure TFrontend_Output_PDF.Initialize;
begin
  FPDF.AddPage;  // Füge eine neue Seite zum PDF-Dokument hinzu
  FPDF.SetFont('Arial', 'B', 12);  // Setze die Schriftart für den Header
end;

procedure TFrontend_Output_PDF.Clear;
begin
  FPDF := TPDFDocument.Create;  // Erstelle ein neues PDF-Dokument, um die Daten zu löschen
end;

procedure TFrontend_Output_PDF.Update;
var
  I, J: Integer;
  CellWidth, CellHeight: Integer;
begin
  if FCSVFile.Size = 0 then Exit;  // Wenn die CSV-Datei leer ist, beenden

  FPDF.AddPage;  // Neue Seite hinzufügen
  FPDF.SetFont('Arial', '', 10);  // Schriftart für den Dateninhalt

  // Berechne die Zellengröße (Breite und Höhe) für die Tabelle
  CellWidth := 40;
  CellHeight := 10;

  // Header-Zeile
  if FCSVFile.useHeader then
  begin
    FPDF.SetFont('Arial', 'B', 12);
    for J := 0 to FCSVFile.ColCount[0] - 1 do
    begin
      FPDF.Cell(CellWidth, CellHeight, FCSVFile.Cell[0, J]);  // Füge Header-Daten hinzu
      FPDF.Ln;  // Neue Zeile
    end;
  end;

  // Daten-Zeilen
  FPDF.SetFont('Arial', '', 10);
  for I := 1 to FCSVFile.RowCount - 1 do
  begin
    for J := 0 to FCSVFile.ColCount[0] - 1 do
    begin
      FPDF.Cell(CellWidth, CellHeight, FCSVFile.Cell[I, J]);  // Füge Zellen-Daten hinzu
    end;
    FPDF.Ln;  // Neue Zeile
  end;

  // Speichern des PDF-Dokuments
  FPDF.Output('CSV_Output.pdf', 'F');  // F als Parameter bedeutet, das Dokument wird gespeichert.
end;

procedure TFrontend_Output_PDF.UpdateCell(ARow, ACol: Integer);
begin
  // Diese Methode könnte verwendet werden, um eine bestimmte Zelle im PDF-Dokument zu aktualisieren,
  // aber da das PDF-Dokument in `Update` einmal erstellt wird, ist das in diesem Fall nicht unbedingt nötig.
end;

procedure TFrontend_Output_PDF.SelectFocus(ARow, ACol: Integer);
begin
  // Dies könnte verwendet werden, um den Fokus auf eine bestimmte Zelle zu setzen (visuell durch Formatierung)
  // Allerdings wird diese Funktion hier nicht weiter genutzt, da PDF statisch ist.
end;

end.

