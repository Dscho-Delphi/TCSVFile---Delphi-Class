unit class_Frontend_Output_TStringlist;

{**
 * class_Frontend_Output_TStringlist.pas
 *
 * Eine Delphi-Klasse zur Verwaltung der Ausgabe von CSV-Daten in einer TStringList.
 * Sie bietet Funktionen zum Aktualisieren einzelner Zellen und zur Sortierung von Ausgaben.
 *
 * @author     Johannes Teitge
 * @version    1.0, BETA
 * @date       08.10.2024
 *
 * Copyright: Die Urheberrechtsbestimmungen dieser Datei richten sich nach den
 * Bestimmungen in der Datei CSVFile.pas.
 *
 *}

interface

uses
  CSVFile, System.Variants, System.Classes, Vcl.Dialogs;

type
  // TFrontend_Output_TStringlist ist eine Klasse, die die Ausgabe von CSV-Daten in eine TStringList verwaltet.
  TFrontend_Output_TStringlist = class(TFrontend_Output_Base)
  private
    FStrings: TStringList; // Liste, die die Ausgabe-Daten speichert
    FOutputColumn: Integer; // Spaltenindex für die Ausgabe
    FIgnoreHeader: Boolean; // Flag, ob die Header-Zeile ignoriert werden soll
  public
    // Konstruktor der Klasse, der eine CSV-Datei akzeptiert
    constructor Create(ACSVFile: TCSVFile); override;

    // Initialisierungsmethode für die Klasse
    procedure Initialize; override;

    // Aktualisierungsmethode für eine bestimmte Zelle
    procedure UpdateCell(ARow, ACol: Integer); override;

    // Methode zur Aktualisierung der gesamten Ausgabe
    procedure Update; override;

    // Eigenschaften zum Zugriff auf private Felder
    property Strings: TStringList read FStrings write FStrings; // Liste mit den Strings
    property OutputColumn: Integer read FOutputColumn write FOutputColumn; // Spalte für die Ausgabe
    property IgnoreHeader: Boolean read FIgnoreHeader write FIgnoreHeader; // Flag zum Ignorieren des Headers
  end;

implementation

{ TFrontend_Output_TStringlist }

// Konstruktor der TFrontend_Output_TStringlist-Klasse
constructor TFrontend_Output_TStringlist.Create(ACSVFile: TCSVFile);
begin
  inherited Create(ACSVFile); // Aufruf des Konstruktors der Basisklasse
  FStrings := TStringList.Create; // Initialisierung der TStringList
  OutputColumn := 0; // Standardwert für die Ausgabespalte
  IgnoreHeader := true; // Standardwert für Header-Option
  AutoOutput := false; // Automatische Ausgabe standardmäßig deaktiviert
  Sort := true; // Sortierung aktivieren
  SortDirection := sdCSVAscending; // Standard-Sortier-Richtung ist aufsteig
end;

// Initialisierungsmethode
procedure TFrontend_Output_TStringlist.Initialize;
begin
  // Hier wird die Initialisierung der Ausgabe-Logik implementiert
  // Beispiel: Leeren der Datenstruktur oder Vorbereitung auf neue Daten
  FStrings.Clear; // Leere die TStringList für neue Daten
end;

// Methode zur Aktualisierung einer bestimmten Zelle
procedure TFrontend_Output_TStringlist.UpdateCell(ARow, ACol: Integer);
Var
  AValue : String;
begin
  // Hier wird die Logik zum Aktualisieren einer bestimmten Zelle implementiert
  if (not Assigned(FCSVFile)) or (not Assigned(FStrings)) then exit; // Überprüfe, ob die CSV-Datei und die TStringList zugewiesen sind

  if ACol=self.FOutputColumn then begin
    if FCSVFile.UseHeader then
      FStrings[ARow-1] := AValue
    else
      FStrings[ARow] := AValue
  end;

end;

// Methode zur Aktualisierung der gesamten Ausgabe
procedure TFrontend_Output_TStringlist.Update;
var
  i, j: Integer; // Schleifen-Indizes
  Data: String; // Variable für den Zellinhalt
begin
  // Hier wird die Logik zur Aktualisierung der gesamten Ausgabe implementiert
  if (not Assigned(FCSVFile)) or (not Assigned(FStrings)) then exit; // Überprüfe, ob die CSV-Datei und die TStringList zugewiesen sind

  // Sortiere die Daten, wenn das Flag gesetzt ist
  if Sort then
  begin
    // Sortiere die CSV-Datei nach der definierten Spalte und Sortierreihenfolge
    CSVFile.Sort(FOutputColumn, FSortDirection = sdCSVDescending);
  end;

  FStrings.Clear; // Leere die TStringList vor dem Befüllen

  // Iteriere über alle Zeilen der CSV-Datei
  for I := 0 to CSVFile.RowCount - 1 do
  begin
    Data := CSVFile.Cell[I, FOutputColumn]; // Hole den Zellinhalt für die aktuelle Zeile und die definierte Spalte
    // Überprüfe, ob die Header-Zeile ignoriert werden soll
    if (I <> 0) or not IgnoreHeader then
    begin
      FStrings.Add(Data); // Füge den Zellinhalt zur TStringList hinzu
    end;
  end;
end;

end.

