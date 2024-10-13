unit class_Frontend_Output_SQLite;

interface

uses
  CSVFile, System.SysUtils, System.Classes, SQLite3, VCL.Dialogs;

type
  {**
   * TFrontend_Output_SQLite ist eine abgeleitete Klasse von TFrontend_Output_Base,
   * die speziell dafür entwickelt wurde, CSV-Daten in eine SQLite-Datenbank zu speichern und abzurufen.
   *
   * Sie ermöglicht das Speichern der CSV-Daten in einer SQLite-Datenbank und stellt Methoden zur Verfügung,
   * um Daten aus der Datenbank abzurufen.
   *
   * @author  Johannes Teitge
   * @version 1.0, BETA
   * @date    12.10.2024
   }
  TFrontend_Output_SQLite = class(TFrontend_Output_Base)
  private
    FDatabase: TSQLite3Database;  //< Referenz auf die SQLite-Datenbank
    FTableName: string;  //< Der Name der Tabelle, in die die CSV-Daten gespeichert werden
    procedure CreateTable;  //< Methode zur Erstellung der Tabelle in der SQLite-Datenbank
    procedure InsertData;  //< Methode zum Speichern der CSV-Daten in der Datenbank
    procedure UpdateData(ARow, ACol: Integer);  //< Methode zur Aktualisierung einer bestimmten Zelle
  public
    constructor Create(ACSVFile: TCSVFile; ADatabaseFile: string; ATableName: string); reintroduce;  //< Konstruktor zur Initialisierung der Klasse
    destructor Destroy; override;  //< Destruktor zum Schließen der Datenbank

    procedure Clear; override;  //< Methode zum Zurücksetzen der Ausgabe
    procedure Initialize; override;  //< Methode zur Initialisierung der Ausgabe
    procedure SelectFocus(ARow, ACol: Integer); override;  //< Methode zum Setzen des Fokus auf eine bestimmte Zelle
    procedure Update; override;  //< Methode zur Aktualisierung der gesamten Ausgabe
    procedure UpdateCell(ARow, ACol: Integer); override;  //< Methode zur Aktualisierung einer bestimmten Zelle
  end;

implementation

uses
  SQLite3Wrap;

{ TFrontend_Output_SQLite }

constructor TFrontend_Output_SQLite.Create(ACSVFile: TCSVFile; ADatabaseFile: string; ATableName: string);
begin
  inherited Create(ACSVFile);
  FDatabase := TSQLite3Database.Create(ADatabaseFile);  // SQLite-Datenbank öffnen
  FTableName := ATableName;
  CreateTable;  // Tabelle erstellen (falls nicht vorhanden)
end;

destructor TFrontend_Output_SQLite.Destroy;
begin
  FDatabase.Free;  // SQLite-Datenbank freigeben
  inherited Destroy;
end;

procedure TFrontend_Output_SQLite.Clear;
begin
  // Lösche die gesamte Tabelle in der Datenbank
  FDatabase.Execute('DELETE FROM ' + FTableName);
end;

procedure TFrontend_Output_SQLite.Initialize;
begin
  if FCSVFile.Size > 0 then
  begin
    InsertData;  // CSV-Daten in die SQLite-Datenbank einfügen
  end;
end;

procedure TFrontend_Output_SQLite.CreateTable;
var
  I: Integer;
  ColumnDefs: string;
begin
  ColumnDefs := '';
  for I := 0 to FCSVFile.ColCount[0] - 1 do
  begin
    if I > 0 then
      ColumnDefs := ColumnDefs + ', ';
    ColumnDefs := ColumnDefs + 'col' + IntToStr(I) + ' TEXT';  // Erstelle Spalten für jede CSV-Spalte
  end;

  // SQL-Statement zur Erstellung der Tabelle
  FDatabase.Execute('CREATE TABLE IF NOT EXISTS ' + FTableName + ' (' + ColumnDefs + ')');
end;

procedure TFrontend_Output_SQLite.InsertData;
var
  I, J: Integer;
  SQL: string;
  Values: string;
begin
  for I := 0 to FCSVFile.RowCount - 1 do
  begin
    Values := '';
    for J := 0 to FCSVFile.ColCount[0] - 1 do
    begin
      if J > 0 then
        Values := Values + ', ';
      Values := Values + '"' + FCSVFile.Cell[I, J] + '"';  // Werte in Anführungszeichen setzen
    end;

    SQL := 'INSERT INTO ' + FTableName + ' (' + 'col' + IntToStr(J) + ') VALUES (' + Values + ')';
    FDatabase.Execute(SQL);
  end;
end;

procedure TFrontend_Output_SQLite.UpdateData(ARow, ACol: Integer);
var
  SQL: string;
  NewValue: string;
begin
  if (ARow >= 0) and (ACol >= 0) and (ARow < FCSVFile.RowCount) and (ACol < FCSVFile.ColCount[0]) then
  begin
    NewValue := FCSVFile.Cell[ARow, ACol];
    SQL := 'UPDATE ' + FTableName + ' SET col' + IntToStr(ACol) + ' = "' + NewValue + '" WHERE rowid = ' + IntToStr(ARow + 1);
    FDatabase.Execute(SQL);  // Update-Abfrage ausführen
  end;
end;

procedure TFrontend_Output_SQLite.Update;
begin
  // Hier können Sie eine vollständige Ausgabe aus der Datenbank abrufen und verarbeiten
  // Zum Beispiel eine Abfrage der gesamten Daten und das Setzen der Daten in ein grid o.ä.
end;

procedure TFrontend_Output_SQLite.UpdateCell(ARow, ACol: Integer);
begin
  UpdateData(ARow, ACol);  // Aktualisiert den Wert einer bestimmten Zelle in der Datenbank
end;

procedure TFrontend_Output_SQLite.SelectFocus(ARow, ACol: Integer);
begin
  // Hier könnte man den Fokus in der UI setzen, aber da wir mit der DB arbeiten, ist das nicht nötig.
  // Eventuell müsste man die Daten für den aktuellen Fokus aus der DB abrufen.
end;

end.

