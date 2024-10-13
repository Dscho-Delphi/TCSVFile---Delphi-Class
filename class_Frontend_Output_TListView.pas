unit class_Frontend_Output_TListView;

{**
 * class_Frontend_Output_TListView.pas
 *
 * Eine Delphi-Klasse zur Verwaltung der Ausgabe von CSV-Daten in einem TListView.
 * Sie bietet Funktionen zum Aktualisieren der Items und zur Sortierung von Ausgaben.
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
  CSVFile, Vcl.ComCtrls, System.Variants, System.Classes, System.SysUtils, system.Messaging,
  Winapi.Windows, Winapi.CommCtrl;

type
  // TFrontend_Output_TListView ist eine Klasse, die die Ausgabe von CSV-Daten in einem TListView verwaltet.
  TFrontend_Output_TListView = class(TFrontend_Output_Base)
  private
    FListView: TListView;  // Referenz auf das TListView
    FIgnoreHeader: Boolean;  // Flag, ob die Header-Zeile ignoriert werden soll
  public
    // Konstruktor der Klasse, der eine CSV-Datei und ein ListView akzeptiert
    constructor Create(ACSVFile: TCSVFile; AListView: TListView); reintroduce;

    // Initialisierungsmethode für die Klasse
    procedure Initialize; override;

    // Aktualisierungsmethode für ein bestimmtes Item
    procedure UpdateCell(ARow, ACol: Integer); override;

    // Methode zur Aktualisierung der gesamten Ausgabe
    procedure Update; override;

    // Eigenschaften zum Zugriff auf private Felder
    property ListView: TListView read FListView;  // Referenz auf das TListView
    property IgnoreHeader: Boolean read FIgnoreHeader write FIgnoreHeader;  // Flag zum Ignorieren des Headers
  end;

implementation

{ TFrontend_Output_TListView }



procedure AutoSizeListViewColumns(AListView: TListView);
var
  i: Integer;
begin
  for i := 0 to AListView.Columns.Count - 1 do
  begin
    // LVM_SETCOLUMNWIDTH verwendet für die automatische Größenanpassung
    SendMessage(AListView.Handle, LVM_SETCOLUMNWIDTH, i, LVSCW_AUTOSIZE);

    // Optional: Manchmal möchten wir sicherstellen, dass die Spalte mindestens die Breite der Überschrift hat
    if AListView.Columns[i].Width < AListView.Canvas.TextWidth(AListView.Columns[i].Caption) + 10 then
    begin
      SendMessage(AListView.Handle, LVM_SETCOLUMNWIDTH, i, LVSCW_AUTOSIZE_USEHEADER);
    end;
  end;
end;



// Konstruktor der TFrontend_Output_TListView-Klasse
constructor TFrontend_Output_TListView.Create(ACSVFile: TCSVFile; AListView: TListView);
begin
  inherited Create(ACSVFile);  // Aufruf des Konstruktors der Basisklasse
  FListView := AListView;      // Speichere die Referenz auf das ListView
  FIgnoreHeader := true;       // Standardwert für Header-Option
end;

// Initialisierungsmethode
procedure TFrontend_Output_TListView.Initialize;
begin
  FListView.Clear;  // Leere das ListView vor der neuen Befüllung

  // Füge die Header-Spalten hinzu, wenn sie nicht ignoriert werden sollen
  if (FCSVFile.RowCount > 0) and ( FCSVFile.UseHeader ) then
  begin
    for var Col := 0 to FCSVFile.ColCount[0] - 1 do
    begin
      FListView.Columns.Add.Caption := FCSVFile.Cell[0, Col];  // Setze die Header
    end;
  end;

  // Füge die Items hinzu
  Update;

  AutoSizeListViewColumns(FListView);
end;

// Methode zur Aktualisierung einer bestimmten Zelle
procedure TFrontend_Output_TListView.UpdateCell(ARow, ACol: Integer);
var
  ListItem: TListItem;
  AValue : String;
begin
  if (ARow >= 0) and (ARow < FListView.Items.Count) then
  begin
    AValue := CSVFile.Cell[ARow,ACol];
    ListItem := FListView.Items[ARow];
    if ACol = 0 then
      ListItem.Caption := VarToStr(AValue)  // Setze die Caption (erste Spalte)
    else
      ListItem.SubItems[ACol - 1] := VarToStr(AValue);  // Setze die SubItems (weitere Spalten)
  end;
end;

// Methode zur Aktualisierung der gesamten Ausgabe
procedure TFrontend_Output_TListView.Update;
var
  I, J,k: Integer;
  ListItem: TListItem;
begin
  if not Assigned(FCSVFile) or not Assigned(FListView) then Exit;

  FListView.Items.BeginUpdate;
  try
    FListView.Clear;

    if FCSVFile.UseHeader then
      K := 1
    else
      k := 0;

    // Iteriere über alle Datenzeilen der CSV-Datei
    for I := k to FCSVFile.RowCount - 1 do
    begin
      // Erstelle ein neues Item für jede Zeile
      ListItem := FListView.Items.Add;
      ListItem.Caption := FCSVFile.Cell[I, 0];  // Setze die erste Spalte als Caption

      // Füge die restlichen Spalten als SubItems hinzu
      for J := 1 to FCSVFile.ColCount[0] - 1 do
      begin
        ListItem.SubItems.Add(FCSVFile.Cell[I, J]);
      end;
    end;
  finally
    FListView.Items.EndUpdate;
  end;
end;

end.

