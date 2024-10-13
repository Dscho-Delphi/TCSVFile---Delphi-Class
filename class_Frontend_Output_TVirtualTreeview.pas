unit class_Frontend_Output_TVirtualTreeview;

{**
 * class_Frontend_Output_TVirtualTreeview.pas
 *
 * Eine Delphi-Klasse zur Verwaltung der Ausgabe von CSV-Daten in einem TVirtualStringTree.
 * Sie bietet Funktionen zum Aktualisieren von Knoten und zur Sortierung von Ausgaben.
 *
 * @author     Johannes Teitge
 * @version    1.1, BETA
 * @date       08.10.2024
 *
 * Copyright: Die Urheberrechtsbestimmungen dieser Datei richten sich nach den
 * Bestimmungen in der Datei CSVFile.pas.
 *}

interface

uses
  CSVFile, VirtualTrees, System.Variants, System.Classes, System.SysUtils, VirtualTrees.Header;

type
  // TNodeData ist eine dynamische Struktur, die die CSV-Daten speichert.
  TNodeData = record
    Data: array of string; //< Dynamisches Array zur Speicherung der Daten
  end;

  // PNodeData ist ein Zeiger auf den TNodeData-Typ
  PNodeData = ^TNodeData;

  // TFrontend_Output_TVirtualTreeview ist eine Klasse, die die Ausgabe von CSV-Daten in einem TVirtualStringTree verwaltet.
  TFrontend_Output_TVirtualTreeview = class(TFrontend_Output_Base)
  private
    FTreeView: TVirtualStringTree; //< Referenz auf das TVirtualStringTree
    FIgnoreHeader: Boolean; //< Flag, ob die Header-Zeile ignoriert werden soll
  public
    // Konstruktor der Klasse, der eine CSV-Datei und eine Treeview akzeptiert
    constructor Create(ACSVFile: TCSVFile; ATreeView: TVirtualStringTree); reintroduce;

    // Initialisierungsmethode für die Klasse
    procedure Initialize; override;

    // Aktualisierungsmethode für eine bestimmte Zeile
    procedure UpdateCell(ARow, ACol: Integer); override;

    // Methode zur Aktualisierung der gesamten Ausgabe
    procedure Update; override;

    // Eigenschaften zum Zugriff auf private Felder
    property TreeView: TVirtualStringTree read FTreeView; //< Referenz auf das TVirtualStringTree
    property IgnoreHeader: Boolean read FIgnoreHeader write FIgnoreHeader; //< Flag zum Ignorieren des Headers
  end;

implementation

{ TFrontend_Output_TVirtualTreeview }

// Konstruktor der TFrontend_Output_TVirtualTreeview-Klasse
constructor TFrontend_Output_TVirtualTreeview.Create(ACSVFile: TCSVFile; ATreeView: TVirtualStringTree);
begin
  inherited Create(ACSVFile); // Aufruf des Konstruktors der Basisklasse
  FTreeView := ATreeView; // Speichere die Referenz auf das TVirtualStringTree
  IgnoreHeader := true; // Standardwert für Header-Optio
end;

// Initialisierungsmethode
procedure TFrontend_Output_TVirtualTreeview.Initialize;
Var
  S : String;
begin
  FTreeView.Clear; // Leere das TreeView vor der neuen Befüllung

  // Setze die Größe der Node-Daten
  FTreeView.NodeDataSize := SizeOf(TNodeData);

  // Setze die Header, wenn nicht ignoriert
  if (FCSVFile.RowCount > 0) then
  begin
    FTreeView.Header.Columns.Clear; // Lösche vorherige Spalten
    // Füge die Header hinzu
    for var Col := 0 to FCSVFile.ColCount[0] - 1 do
    begin
      S := FCSVFile.Cell[0, Col];
      FTreeView.Header.Columns.Add.Text := S; // Setze den Header
    end;
  end;

  // Setze die Breite der Spalten nach Bedarf
  for var i := 0 to FTreeView.Header.Columns.Count - 1 do
  begin
    FTreeView.Header.Columns[i].Width := 100; // Setze eine Standardbreite (z.B. 100 Pixel)
  end;


  // Setze AutoSizeIndex auf die letzte Spalte
  FTreeView.Header.AutoSizeIndex := FTreeView.Header.Columns.Count - 1;

  // Aktiviere die automatische Größenanpassung
  FTreeView.Header.Options := FTreeView.Header.Options + [hoAutoResize];

   // Erlaube das Sortieren
    FTreeView.Header.AutoSizeIndex := FTreeView.Header.Columns.Count - 1;
    FTreeView.Header.Options := FTreeView.Header.Options + [hoAutoResize, hoVisible, hoOwnerDraw]; // Header-Optionen aktivieren


  // Füge Knoten hinzu, falls bereits Daten vorhanden sind
  Update; // Rufe die Methode zur Befüllung des TreeViews auf
end;


// Methode zur Aktualisierung einer bestimmten Zeile
procedure TFrontend_Output_TVirtualTreeview.UpdateCell(ARow, ACol: Integer);
var
  Node: PVirtualNode; // Zeiger auf den aktuellen Knoten
  AValue : String;
begin
  // Hier wird die Logik zum Aktualisieren einer bestimmten Zeile im Treeview implementiert
  if Assigned(FTreeView) and Assigned(FCSVFile) then
  begin
    Node := FTreeView.GetFirst; // Start bei dem ersten Knoten
    for var I := 0 to ARow do
    begin
      if Assigned(Node) then
      begin
        if I = ARow then
        begin
          var NodeData := PNodeData(Node^.GetData); // Typcasting auf TNodeData
          // Aktualisiere den Knoten mit dem neuen Wert
          if (ACol < Length(NodeData.Data)) then
            NodeData.Data[ACol] := AValue; // Setze den neuen Wert
          Break; // Breche die Schleife ab, wenn der Knoten gefunden wurde
        end;
        Node := FTreeView.GetNext(Node); // Gehe zum nächsten Knoten
      end;
    end;
  end;
end;

// Methode zur Aktualisierung der gesamten Ausgabe
procedure TFrontend_Output_TVirtualTreeview.Update;
var
  Node: PVirtualNode;
  NodeData: PNodeData;
  I: Integer;
  S : String;
begin
  // Hier wird die Logik zur Aktualisierung der gesamten Ausgabe implementiert
  if (not Assigned(FCSVFile)) or (not Assigned(FTreeView)) then Exit;

  FTreeView.Clear; // Baum vorher leeren

  // Iteriere über alle Zeilen der CSV-Datei
  for I := 1 to FCSVFile.RowCount - 1 do
  begin

    Node := FTreeView.AddChild(nil); // Neuen Knoten hinzufügen
    NodeData := FTreeView.GetNodeData(Node); // Hole die Knotendaten

    // Hier füllst du die Knotendaten
    SetLength(NodeData.Data, FCSVFile.ColCount[0]); // Setze die Länge des Data-Arrays
    for var j := 0 to FCSVFile.ColCount[0] - 1 do
    begin
      S := FCSVFile.Cell[I, j];
      NodeData.Data[j] := S; // Setze die Daten aus der CSV
    end;
  end;

  FTreeView.Invalidate; // Aktualisiere die Anzeige des Baums
end;

end.

