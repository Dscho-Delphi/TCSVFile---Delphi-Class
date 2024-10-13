unit CSVFile;

{**
 * CSVFile.pas
 *
 * Eine Delphi-Klasse zum Lesen und Schreiben von CSV-Dateien mit Unterstützung für mehrzeilige Zellen und die Handhabung von Trennzeichen innerhalb von Anführungszeichen.
 *
 * @autor      Johannes Teitge
 * @version    1.1, BETA
 * @datum      01.10.2024
 *
 * Copyright (c) 2024 Johannes Teitge, tmedia cross communications
 * Alle Rechte vorbehalten.
 *
 * Diese Software ist Open Source. Die Verbreitung und Nutzung in Quell- und
 * Binärform, mit oder ohne Änderungen, ist gestattet, sofern die folgenden Bedingungen
 * erfüllt sind:
 * - Die Weitergabe des Quellcodes muss den oben genannten Copyright-Hinweis,
 *   diese Liste der Bedingungen und den folgenden Haftungsausschluss enthalten.
 * - Weder der Name des Copyright-Inhabers noch die Namen seiner Mitwirkenden dürfen
 *   ohne vorherige schriftliche Genehmigung verwendet werden, um Produkte zu unterstützen oder
 *   für sie zu werben, die aus dieser Software abgeleitet sind.
 * - Wenn Sie diese Klasse kommerziell verwenden, bitte ich freundlich um eine Spende
 *   an folgende Organisation: https://www.nierenstiftung.de/spenden-und-helfen/.
 *   Diese Bitte ergibt sich aus meinen persönlichen Erfahrungen als Dialysepatient und
 *   als jemand, der an Nierenerkrankungen leidet.
 * - Wenn Sie Änderungen an dieser Klasse vornehmen, müssen Sie den ursprünglichen Autorennamen
 *   (Johannes Teitge) beibehalten und einen Hinweis darauf geben, dass die Arbeit auf der
 *   ursprünglichen Version basiert.
 *
 * DIESE SOFTWARE WIRD "WIE BESEHEN" OHNE JEGLICHE GEWÄHRLEISTUNG BEREITGESTELLT,
 * WEDER AUSDRÜCKLICH NOCH STILLSCHWEIGEND.
 *}

interface

uses
  Windows,System.Classes, System.SysUtils, System.Generics.Collections, System.Variants,
  System.Generics.Defaults, System.Math, System.Zip, System.Win.ComObj,
  System.JSON, System.RegularExpressions;

type

  {
    TCSVPos repräsentiert die Position eines bestimmten Elements innerhalb einer CSV-Struktur.
    Diese Struktur speichert die Zeile und die Spalte eines bestimmten Wertes oder Zelleninhalts
    in einer CSV-Datei, um den Zugriff auf spezifische Daten zu erleichtern.

    @field X: Integer
      Die horizontale Position (Zeile) in der CSV-Struktur. Der Wert `X` gibt die Zeilennummer an,
      in der sich das Element befindet.

    @field Y: Integer
      Die vertikale Position (Spalte) in der CSV-Struktur. Der Wert `Y` gibt die Spaltennummer
      an, in der sich das Element befindet.
  }
  TCSVPos = record
    X: Integer;  //< Die horizontale Reihe (Zeile) in der CSV-Struktur.
    Y: Integer;  //< Die vertikale Reihe (Spalte) in der CSV-Struktur.
  end;

  {
    TCellChange repräsentiert eine geänderte Zelle in einer CSV-Struktur. Diese Struktur speichert
    die Position der geänderten Zelle sowie die Historie der vorherigen Werte der Zelle. So können
    alle Änderungen an einer Zelle nachverfolgt werden.

    @field Row: Integer
      Die horizontale Position (Zeile) der geänderten Zelle in der CSV-Struktur. Der Wert `Row` gibt
      an, welche Zeile die geänderte Zelle enthält.

    @field Col: Integer
      Die vertikale Position (Spalte) der geänderten Zelle in der CSV-Struktur. Der Wert `Col` gibt
      an, welche Spalte die geänderte Zelle enthält.

    @field History: TStringList
      Eine Liste, die die Historie der Werte speichert, die in der Zelle gespeichert waren. Jeder
      Eintrag in der Liste repräsentiert einen früheren Wert, der in dieser Zelle war. Dies ermöglicht
      es, Änderungen nachzuvollziehen und gegebenenfalls rückgängig zu machen.
  }
  TCellChange = record
    Row: Integer;       //< Die horizontale Reihe (Zeile) in der CSV-Struktur.
    Col: Integer;       //< Die vertikale Reihe (Spalte) in der CSV-Struktur.
    History: TStringList;  //< Liste, um die Historie der Werte zu speichern.
  end;





  TCalculationType = (ctAverage, ctCount, ctMax, ctMin, ctProduct, ctSum);  //< Berechnungstypen: Durchschnitt, Anzahl, Maximalwert, Minimalwert, Produkt, Summe
  TCSVSortDirection = (sdCSVAscending, sdCSVDescending);  //< Sortierrichtung für CSV: Aufsteigend oder Absteigend
  TDataType = (dtCurrency, dtDate, dtDateTime, dtDouble, dtInteger, dtString);  //< Datentypen: Währung, Datum, Datum/Zeit, Fließkommazahl, Ganzzahl, String
  TLineBreakStyle = (lbsMac, lbsNone, lbsUnix, lbsWindows);  //< Zeilenumbruchstile: Mac, Keine, Unix, Windows

  { In Planung:
    Varianz und Standardabweichung für statistische Analysen.
    Maximale/Minimale Differenzen zwischen den Werten.
    Prozentuale Veränderung zwischen zwei Werten oder einem Durchschnitt.
    Median
  }


  TCSVFile = class; // Forward declaration

  {
    TFrontend_Output_Base ist eine abstrakte Basisklasse, die als Fundament für die Entwicklung von
    spezifischen Ausgabe-Komponenten dient, die mit CSV-Dateien interagieren. Diese Klasse stellt grundlegende
    Methoden und Eigenschaften zur Verfügung, die für die Initialisierung, das Zurücksetzen, die Aktualisierung
    der Ausgabe sowie die Sortierung und Fokussierung auf spezifische Zellen erforderlich sind.

    Abgeleitete Klassen, die diese Basisklasse verwenden, müssen die abstrakten Methoden `Initialize`,
    `Clear`, `Update`, `UpdateCell` und `SelectFocus` implementieren, um die spezifischen Logiken für die
    jeweilige Ausgabeform (z.B. XML, JSON, Excel, etc.) bereitzustellen. Die Klasse ermöglicht so eine
    Erweiterbarkeit durch Vererbung, um neue Ausgabeformate hinzuzufügen, die nahtlos mit der CSV-Datenstruktur
    zusammenarbeiten.

    @bold(Beispiel:)@br
    Ein Beispiel für eine abgeleitete Klasse im selben Verzeichnis könnte `TFrontend_Output_TStringGrid` sein,
    die diese Methoden implementiert, um CSV-Daten in einemn TStringrid anzuzeigen und weiter zu bearbeiten.

    @author Johannes Teitge
    @version 1.0
    @created 12.10.2024
  }
  TFrontend_Output_Base = class
  protected
    FAutoAutput: Boolean;  //< Gibt an, ob die automatische Ausgabe aktiviert ist
    FCSVFile: TCSVFile;  //< Referenz auf TCSVFile
    FSort: Boolean;  //< Gibt an, ob die Daten sortiert werden sollen
    FSortDirection: TCSVSortDirection;  //< Gibt die Sortierrichtung an: sdCSVAscending oder sdCSVDescending
  public
    constructor Create(ACSVFile: TCSVFile); virtual;  //< Konstruktor zur Initialisierung der Output-Komponente mit einer Referenz auf TCSVFile
    procedure Clear; virtual; abstract;  //< Abstracte Methode zum Zurücksetzen der Komponente
    procedure Initialize; virtual; abstract;  //< Abstracte Methode zur Initialisierung der Output-Komponente
    procedure SelectFocus(ARow, ACol: Integer); virtual; abstract;  //< Abstracte Methode, um den Fokus auf eine Zelle in der Output-Komponente zu setzen
    procedure Update; virtual; abstract;  //< Abstracte Methode zur Aktualisierung der gesamten Output-Komponente
    procedure UpdateCell(ARow, ACol: Integer); virtual; abstract;  //< Abstracte Methode zum Aktualisieren einer bestimmten Zelle in der Output-Komponente

    property AutoOutput: Boolean read FAutoAutput write FAutoAutput;  //< Gibt an, ob die automatische Ausgabe aktiviert ist
    property CSVFile: TCSVFile read FCSVFile;  //< Getter für die CSV-Datei, die mit der Output-Komponente verbunden ist
    property Sort: Boolean read FSort write FSort;  //< Gibt an, ob die Daten sortiert werden sollen
    property SortDirection: TCSVSortDirection read FSortDirection write FSortDirection;  //< Gibt die Sortierrichtung an (aufsteigend oder absteigend)
  end;


  TOnAfterReadRow = procedure(RowIndex: Integer) of object;  //< Event, das nach dem Lesen einer Zeile ausgelöst wird
  TOnBeforeReadCell = procedure(RowIndex, ColIndex: Integer; var Value: String) of object;  //< Event, das vor dem Lesen einer Zelle ausgelöst wird
  TOnBeforeReadRow = procedure(RowIndex: Integer) of object;  //< Event, das vor dem Lesen einer Zeile ausgelöst wird
  TOnFoundEvent = procedure(Sender: TObject; Position: TCSVPos; Value: String) of object;  //< Event, das bei einem Suchtreffer ausgelöst wird
  TProgressEvent = procedure(Sender: TObject; ProcessedKB, MaxKB: Integer; Percent: Double) of object;  //< Event, das den Fortschritt eines Vorgangs anzeigt


  {
    TCSVFile - Eine Klasse zur Verwaltung und Verarbeitung von CSV-Dateien @br

    Diese Klasse bietet eine Vielzahl an Methoden, um CSV-Dateien zu laden, zu speichern, zu durchsuchen, @br
    zu bearbeiten und zu analysieren. Sie unterstützt grundlegende Operationen wie das Einfügen, Löschen und @br
    Kombinieren von Zeilen und Spalten, das Berechnen von Werten (z.B. Summe, Durchschnitt) sowie das Suchen @br
    und Ersetzen von Text in Zellen. Die Klasse ermöglicht die Verarbeitung von CSV-Daten mit flexiblen @br
    Trennzeichen, Zeichencodierungen und Zeilenumbruchstilen. @br @br

    Die Klasse enthält auch ein System zur Nachverfolgung von Änderungen an Zellen, mit Unterstützung für die @br
    Historie von Zellenänderungen. Ein Event-Handling-Mechanismus erlaubt das Erstellen benutzerdefinierter @br
    Reaktionen bei bestimmten Operationen, wie z.B. das Lesen von Zeilen oder das Finden von Suchtreffern. @br @br

    Sie können die CSV-Daten in verschiedenen Formaten exportieren, einschließlich XML, JSON und XLSX. @br @br

    Über die `Output`-Eigenschaft können Klassen, die vom Typ `TFrontend_Output_Base` abgeleitet sind, verwendet @br
    werden. Dies ermöglicht die flexible Anbindung an beliebige Komponenten, APIs, Datenbanken, Files oder andere @br
    externe Systeme, um CSV-Daten nach Bedarf weiterzugeben oder zu verarbeiten. @br @br

    Diese Klasse bietet somit eine robuste Basis, die entweder direkt genutzt oder durch abgeleitete Klassen @br
    angepasst werden kann, um spezifische Anforderungen an die Verarbeitung von CSV-Daten zu erfüllen. @br
  }
  TCSVFile = class
  private
    FAutoselect : Boolean;
    FChanges: TList<TCellChange>;  //< Liste für geänderte Zellen in der Form [Row][Cell][History]
    FData: TList<TStringList>;  //< CSV-Daten in der Form [Row]Cell[]
    FEncoding: TEncoding;  //< Kodierung der CSV-Datei
    FLastMatches: TArray<TCSVPos>;  //< Liste der letzten Suchtreffer
    FLastSearchPos: Integer;  //< Position des letzten Suchtreffers
    FLineBreakStyle: TLineBreakStyle;  //< Zeilenumbruchstil für die CSV-Datei (lbsWindows, lbsMac, lbsUnix, lbsNone)
    FOnAfterReadRow: TOnAfterReadRow;  //< Event, das nach dem Lesen einer Zeile ausgelöst wird
    FOnBeforeReadCell: TOnBeforeReadCell;  //< Event, das vor dem Lesen einer Zelle ausgelöst wird
    FOnBeforeReadRow: TOnBeforeReadRow;  //< Event, das vor dem Lesen einer Zeile ausgelöst wird
    FOnFound: TOnFoundEvent;  //< Event, das bei einem Suchtreffer ausgelöst wird
    FOnProgress: TProgressEvent;  //< Event, das bei Fortschritt in einem Vorgang ausgelöst wird
    FOutput: TFrontend_Output_Base;  //< Referenz auf die Ausgabekomponente
    FQuotes: Char;  //< Zeichen für Anführungszeichen in der CSV-Datei
    FSeparator: Char;  //< Trennzeichen für die CSV-Datei
    FUseHeader: Boolean;  //< Gibt an, ob die CSV-Datei eine Header-Zeile enthält
    FUseFastParsing : Boolean;
    FParseTime: Integer;



    procedure AddValueToArray(var Values: TArray<String>; const CellStrValue: String);  //< Funktion zum Hinzufügen von Werten zum dynamischen Array
    procedure Assign(Source: TList<TStringList>);  //< Weist Daten aus einer bestehenden Liste zu
    procedure ParseCSVContent(const Content: string);  //< Parst CSV-Daten aus einem String
    procedure ProcessRecord(Fields: TStringList);  //< Verarbeitet ein Datensatz-Objekt aus einer TStringList
    function GetCell(Row, Col: Integer): string;  //< Gibt den Inhalt einer Zelle an der angegebenen Zeile und Spalte zurück
    function GetCellChanged(Row, Col: Integer): Boolean;  //< Getter für CellChanged, prüft ob eine Zelle geändert wurde
    function GetColCount(Row: Integer): Integer;  //< Gibt die Anzahl der Spalten in einer Zeile zurück
    function GetLineBreak: string;  //< Gibt den verwendeten Zeilenumbruchstil als String zurück
    function GetRowCount: Integer;  //< Gibt die Anzahl der Zeilen zurück
    function GetSize: Integer;  //< Gibt die Größe der Daten (z.B. Anzahl der Zellen) zurück
    procedure SetCell(Row, Col: Integer; const Value: string);  //< Setzt den Wert einer Zelle an der angegebenen Zeile und Spalte
    procedure SetCellChanged(Row, Col: Integer; const Value: Boolean);  //< Setter für CellChanged, markiert eine Zelle als geändert oder nicht
    procedure SetOuput(Value: TFrontend_Output_Base);  //< Setzt die Ausgabekomponente (Output)

  public

    {
      TCSVFile.Create - Konstruktor für die CSV-Datei-Klasse @br @br

      Dieser Konstruktor initialisiert eine Instanz der TCSVFile-Klasse. Er setzt die Standardwerte @br
      für die CSV-Datei und erstellt die notwendigen Datenstrukturen für die Speicherung der CSV-Daten @br
      und Änderungen. Zusätzlich ermöglicht der Konstruktor das Festlegen eines spezifischen Encodings @br
      und Trennzeichens, wenn dies gewünscht wird. @br @br

      Parameter: @br
        AEncoding: TEncoding (optional) - Das gewünschte Encoding der CSV-Datei. Falls nicht angegeben, @br
        wird UTF-8 als Standard verwendet. @br
        ASeparator: Char (optional) - Das Trennzeichen, das zur Trennung von Feldern in der CSV-Datei @br
        verwendet wird. Standardmäßig wird das Komma (',') verwendet. @br @br

      Es werden die folgenden Felder der TCSVFile-Klasse initialisiert: @br
        - FData: TList<TStringList> - Eine Liste, die die Daten der CSV-Datei speichert. @br
        - FChanges: TList<TCellChange> - Eine Liste, die die Änderungen an den Zellen speichert. @br
        - FEncoding: TEncoding - Das Encoding für die CSV-Datei, standardmäßig UTF-8. @br
        - FSeparator: Char - Das Trennzeichen für die CSV-Datei, standardmäßig das Komma (','). @br
        - FQuotes: Char - Das Zeichen für Anführungszeichen in der CSV-Datei, standardmäßig das doppelte Anführungszeichen ('"'). @br
        - FUseHeader: Boolean - Gibt an, ob die CSV-Datei eine Kopfzeile enthält. Standardmäßig auf `True` gesetzt. @br
        - FLineBreakStyle: TLineBreakStyle - Der Stil für den Zeilenumbruch in der CSV-Datei, standardmäßig `lbsWindows`. @br @br

      Beispiel: @br
        CSVFile := TCSVFile.Create(TEncoding.UTF8, ';');  // Erstellt eine CSV-Datei mit UTF-8 Encoding und Semikolon als Trennzeichen.
    }
    constructor Create(AEncoding: TEncoding = nil; ASeparator: char = ',');
    destructor Destroy; override;  //< Zerstört das Objekt und gibt Ressourcen frei
    procedure Clear();  //< Leert die CSV-Daten und setzt alle Änderungen zurück
    procedure ClearCol(Col: Integer);  //< Löscht alle Zellen in einer bestimmten Spalte

    procedure ParseMultilineCSV(Stream: TStream);  //< Parst eine CSV-Datei mit mehreren Zeilen aus einem Stream
    procedure LoadMultilineCSVFromString(const CSVText: string);  //< Lädt eine CSV-Datei mit mehreren Zeilen aus einem String
    procedure LoadMultilineCSVFromFile(const FileName: string);  //< Lädt eine CSV-Datei mit mehreren Zeilen von einer Datei
    procedure ParseFastCSVFromFile(const FileName: string);
    procedure ParseMultilineCSVold(const FileName: string);  //< Alte Methode, um eine CSV-Datei mit mehreren Zeilen zu parsen
    procedure InsertFromText(const Content: string; SepChar: Char = #0);  //< Fügt CSV-Daten aus einem Textstring ein
    procedure LoadFromFile(const FileName: string; AnalyzeFile:Boolean=true);  //< Lädt CSV-Daten aus einer Datei

    function CombineCols(Col: Integer; BlankString: String = ''): Boolean;  //< Kombiniert alle Zellen einer Spalte zu einer einzigen Zelle
    function CopyColTo(Col1, Col2: Integer; Append: Boolean = False; Separator: Char = ' '): Boolean;  //< Kopiert den Inhalt einer Spalte in eine andere
    procedure CopyToList(Col: Integer; TargetList: TStringList; IgnoreHeader: Boolean = False);  //< Kopiert eine Spalte in eine TStringList
    function GetCellChange(Row, Col: Integer): TCellChange;  //< Gibt die Änderung einer bestimmten Zelle zurück
    function DeleteCol(Index: Integer): Boolean;  //< Löscht eine Spalte
    function DeleteRow(Index: Integer): Boolean;  //< Löscht eine Zeile
    function DetectDataType(const Value: string): TDataType;  //< Erkennt den Datentyp des Wertes einer Zelle
    function GetColPos(Name: String): Integer;  //< Gibt die Position der Spalte mit dem angegebenen Namen zurück
    function GetEncoding(const Content: string): TEncoding; overload;
    function GetEncoding(Filename: String; MaxBytes: Integer): TEncoding; overload;
    function GetLineBreakStyleFromFile(Filename: String; MaxBytes: Integer = 4096): TLineBreakStyle;
    function GetLineBreakStyleFromString(const Content: string): TLineBreakStyle;
    function GetSeparatorFromFile(Filename: String; MaxBytes: Integer = 4096): Char;  //< Liest den Separator aus einer Datei
    function GetSeparatorFromString(Text: String; MaxBytes: Integer = 4096): Char;  //< Liest den Separator aus einem Textstring
    function GetQuoteFromFile(Separator: Char; Filename: String; MaxBytes: Integer = 4096): Char;
    function GetQuoteFromString(Separator: Char; Text: String; MaxBytes: Integer = 4096): Char;
    function Replace(SearchText: String; ReplaceText: String; Column: Integer = -1; RegexOn: Boolean = False;
                     CaseSensitive: Boolean = True; ReplaceAll: Boolean = True; UpdateGrid: Boolean = True): Integer;  //< Sucht und ersetzt Text in einer Spalte
    function SearchEx(SearchText: String; Column: Integer = -1; RegexOn: Boolean = False): TArray<TCSVPos>;  //< Erweitere Suche in einer Spalte mit Regex-Unterstützung
    function Search(SearchText: String; Column: Integer = -1; RegexOn: Boolean = False): TCSVPos;  //< Sucht nach einem Text in einer Spalte, Standard-Suche
    function SearchFirst(): TCSVPos;  //< Gibt die Position des ersten Suchtreffers zurück
    function SearchLast(): TCSVPos;  //< Gibt die Position des letzten Suchtreffers zurück
    function SearchNext(): TCSVPos;  //< Gibt die Position des nächsten Suchtreffers zurück
    function HasSearchNext: Boolean;  //< Überprüft, ob es einen nächsten Suchtreffer gibt
    function GetasText(): String;  //< Gibt die CSV-Daten als Text zurück
    function InsertCol(ColumnIndex: Integer; const Header: string = ''): Integer;  //< Fügt eine neue Spalte mit Header ein
    function InsertRow(ColumnCount: Integer; Position: Integer): Integer;  //< Fügt eine neue Zeile an einer angegebenen Position ein
    function IsCellChanged(Row, Col: Integer): Boolean;  //< Prüft, ob eine Zelle geändert wurde
    procedure RemoveRange(StartIndex, Count: Integer);  //< Entfernt einen Bereich von Zellen
    function Calculate(IsRow: Boolean; Index: Integer; Exclude: array of Integer; var CSVPos: TCSVPos; CalcType: TCalculationType = ctSum): Double;  //< Führt Berechnungen für eine Zeile oder Spalte durch
    procedure RowToList(RowNumber: Integer; List: TStringList);  //< Kopiert eine Zeile in eine TStringList
    procedure SaveToFile(const FileName: string);  //< Speichert die CSV-Daten in einer Datei
    procedure Sort(Column: Integer; Descending: Boolean = false; IgnoreHeader: Boolean = true);  //< Sortiert eine Spalte aufsteigend oder absteigend
    procedure SortExt(Column: Integer; Descending: Boolean = false; IgnoreHeader: Boolean = true; DataType: TDataType = dtString);  //< Erweiterte Sortierung mit Datentyp-Unterstützung
    function AppendRow(ColumnCount: Integer): Integer;  //< Fügt eine neue Zeile ans Ende der CSV an
    procedure ExportAsXML(const Filename: string);  //< Exportiert die CSV-Daten als XML-Datei
    procedure ExportAsJSON(const Filename: string);  //< Exportiert die CSV-Daten als JSON-Datei
    procedure ExportAsXLSX(const Filename: string);  //< Exportiert die CSV-Daten als XLSX-Datei
    function HasChanges: Boolean;  //< Prüft, ob Änderungen an den Daten vorgenommen wurden
    function ChangesCount: Integer;  //< Gibt die Anzahl der Änderungen zurück
    procedure ClearChanges;  //< Setzt die Änderungs-Liste zurück
    function GetHistory(ARow, ACol: Integer): TStringlist;  //< Gibt die Historie von Änderungen in einer Zelle zurück
    procedure SwapCol(Col1, Col2: Integer);
    procedure SwapRow(Row1, Row2: Integer);

    property Autoselect: Boolean read fAutoselect write fAutoselect;
    property Cell[Row, Col: Integer]: string read GetCell write SetCell; default;  //< Zugriff auf eine Zelle
    property Changed[Row, Col: Integer]: Boolean read GetCellChanged write SetCellChanged;  //< Getter und Setter für CellChanged
    property ColCount[Row: Integer]: Integer read GetColCount;  //< Gibt die Anzahl der Spalten für eine Zeile zurück
    property Data: TList<TStringList> read fData;  //< Zugriff auf die zugrunde liegenden CSV-Daten
    property Encoding: TEncoding read FEncoding write FEncoding;  //< Legt die Codierung für die CSV-Datei fest
    property LineBreakStyle: TLineBreakStyle read FLineBreakStyle write FLineBreakStyle;  //< Legt den Zeilenumbruchstil für die CSV-Datei fest
    procedure MoveColBy(Col, Count: Integer); //< Gesamte Spalten um eine bestimmte Anzahl verschieben (Minus nach links, Positiv nach rechts)
    procedure MoveRowBy(Row, Count: Integer); //< Gesamte Zeile um eine bestimmte Anzahl verschieben (Minus nach Oben, Positiv nach Unten)
    property Output: TFrontend_Output_Base read FOutput write SetOuput;  //< Zugriff auf die Ausgabe-Komponente
    Property ParseTime: Integer read FParseTime;
    property RowCount: Integer read GetRowCount;  //< Gibt die Anzahl der Zeilen in der CSV zurück
    property Separator: Char read FSeparator write FSeparator;  //< Legt das Trennzeichen für die CSV-Datei fest
    property Size: Integer read GetSize;  //< Gibt die Größe der CSV-Daten zurück
    property UseFastParsing : Boolean read FUseFastParsing write FUseFastParsing;
    property UseHeader: Boolean read FUseHeader write FUseHeader;  //< Gibt an, ob die CSV-Datei einen Header enthält

    property OnAfterReadRow: TOnAfterReadRow read FOnAfterReadRow write FOnAfterReadRow;  //< Ereignis nach dem Lesen einer Zeile
    property OnBeforeReadCell: TOnBeforeReadCell read FOnBeforeReadCell write FOnBeforeReadCell;  //< Ereignis vor dem Lesen einer Zelle
    property OnBeforeReadRow: TOnBeforeReadRow read FOnBeforeReadRow write FOnBeforeReadRow;  //< Ereignis vor dem Lesen einer Zeile
    property OnFound: TOnFoundEvent read FOnFound write FOnFound;  //< Ereignis, wenn ein Suchtreffer gefunden wurde
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;  //< Ereignis für den Fortschritt der Operation


  end;


  function FormatMilliseconds(Milliseconds: Integer; IncludingIdentifier: Boolean = True; Identifiers: String = 'Sekunden|Minuten'): string;
  function EncodingToString(Encoding: TEncoding): string;
  function ReadFileContent(Filename: String; MaxBytes: Integer): string;
  function NormalizeDecimalSeparator(const Value: String; UseWindowsSettings: Boolean = True): String;  //< Normalisiert den Dezimaltrennzeichen für die gegebene Zahl
  function RemoveSurroundingQuotes(const AField: string;FQuotes:Char): string;  //< Entfernt umgebende Anführungszeichen von einem Feld

{
  Funktion zur Berechnung des Durchschnitts (Mittelwert) der Werte im Array @br
  Der Durchschnitt ist der Mittelwert aller Zahlen im Array. @br
  Er wird berechnet, indem die Summe aller Werte durch die Anzahl der Werte im Array geteilt wird. @br
  @br@bold(Anwendungsbeispiel:) @br
  - Bei der Analyse von Testergebnissen: Der Durchschnitt kann verwendet werden, um den Mittelwert der Schülerbewertungen in einer Prüfung zu berechnen. @br
  - Bei der Berechnung des Durchschnittsgewinns: Wenn du den durchschnittlichen Gewinn eines Unternehmens über mehrere Jahre hinweg berechnen möchtest, kannst du diese Funktion verwenden. @br
  - Bei der Analyse von Produktpreisen: Wenn du den Durchschnittspreis von Produkten berechnen möchtest, die auf einer Website verkauft werden, hilft dir diese Funktion, die mittleren Preise zu berechnen. @br
}
  function Calc_Average(const Values: TArray<String>): Double;

{
  Funktion zur Berechnung der Anzahl der Werte im Array @br
  Diese Funktion zählt die Anzahl der Elemente im Array, die nicht leer sind. @br
  Leere Strings werden hierbei ignoriert. @br
  @br@bold(Anwendungsbeispiel:) @br
  - Bei der Analyse von Umfragedaten: Wenn du die Anzahl der Antworten ermitteln möchtest, die in einem Umfragedatensatz ausgefüllt wurden (ohne leere Einträge zu berücksichtigen), kannst du diese Funktion verwenden. @br
  - Bei der Berechnung der Teilnehmerzahl in einer Veranstaltung: Wenn du wissen möchtest, wie viele Personen eine Veranstaltung besucht haben (ohne leere Einträge zu zählen), nutzt du diese Funktion. @br
  - Bei der Verarbeitung von Logdaten: Wenn du die Anzahl der Ereignisse oder Fehlerprotokolle ermitteln möchtest, kannst du diese Funktion verwenden, um nur die gefüllten Datensätze zu zählen und leere Logeinträge zu ignorieren. @br
}
  function Calc_Count(const Values: TArray<String>): Integer;

{
  Funktion zur Berechnung des maximalen Werts im Array und zur Rückgabe der Position des Maximalwerts @br
  Der maximale Wert ist der größte Wert im Array. @br
  Die Position (Index) des Maximalwerts im Array wird ebenfalls zurückgegeben, um die genaue Lage des größten Werts im Array zu ermitteln. @br
  @br@bold(Anwendungsbeispiel:) @br
  - Bei der Analyse von Verkaufszahlen: Wenn du den höchsten Umsatz in einer Liste von Verkäufen herausfinden möchtest, kannst du diese Funktion verwenden, um den **Maximalwert (z.B. höchster Umsatz)** sowie den **Tag (Position)** zu ermitteln, an dem dieser Umsatz erzielt wurde. @br
  - Bei der Bestimmung des besten Ergebnisses in einem Test: Wenn du den **besten** Testergebnis (z.B. die höchste Punktzahl) und die Position (z.B. den Schüler) finden möchtest, kannst du diese Funktion verwenden, um das **höchste Ergebnis** und dessen **Position** zu ermitteln. @br
  - In der Analyse von Aktienkursen: Wenn du den **höchsten** Kurs innerhalb einer bestimmten Periode ermitteln möchtest, liefert dir diese Funktion den **maximalen Kurs** und den **Index (Tag/Zeitraum)**, an dem dieser Preis erreicht wurde. @br
}
  function Calc_Max(const Values: TArray<String>; var Pos: TCSVPos): Double;

{
  Funktion zur Berechnung des minimalen Werts im Array und zur Rückgabe der Position des Minimalwerts @br
  Der minimale Wert ist der kleinste Wert im Array. @br
  Die Position (Index) des Minimalwerts im Array wird ebenfalls zurückgegeben, um die genaue Lage des kleinsten Werts im Array zu ermitteln. @br
  @br@bold(Anwendungsbeispiel:) @br
  - Bei der Analyse von Temperaturen: Wenn du die niedrigste Temperatur in einem Datensatz suchst und wissen möchtest, an welchem Tag diese gemessen wurde, kannst du diese Funktion verwenden, um den **Minimalwert (z.B. die tiefste Temperatur)** sowie den **Tag (Position)** zu ermitteln. @br
  - Bei der Bestimmung des günstigsten Preises: Wenn du die **günstigste** Preisoption in einer Liste von Produkten ermitteln möchtest, kannst du den minimalen Preis und dessen Position (Produktname oder Index) mit dieser Funktion herausfinden. @br
  - In der mathematischen Optimierung: Wenn du den **kleinsten** Wert einer bestimmten Reihe von Messdaten ermitteln möchtest, um etwa den **besten Wert** für eine Optimierungsformel zu verwenden. @br
}
  function Calc_Min(const Values: TArray<String>; var Pos: TCSVPos): Double;

{
  Funktion zur Berechnung des Produkts der Werte im Array @br
  Das Produkt ist das Ergebnis der Multiplikation aller Werte im Array. @br
  Wenn das Array leere oder ungültige Werte enthält, die keine Zahlen sind, werden diese ignoriert. @br
  @br@bold(Anwendungsbeispiel:) @br
  - Bei der Berechnung des Gesamtgewichts eines Pakets: Wenn du das Gewicht jedes Teils des Pakets hast, kannst du das **Gesamtgewicht** berechnen, indem du die Einzelgewichte miteinander multiplizierst. @br
  - Bei der Berechnung des Endpreises eines Produkts, wenn Rabatte in Form von Multiplikatoren gegeben sind: Zum Beispiel, wenn ein Produkt mit einem Rabattfaktor von 0,8 verkauft wird, multiplizierst du den Preis mit 0,8, um den Endpreis zu berechnen. @br
  - In der Physik bei der Berechnung von Widerstand in einem parallelen Schaltkreis: Das Produkt von Widerstandswerten kann in bestimmten Formeln verwendet werden, um den Gesamtwiderstand zu ermitteln. @br
  - Bei der Berechnung der kumulierten Wachstumsrate in der Finanzmathematik: Wenn du mehrere Wachstumsraten über Zeiträume hinweg hast, kannst du das Produkt dieser Raten verwenden, um den Gesamtwachstumsfaktor zu ermitteln. @br
}
  function Calc_Product(const Values: TArray<String>): Double;

{
  Funktion zur Berechnung der Summe der Werte im Array @br
  Die Summe ist das Ergebnis der Addition aller Werte im Array. @br
  Wenn das Array leere oder ungültige Werte enthält, die keine Zahlen sind, werden diese ignoriert. @br
  @br@bold(Anwendungsbeispiel:) @br
  - Bei der Analyse der Gesamtkosten eines Projekts: Die Summe der einzelnen Kostenpunkte ergibt die **Gesamtkosten** des Projekts. @br
  - Bei der Berechnung der Gesamtzahl von verkauften Einheiten: Die Summe der Verkaufszahlen aus verschiedenen Regionen gibt die **Gesamtverkaufszahl** an. @br
  - In einer Budgetübersicht: Wenn man die Ausgaben eines Monats summiert, erhält man die **Gesamtausgaben** für diesen Monat. @br
  - Bei der Berechnung des Gesamtgehalts: Die Summe der einzelnen Gehälter der Mitarbeiter ergibt das **Gesamtgehalt** des Unternehmens. @br
}
  function Calc_Sum(const Values: TArray<String>): Double;

{
  Funktion zur Berechnung der Varianz @br
  Die Varianz misst, wie weit die einzelnen Werte eines Datensatzes im Durchschnitt vom Mittelwert (Durchschnitt) entfernt sind. @br
  Eine hohe Varianz bedeutet, dass die Werte weit vom Durchschnitt entfernt sind, während eine niedrige Varianz darauf hinweist, dass die Werte nahe beieinander liegen. @br
  Die Varianz ist ein Maß für die **Streuung** der Daten. Sie hilft zu verstehen, wie stark die Werte variieren oder schwanken. @br
  @br@bold(Anwendungsbeispiel:) @br
  - Bei der Analyse von Investitionsrenditen: Eine hohe Varianz zeigt, dass die Renditen stark schwanken, was auf eine größere Unsicherheit oder Risiko hindeutet. Eine niedrige Varianz zeigt stabile und gleichmäßige Renditen an. @br
  - Bei der Qualitätskontrolle: Eine hohe Varianz in den Produktionsfehlern deutet darauf hin, dass die Fehlerquote stark variiert, während eine niedrige Varianz darauf hinweist, dass die Qualität konstant ist. @br
  - Bei der Analyse von Schülernoten: Eine hohe Varianz bedeutet, dass die Noten weit auseinanderliegen (es gibt viele sehr gute und sehr schlechte Noten), während eine niedrige Varianz darauf hinweist, dass die Noten nahe beieinander liegen. @br
}
  function Calc_Variance(const Values: TArray<String>): Double;

{
  Funktion zur Berechnung der Standardabweichung @br
  Die Standardabweichung ist die Quadratwurzel der Varianz und gibt die durchschnittliche Abweichung der Werte vom Mittelwert an. @br
  Sie ist ein Maß für die Streuung der Daten und hilft zu verstehen, wie stark die Werte um den Durchschnitt schwanken. @br
  Eine kleinere Standardabweichung zeigt an, dass die Werte nahe am Mittelwert liegen, während eine größere Standardabweichung auf eine größere Streuung der Daten hinweist. @br
  @br@bold(Anwendungsbeispiel:) @br
  - Bei der Analyse von Schülernoten: Eine geringe Standardabweichung zeigt, dass die meisten Schüler ähnliche Noten haben, während eine hohe Standardabweichung auf eine größere Variation in den Noten hinweist. @br
  - Bei der Berechnung von Aktienkursen: Eine niedrige Standardabweichung zeigt eine geringe Volatilität der Kurse, während eine hohe Standardabweichung eine größere Schwankungsbreite der Kurse anzeigt. @br
  - Bei der Messung von Produktionsfehlern: Eine niedrige Standardabweichung zeigt an, dass die Fehlerquote konstant ist, während eine hohe Standardabweichung auf unregelmäßige Fehler in der Produktion hinweist. @br
}
  function Calc_StandardDeviation(const Values: TArray<String>): Double;

{
  Funktion zur Berechnung des Median @br
  Der Median ist der mittlere Wert eines Datensatzes, wenn er der Größe nach sortiert ist. @br
  - Bei einer ungeraden Anzahl von Werten ist der Median der mittlere Wert. @br
  - Bei einer geraden Anzahl von Werten ist der Median der Durchschnitt der beiden mittleren Werte. @br
  Der Median ist robust gegenüber Ausreißern, da er nur die mittlere Position berücksichtigt und nicht die tatsächlichen Werte. @br
  @br@bold(Anwendungsbeispiel:) @br
  Bei einer Analyse von Gehältern: Wenn du die Gehälter von Mitarbeitern hast, könnte der Median den Wert darstellen, bei dem die Hälfte der Mitarbeiter mehr und die andere Hälfte weniger verdient. @br
  In einer Analyse von Umfrageergebnissen zur Zufriedenheit: Wenn 50% der Umfrageteilnehmer zufrieden sind, wäre der Median der Wert, bei dem genau die Hälfte der Befragten eine höhere und die andere Hälfte eine niedrigere Bewertung abgegeben hat. @br
  Der Median ist besonders nützlich, wenn der Datensatz Ausreißer enthält, die den Durchschnitt verzerren könnten. @br
}
   function Calc_Median(const Values: TArray<Double>): Double;

{
  Funktion zur Berechnung der Spannweite (Range) @br
  Die Spannweite ist die Differenz zwischen dem größten und dem kleinsten Wert im Datensatz. @br
  Sie zeigt die gesamte Streuung der Daten, berücksichtigt jedoch nicht die Verteilung der Werte. @br
  Eine große Spannweite deutet darauf hin, dass die Daten weit auseinander liegen, während eine kleine Spannweite auf eine enge Streuung hinweist. @br
  @br@bold(Anwendungsbeispiel:) @br
  Bei einer Analyse von Temperaturen: Wenn die höchste Temperatur 30°C und die niedrigste Temperatur 10°C beträgt, ist die Spannweite 20°C. @br
  In einer Analyse von Prüfungsergebnissen: Wenn die besten Noten 1,0 und die schlechtesten Noten 5,0 sind, beträgt die Spannweite 4,0. @br
  Die Spannweite kann hilfreich sein, um einen ersten Überblick über die Streuung der Daten zu bekommen, ist jedoch weniger genau als z.B. der Interquartilsabstand (IQR), da sie nur die Extremwerte berücksichtigt. @br
}
function Calc_Range(const Values: TArray<Double>): Double;

{
  Funktion zur Berechnung der Quartile (Q1, Median, Q3) @br
  Quartile teilen den Datensatz in vier gleich große Teile. @br
  - Q1 (erstes Quartil) ist der Median der unteren Hälfte der Daten (25% der Werte liegen darunter). @br
  - Q2 (zweites Quartil) ist der Median des gesamten Datensatzes (50% der Werte liegen darunter). @br
  - Q3 (drittes Quartil) ist der Median der oberen Hälfte der Daten (75% der Werte liegen darunter). @br
  Der Interquartilsabstand (IQR) ist die Differenz zwischen Q3 und Q1. @br
  Er gibt an, wie weit die mittleren 50% der Werte auseinander liegen. @br@br
  @bold(Anwendungsbeispiel:) @br
  Bei einer Analyse von Gehältern: Q1 zeigt die mittleren Gehälter der unteren 25 % der Mitarbeiter, der Median (Q2) gibt den "durchschnittlichen" Gehaltswert an, und Q3 zeigt die mittleren Gehälter der oberen 25 %. @br
  Ein hoher Unterschied zwischen Q1 und Q3 (großer IQR) deutet auf eine größere Spreizung der Gehälter hin, während ein kleiner Unterschied auf eine geringere Gehaltsdifferenz hindeutet. @br
  In einer Analyse von Prüfungsergebnissen zeigt Q1 die untersten 25% der Noten, Q2 den Median (also den mittleren Wert), und Q3 zeigt die oberen 25% der Noten. @br
}
function Calc_Quartiles(const Values: TArray<Double>): TArray<Double>;

{
  Funktion zur Berechnung des Interquartilsabstands (IQR) @br
  Der IQR ist die Differenz zwischen dem dritten (Q3) und dem ersten Quartil (Q1). @br
  Der IQR misst die Streuung der mittleren 50% der Daten. @br
  Der Interquartilsabstand hilft dabei, Ausreißer zu erkennen, da Werte außerhalb des IQR oft als Ausreißer betrachtet werden. @br
  - Ein hoher IQR deutet auf eine große Streuung der mittleren 50% der Werte hin. @br
  - Ein niedriger IQR deutet auf eine geringe Streuung der mittleren 50% der Werte hin. @br
  @bold(Anwendungsbeispiel:) @br
  Bei einer Analyse von Prüfungsnoten: Wenn die Noten stark variieren, ist der IQR groß, was auf eine größere Streuung der mittleren 50% der Noten hinweist. @br
  Bei einer Analyse von Gehältern in einer Firma: Ein kleiner IQR könnte darauf hindeuten, dass die mittleren 50% der Gehälter sehr ähnlich sind, während ein großer IQR auf eine größere Gehaltsdifferenz hindeutet. @br
}
function Calc_IQR(const Values: TArray<Double>): Double;

{
  Funktion zur Berechnung des Modus (Mode) @br
  Der Modus ist der Wert, der im Datensatz am häufigsten vorkommt. @br
  Ein Datensatz kann keinen Modus, einen Modus oder mehrere Modi haben. @br
  - Wenn mehrere Werte die gleiche maximale Häufigkeit haben, wird NaN zurückgegeben, um darauf hinzuweisen, dass es keinen eindeutigen Modus gibt. @br
  @bold(Anwendungsbeispiel:) @br
  Bei einer Analyse von Produktbewertungen: Wenn man die Bewertung "5" am häufigsten sieht, ist 5 der Modus. @br
  Bei einer Analyse von Prüfungsnoten: Wenn die Note "1" am häufigsten vorkommt, ist 1 der Modus. @br
  Wenn mehrere Werte die gleiche Häufigkeit haben (z.B. 3 und 5 in einem Datensatz), wird NaN zurückgegeben, um zu kennzeichnen, dass es keinen eindeutigen Modus gibt. @br
}
function Calc_Mode(const Values: TArray<Double>): Double;

{
  Funktion zur Konvertierung von TArray<String> in TArray<Double> @br
  Diese Funktion versucht, jeden String im Array in einen Double-Wert zu konvertieren. @br
  Falls die Konvertierung fehlschlägt, wird NaN zurückgegeben, um ungültige Daten zu kennzeichnen. @br
}
function ConvertStringArrayToDoubleArray(const Values: TArray<String>): TArray<Double>;



implementation


function FormatMilliseconds(Milliseconds: Integer; IncludingIdentifier: Boolean = True; Identifiers: String = 'Sekunden|Minuten'): string;
var
  Minutes, Seconds, MilliSecondsRest: Integer;
  SecondsIdentifier, MinutesIdentifier: String;
  IdentifiersArray: TArray<String>;
begin
  // Standardidentifikatoren auf Englisch, falls Identifiers leer ist
  if Identifiers = '' then
    Identifiers := 'seconds|minutes';

  // Identifikatoren basierend auf der Pipe-Trennung in zwei Teile aufteilen
  IdentifiersArray := Identifiers.Split(['|']);
  SecondsIdentifier := IdentifiersArray[0]; // Erster Teil ist für Sekunden
  MinutesIdentifier := IdentifiersArray[1]; // Zweiter Teil ist für Minuten

  // Berechne die Minuten, Sekunden und verbleibenden Millisekunden
  Minutes := Milliseconds div 60000;  // 1 Minute = 60000 Millisekunden
  Seconds := (Milliseconds mod 60000) div 1000;  // 1 Sekunde = 1000 Millisekunden
  MilliSecondsRest := Milliseconds mod 1000;  // Verbleibende Millisekunden

  if Minutes > 0 then
    // Formatierung als 'MM:SS.mmm', wenn Minuten vorhanden sind
    Result := Format('%d:%2.2d.%3.3d', [Minutes, Seconds, MilliSecondsRest])
  else
    // Formatierung als 'SS.mmm', wenn keine Minuten vorhanden sind
    Result := Format('%d.%3.3d', [Seconds, MilliSecondsRest]);

  // Füge Identifikatoren nur hinzu, wenn IncludingIdentifier True ist
  if IncludingIdentifier then
  begin
    if Minutes > 0 then
      Result := Result + ' ' + MinutesIdentifier  // Füge Minuten-Identifikator hinzu
    else
      Result := Result + ' ' + SecondsIdentifier;  // Füge Sekunden-Identifikator hinzu
  end;
end;



// Allgemeine und Helperfunktionen
function EncodingToString(Encoding: TEncoding): string;
begin
  if Encoding = TEncoding.UTF8 then
    Result := 'UTF-8'
  else if Encoding = TEncoding.Unicode then
    Result := 'Unicode'
  else if Encoding = TEncoding.ASCII then
    Result := 'ASCII'
  else if Encoding = TEncoding.BigEndianUnicode then
    Result := 'Big-Endian Unicode'
  else if Encoding = TEncoding.UTF7 then
    Result := 'UTF-7'
  else
    Result := 'Unknown Encoding';
end;

function ReadFileContent(Filename: String; MaxBytes: Integer): string;
var
  FileStream: TFileStream;
  Buffer: TBytes;
  BytesRead: Integer;
begin
  FileStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Buffer, MaxBytes);  // Puffer auf MaxBytes setzen
    BytesRead := FileStream.Read(Buffer[0], MaxBytes);  // Datei lesen
    SetLength(Buffer, BytesRead);  // Puffer auf tatsächlich gelesene Bytes anpassen
    Result := TEncoding.UTF8.GetString(Buffer);  // Bytes in String umwandeln
  finally
    FileStream.Free;
  end;
end;

function NormalizeDecimalSeparator_old(const Value: String): String;
var
  DecimalSeparator: Char;
  NormalizedValue: String;
begin
  DecimalSeparator := FormatSettings.DecimalSeparator;
  NormalizedValue := Value;

  // Normalisieren, abhängig vom Dezimaltrennzeichen
  if DecimalSeparator = ',' then
    NormalizedValue := StringReplace(NormalizedValue, '.', ',', [rfReplaceAll])
  else if DecimalSeparator = '.' then
    NormalizedValue := StringReplace(NormalizedValue, ',', '.', [rfReplaceAll]);

  Result := NormalizedValue;
end;

function NormalizeDecimalSeparator(const Value: String; UseWindowsSettings: Boolean = True): String;
var
  DecimalSeparator, ThousandSeparator: Char;
  NormalizedValue: String;
  CurrentDecimalSeparator, CurrentThousandSeparator: Char;
begin
  // Standardmäßig die aktuellen FormatSettings verwenden
  DecimalSeparator := FormatSettings.DecimalSeparator;
  ThousandSeparator := FormatSettings.ThousandSeparator;

  // Wenn die Windows-Einstellungen verwendet werden sollen, hole sie aus FormatSettings
  if UseWindowsSettings then
  begin
    DecimalSeparator := FormatSettings.DecimalSeparator;
    ThousandSeparator := FormatSettings.ThousandSeparator;
  end;

  // Erkenne das aktuelle Dezimal- und Tausendertrennzeichen im String
  // Wenn der String Punkte enthält, könnte dies entweder ein Tausender- oder Dezimaltrennzeichen sein
  if (Pos('.', Value) > 0) and (Pos(',', Value) > 0) then
  begin
    // Falls beide vorhanden sind, gehen wir davon aus, dass Punkt das Tausendertrennzeichen ist und Komma das Dezimaltrennzeichen
    CurrentDecimalSeparator := ',';
    CurrentThousandSeparator := '.';
  end
  else if Pos(',', Value) > 0 then
  begin
    // Falls nur ein Komma vorhanden ist, gehen wir davon aus, dass es das Dezimaltrennzeichen ist
    CurrentDecimalSeparator := ',';
    CurrentThousandSeparator := '.';
  end
  else if Pos('.', Value) > 0 then
  begin
    // Falls nur ein Punkt vorhanden ist, gehen wir davon aus, dass er das Dezimaltrennzeichen ist
    CurrentDecimalSeparator := '.';
    CurrentThousandSeparator := ',';
  end
  else
  begin
    // Wenn keine Trennzeichen erkannt werden, bleibt der String unverändert
    Result := Value;
    Exit;
  end;

  // Entferne zunächst alle Tausendertrennzeichen
  NormalizedValue := StringReplace(Value, CurrentThousandSeparator, '', [rfReplaceAll]);

  // Ersetze das erkannte Dezimaltrennzeichen mit dem System-Dezimaltrennzeichen
  NormalizedValue := StringReplace(NormalizedValue, CurrentDecimalSeparator, DecimalSeparator, [rfReplaceAll]);

  Result := NormalizedValue;
end;


// Helper function to remove surrounding quotes from a field
function RemoveSurroundingQuotes(const AField: string;FQuotes:Char): string;
begin
  Result := AField;
  if (Length(AField) > 1) and (AField[1] = FQuotes) and (AField[Length(AField)] = FQuotes) then begin
    // Remove leading and trailing quotes
    Result := Copy(AField, 2, Length(AField) - 2);
  end;
end;


// Berechnet die Summe
function Calc_Sum(const Values: TArray<String>): Double;
var
  I: Integer;
  CellValue: Double;
  SumValue: Double;
  DecimalSeparator: Char;
  NormalizedValue: String;
begin
  SumValue := 0;
  DecimalSeparator := FormatSettings.DecimalSeparator;

  for I := 0 to High(Values) do
  begin
    NormalizedValue := Values[I];

    // Dezimaltrennzeichen normalisieren (kann auch direkt hier gemacht werden)
    if DecimalSeparator = ',' then
      NormalizedValue := StringReplace(NormalizedValue, '.', ',', [rfReplaceAll]);
    if DecimalSeparator = '.' then
      NormalizedValue := StringReplace(NormalizedValue, ',', '.', [rfReplaceAll]);

    // Versuche, den String in eine Zahl zu konvertieren
    if TryStrToFloat(NormalizedValue, CellValue) then
    begin
      SumValue := SumValue + CellValue; // Zur Summe hinzufügen
    end;
  end;

  Result := SumValue;
end;

function Calc_Count(const Values: TArray<String>): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(Values) do
  begin
    // Zähle nur nicht-leere Zellen
    if Trim(Values[I]) <> '' then
      Result := Result + 1;
  end;
end;

function Calc_Average(const Values: TArray<String>): Double;
var
  I: Integer;
  SumValue: Double;
  CellValue: Double;
  CountValue: Integer;
  NormalizedValue: String;
begin
  // Wenn keine Werte vorhanden sind, gib 0 zurück
  if Length(Values) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Initialisiere die Summe und die Anzahl der Werte
  SumValue := 0;
  CountValue := 0;

  // Gehe durch jedes Element im Array
  for I := 0 to High(Values) do
  begin
    // Normalisiere den String, um das Dezimaltrennzeichen zu behandeln
    NormalizedValue := NormalizeDecimalSeparator(Values[I]);

    // Versuche, den String in eine Zahl zu konvertieren
    if TryStrToFloat(NormalizedValue, CellValue) then
    begin
      // Addiere den Wert zur Summe und erhöhe den Zähler
      SumValue := SumValue + CellValue;
      CountValue := CountValue + 1;
    end;
  end;

  // Berechne den Durchschnitt (Vermeidung einer Division durch 0)
  if CountValue > 0 then
    Result := SumValue / CountValue
  else
    Result := 0;
end;


function Calc_Min(const Values: TArray<String>; var Pos: TCSVPos): Double;
var
  I: Integer;
  CellValue, MinValue: Double;
  NormalizedValue: String;
begin
  MinValue := MaxDouble;  // Initialisiere mit einem sehr großen Wert
  Pos.Y := -1;          // Standardposition für nicht gefunden

  for I := 0 to High(Values) do
  begin
    NormalizedValue := NormalizeDecimalSeparator(Values[I]);

    if TryStrToFloat(NormalizedValue, CellValue) then
    begin
      if CellValue < MinValue then
      begin
        MinValue := CellValue;  // Aktualisiere, wenn ein kleinerer Wert gefunden wird
        Pos.Y := I;           // Setze die Position
      end;
    end;
  end;

  if MinValue = MaxDouble then
    Result := 0  // Kein gültiger Wert gefunden, Rückgabe 0
  else
    Result := MinValue;
end;


function Calc_Max(const Values: TArray<String>; var Pos: TCSVPos): Double;
var
  I: Integer;
  CellValue: Double;
  MaxValue: Double;
  NormalizedValue: String;
begin
  // Wenn keine Werte vorhanden sind, gib 0 zurück
  if Length(Values) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Initialisiere MaxValue mit einem sehr niedrigen Wert
  MaxValue := -MaxDouble;  // MaxDouble ist der kleinste mögliche Wert für Double
  Pos.Y := -1;           // Standardposition für nicht gefunden

  // Gehe durch jedes Element im Array
  for I := 0 to High(Values) do
  begin
    // Normalisiere den String, um das Dezimaltrennzeichen zu behandeln
    NormalizedValue := NormalizeDecimalSeparator(Values[I]);

    // Versuche, den String in eine Zahl zu konvertieren
    if TryStrToFloat(NormalizedValue, CellValue) then
    begin
      // Aktualisiere das Maximum, wenn der aktuelle Wert größer ist
      if CellValue > MaxValue then
      begin
        MaxValue := CellValue;
        Pos.Y := I;           // Setze die Position
      end;
    end;
  end;

  // Gib den größten gefundenen Wert zurück
  Result := MaxValue;
end;



function Calc_Product(const Values: TArray<String>): Double;
var
  I: Integer;
  ProductValue: Double;
  CellValue: Double;
  NormalizedValue: String;
begin
  // Wenn keine Werte vorhanden sind, gib 1 zurück (neutrales Element der Multiplikation)
  if Length(Values) = 0 then
  begin
    Result := 1;
    Exit;
  end;

  // Initialisiere das Produkt mit 1 (neutrales Element der Multiplikation)
  ProductValue := 1;

  // Gehe durch jedes Element im Array
  for I := 0 to High(Values) do
  begin
    // Normalisiere den String, um das Dezimaltrennzeichen zu behandeln
    NormalizedValue := NormalizeDecimalSeparator(Values[I]);

    // Versuche, den String in eine Zahl zu konvertieren
    if TryStrToFloat(NormalizedValue, CellValue) then
    begin
      // Multipliziere den Wert mit dem bisherigen Produkt
      ProductValue := ProductValue * CellValue;
    end;
  end;

  // Rückgabe des berechneten Produkts
  Result := ProductValue;
end;

// Funktion zur Berechnung der Varianz
function Calc_Variance(const Values: TArray<String>): Double;
var
  I: Integer;
  SumValue: Double;
  CountValue: Integer;
  CellValue: Double;
  NormalizedValue: String;
  Mean: Double;
  VarianceSum: Double;
begin
  // Wenn keine Werte vorhanden sind, gib 0 zurück
  if Length(Values) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Initialisiere die Summe und die Anzahl der Werte
  SumValue := 0;
  CountValue := 0;

  // Berechne den Mittelwert
  for I := 0 to High(Values) do
  begin
    NormalizedValue := NormalizeDecimalSeparator(Values[I]);

    if TryStrToFloat(NormalizedValue, CellValue) then
    begin
      SumValue := SumValue + CellValue;
      CountValue := CountValue + 1;
    end;
  end;

  // Berechne den Mittelwert (Mean)
  if CountValue > 0 then
    Mean := SumValue / CountValue
  else
    Mean := 0;

  // Berechne die Varianz
  VarianceSum := 0;
  for I := 0 to High(Values) do
  begin
    NormalizedValue := NormalizeDecimalSeparator(Values[I]);

    if TryStrToFloat(NormalizedValue, CellValue) then
    begin
      VarianceSum := VarianceSum + Sqr(CellValue - Mean);
    end;
  end;

  // Varianz = Summe der quadrierten Abweichungen / Anzahl der Werte
  if CountValue > 0 then
    Result := VarianceSum / CountValue
  else
    Result := 0;
end;

// Funktion zur Berechnung der Standardabweichung
function Calc_StandardDeviation(const Values: TArray<String>): Double;
begin
  // Die Standardabweichung ist die Quadratwurzel der Varianz
  Result := Sqrt(Calc_Variance(Values));
end;

function Calc_Median(const Values: TArray<Double>): Double;
var
  SortedValues: TArray<Double>;
begin
  SortedValues := Values;
  TArray.Sort<Double>(SortedValues);

  if Length(SortedValues) mod 2 = 0 then
    Result := (SortedValues[Length(SortedValues) div 2 - 1] + SortedValues[Length(SortedValues) div 2]) / 2
  else
    Result := SortedValues[Length(SortedValues) div 2];
end;

function Calc_Range(const Values: TArray<Double>): Double;
begin
  Result := MaxValue(Values) - MinValue(Values);
end;

function Calc_Quartiles(const Values: TArray<Double>): TArray<Double>;
var
  SortedValues: TArray<Double>;
begin
  SortedValues := Values;
  TArray.Sort<Double>(SortedValues);

  Result[0] := SortedValues[Length(SortedValues) div 4];  // Q1
  Result[1] := SortedValues[Length(SortedValues) div 2];  // Median (Q2)
  Result[2] := SortedValues[(Length(SortedValues) * 3) div 4];  // Q3
end;

function Calc_IQR(const Values: TArray<Double>): Double;
var
  Quartiles: TArray<Double>;
begin
  Quartiles := Calc_Quartiles(Values);
  Result := Quartiles[2] - Quartiles[0];
end;


function Calc_Mode(const Values: TArray<Double>): Double;
var
  ValueCounts: TDictionary<Double, Integer>;
  Value: Double;  // Ändere den Typ von 'Integer' zu 'Double'
  MaxCount, Count: Integer;
begin
  ValueCounts := TDictionary<Double, Integer>.Create;
  MaxCount := 0;
  Result := NaN;  // Unbestimmt, falls kein Modus existiert

  try
    for Value in Values do  // Der Wert ist jetzt vom Typ Double
    begin
      if ValueCounts.ContainsKey(Value) then
        ValueCounts[Value] := ValueCounts[Value] + 1
      else
        ValueCounts.Add(Value, 1);

      Count := ValueCounts[Value];
      if Count > MaxCount then
      begin
        MaxCount := Count;
        Result := Value;
      end
      else if Count = MaxCount then
        Result := NaN;  // Falls mehrere Werte die gleiche Häufigkeit haben, gib NaN zurück
    end;
  finally
    ValueCounts.Free;
  end;
end;

function ConvertStringArrayToDoubleArray(const Values: TArray<String>): TArray<Double>;
var
  I: Integer;
begin
  SetLength(Result, Length(Values));  // Setzt die Länge des Ergebnis-Arrays auf die Länge des Eingabe-Arrays

  for I := 0 to High(Values) do
  begin
    // Versuche, den String-Wert in einen Double zu konvertieren
    if not TryStrToFloat(Values[I], Result[I]) then
    begin
      Result[I] := NaN;  // Falls die Konvertierung fehlschlägt, setze NaN für diesen Wert
    end;
  end;
end;











// Klasse TCSVFile

constructor TCSVFile.Create(AEncoding: TEncoding = nil; ASeparator: char = ',');
begin
  FData := TList<TStringList>.Create;   // Initialisiere die Liste zur Speicherung der CSV-Daten.
  FChanges := TList<TCellChange>.Create; //< Initialisiere die Änderungs-Liste zur Verfolgung von Zelländerungen.

  // Standardwerte setzen
  if Assigned(AEncoding) then
    FEncoding := AEncoding   // Wenn ein Encoding angegeben wurde, dieses verwenden.
  else
    FEncoding := TEncoding.UTF8;  //< Setze das Standard-Encoding auf UTF-8, falls kein Encoding angegeben wurde.

  FSeparator := ASeparator; //< Setze das Standard-Trennzeichen, z.B. Komma (','), falls keines angegeben wurde.
  FQuotes := '"';  //< Setze das Zeichen für Anführungszeichen auf das doppelte Anführungszeichen.
  FUseHeader := True; //< Standardmäßig wird davon ausgegangen, dass die CSV-Datei eine Kopfzeile enthält.
  FLineBreakStyle := lbsWindows; //< Setze den Standard-Zeilenumbruchstil auf Windows (Carriage Return + Line Feed).
  FUseFastParsing := True;
end;


destructor TCSVFile.Destroy;
var
  i: Integer;
begin
  for i := 0 to FData.Count - 1 do
    FData[i].Free;
  FData.Free;
  FChanges.Free; //< Freigabe der Änderungs-Liste
  inherited;
end;

function TCSVFile.GetCell(Row, Col: Integer): string;
begin
  if (Row >= 0) and (Row < FData.Count) and (Col >= 0) and (Col < FData[Row].Count) then
    Result := FData[Row][Col]
  else
    raise Exception.CreateFmt('Cell [%d, %d] out of range', [Row, Col]);
end;

function TCSVFile.GetRowCount: Integer;
begin
  Result := FData.Count;
end;

function TCSVFile.GetColCount(Row: Integer): Integer;
begin
  if (Row >= 0) and (Row < FData.Count) then
    Result := FData[Row].Count
  else
    raise Exception.CreateFmt('Row [%d] out of range', [Row]);
end;


procedure TCSVFile.MoveColBy(Col, Count: Integer);
var
  Row: Integer;
  Temp: string;
begin
  if Count < 0 then begin
    if (Col = 0) or (Col > FData[0].Count) then raise Exception.CreateFmt('Column %d out of range', [Col])
  end
  else begin
    if (Col < 0) or (Col >= FData[0].Count) then raise Exception.CreateFmt('Column %d out of range', [Col]);
  end;

  // Über alle Zeilen iterieren
  try
  for Row := 0 to FData.Count - 1 do
  begin
    // Speichern des Werts der aktuellen Spalte
    Temp := FData[Row][Col];
    // Verschieben der Spalte
    FData[Row].Delete(Col);
    FData[Row].Insert(Col + Count, Temp);
  end;
  EXCEPT

  end;
end;

procedure TCSVFile.MoveRowBy(Row, Count: Integer);
var
  TempRow: TStringList;
begin
  if (Row < 0) or (Row >= FData.Count) then
    raise Exception.CreateFmt('Row %d out of range', [Row]);

  // Die gewünschte Zeile speichern
  TempRow := FData[Row];
  // Entfernen der Zeile
  FData.Delete(Row);
  // Einfügen der Zeile an neuer Position
  FData.Insert(Row + Count, TempRow);
end;

procedure TCSVFile.LoadFromFile(const FileName: string; AnalyzeFile: Boolean = true);
var
  FileStream: TFileStream;
  Reader: TStreamReader;
  Line: string;
  C: Char;
  StartTime, EndTime: Cardinal; // Start- und Endzeit für das Parsen
begin
  self.Clear;

  if assigned(FOutput) then
    FOutput.Clear;

  if AnalyzeFile then
  begin
    fSeparator := self.GetSeparatorFromFile(Filename);
    self.FQuotes := self.GetQuoteFromFile(fSeparator, Filename);
    self.GetLineBreakStyleFromFile(Filename);
  end;

  // Initialisiere die Zeitmessung
  StartTime := GetTickCount;

  if self.FUseFastParsing then
  begin
    ParseFastCSVFromFile(Filename);
  end
  else
  begin
    self.LoadMultilineCSVFromFile(Filename);
  end;

  // Zeit nach dem Parsen erfassen
  EndTime := GetTickCount;

  // Berechne die Zeit, die das Parsen gedauert hat
  FParseTime := EndTime - StartTime;

  if assigned(self.FOutput) then
    FOutput.Initialize;
end;
{

  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    // StreamReader mit dem angegebenen Encoding erstellen
    Reader := TStreamReader.Create(FileStream, FEncoding);
    try
      FData.Clear;
      while not Reader.EndOfStream do begin
        Line := Reader.ReadLine;
        ParseCSVContent(Line);
      end;
    finally
      Reader.Free;
    end;
  finally
    FileStream.Free;
  end;
}


procedure TCSVFile.SaveToFile(const FileName: string);
var
  FileStream: TFileStream;
  Writer: TStreamWriter;
  i, j: Integer;
  Line: string;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    // StreamWriter mit dem angegebenen Encoding erstellen
    Writer := TStreamWriter.Create( FileStream, FEncoding);
    try
      for i := 0 to FData.Count - 1 do begin
        Line := '';
        for j := 0 to FData[i].Count - 1 do begin
          if j > 0 then
            Line := Line + FSeparator;
          Line := Line + FData[i][j];
        end;
        Writer.WriteLine(Line);
      end;
    finally
      Writer.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TCSVFile.InsertFromText(const Content: string;SepChar:Char=#0);
Var
  C : Char;
begin
  FData.Clear;

  // Wenn kein Separator, dann automatisch erkennen
  if SepChar=#0 then begin
    C := self.GetSeparatorFromString(Content);
    if C = #0 then
      FSeparator := ';'
    else
      FSeparator := C;
  end
  else
    self.FSeparator := SepChar;

  ParseCSVContent(Content);
end;


function TCSVFile.Replace(SearchText: String; ReplaceText: String; Column: Integer = -1; RegexOn: Boolean = False;
                           CaseSensitive: Boolean = True; ReplaceAll: Boolean = True; UpdateGrid: Boolean = True): Integer;
var
  Row, Col, StartCol, EndCol: Integer;
  CellValue: String;
  RegEx: TRegEx;
  MatchFound: Boolean;
  ReplaceCount: Integer;
  SearchOptions: TReplaceFlags;
begin
  ReplaceCount := 0; //< Zählt die Anzahl der Ersetzungen

  //< Setze die Suchoptionen je nach Case-Sensitivity
  SearchOptions := [rfReplaceAll]; //< Standardmäßig alle Vorkommen ersetzen
  if not CaseSensitive then
    Include(SearchOptions, rfIgnoreCase); //< Groß-/Kleinschreibung ignorieren

  // Wenn RegexOn gesetzt ist, erstelle ein RegEx-Objekt
  if RegexOn then
  begin
    if CaseSensitive then
      RegEx := TRegEx.Create(SearchText)  // Case-Sensitive Regex
    else
      RegEx := TRegEx.Create(SearchText, [roIgnoreCase]);  // Case-Insensitive Regex
  end;

  // Falls eine bestimmte Spalte angegeben wurde, nur diese durchsuchen
  if Column >= 0 then
  begin
    StartCol := Column;
    EndCol := Column;
  end
  else
  begin
    // Ansonsten alle Spalten durchsuchen
    StartCol := 0;
    EndCol := FData[0].Count - 1; //< Annahme: FData speichert Daten der Tabelle
  end;

  // Durchlaufe alle Zeilen
  for Row := 0 to FData.Count - 1 do
  begin
    // Durchlaufe die angegebenen Spalten oder alle, falls keine angegeben wurde
    for Col := StartCol to EndCol do
    begin
      CellValue := FData[Row][Col]; // Hol den Zellenwert

      // Prüfen, ob wir reguläre Ausdrücke verwenden sollen
      if RegexOn then
      begin
        // Nur ersetzen, wenn ein Treffer gefunden wurde
        if RegEx.IsMatch(CellValue) then
        begin
          FData[Row][Col] := RegEx.Replace(CellValue, ReplaceText); // Ersetzen
          Inc(ReplaceCount); // Zähler erhöhen

          // Wenn Grid-Update gewünscht und FGrid zugewiesen ist, auch das Grid aktualisieren
   //       if UpdateGrid and Assigned(FGrid) then FGrid.Cells[Col, Row] := FData[Row][Col];

          // Wenn nur das erste Vorkommen ersetzt werden soll, beende die Funktion
          if not ReplaceAll then
            Exit(ReplaceCount);
        end;
      end
      else
      begin
        // Prüfen, ob ein Treffer ohne Regex gefunden wird
        MatchFound := (Pos(SearchText, CellValue) > 0);

        // Wenn Treffer gefunden, ersetze den Text
        if MatchFound then
        begin
          if not ReplaceAll then
          begin
            // Nur das erste Vorkommen ersetzen
            FData[Row][Col] := StringReplace(CellValue, SearchText, ReplaceText, []);
            Inc(ReplaceCount);

            // Wenn Grid-Update gewünscht und FGrid zugewiesen ist, das Grid aktualisieren
//            if UpdateGrid and Assigned(FGrid) then FGrid.Cells[Col, Row] := FData[Row][Col];

            Exit(ReplaceCount); // Sofort nach dem ersten Treffer aufhören
          end
          else
          begin
            // Alle Vorkommen ersetzen
            FData[Row][Col] := StringReplace(CellValue, SearchText, ReplaceText, SearchOptions);
            Inc(ReplaceCount); // Zähler erhöhen

            // Wenn Grid-Update gewünscht und FGrid zugewiesen ist, das Grid aktualisieren
//            if UpdateGrid and Assigned(FGrid) then FGrid.Cells[Col, Row] := FData[Row][Col];
          end;
        end;
      end;
    end;
  end;

  // Rückgabe der Anzahl der Ersetzungen
  Result := ReplaceCount;
end;



function TCSVFile.SearchEx(SearchText: String; Column: Integer = -1; RegexOn: Boolean = False): TArray<TCSVPos>;
var
  Row, Col, StartCol, EndCol: Integer;
  CellValue: String;
  RegEx: TRegEx;
  MatchFound: Boolean;
  Matches: TArray<TCSVPos>;
  PosCount: Integer;
  GridPos: TCSVPos;
begin
  // Initialisiere das dynamische Array
  SetLength(Matches, 0);
  PosCount := 0;

  // Wenn RegexOn gesetzt ist, erstelle ein RegEx-Objekt
  if RegexOn then
    RegEx := TRegEx.Create(SearchText);

  // Falls eine bestimmte Spalte angegeben wurde, nur diese durchsuchen
  if Column >= 0 then
  begin
    StartCol := Column;
    EndCol := Column;
  end
  else
  begin
    // Ansonsten alle Spalten durchsuchen
    StartCol := 0;
    EndCol := FData[0].Count - 1; // Annahme: FData speichert Daten der Tabelle
  end;

  // Durchlaufe alle Zeilen
  for Row := 0 to FData.Count - 1 do
  begin
    // Durchlaufe die angegebenen Spalten oder alle, falls keine angegeben wurde
    try
    for Col := StartCol to EndCol do
    begin
      CellValue := FData[Row][Col]; // Hole den Zellenwert

      // Prüfen, ob wir reguläre Ausdrücke verwenden sollen
      if RegexOn then
        MatchFound := RegEx.IsMatch(CellValue)
      else
        MatchFound := (Pos(SearchText, CellValue) > 0);

      // Falls Treffer gefunden, speichere die Position
      if MatchFound then
      begin
        GridPos.X := Col;
        GridPos.Y := Row;

        // Füge den Treffer in das dynamische Array ein
        Inc(PosCount);
        SetLength(Matches, PosCount);
        Matches[PosCount - 1] := GridPos;
      end;
    end;
    except
    end;
  end;

  // Rückgabe des Arrays mit allen gefundenen Positionen
  Result := Matches;
end;


function TCSVFile.Search(SearchText: String; Column: Integer = -1; RegexOn: Boolean = False): TCSVPos;
Var
  FGridpos : TCSVPos;
  CellValue: String;
begin
  FLastMatches := SearchEx(SearchText, Column, RegexOn); // Alle Treffer holen
  FLastSearchPos := 0; // Start bei erstem Treffer
  FGridpos.X := -1;
  FgridPos.Y := -1;

  if Length(FLastMatches) > 0 then begin
    FGridPos := FLastMatches[FLastSearchPos]; // Rückgabe des ersten Treffers
    CellValue := Cell[FGridPos.Y, FGridPos.X]; // Zellenwert über das Property holen

    // Wenn GridAutoselect auf True gesetzt ist, wähle die Zelle automatisch aus
    if FAutoselect and assigned(FOutput) then begin
      fOutput.SelectFocus(FGridPos.Y,FGridPos.X); // Zelle im Grid markieren und Fokus setzen
    end;

    // Rufe das OnFound-Event auf, wenn es zugewiesen ist
    if Assigned(FOnFound) then
      FOnFound(Self, FGridPos, CellValue);

  end
  else begin
    FGridpos.X := -1;
    FgridPos.Y := -1;
  end;

  Result := FGridPos; // Rückgabe der Suchposition
end;

function TCSVFile.SearchFirst(): TCSVPos;
Var
  FGridpos : TCSVPos;
  CellValue: String;
begin
  // Initialisiere die Suchposition als ungültig (-1, -1)
  FGridpos.X := -1;
  FgridPos.Y := -1;

  // Prüfen, ob Treffer vorhanden sind
  if Length(FLastMatches) > 0 then
  begin
    FLastSearchPos := 0; // Setze auf den ersten Treffer
    FGridPos := FLastMatches[FLastSearchPos]; // Rückgabe des ersten Treffers
    CellValue := Cell[FGridPos.Y, FGridPos.X]; // Zellenwert über das Property holen

    // Automatische Selektion und Fokussierung im Grid, falls aktiviert
//    if FGridAutoSelect and Assigned(FGrid) then SelectFocusGridCell(FGridPos);

    // Rufe das OnFound-Event auf, wenn es zugewiesen ist
    if Assigned(FOnFound) then
      FOnFound(Self, FGridPos, CellValue);
  end;

  // Rückgabe der neuen Trefferposition oder (-1, -1), wenn kein Treffer gefunden wurde
  Result := FGridPos;
end;

function TCSVFile.SearchLast(): TCSVPos;
Var
  FGridpos : TCSVPos;
  CellValue: String;
begin
  // Initialisiere die Suchposition als ungültig (-1, -1)
  FGridPos.X := -1;
  FGridPos.Y := -1;

  // Prüfen, ob Treffer vorhanden sind
  if Length(FLastMatches) > 0 then
  begin
    FLastSearchPos := Length(FLastMatches) - 1; // Setze auf den letzten Treffer
    FGridPos := FLastMatches[FLastSearchPos]; // Rückgabe des letzten Treffers
    CellValue := Cell[FGridPos.Y, FGridPos.X]; // Zellenwert über das Property holen

    // Automatische Selektion und Fokussierung im Grid, falls aktiviert
//    if FGridAutoSelect and Assigned(FGrid) then SelectFocusGridCell(FGridPos);

    // Rufe das OnFound-Event auf, wenn es zugewiesen ist
    if Assigned(FOnFound) then
      FOnFound(Self, FGridPos, CellValue);
  end;

  // Rückgabe der neuen Trefferposition oder (-1, -1), wenn kein Treffer gefunden wurde
  Result := FGridPos;
end;

function TCSVFile.SearchNext(): TCSVPos;
Var
  FGridpos : TCSVPos;
  CellValue: String;
begin
  // Initialisiere die Suchposition als ungültig (-1, -1)
  FGridPos.X := -1;
  FGridPos.Y := -1;

  // Prüfen, ob noch weitere Suchtreffer existieren
  if FLastSearchPos < Length(FLastMatches) - 1 then
  begin
    Inc(FLastSearchPos); // Nächster Treffer
    FGridPos := FLastMatches[FLastSearchPos]; // Treffer-Position ermitteln
    CellValue := Cell[FGridPos.Y, FGridPos.X]; // Zellenwert über das Property holen

    // Wenn GridAutoselect auf True gesetzt ist, wähle die Zelle automatisch aus
    if FAutoselect and assigned(FOutput) then begin
      fOutput.SelectFocus(FGridPos.Y,FGridPos.X); // Zelle im Grid markieren und Fokus setzen
    end;

    // Rufe das OnFound-Event auf, wenn es zugewiesen ist
    if Assigned(FOnFound) then
      FOnFound(Self, FGridPos, CellValue);
  end;

  // Rückgabe der neuen Trefferposition oder (-1, -1), wenn kein weiterer Treffer
  Result := FGridPos;
end;

function TCSVFile.HasSearchNext: Boolean;
begin
  // Prüfen, ob noch weitere Suchergebnisse vorhanden sind
  Result := (FLastSearchPos < Length(FLastMatches) - 1);
end;

function TCSVFile.GetAsText: string;
var
  i, j: Integer;
  Line: string;
begin
  Result := '';
  for i := 0 to FData.Count - 1 do begin
    Line := '';
    for j := 0 to FData[i].Count - 1 do begin
      if j > 0 then
        Line := Line + FSeparator; // Separator zwischen den Zellen
      Line := Line + FData[i][j]; // Zelleninhalt hinzufügen
    end;
    Result := Result + Line + sLineBreak; // Zeilenumbruch hinzufügen
  end;
end;

function TCSVFile.AppendRow(ColumnCount: Integer):Integer;
var
  NewRow: TStringList;
begin
  NewRow := TStringList.Create;
  try
    // Füge leere Zellen zur neuen Zeile hinzu
    NewRow.Capacity := ColumnCount; // Setze die Kapazität auf die Anzahl der Spalten
    for var i := 1 to ColumnCount do begin
      NewRow.Add(''); // Leere Zelle hinzufügen
    end;

    // Füge die neue Zeile zu FData hinzu
    FData.Add(NewRow);

    Result := FData.Count - 1; // Den Index der neuen Zeile zurückgeben
  except
    NewRow.Free; // Sicherstellen, dass die neue Zeile freigegeben wird, falls ein Fehler auftritt
    raise;
  end;
end;

function TCSVFile.InsertRow(ColumnCount: Integer; Position: Integer): Integer;
var
  NewRow: TStringList;
begin
  if Position < 0 then
    Position := 0
  else if Position > FData.Count then
    Position := FData.Count; // Stellen Sie sicher, dass die Position innerhalb der Grenzen bleibt

  NewRow := TStringList.Create;
  try
    NewRow.Capacity := ColumnCount; // Kapazität für die neue Zeile setzen
    FData.Insert(Position, NewRow); // Die neue Zeile an der angegebenen Position einfügen

    Result := Position; // Den Index der neu eingefügten Zeile zurückgeben
  except
    NewRow.Free; // Stelle sicher, dass die Ressourcen freigegeben werden
    raise; // Fehler weitergeben
  end;
end;

function TCSVFile.InsertCol(ColumnIndex: Integer; const Header: string = ''): Integer;
var
  i: Integer;

function ColumnCount: Integer;
begin
  if FData.Count > 0 then
    Result := FData[0].Count // Anzahl der Spalten anhand der ersten Zeile ermitteln
  else
    Result := 0; // Wenn keine Daten vorhanden sind, 0 zurückgeben
end;

begin
  // Überprüfen, ob der Index gültig ist
  if ColumnIndex < 0 then
    ColumnIndex := 0
  else if ColumnIndex > ColumnCount then
    ColumnIndex := ColumnCount; // Fügen Sie am Ende hinzu, wenn der Index zu groß ist

  // Iteriere über alle Zeilen und füge eine leere Zelle an der angegebenen Spalte hinzu
  for i := 0 to FData.Count - 1 do begin
    // Überprüfen, ob die Zeile bereits so viele Spalten hat
    if ColumnIndex < FData[i].Count then
      FData[i].Insert(ColumnIndex, '') // Füge eine leere Zelle hinzu
    else
      FData[i].Add(''); // Füge am Ende eine leere Zelle hinzu
  end;

  // Setzen des Headers in der ersten Zeile, wenn angegeben
  if Header <> '' then
  begin
    if FData.Count > 0 then begin
      // Setze den Header in der angegebenen Spalte
      FData[0][ColumnIndex] := Header;
    end
    else begin
      // Wenn es keine Zeilen gibt, erstelle eine neue Zeile mit dem Header
      FData.Add(TStringList.Create);
      FData[0].Add(Header); // Füge den Header als erste Zelle hinzu
    end;
  end;

  Result := ColumnIndex; // Rückgabe des Index der neu eingefügten Spalte
end;

function TCSVFile.CombineCols(Col: Integer; BlankString: String = ''): Boolean;
var
  i, j: Integer;
  CombinedValue: String;
begin
  Result := False;

  // Überprüfen, ob der Index gültig ist
  if (Col < 0) or (FData.Count = 0) or (Col >= FData[0].Count) then
    Exit;

  // Kombiniere die Spalten ab Col
  for i := 0 to FData.Count - 1 do begin
    CombinedValue := '';

    for j := Col to FData[i].Count - 1 do begin
      if (CombinedValue <> '') then
        CombinedValue := CombinedValue + BlankString; // Füge BlankString hinzu
      CombinedValue := CombinedValue + FData[i][j]; // Wert hinzufügen
    end;

    // Setze den kombinierten Wert in die Spalte Col
    FData[i][Col] := CombinedValue;

    // Lösche die restlichen Spalten
    for j := FData[i].Count - 1 downto Col + 1 do begin
      if (j < FData[i].Count) then
        FData[i].Delete(j); // Lösche die Spalte nach der ersten kombinierten
    end;
  end;

  Result := True; // Erfolg
end;

function TCSVFile.DetectDataType(const Value: string): TDataType;
var
  IntValue: Integer;
  FloatValue: Double;
  DateValue: TDateTime;
  IsValidDate: Boolean;
begin
  if TryStrToInt(Value, IntValue) then
    Result := dtInteger
  else if TryStrToFloat(Value, FloatValue) then
    Result := dtDouble
  else
  begin
    // Versuch, das Datum zu erkennen
    IsValidDate := TryStrToDateTime(Value, DateValue); // Verwende die Standardversion
    if IsValidDate then
      Result := dtDateTime
    else
      Result := dtString;
  end;
end;


procedure TCSVFile.RowToList(RowNumber: Integer; List: TStringList);
var
  i: Integer;
begin
  // Überprüfen, ob die Zeile innerhalb der Grenzen liegt
  if (RowNumber < 0) or (RowNumber >= FData.Count) then
    raise Exception.CreateFmt('Row %d out of range', [RowNumber]);

  // Die Liste leeren, bevor neue Werte hinzugefügt werden
  List.Clear;

  // Alle Zellen der angegebenen Zeile zur Liste hinzufügen
  for i := 0 to FData[RowNumber].Count - 1 do
  begin
    List.Add(FData[RowNumber][i]);
  end;
end;

function TCSVFile.GetColPos(Name: String): Integer;
var
  i: Integer;
begin
  Result := -1; // Standardwert, falls der Name nicht gefunden wird
  if (FData.Count > 0) and (FData[0].Count > 0) then
  begin
    for i := 0 to FData[0].Count - 1 do
    begin
      if FData[0][i] = Name then
      begin
        Result := i; // Index der Spalte gefunden
        Exit;
      end;
    end;
  end;
end;

function TCSVFile.CopyColTo(Col1, Col2: Integer; Append: Boolean = False; Separator: Char = ' '): Boolean;
var
  i: Integer;
  Value1, Value2: String;
begin
  Result := False;

  // Überprüfen, ob die Indizes gültig sind
  if (Col1 < 0) or (Col2 < 0) or (FData.Count = 0) or
     (Col1 >= FData[0].Count) or (Col2 >= FData[0].Count) or (Col1 = Col2) then
    Exit;

  // Werte von Col1 nach Col2 kopieren
  for i := 0 to FData.Count - 1 do begin
    Value1 := FData[i][Col1];
    Value2 := FData[i][Col2];

    if Append then begin
      // Anhängen der Werte mit dem Separator
      if (Value1 <> '') and (Value2 <> '') and not Value2.EndsWith(Separator) then
        FData[i][Col2] := Value2 + Separator + Value1
      else if (Value1 <> '') then
        FData[i][Col2] := Value1;
    end
    else begin
      // Überschreiben des Wertes in Col2
      FData[i][Col2] := Value1;
    end;
  end;

  Result := True; // Erfolg
end;



function TCSVFile.GetSize: Integer;
var
  Row, Col: Integer;
  TotalSize: Integer;
  CellValue: String;
begin
  TotalSize := 0;

  // Durchlaufe alle Zeilen
  for Row := 0 to FData.Count - 1 do
  begin
    // Durchlaufe alle Spalten der jeweiligen Zeile
    for Col := 0 to FData[Row].Count - 1 do
    begin
      CellValue := FData[Row][Col];
      // Addiere die Länge jedes Zellenwertes in Bytes zur Gesamtgröße
      TotalSize := TotalSize + Length(CellValue);
    end;
  end;

  Result := TotalSize;
end;


procedure TCSVFile.Clear();
begin
  fdata.Clear;
end;

procedure TCSVFile.ClearCol(Col: Integer);
var
  i: Integer;
begin
  // Überprüfen, ob der Index gültig ist
  if (Col < 0) or (FData.Count = 0) or (Col >= FData[0].Count) then
    Exit;

  // Leeren der angegebenen Spalte
  for i := 0 to FData.Count - 1 do begin
    FData[i][Col] := ''; // Setze den Wert in der Spalte auf einen leeren String
  end;
end;



function TCSVFile.DeleteRow(Index: Integer): Boolean;
begin
  Result := False;

  if (Index >= 0) and (Index < FData.Count) then begin
    FData.Delete(Index); // Löschen der Zeile
    Result := True; // Erfolg
  end;
end;

function TCSVFile.DeleteCol(Index: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;

  if (Index >= 0) and (FData.Count > 0) and (Index < FData[0].Count) then   begin
    for i := 0 to FData.Count - 1 do begin
      FData[i].Delete(Index); // Löschen der Spalte für jede Zeile
    end;
    Result := True; // Erfolg
  end;
end;





function TCSVFile.GetLineBreakStyleFromString(const Content: string): TLineBreakStyle;
var
  i, ContentLength: Integer;
  CountWindows, CountMac, CountUnix: Integer;
begin
  CountWindows := 0;
  CountMac := 0;
  CountUnix := 0;
  ContentLength := Length(Content);

  i := 1;
  while i <= ContentLength do
  begin
    if (i < ContentLength) and (Content[i] = #13) and (Content[i + 1] = #10) then
    begin
      // Windows-Zeilenumbruch: #13#10
      Inc(CountWindows);
      Inc(i); // Überspringe das nächste Zeichen (#10)
    end
    else if Content[i] = #13 then
    begin
      // Mac-Zeilenumbruch: #13
      Inc(CountMac);
    end
    else if Content[i] = #10 then
    begin
      // Unix-Zeilenumbruch: #10
      Inc(CountUnix);
    end;

    Inc(i);
  end;

  // Bestimme den häufigsten Zeilenumbruch
  if (CountWindows >= CountMac) and (CountWindows >= CountUnix) then
    Result := lbsWindows
  else if (CountMac >= CountWindows) and (CountMac >= CountUnix) then
    Result := lbsMac
  else if (CountUnix >= CountWindows) and (CountUnix >= CountMac) then
    Result := lbsUnix
  else
    Result := lbsNone;
end;

function TCSVFile.GetLineBreakStyleFromFile(Filename: String; MaxBytes: Integer = 4096): TLineBreakStyle;
var
  Content: string;
begin
  Content := ReadFileContent(Filename, MaxBytes);  // Lese den Dateiinhalt
  Result := GetLineBreakStyleFromString(Content);  // Analysiere den Zeilenumbruchstil
end;

{
procedure TCSVFile.ParseCSVContent(const Content: string);
var
  Row: TStringList;
  Cell, LineBreak: string;
  i, FileSize, ProcessedSize, CurrentKB, MaxKB: Integer;
  InQuotes: Boolean;
  Percent: Double;
  LastProgress: Int64;
begin
  Row := TStringList.Create;
  try
    Row.Delimiter := FSeparator;
    Row.StrictDelimiter := True;

    // Zeilenumbruch-Stil basierend auf der Property
    case FLineBreakStyle of
      lbsWindows: LineBreak := #13#10;
      lbsMac: LineBreak := #13;
      lbsUnix: LineBreak := #10;
    end;

    // Dateigröße für den Fortschritt
    FileSize := Length(Content);
    MaxKB := FileSize div 1024;
    ProcessedSize := 0;
    LastProgress := 0;

    // Verarbeitung
    InQuotes := False;
    Cell := '';
    i := 1;

    while i <= Length(Content) do
    begin
      // Prüfen, ob es ein Anführungszeichen ist
      if Content[i] = '"' then
      begin
        if InQuotes then
        begin
          // Wenn das nächste Zeichen wieder ein Anführungszeichen ist, handelt es sich um ein escaped Anführungszeichen
          if (i < Length(Content)) and (Content[i + 1] = '"') then
          begin
            Cell := Cell + '"'; // Füge ein Anführungszeichen hinzu
            Inc(i); // Überspringe das zweite Anführungszeichen
          end
          else
            InQuotes := False; // Beende die Quote-Umgebung
        end
        else
          InQuotes := True; // Beginne eine Quote-Umgebung
      end
      else if (Content[i] = FSeparator) and not InQuotes then
      begin
        // Füge die Zelle hinzu, wenn ein Separator gefunden wird und wir nicht in Anführungszeichen sind
        Row.Add(Trim(Cell)); // Trim entfernt unnötige Leerzeichen
        Cell := '';
      end
      else if (Copy(Content, i, Length(LineBreak)) = LineBreak) and not InQuotes then
      begin
        // Zeilenumbruch außerhalb von Anführungszeichen -> Zeilenende
        Row.Add(Trim(Cell)); // Trim entfernt unnötige Leerzeichen
        Cell := '';

        // Vor dem Hinzufügen der Zeile das OnBeforeReadRow-Ereignis auslösen
        if Assigned(FOnBeforeReadRow) then
          FOnBeforeReadRow(FData.Count);

        // Füge die Zeile zu den Daten hinzu
        if Row.Count > 0 then
        begin
          FData.Add(TStringList.Create);
          FData[FData.Count - 1].Assign(Row);
          Row.Clear;
        end;

        // Nach dem Hinzufügen der Zeile das OnAfterReadRow-Ereignis auslösen
        if Assigned(FOnAfterReadRow) then
          FOnAfterReadRow(FData.Count - 1);

        // Überspringe die Länge des Zeilenumbruchs
        Inc(i, Length(LineBreak) - 1);
      end
      else
      begin
        // Füge den aktuellen Inhalt der Zelle hinzu
        Cell := Cell + Content[i];
      end;

      Inc(i);
      Inc(ProcessedSize);

      // Fortschritt berechnen
      if (ProcessedSize - LastProgress) >= 1024 then
      begin
        LastProgress := ProcessedSize;
        CurrentKB := ProcessedSize div 1024;
        Percent := (ProcessedSize / FileSize) * 100;

        if Assigned(FOnProgress) then
          FOnProgress(Self, CurrentKB, MaxKB, Percent);
      end;
    end;

    // Letzte Zelle und Zeile nach der Schleife hinzufügen
    if Cell <> '' then
      Row.Add(Trim(Cell)); // Trim entfernt unnötige Leerzeichen

    if Row.Count > 0 then
    begin
      if Assigned(FOnBeforeReadRow) then
        FOnBeforeReadRow(FData.Count);

      FData.Add(TStringList.Create);
      FData[FData.Count - 1].Assign(Row);

      if Assigned(FOnAfterReadRow) then
        FOnAfterReadRow(FData.Count - 1);
    end;
  finally
    Row.Free;
  end;
end;
}

procedure TCSVFile.ParseCSVContent(const Content: string);
var
  Row: TStringList;
  Line: string;
  LBStyle : TLineBreakStyle;
  LineBreak: string;
  Lines: TStringList;
  FileSize, ProcessedSize, CurrentKB, MaxKB: Integer;
  Percent: Double;
  LastProgress: Int64;
  i: Integer;
begin
  Row := TStringList.Create;
  Lines := TStringList.Create;
  try
    Row.Delimiter := FSeparator;
    Row.QuoteChar := FQuotes;
    Row.StrictDelimiter := True; // Verhindert, dass Leerzeichen als Trennzeichen interpretiert werden


    // Laden des gesamten Inhalts in eine TStringList, die auf dem gewählten Zeilenumbruch aufteilt
    LBStyle := GetLineBreakStyleFromString(Content);
    if LBStyle = lbsNone then
      LBStyle := FLineBreakStyle;

    // Zeilenumbruch-Stil basierend auf der Property
    case LBStyle of
      lbsWindows: LineBreak := #13#10;
      lbsMac: LineBreak := #13;
      lbsUnix: LineBreak := #10;
    end;



    // Laden des gesamten Inhalts in eine TStringList, die auf dem gewählten Zeilenumbruch aufteilt
    if LBStyle <> lbsWindows then
      Lines.Text := StringReplace(Content, LineBreak, sLineBreak, [rfReplaceAll])
    else
      Lines.Text := Content;

    // Dateigröße für den Fortschritt
    FileSize := Length(Content);
    MaxKB := FileSize div 1024;
    ProcessedSize := 0;
    LastProgress := 0;

    // Verarbeitung jeder Zeile
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];

      // Setze DelimitedText für die aktuelle Zeile, dies parst automatisch
      Row.DelimitedText := Line;

      // Vor dem Hinzufügen der Zeile das OnBeforeReadRow-Ereignis auslösen
      if Assigned(FOnBeforeReadRow) then
        FOnBeforeReadRow(FData.Count);

      // Füge die Zeile zu den Daten hinzu
      if Row.Count > 0 then
      begin
        FData.Add(TStringList.Create);
        FData[FData.Count - 1].Assign(Row);
      end;

      // Nach dem Hinzufügen der Zeile das OnAfterReadRow-Ereignis auslösen
      if Assigned(FOnAfterReadRow) then
        FOnAfterReadRow(FData.Count - 1);

      // Fortschritt berechnen
      ProcessedSize := ProcessedSize + Length(Line) + Length(LineBreak);
      if (ProcessedSize - LastProgress) >= 1024 then
      begin
        LastProgress := ProcessedSize;
        CurrentKB := ProcessedSize div 1024;
        Percent := (ProcessedSize / FileSize) * 100;

        if Assigned(FOnProgress) then
          FOnProgress(Self, CurrentKB, MaxKB, Percent);
      end;
    end;
  finally
    Row.Free;
    Lines.Free;
  end;
end;


function TCSVFile.IsCellChanged(Row, Col: Integer): Boolean;
var
  I: Integer;
begin
  // Durchsuche die Änderungs-Liste nach der angegebenen Zelle
  for I := 0 to FChanges.Count - 1 do
  begin
    if (FChanges[I].Row = Row) and (FChanges[I].Col = Col) then
      Exit(True);
  end;
  Result := False; // Zelle ist nicht geändert
end;



function TCSVFile.Calculate(IsRow: Boolean; Index: Integer; Exclude: array of Integer;  var CSVPos: TCSVPos; CalcType: TCalculationType=ctSum): Double;
var
  Values: TArray<String>;  // Verwende ein dynamisches Array, um die Werte zu speichern
  I, ColOrRow: Integer;
  IsExcluded, HasHeader: Boolean;
begin
  Values := [];
  HasHeader := FUseHeader;  // Berücksichtige, ob ein Header vorhanden ist

  // Überprüfen, ob die Berechnung für eine Zeile oder Spalte durchgeführt werden soll
  if IsRow then
  begin
    // Berechnung für die Zeile
    for I := 0 to FData[Index].Count - 1 do
    begin
      IsExcluded := False;

      // Überprüfen, ob der aktuelle Index in der Exclude-Liste ist
      for ColOrRow in Exclude do
      begin
        if I = ColOrRow then
        begin
          IsExcluded := True;
          Break;  // Wenn ausgeschlossen, nicht weiter prüfen
        end;
      end;

      if not IsExcluded then
      begin
        // Füge den Wert zum Array hinzu (Header in der ersten Zeile ignorieren, falls gesetzt)
        if not (HasHeader and (Index = 0)) then
          AddValueToArray(Values, GetCell(Index, I));
      end;
    end;
  end
  else
  begin
    // Berechnung für die Spalte
    for I := 0 to FData.Count - 1 do
    begin
      IsExcluded := False;

      // Überprüfen, ob der aktuelle Index in der Exclude-Liste ist
      for ColOrRow in Exclude do
      begin
        if I = ColOrRow then
        begin
          IsExcluded := True;
          Break;
        end;
      end;

      if not IsExcluded then
      begin
        // Füge den Wert zum Array hinzu (Header in der ersten Zeile ignorieren, falls gesetzt)
        if not (HasHeader and (I = 0)) then
          AddValueToArray(Values, GetCell(I, Index));
      end;
    end;
  end;

  // Je nach Berechnungstyp rufe die entsprechende Funktion auf
  CSVPos.X := Index;
  case CalcType of
    ctSum: Result := Calc_Sum(Values);           // Berechnung der Summe
    ctCount: Result := Calc_Count(Values);       // Zählen der Elemente
    ctAverage: Result := Calc_Average(Values);   // Berechnung des Durchschnitts
    ctMin: Result := Calc_Min(Values,CSVPos);           // Berechnung des Minimums
    ctMax: Result := Calc_Max(Values,CSVPos);           // Berechnung des Maximums
    ctProduct: Result := Calc_Product(Values);   // Berechnung des Produkts
    // ctMedian: Result := Calc_Median(Values);     // Berechnung des Medians
  else
    Result := 0;  // Fallback, wenn kein gültiger CalcType gewählt wurde
  end;
end;


// Funktion zum Hinzufügen von Werten zum dynamischen Array
procedure TCSVFile.AddValueToArray(var Values: TArray<String>; const CellStrValue: String);
begin
  // Einfach den String-Wert (ohne Konvertierung) hinzufügen
  SetLength(Values, Length(Values) + 1);
  Values[High(Values)] := Trim(CellStrValue);  // Füge den Zellwert als String hinzu
end;


procedure TCSVFile.SetCell(Row, Col: Integer; const Value: string);
begin
  // Überprüfen, ob die Zelle in FData existiert und geändert werden kann
  if (Row >= 0) and (Row < FData.Count) and (Col >= 0) and (Col < FData[Row].Count) then
  begin
    // Wert in FData ändern
    SetCellChanged(Row,Col,True);
    FData[Row][Col] := Value;
    // Prüfen, ob FGrid zugewiesen ist und die Grenzen des Grids passen

    if Assigned( FOutput ) then begin
//        FOutput.UpdateCell() Row,Col);
//        FOutput.SelectFocus(Row,Col);
    end;


  end
  else
    raise Exception.CreateFmt('Data Cell [%d, %d] out of range', [Row, Col]);
end;

procedure TCSVFile.SetCellChanged(Row, Col: Integer; const Value: Boolean);
var
  Change: TCellChange;
  oldValue : String;
  I: Integer;
begin
  if Value then begin

    if GetCellChanged(Row, Col) then begin
      Change := GetCellChange(Row,Col);
      OldValue := FData[Row][Col];
      Change.History.Add(OldValue);
    end
    else begin
      Change.Row := Row;
      Change.Col := Col;
      Change.History := TStringList.Create;

      OldValue := FData[Row][Col];
      Change.History.Add(OldValue); // Füge den alten Wert zur Historie hinzu

      FChanges.Add(Change);

    end;





    exit;

    // Wenn Value true ist, dann füge die Änderung hinzu, falls sie nicht existiert
    if not GetCellChanged(Row, Col) then
    begin
      Change.Row := Row;
      Change.Col := Col;
      Change.History := TStringList.Create;

      OldValue := Cell[Row,Col];
      Change.History.Add(OldValue); // Füge den alten Wert zur Historie hinzu

      FChanges.Add(Change);

    end
    else begin


    end;
  end
  else
  begin
    // Wenn Value false ist, entferne die Änderung
    for I := 0 to FChanges.Count - 1 do
    begin
      if (FChanges[I].Row = Row) and (FChanges[I].Col = Col) then
      begin
        FChanges.Delete(I);
        Break;
      end;
    end;
  end;
end;


function TCSVFile.GetCellChanged(Row, Col: Integer): Boolean;
var
  I: Integer;
begin
  // Überprüfen, ob die Zelle in der Liste der Änderungen vorhanden ist
  Result := False;
  for I := 0 to FChanges.Count - 1 do
  begin
    if (FChanges[I].Row = Row) and (FChanges[I].Col = Col) then
    begin
      Result := True; // Die Zelle ist geändert
      Break;
    end;
  end;
end;

function TCSVFile.GetCellChange(Row, Col: Integer):TCellChange;
var
  i: Integer;
begin
  // Standardwert zurückgeben
  Result.Row := -1; // Ungültige Zeile
  Result.Col := -1; // Ungültige Spalte
  Result.History := nil; // Historie ist nil

  // Durchsuche die Änderungen, um den entsprechenden Eintrag zu finden
  for i := 0 to FChanges.Count - 1 do
  begin
    if (FChanges[i].Row = Row) and (FChanges[i].Col = Col) then
    begin
      // Finde den Eintrag und gib ihn zurück
      Result := FChanges[i];
      Exit;
    end;
  end;

  // Wenn kein Eintrag gefunden wurde, wird ein leeres TCellChange zurückgegeben


end;





procedure TCSVFile.RemoveRange(StartIndex, Count: Integer);
var
  I: Integer;
begin
  for I := StartIndex + Count - 1 downto StartIndex do
    FData.Delete(I);
end;

procedure TCSVFile.Assign(Source: TList<TStringList>);
var
  I: Integer;
  NewStringList: TStringList;
begin
  // Clear the current list (FData)
  FData.Clear;

  // Copy each element from the Source list to FData
  for I := 0 to Source.Count - 1 do
  begin
    // Create a new TStringList and assign the content from the source
    NewStringList := TStringList.Create;
    try
      NewStringList.Assign(Source[I]);  // TStringList.Assign copies all strings
      FData.Add(NewStringList);         // Add to FData
    except
      NewStringList.Free;  // Make sure we free the memory in case of an error
      raise;
    end;
  end;
end;


procedure TCSVFile.CopyToList(Col: Integer; TargetList: TStringList; IgnoreHeader: Boolean = False);
var
  I: Integer;
begin
  // Clear the target list
  TargetList.Clear;

  // Start from the first or second row based on IgnoreHeader
  for I := Ord(IgnoreHeader) to FData.Count - 1 do
  begin
    // Ensure the column exists in the current row
    if Col < FData[I].Count then
      TargetList.Add(FData[I][Col]);  // Add the column value to the target list
  end;
end;


function TCSVFile.GetSeparatorFromString(Text: String; MaxBytes: Integer=4096): Char;
var
  SeparatorCounts: array[0..2] of Integer; // Indizes: 0 = ',', 1 = ';', 2 = '\t'
  Delimiters: array[0..2] of Char; // Array der Delimiter
  i, MaxIndex, TextLength: Integer;
  MaxCount: Integer;
  SubText: String;
begin
  Result := #0; // Standardwert, falls kein Trennzeichen gefunden wird
  Delimiters[0] := ',';
  Delimiters[1] := ';';
  Delimiters[2] := #9; // Tabulator

  // Zähle die Vorkommen der Delimiter
  for i := 0 to High(Delimiters) do
    SeparatorCounts[i] := 0;

  // Begrenze den Text auf MaxBytes Zeichen
  TextLength := Length(Text);
  if TextLength > MaxBytes then
    SubText := Copy(Text, 1, MaxBytes)
  else
    SubText := Text;

  // Zähle die Vorkommen jedes Delimiters im SubText
  for i := 0 to High(Delimiters) do
  begin
    SeparatorCounts[i] := Length(SubText) - Length(StringReplace(SubText, Delimiters[i], '', [rfReplaceAll]));
  end;

  // Bestimme das Trennzeichen basierend auf den Zählungen
  MaxCount := -1;
  MaxIndex := -1;
  for i := 0 to High(SeparatorCounts) do
  begin
    if SeparatorCounts[i] > MaxCount then
    begin
      MaxCount := SeparatorCounts[i];
      MaxIndex := i;
    end;
  end;

  // Gebe das häufigste Trennzeichen zurück, falls es eines gibt
  if MaxIndex <> -1 then
    Result := Delimiters[MaxIndex];
end;

function TCSVFile.GetSeparatorFromFile(Filename: String; MaxBytes: Integer = 4096): Char;
var
  Content: string;
begin
  Content := ReadFileContent(Filename, MaxBytes);  // Lese den Dateiinhalt
  Result := GetSeparatorFromString(Content, MaxBytes);  // Analysiere den Separator
end;



function TCSVFile.GetEncoding(const Content: string): TEncoding;
begin
  if (Length(Content) >= 3) and (Content[1] = #$EF) and (Content[2] = #$BB) and (Content[3] = #$BF) then
    Result := TEncoding.UTF8 // UTF-8 BOM
  else if (Length(Content) >= 2) and (Content[1] = #$FF) and (Content[2] = #$FE) then
    Result := TEncoding.Unicode // UTF-16 LE BOM
  else if (Length(Content) >= 2) and (Content[1] = #$FE) and (Content[2] = #$FF) then
    Result := TEncoding.BigEndianUnicode // UTF-16 BE BOM
  else
    Result := TEncoding.Default;  // Fallback: Standard-Encoding (wenn keine BOM gefunden)
end;


function TCSVFile.GetEncoding(Filename: String; MaxBytes: Integer): TEncoding;
var
  RawContent: string;
  DetectedEncoding: TEncoding;
begin
  // Lese den Inhalt der Datei (limitiert auf MaxBytes)
  RawContent := ReadFileContent(Filename, MaxBytes);

  // Erkenne das Encoding basierend auf dem Inhalt
  DetectedEncoding := GetEncoding(RawContent);

  // Gib das erkannte Encoding zurück
  Result := DetectedEncoding;
end;

function TCSVFile.GetQuoteFromString(Separator: Char; Text: String; MaxBytes: Integer = 4096): Char;
var
  QuoteCounts: array[0..1] of Integer; // Indizes: 0 = '"', 1 = '''
  Quotes: array[0..1] of Char; // Array der Quotes
  i, MaxIndex, TextLength: Integer;
  MaxCount: Integer;
  SubText: String;
  InQuotedField: Boolean;
begin
  Result := #0; // Standardwert, falls kein Quotenzeichen gefunden wird

  Quotes[0] := '"';
  Quotes[1] := ''''; // Einfaches Anführungszeichen

  // Zähle die Vorkommen der Quotes
  for i := 0 to High(Quotes) do
    QuoteCounts[i] := 0;

  // Begrenze den Text auf MaxBytes Zeichen
  TextLength := Length(Text);
  if TextLength > MaxBytes then
    SubText := Copy(Text, 1, MaxBytes)
  else
    SubText := Text;

  InQuotedField := False;

  // Gehe den Text durch und zähle Quotenzeichen, die Felder umschließen
  for i := 1 to Length(SubText) do
  begin
    // Wenn wir auf einen Separator oder den Anfang treffen
    if (SubText[i] = Separator) or (i = 1) then
    begin
      // Prüfe auf Quotenzeichen nach einem Separator oder am Anfang des Textes
      if (i < Length(SubText)) and ((SubText[i+1] = Quotes[0]) or (SubText[i+1] = Quotes[1])) then
      begin
        InQuotedField := True;

        // Zähle das Quotenzeichen
        if SubText[i+1] = Quotes[0] then
          Inc(QuoteCounts[0])
        else if SubText[i+1] = Quotes[1] then
          Inc(QuoteCounts[1]);
      end;
    end
    else if InQuotedField and (SubText[i] = Separator) then
      InQuotedField := False;
  end;

  // Bestimme das häufigste Quotenzeichen basierend auf den Zählungen
  MaxCount := -1;
  MaxIndex := -1;
  for i := 0 to High(QuoteCounts) do
  begin
    if QuoteCounts[i] > MaxCount then
    begin
      MaxCount := QuoteCounts[i];
      MaxIndex := i;
    end;
  end;

  // Gebe das häufigste Quotenzeichen zurück, falls es eines gibt
  if MaxIndex <> -1 then
    Result := Quotes[MaxIndex];
end;

function TCSVFile.GetQuoteFromFile(Separator: Char; Filename: String; MaxBytes: Integer = 4096): Char;
var
  FileContent: string;
begin
  // Lese die Datei in einen String
  FileContent := ReadFileContent(Filename, MaxBytes);

  // Verwende die existierende Funktion für Strings
  Result := GetQuoteFromString(Separator, FileContent, MaxBytes);
end;

procedure TCSVFile.SortExt(Column: Integer; Descending: Boolean = false; IgnoreHeader: Boolean = true; DataType: TDataType = dtString);
var
  DataToSort: TList<TStringList>;
  I: Integer;
begin
  DataToSort := TList<TStringList>.Create;

  try
    if IgnoreHeader then
    begin
      for I := 1 to FData.Count - 1 do
        DataToSort.Add(FData[I]);
    end
    else
    begin
      DataToSort.AddRange(FData);
    end;

    DataToSort.Sort(
      TComparer<TStringList>.Construct(
        function(const Left, Right: TStringList): Integer
        var
          LeftValue, RightValue: string;
          LeftInt, RightInt: Integer;
          LeftDouble, RightDouble: Double;
          LeftDate, RightDate: TDateTime;
        begin
          case DataType of
            dtString:
            begin
              LeftValue := Left[Column];
              RightValue := Right[Column];
              if Descending then
                Result := CompareStr(RightValue, LeftValue)
              else
                Result := CompareStr(LeftValue, RightValue);
            end;
            dtInteger:
            begin


              LeftInt := StrToInt(Left[Column]);
              RightInt := StrToInt(Right[Column]);
              if Descending then
                Result := RightInt - LeftInt
              else
                Result := LeftInt - RightInt;
            end;
            dtDouble:
            begin
              LeftDouble := StrToFloat(Left[Column]);
              RightDouble := StrToFloat(Right[Column]);
              if Descending then
                Result := Round((RightDouble - LeftDouble) * 10000) // Multiplizieren für Genauigkeit
              else
                Result := Round((LeftDouble - RightDouble) * 10000);
            end;
            dtDate, dtDateTime:
            begin
              LeftDate := StrToDateTime(Left[Column]);
              RightDate := StrToDateTime(Right[Column]);
              if Descending then
                Result := CompareValue(RightDate, LeftDate)
              else
                Result := CompareValue(LeftDate, RightDate);
            end;
            dtCurrency:
            begin
              LeftDouble := StrToFloat(Left[Column]);
              RightDouble := StrToFloat(Right[Column]);
              if Descending then
                Result := Round((RightDouble - LeftDouble) * 10000) // Multiplizieren für Genauigkeit
              else
                Result := Round((LeftDouble - RightDouble) * 10000);
            end;
          else
            // Default-Fall für dtNone oder unbekannte Typen
            LeftValue := Left[Column];
            RightValue := Right[Column];
            if Descending then
              Result := CompareStr(RightValue, LeftValue)
            else
              Result := CompareStr(LeftValue, RightValue);
          end;
        end
      )
    );

    if IgnoreHeader then
    begin
      for I := 1 to FData.Count - 1 do
        FData[I] := DataToSort[I - 1];
    end
    else
    begin
      Assign(DataToSort);
    end;

  finally
    DataToSort.Free;
  end;
end;


procedure TCSVFile.Sort(Column: Integer; Descending: Boolean = false; IgnoreHeader: Boolean = true);
var
  DataToSort: TList<TStringList>; // Liste, die die Daten zum Sortieren speichert
  I: Integer;
begin
  // Erstelle eine neue Liste, um die zu sortierenden Daten zu speichern
  DataToSort := TList<TStringList>.Create;

  try
    // Wenn IgnoreHeader wahr ist, beginne ab der zweiten Zeile (Index 1)
    if IgnoreHeader then
    begin
      for I := 1 to FData.Count - 1 do  // Überspringe die erste Zeile (Header)
        DataToSort.Add(FData[I]);
    end
    else
    begin
      // Andernfalls kopiere alle Zeilen in die neue Liste
      DataToSort.AddRange(FData);
    end;

    // Sortiere die Daten
    DataToSort.Sort(
      TComparer<TStringList>.Construct(
        function(const Left, Right: TStringList): Integer
        var
          LeftValue, RightValue: string; // Werte zum Vergleichen
          LeftNum, RightNum: Integer; // Konvertierte numerische Werte
        begin
          LeftValue := Left[Column]; // Wert der linken Zeile
          RightValue := Right[Column]; // Wert der rechten Zeile

          // Überprüfe, ob die Werte als Ganzzahlen geparst werden können
          if TryStrToInt(LeftValue, LeftNum) and TryStrToInt(RightValue, RightNum) then
          begin
            if Descending then
              Result := RightNum - LeftNum  // Absteigende Reihenfolge
            else
              Result := LeftNum - RightNum;  // Aufsteigende Reihenfolge
          end
          else
          begin
            // Standard String-Vergleich
            if Descending then
              Result := CompareStr(RightValue, LeftValue)
            else
              Result := CompareStr(LeftValue, RightValue);
          end;
        end
      )
    );

    // Ersetze die sortierten Daten zurück in FData
    if IgnoreHeader then
    begin
      // Behalte die Header-Zeile und füge die sortierten Daten hinzu
      for I := 1 to FData.Count - 1 do
        FData[I] := DataToSort[I - 1];  // Füge die sortierten Daten ab Index 1 wieder ein
    end
    else
    begin
      // Ersetze die gesamte Liste, wenn kein Header ignoriert wurde
      Assign(DataToSort);
    end;

  finally
    DataToSort.Free;  // Befreie die temporäre Liste
  end;
end;



Procedure TCSVFile.SetOuput(Value: TFrontend_Output_Base);
begin
  if assigned(Value) then begin
    FOutput := Value;
    Value.Initialize;
  end;

end;




function TCSVFile.HasChanges: Boolean;
begin
  Result := FChanges.Count > 0; // Gibt true zurück, wenn Änderungen vorhanden sind
end;

function TCSVFile.ChangesCount: Integer;
begin
  Result := FChanges.Count; // Gibt Anzahlder Änderungen zurück
end;

procedure TCSVFile.ClearChanges;
begin
  FChanges.Clear; // Löscht die Liste der Änderungen
end;

function TCSVFile.GetHistory(ARow,ACol : Integer):TStringlist;
Var
  Change : TCellChange;
  History : TStringlist;
begin
  History := nil;

  if Changed[ARow,ACol] then begin
    Change := GetCellChange(ARow,ACol);
    if assigned(Change.History) then
      History := Change.History;
  end;

  result := History;
end;


procedure TCSVFile.ExportAsXML(const Filename: string);
var
  I, J: Integer;
  XMLDoc: TStringList;
begin
  XMLDoc := TStringList.Create;
  try
    XMLDoc.Add('<?xml version="1.0" encoding="UTF-8"?>');
    XMLDoc.Add('<Data>');

    // Loop through rows
    for I := 0 to FData.Count - 1 do
    begin
      XMLDoc.Add('  <Row>');

      // Loop through columns
      for J := 0 to FData[I].Count - 1 do
      begin
        XMLDoc.Add(Format('    <Column%d>%s</Column%d>', [J, FData[I][J], J]));
      end;

      XMLDoc.Add('  </Row>');
    end;

    XMLDoc.Add('</Data>');
    // Save to file
    XMLDoc.SaveToFile(Filename);
  finally
    XMLDoc.Free;
  end;
end;


procedure TCSVFile.ExportAsJSON(const Filename: string);
var
  JSONArr: TJSONArray;
  JSONObj: TJSONObject;
  Header: TStringList;
  I, J: Integer;
  JSONStr: TStringList;
  FormattedJSON: string;
begin
  JSONArr := TJSONArray.Create;
  JSONStr := TStringList.Create;
  try
    // Wenn FUseHeader = True, nimm die erste Zeile als Header
    if FUseHeader and (FData.Count > 0) then
    begin
      Header := FData[0];  // Erste Zeile als Header verwenden

      // Daten ab der zweiten Zeile exportieren
      for I := 1 to FData.Count - 1 do
      begin
        JSONObj := TJSONObject.Create;
        for J := 0 to FData[I].Count - 1 do
        begin
          // Header als Schlüssel und entsprechende Spalte als Wert
          if J < Header.Count then // Überprüfen, ob der Header für den aktuellen Index vorhanden ist
            JSONObj.AddPair(Header[J], FData[I][J]); // Fügt den Wert hinzu
        end;
        JSONArr.AddElement(JSONObj);
      end;
    end
    else
    begin
      // Ohne Header, verwende Standard-Schlüssel (Col1, Col2, ...)
      for I := 0 to FData.Count - 1 do
      begin
        JSONObj := TJSONObject.Create;
        for J := 0 to FData[I].Count - 1 do
        begin
          JSONObj.AddPair('Col' + IntToStr(J + 1), FData[I][J]); // Fügt den Wert hinzu
        end;
        JSONArr.AddElement(JSONObj);
      end;
    end;

    // JSON in eine formatierte Zeichenfolge umwandeln
    FormattedJSON := JSONArr.ToString;

    // Einrückung und Zeilenumbrüche hinzufügen
    FormattedJSON := StringReplace(FormattedJSON, '[', '[' + sLineBreak + #9, [rfReplaceAll]);
    FormattedJSON := StringReplace(FormattedJSON, ']', sLineBreak + ']', [rfReplaceAll]);
    FormattedJSON := StringReplace(FormattedJSON, '},{', '},' + sLineBreak + #9 + '{', [rfReplaceAll]);
    FormattedJSON := StringReplace(FormattedJSON, '{', '{' + sLineBreak + #9, [rfReplaceAll]);
    FormattedJSON := StringReplace(FormattedJSON, '}', sLineBreak + #9 + '}', [rfReplaceAll]); // Tabulator vor der schließenden Klammer hinzufügen
    FormattedJSON := StringReplace(FormattedJSON, '},',   '},', [rfReplaceAll]); // Tabulator vor der schließenden Klammer und Komma hinzufügen
    FormattedJSON := StringReplace(FormattedJSON, ',', ',' + sLineBreak + #9, [rfReplaceAll]);

    // Speichern des formatierten JSON-Strings in die Datei
    JSONStr.Text := FormattedJSON;
    JSONStr.SaveToFile(Filename);
  finally
    JSONArr.Free;
    JSONStr.Free;
  end;
end;





procedure TCSVFile.ExportAsXLSX(const Filename: string);
var
  ExcelApp, Workbook, Worksheet: OLEVariant;
  I, J: Integer;
begin
  try
    // Prüfen, ob die Datei bereits existiert, und sie löschen
    if FileExists(Filename) then
      DeleteFile(Filename);  // Datei löschen, um Überschreiben ohne Rückfrage zu ermöglichen

    // Excel starten
    ExcelApp := CreateOleObject('Excel.Application');
    ExcelApp.Visible := False;  // Excel unsichtbar im Hintergrund

    // Neues Workbook erstellen
    Workbook := ExcelApp.Workbooks.Add;
    Worksheet := Workbook.Worksheets[1];  // Erste Arbeitsmappe auswählen

    // Daten aus der CSV-Datei in die Excel-Arbeitsmappe schreiben
    for I := 0 to FData.Count - 1 do
    begin
      for J := 0 to FData[I].Count - 1 do
      begin
        Worksheet.Cells[I + 1, J + 1] := FData[I][J];  // Werte in Zellen schreiben
      end;
    end;

    // XLSX-Datei speichern (überschreiben, falls sie existiert)
    Workbook.SaveAs(Filename, 51);  // 51 = xlOpenXMLWorkbook (XLSX Format)
    Workbook.Close(False);  // Arbeitsmappe schließen ohne zu speichern
  finally
    // Excel beenden
    ExcelApp.Quit;
    ExcelApp := Unassigned;
  end;
end;



function TCSVFile.GetLineBreak: string;
begin
  case FLineBreakStyle of
    lbsWindows: Result := #13#10;  // Windows line break
    lbsMac: Result := #13;         // Mac OS classic line break
    lbsUnix: Result := #10;        // Unix (Linux, macOS) line break
    lbsNone: Result := '';         // No specific line break
  else
    Result := '';                  // Default: no line break
  end;
end;


procedure TCSVFile.ProcessRecord(Fields: TStringList);
var
  NewRecord: TStringList;
  i: Integer;
begin
  NewRecord := TStringList.Create;
  try
    for i := 0 to Fields.Count - 1 do
      NewRecord.Add(Fields[i]);
    FData.Add(NewRecord);
  except
    NewRecord.Free;
    raise;
  end;
end;


{
procedure TCSVFile.ParseMultilineCSV(const FileName: string);
var
  FileStream: TFileStream;
  Reader: TStreamReader;
  Line, Field: string;
  InQuotes: Boolean;
  Fields: TStringList;
  ch: Char; // Use ch instead of Char to avoid compiler error
  LineBreak: string;

begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    // Use the specified encoding (FEncoding) when creating the reader
    Reader := TStreamReader.Create(FileStream, FEncoding);
    Fields := TStringList.Create;
    try
      InQuotes := False;
      Field := '';
      LineBreak := GetLineBreak;

      while not Reader.EndOfStream do
      begin
        Line := Reader.ReadLine;

        // Adjust for line breaks specific to FLineBreakStyle
        if (FLineBreakStyle <> lbsNone) and (Pos(LineBreak, Line) > 0) then
        begin
          // Replace custom line breaks
          Line := StringReplace(Line, LineBreak, '', [rfReplaceAll]);
        end;

        for ch in Line do
        begin
          if ch = FQuotes then
          begin
            // Toggle the InQuotes flag for quoted fields
            InQuotes := not InQuotes;
            Field := Field + ch;
          end
          else if ch = FSeparator then
          begin
            if not InQuotes then
            begin
              // End of a field when not inside quotes
              Fields.Add(Field);
              Field := '';
            end
            else
              Field := Field + ch; // Inside quotes, keep the separator as part of the field
          end
          else if (ch = #13) or (ch = #10) then
          begin
            // Handle line breaks inside quoted fields properly
            if InQuotes then
            begin
              // Add the line break character inside the quoted field (multiline support)
              Field := Field + ch;
            end
            else
            begin
              // If not in quotes, treat newlines as record separators
              Fields.Add(Field);
              ProcessRecord(Fields); // Process the current record
              Fields.Clear;
              Field := '';
            end;
          end
          else
          begin
            // Any other character gets appended to the current field
            Field := Field + ch;
          end;
        end;

        // After processing the line, if inside quotes, read the next line and continue
        if InQuotes then
        begin
          // Add a newline to preserve multiline within quotes
          Field := Field + LineBreak;
        end
        else
        begin
          // End of line, process current field if not inside quotes
          if Field <> '' then
            Fields.Add(Field);

          if Fields.Count > 0 then
            ProcessRecord(Fields); // Process the collected fields as a record

          Fields.Clear;
          Field := '';
        end;
      end;

      // Handle any remaining data at the end of the file
      if Field <> '' then
        Fields.Add(Field);

      if Fields.Count > 0 then
        ProcessRecord(Fields);

    finally
      Fields.Free;
      Reader.Free;
    end;
  finally
    FileStream.Free;
  end;
end;
}



procedure TCSVFile.LoadMultilineCSVFromFile(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    ParseMultilineCSV(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TCSVFile.LoadMultilineCSVFromString(const CSVText: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(CSVText);
  try
    // Convert the CSVText string to a byte array and write it to the memory stream
//    MemoryStream.Write(Pointer(CSVText)^, Length(CSVText));
    Stream.Position := 0; // Set the position to the start of the stream

    ParseMultilineCSV(Stream);
  finally
    Stream.Free;
  end;
end;



procedure TCSVFile.ParseFastCSVFromFile(const FileName: string);
var
  Reader: TStreamReader;
  Line: string;
  Fields: TStringList;
  FileStream: TFileStream;
begin
  try
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    Reader := TStreamReader.Create(FileStream, FEncoding);
    Fields := TStringList.Create;

    Fields.Delimiter := FSeparator;
    Fields.StrictDelimiter := True;

    while not Reader.EndOfStream do
    begin
      Line := Reader.ReadLine;

      // Schnellere Methode zum Aufteilen der Zeile in Felder
      Fields.DelimitedText := Line;


      // Process the current record
      ProcessRecord(Fields);
    end;
  except
  end;

  if assigned(Fields) then Fields.Free;
  if assigned(Reader) then Reader.Free;
  if assigned(FileStream) then FileStream.Free;
end;

{ Probleme mit Leerern Feldern
procedure TCSVFile.ParseMultilineCSV(Stream: TStream);
var
  Reader: TStreamReader;
  Line, Field: string;
  InQuotes: Boolean;
  Fields: TStringList;
  ch: Char;

begin
  Reader := TStreamReader.Create(Stream, FEncoding);
  Fields := TStringList.Create;
  try
    InQuotes := False;
    Field := '';

    while not Reader.EndOfStream do
    begin
      Line := Reader.ReadLine;

      for ch in Line do
      begin
        if ch = FQuotes then
        begin
          // Toggle the InQuotes flag for quoted fields
          InQuotes := not InQuotes;
        end
        else if ch = FSeparator then
        begin
          if not InQuotes then
          begin
            // End of a field when not inside quotes
            Fields.Add(Field); // Add the field directly, without removing quotes
            Field := '';
          end
          else
          begin
            Field := Field + ch; // Inside quotes, keep the separator as part of the field
          end;
        end
        else if (ch = #13) or (ch = #10) then
        begin
          if InQuotes then
          begin
            Field := Field + ch; // Line breaks inside quotes
          end
          else
          begin
            // End of a record
            Fields.Add(Field); // Add the last field
            ProcessRecord(Fields); // Process the current record
            Fields.Clear;
            Field := '';
          end;
        end
        else
        begin
          Field := Field + ch; // Normal character
        end;
      end;

      // After finishing reading the line, add any remaining field
      if InQuotes then
      begin
        Field := Field + sLineBreak; // Add line break to field if still in quotes
      end
      else
      begin
        if Field <> '' then
          Fields.Add(Field); // Add the last field

        if Fields.Count > 0 then
          ProcessRecord(Fields); // Process the collected fields as a record

        Fields.Clear;
        Field := '';
      end;
    end;

    // Handle any remaining data at the end of the file
    if Field <> '' then
      Fields.Add(Field);

    if Fields.Count > 0 then
      ProcessRecord(Fields);

  finally
    Fields.Free;
    Reader.Free;
  end;
end;
}


procedure TCSVFile.ParseMultilineCSV(Stream: TStream);
var
  Reader: TStreamReader;
  Line, Field: string;
  InQuotes: Boolean;
  Fields: TStringList;
  ch: Char;

begin
  Reader := TStreamReader.Create(Stream, FEncoding);
  Fields := TStringList.Create;
  try
    InQuotes := False;
    Field := '';

    while not Reader.EndOfStream do
    begin
      Line := Reader.ReadLine;

      for ch in Line do
      begin
        if ch = FQuotes then
        begin
          // Toggle the InQuotes flag for quoted fields
          InQuotes := not InQuotes;
        end
        else if ch = FSeparator then
        begin
          if not InQuotes then
          begin
            // End of a field when not inside quotes
            Fields.Add(Field); // Add the field directly, even if empty
            Field := '';
          end
          else
          begin
            Field := Field + ch; // Inside quotes, keep the separator as part of the field
          end;
        end
        else if (ch = #13) or (ch = #10) then
        begin
          if InQuotes then
          begin
            Field := Field + ch; // Line breaks inside quotes
          end
          else
          begin
            // End of a record
            Fields.Add(Field); // Add the last field, even if empty
            ProcessRecord(Fields); // Process the current record
            Fields.Clear;
            Field := '';
          end;
        end
        else
        begin
          Field := Field + ch; // Normal character
        end;
      end;

      // After finishing reading the line, add any remaining field
      if InQuotes then
      begin
        Field := Field + sLineBreak; // Add line break to field if still in quotes
      end
      else
      begin
        Fields.Add(Field); // Add the last field, even if empty

        if Fields.Count > 0 then
          ProcessRecord(Fields); // Process the collected fields as a record

        Fields.Clear;
        Field := '';
      end;
    end;

    // Handle any remaining data at the end of the file
    if Field <> '' then
      Fields.Add(Field)
    else if Fields.Count > 0 then
      Fields.Add(''); // Handle the case where the last field is empty

    if Fields.Count > 0 then
      ProcessRecord(Fields);

  finally
    Fields.Free;
    Reader.Free;
  end;
end;








procedure TCSVFile.ParseMultilineCSVold(const FileName: string);
var
  FileStream: TFileStream;
  Reader: TStreamReader;
  Line, Field: string;
  InQuotes: Boolean;
  Fields: TStringList;
  ch: Char;
  LineBreak: string;

  // Helper function to remove surrounding quotes from a field
  function RemoveSurroundingQuotes(const AField: string): string;
  begin
    Result := AField;
    if (Length(AField) > 1) and (AField[1] = FQuotes) and (AField[Length(AField)] = FQuotes) then
    begin
      // Remove leading and trailing quotes
      Result := Copy(AField, 2, Length(AField) - 2);
    end;
  end;

begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Reader := TStreamReader.Create(FileStream, FEncoding);
    Fields := TStringList.Create;
    try
      InQuotes := False;
      Field := '';
      LineBreak := GetLineBreak;

      while not Reader.EndOfStream do
      begin
        Line := Reader.ReadLine;

        // Adjust for line breaks specific to FLineBreakStyle
        if (FLineBreakStyle <> lbsNone) and (Pos(LineBreak, Line) > 0) then
        begin
          Line := StringReplace(Line, LineBreak, '', [rfReplaceAll]);
        end;

        for ch in Line do
        begin
          if ch = FQuotes then
          begin
            // Toggle the InQuotes flag for quoted fields
            InQuotes := not InQuotes;
            Field := Field + ch;
          end
          else if ch = FSeparator then
          begin
            if not InQuotes then
            begin
              // End of a field when not inside quotes
              // Remove surrounding quotes before adding to Fields
              Fields.Add(RemoveSurroundingQuotes(Field));
              Field := '';
            end
            else
              Field := Field + ch; // Inside quotes, keep the separator as part of the field
          end
          else if (ch = #13) or (ch = #10) then
          begin
            // Handle line breaks inside quoted fields properly
            if InQuotes then
            begin
              Field := Field + ch;
            end
            else
            begin
              Fields.Add(RemoveSurroundingQuotes(Field));
              ProcessRecord(Fields); // Process the current record
              Fields.Clear;
              Field := '';
            end;
          end
          else
          begin
            Field := Field + ch;
          end;
        end;

        if InQuotes then
        begin
          Field := Field + LineBreak;
        end
        else
        begin
          if Field <> '' then
            Fields.Add(RemoveSurroundingQuotes(Field));

          if Fields.Count > 0 then
            ProcessRecord(Fields); // Process the collected fields as a record

          Fields.Clear;
          Field := '';
        end;
      end;

      // Handle any remaining data at the end of the file
      if Field <> '' then
        Fields.Add(RemoveSurroundingQuotes(Field));

      if Fields.Count > 0 then
        ProcessRecord(Fields);

    finally
      Fields.Free;
      Reader.Free;
    end;
  finally
    FileStream.Free;
  end;
end;



procedure TCSVFile.SwapCol(Col1, Col2: Integer);
var
  Row: Integer;
  Temp: string;
begin
  // Sicherstellen, dass die Spaltenwerte innerhalb des gültigen Bereichs liegen
  if (Col1 < 0) or (Col1 >= FData[0].Count) or (Col2 < 0) or (Col2 >= FData[0].Count) then
    raise Exception.CreateFmt('One of the columns (%d, %d) is out of range', [Col1, Col2]);

  // Über alle Zeilen iterieren und die Zellen der beiden Spalten tauschen
  for Row := 0 to FData.Count - 1 do
  begin
    Temp := FData[Row][Col1];
    FData[Row][Col1] := FData[Row][Col2];
    FData[Row][Col2] := Temp;
  end;
end;




procedure TCSVFile.SwapRow(Row1, Row2: Integer);
var
  TempRow: TStringList;
begin
  // Sicherstellen, dass die Zeilenwerte innerhalb des gültigen Bereichs liegen
  if (Row1 < 0) or (Row1 >= FData.Count) or (Row2 < 0) or (Row2 >= FData.Count) then
    raise Exception.CreateFmt('One of the rows (%d, %d) is out of range', [Row1, Row2]);

  // Temporäre Kopie der ersten Zeile erstellen
  TempRow := TStringList.Create;
  try
    TempRow.Assign(FData[Row1]);

    // Die Zeilen tauschen
    FData[Row1].Assign(FData[Row2]);
    FData[Row2].Assign(TempRow);
  finally
    TempRow.Free;
  end;
end;




// FRONTEND OUTPUT
constructor TFrontend_Output_Base.Create(ACSVFile: TCSVFile);
begin
  // Setze das CSVFile
  FCSVFile := ACSVFile;
end;






end.

