unit CSVTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CSVFile, Vcl.StdCtrls, Vcl.Grids,
  SynEdit, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ActnMenus, Vcl.Menus,
  Vcl.ComCtrls, RzTabs, Vcl.ExtCtrls, System.Actions, Vcl.ActnList,
  Vcl.ButtonStylesAttributes, Vcl.StyledButton, Vcl.CategoryButtons,
  Vcl.StyledCategoryButtons, Vcl.StyledTaskDialog,

  class_Frontend_Output_TStringGrid, Vcl.ControlList, Vcl.NumberBox;

type
  TForm9 = class(TForm)
    ActionList1: TActionList;
    ac_open: TAction;
    Panel2: TPanel;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Button3: TButton;
    Aufsteigend: TRadioButton;
    RadioButton1: TRadioButton;
    CSVGrid: TStringGrid;
    Panel1: TPanel;
    Button1: TButton;
    ac_save: TAction;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Panel3: TPanel;
    Label4: TLabel;
    Memo1: TMemo;
    Label5: TLabel;
    Button2: TButton;
    Button5: TButton;
    Button6: TButton;
    ProgressBar1: TProgressBar;
    Label3: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Edit1: TEdit;
    Button7: TButton;
    ac_search: TAction;
    Button8: TButton;
    ac_next: TAction;
    Button9: TButton;
    Button11: TButton;
    Label7: TLabel;
    Button12: TButton;
    Button13: TButton;
    Label8: TLabel;
    CheckBox1: TCheckBox;
    Panel4: TPanel;
    Label9: TLabel;
    Label10: TLabel;
    Button4: TButton;
    Label11: TLabel;
    NumberBox1: TNumberBox;
    Label12: TLabel;
    NumberBox2: TNumberBox;
    procedure FormCreate(Sender: TObject);
    procedure ac_openExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure AufsteigendClick(Sender: TObject);
    procedure ac_saveExecute(Sender: TObject);
    procedure CSVGridClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ac_searchUpdate(Sender: TObject);
    procedure ac_searchExecute(Sender: TObject);
    procedure ac_nextUpdate(Sender: TObject);
    procedure ac_nextExecute(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    CSVFile: TCSVFile;
    Output : TFrontend_Output_TStringGrid;
    Filename : String;
    SelRow,SelCol : Integer;

    Procedure OnBeforeReadCell(RowIndex, ColIndex: Integer; Var Value:String);
    Procedure OnProgress(Sender: TObject; ProcessedKB, MaxKB: Integer; Percent: Double);
    procedure OnFound(Sender: TObject; Position: TCSVPos; Value: String);
    Procedure OpenFile();
    Procedure SaveFile();
    Procedure SetCaption();
    Procedure SetCellValue();
  end;

  function DataTypeToString(DataType: TDataType): string;

var
  Form9: TForm9;

implementation

{$R *.dfm}


function DataTypeToString(DataType: TDataType): string;
begin
  case DataType of
    dtString:   Result := 'String';
    dtInteger:  Result := 'Integer';
    dtDouble:   Result := 'Double';
    dtDateTime: Result := 'DateTime';
  else
    Result := 'Unknown';
  end;
end;




procedure TForm9.FormCreate(Sender: TObject);
begin
  CSVFile := TCSVFile.Create(TEncoding.UTF8,';');
  CSVFile.OnFound := self.OnFound;
  SelRow := -1;
  SelCol := -1;

  Output := TFrontend_Output_TStringGrid.Create(CSVFile, CSVGrid);
  CSVFile.Output := Output;



  Form9.DoubleBuffered := true;
  Panel2.DoubleBuffered := true;
end;

procedure TForm9.FormDestroy(Sender: TObject);
begin
  if assigned(CSVFile) then
    CSVFile.Free;
end;

Procedure TForm9.OnBeforeReadCell(RowIndex, ColIndex: Integer; Var Value:String);
begin
  Value := Value + '#';
end;

Procedure TForm9.OnProgress(Sender: TObject; ProcessedKB, MaxKB: Integer; Percent: Double);
begin
    ProgressBar1.Max := MaxKB;
    ProgressBar1.Position := ProcessedKB;
    ProgressBar1.Invalidate;  // Neuzeichnen der ProgressBar ohne Flackern
end;

procedure TForm9.OnFound(Sender: TObject; Position: TCSVPos; Value: String);
begin
  ShowMessage(Value+': ' + Format('Gefunden bei: Zeile %d, Spalte %d', [Position.Y, Position.X]));
end;

procedure TForm9.RadioButton1Click(Sender: TObject);
begin
  ComboBox1Change(Self);
end;

procedure TForm9.ac_nextExecute(Sender: TObject);
begin
  CSVFile.SearchNext();
end;

procedure TForm9.ac_nextUpdate(Sender: TObject);
begin
  (sender as TAction).Enabled := CSVFile.HasSearchNext;
end;

procedure TForm9.ac_openExecute(Sender: TObject);
begin
  try

    ProgressBar1.Visible := true;
    OpenFile();
    ProgressBar1.Visible := false;

    Label9.Caption := 'Anzahl Zeilen: ' + inttostr( CSVFile.Data.Count ) + ' | ' + 'Anzahl Spalten: ' + inttostr( CSVFile.Data[0].Count );
    Label10.Caption := 'Benötigte Zeit um die Daten zu parsen: ' + FormatMilliseconds( CSVFile.ParseTime );
  except
    On E:Exception do begin
      ShowMessage( E.Message );
    end;
  end;
end;

procedure TForm9.ac_saveExecute(Sender: TObject);
begin
  SaveFile();
end;

procedure TForm9.ac_searchExecute(Sender: TObject);
begin
  CSVFile.AutoSelect := true;
  CSVFile.Search(Edit1.Text);
end;

procedure TForm9.ac_searchUpdate(Sender: TObject);
begin
  (sender as TAction).Enabled :=   assigned(CSVFile) and (CSVFile.Size > 0) and (edit1.Text <> '');
end;

procedure TForm9.AufsteigendClick(Sender: TObject);
begin
  ComboBox1Change(Self);
end;

procedure TForm9.Button11Click(Sender: TObject);
Var
  i : Integer;
  Sel : TGridRect;
begin
  Sel := csvgrid.Selection;
  Application.ProcessMessages;

  i := CSVGrid.Row;
  CSVFile.MoveRowBy(i,1);

  inc(Sel.Top);
  inc(Sel.Bottom);
  CSVFile.Output.Update;
  csvGrid.Selection := Sel;

end;

procedure TForm9.Button12Click(Sender: TObject);
Var
  i : Integer;
  Sel : TGridRect;
begin
  Sel := csvgrid.Selection;
  Application.ProcessMessages;

  i := CSVGrid.Col;
  CSVFile.MoveColBy(i,-1);

  inc(Sel.Left,-1);
  inc(Sel.Right,-1);
  CSVFile.Output.Update;
  csvGrid.Selection := Sel;

end;

procedure TForm9.Button13Click(Sender: TObject);
Var
  i : Integer;
  Sel : TGridRect;
begin
  Sel := csvgrid.Selection;
  Application.ProcessMessages;

  i := CSVGrid.Col;
  CSVFile.MoveColBy(i,1);

  inc(Sel.Left,1);
  inc(Sel.Right,1);
  CSVFile.Output.Update;
  csvGrid.Selection := Sel;

end;

procedure TForm9.Button2Click(Sender: TObject);
begin
  CSVFile.Clear;
  CSVFile.Output.Initialize;
  CSVFile.Output.Update;
  Combobox1.Items.Clear;
end;

procedure TForm9.Button4Click(Sender: TObject);
Var
  Colcount,RowCount,i,j : Integer;
  Data,List : TStringlist;
  NewRow : String;


function GetRandomWord: string;
const
  WordList: array[0..49] of string = (
    'Apfel', 'Banane', 'Kirsche', 'Drachen', 'Elefant',
    'Frosch', 'Giraffe', 'Haus', 'Igel', 'Junge',
    'Katze', 'Löwe', 'Maus', 'Nashorn', 'Oktopus',
    'Papagei', 'Qualle', 'Rabe', 'Schnabeltier', 'Tiger',
    'Uhu', 'Vogel', 'Wal', 'Zebra', 'Äpfel',
    'Bär', 'Chamäleon', 'Delfin', 'Eidechse', 'Flamingo',
    'Gans', 'Heuschrecke', 'Ibis', 'Jaguar', 'Känguru',
    'Luchs', 'Murmeltier', 'Nilpferd', 'Otter', 'Panda',
    'Qualle', 'Ratte', 'Schaf', 'Tarantel', 'Uakari',
    'Viper', 'Wiesel', 'Zebra', 'Zikade', 'Äffchen'
  );
var
  RandomIndex: Integer;
begin
  Randomize; // Initialisiere den Zufallszahlengenerator
  RandomIndex := Random(Length(WordList)); // Wähle einen zufälligen Index
  Result := WordList[RandomIndex]; // Gib das zufällige Wort zurück
end;

begin
  Panel2.DoubleBuffered := true;
  Progressbar1.Position := 0;
  Progressbar1.Visible := true;

  CSVFile.Clear;
  CSVFile.Output.Initialize;
  CSVFile.Output.Update;
  selcol := -1;
  selrow := -1;
  SetCellValue();

  CSVFile.OnProgress := OnProgress;

  RowCount := NumberBox1.ValueInt;
  ColCount:= NumberBox2.ValueInt;
  Data := TStringlist.Create;
  for i := 0 to Rowcount-1 do begin
    // CSVFile.AppendRow(ColCount);

    // Nun die Inhalte
    NewRow := '';
    for j := 0 to Colcount-1 do begin
      // CSVFile.Cell[i,j] := GetRandomWord;
      if NewRow <> '' then NewRow := NewRow + ';';
      NewRow := NewRow+ GetRandomWord;
    end;

    Data.Add(NewRow);

  end;


  CSVFile.InsertFromText(Data.Text, ';');

  List := TStringlist.Create;
  CSVFile.RowToList(0,List);
  Combobox1.Items.Assign(List);
  List.Free;

  CSVFile.OnProgress := nil;
  Progressbar1.Position := 0;
  Progressbar1.Visible := false;

  CSVFile.Output.Initialize;
  CSVFile.Output.Update;

  Label9.Caption := 'Anzahl Zeilen: ' + inttostr( CSVFile.Data.Count ) + ' | ' + 'Anzahl Spalten: ' + inttostr( CSVFile.Data[0].Count );
  Label10.Caption := 'Benötigte Zeit um die Daten zu parsen: ' + FormatMilliseconds( CSVFile.ParseTime );

end;

procedure TForm9.Button5Click(Sender: TObject);
Var
  ARow,ACol : Integer;
begin
  ARow := selRow;
  ACol := SelCol;
  CSVFile.Cell[ARow,ACol] := Memo1.Text;
  CSVFile.Output.UpdateCell(ARow,ACol);
  CSVFile.Output.SelectFocus(ARow,ACol);
end;

procedure TForm9.Button6Click(Sender: TObject);
Var
  D : Double;
  Positions: TArray<TCSVPos>;
  Pos: TCSVPos;
  SearchText, Filename: String;
begin
//   CSVFile[10,10] := '100';
//  CSVFile.ExportAsXLSX('c:\tmp\test.xlsx');
//  CSVFile.ExportAsJSON('c:\tmp\test.json');

   //   ctSum, ctCount, ctAverage, ctMin, ctMax, ctProduct
//   D := csvFile.Calculate(false, 4, [],ctCount);
//  ShowMessage(Format('%.2f', [D])); // Gibt den Wert mit 2 Dezimalstellen aus
//   CSVFile.Search('*öl*');



//  CSVFile.MoveColBy(0,1);

//  CSVFile.SwapCol(0,3);
//  CSVFile.Output.Update;



  if OpenDialog1.Execute then begin
    Filename := OpenDialog1.FileName;

    CSVFile.Clear();



    CSVFile.Separator := CSVFile.GetSeparatorFromFile(Filename);
    CSVFile.LineBreakStyle := lbsWindows;

    CSVFile.LoadFromFile( Filename );
//    CSVFile.ParseMultilineCSVold( Filename );

    CSVFile.Output.Update;


  end;


     exit;

     CSVFile.Replace('Schöller', 'Schoeller');

     SearchText := 'Schoeller';
     Positions := CSVFile.SearchEx(SearchText);
    for Pos in Positions do
      ShowMessage(Format('Gefunden bei: Zeile %d, Spalte %d', [Pos.Y, Pos.X]));

end;

procedure TForm9.Button9Click(Sender: TObject);
Var
  i : Integer;
  Sel : TGridRect;
begin
  Sel := csvgrid.Selection;
  Application.ProcessMessages;

  i := CSVGrid.Row;
  CSVFile.MoveRowBy(i,-1);

  inc(Sel.Top,-1);
  inc(Sel.Bottom,-1);
  CSVFile.Output.Update;
  csvGrid.Selection := Sel;
end;

procedure TForm9.ComboBox1Change(Sender: TObject);
var
  Name: string;
  ColPos: Integer;
  DTyp: TDataType;
begin
  Name := ComboBox1.Text;

  if trim(Name) <> '' then begin
    // Setze den Mauszeiger auf Sanduhr
    Screen.Cursor := crHourGlass;
    try
      ColPos := CSVFile.GetColPos(Name);

      // Ermittele den Datentyp der Spalte für die Sortierung
      DTyp := CSVFile.DetectDataType(CSVFile.Cell[1, ColPos]);

      // Sortiere die Daten
      CSVFile.SortExt(ColPos, RadioButton1.Checked, true, DTyp);

      // Aktualisiere das Grid nach der Sortierung
  CSVFile.Output.Initialize;
  CSVFile.Output.Update;

      // Setze den Zellwert (falls erforderlich)
      SetCellValue;
    finally
      // Setze den Mauszeiger wieder auf den Standard-Cursor zurück
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TForm9.CSVGridClick(Sender: TObject);
begin
  // Hole die aktuelle Zeilen- und Spaltennummer
  SelCol := CSVGrid.Col;
  SelRow := CSVGrid.Row;
  SetCellValue();
end;

Procedure TForm9.OpenFile();
Var
  List : TStringlist;
  GR:TGridRect;
begin

  if OpenDialog1.Execute then begin
    Screen.Cursor := crHourGlass;
    Filename := OpenDialog1.FileName;

    CSVFile.Clear();
    with GR do begin
    GR.Left:=0;
    GR.Right:=0;
    GR.Top:=0;
    GR.Bottom:=0;
    end;
    CSVGrid.Selection:=GR;

    SelRow := -1;
    SelCol := -1;


    CSVFile.UseFastParsing := not checkbox1.Checked;
    CSVFile.LoadFromFile(Filename);
    CSVFile.Output.Initialize;
    CSVFile.Output.Update;
    Output.AutoSizeColumnsAndRows();

    List := TStringlist.Create;
    CSVFile.RowToList(0,List);
    Combobox1.Items.Assign(List);
    List.Free;
    Screen.Cursor := crDefault;
  end;
  SetCaption();
  SetCellValue();

end;

Procedure TForm9.SaveFile();
begin
  if SaveDialog1.Execute then begin
    Screen.Cursor := crHourGlass;
    Filename := SaveDialog1.FileName;
    CSVFile.SaveToFile(Filename);
    Screen.Cursor := crDefault;
  end;
  SetCaption();
end;

Procedure TForm9.SetCaption();
begin
  if FileExists(Filename) then begin
    Form9.Caption := Form9.Hint + ' - Dateiname: '+Filename;
  end
  else
    Form9.Caption := Form9.Hint;

end;

Procedure TForm9.SetCellValue();
var
  CellValue: String;
begin
  if (SelCol=-1) or (SelRow=-1) then begin
    Memo1.Text := '';
    Label5.Caption := '';
  end
  else begin
    // Hole den Inhalt der Zelle
    CellValue := CSVFile.Cell[SelRow,SelCol];
    Memo1.Text := CellValue;
    Label5.Caption := 'Zeile:'+inttostr(SelRow) + ' - Spalte: '+inttostr(SelCol);
  end;

end;

end.
