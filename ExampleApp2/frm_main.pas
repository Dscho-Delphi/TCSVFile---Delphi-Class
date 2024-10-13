unit frm_main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CSVFile, Vcl.Grids;

type
  TfrmMani = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure SearchZIPCode;
  public
    { Public-Deklarationen }
    ZIPFilename: String;
    ZIPDB: TCSVFile;
  end;

var
  frmMani: TfrmMani;

implementation

{$R *.dfm}

procedure TfrmMani.SearchZIPCode;
var
  Position: TCSVPos;
begin
  try
    Position := ZIPDB.Search(trim(Edit2.Text), 0);
    if (Position.X > -1) and (Position.Y > -1) then
    begin
      Edit3.Text := ZIPDB.Cell[Position.Y, 1];
      Edit4.Text := ZIPDB.Cell[Position.Y, 3];
    end
    else
    begin
      Edit3.Text := '';
      Edit4.Text := '';
    end;
  except
    // Fehlerbehandlung kann hier hinzugefügt werden, falls nötig
  end;
end;

procedure TfrmMani.Edit2Change(Sender: TObject);
begin
  if Edit2.GetTextLen >= 5 then
    SearchZIPCode;
end;

procedure TfrmMani.FormCreate(Sender: TObject);
begin
  ZIPFilename := ExtractFilePath(Application.ExeName) + 'zip.csv';
  if FileExists(ZIPFilename) then
  begin
    ZIPDB := TCSVFile.Create(TEncoding.UTF8, ';');
    ZIPDB.LoadFromFile(ZIPFilename);
  end;
end;

procedure TfrmMani.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ZIPDB);  // Kürzere und sicherere Variante zum Freigeben
end;

end.

