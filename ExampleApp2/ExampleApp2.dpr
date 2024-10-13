program ExampleApp2;

uses
  Vcl.Forms,
  frm_main in 'frm_main.pas' {frmMani},
  CSVFile in '..\CSVFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMani, frmMani);
  Application.Run;
end.
