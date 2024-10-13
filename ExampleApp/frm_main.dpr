program frm_main;

uses
  Vcl.Forms,
  CSVTest in 'CSVTest.pas' {Form9},
  Vcl.Themes,
  Vcl.Styles,
  CSVFile in '..\CSVFile.pas',
  class_Frontend_Output_TStringGrid in '..\class_Frontend_Output_TStringGrid.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Silver');
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
