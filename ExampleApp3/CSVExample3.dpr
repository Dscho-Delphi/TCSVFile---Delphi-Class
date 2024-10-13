program CSVExample3;

uses
  Vcl.Forms,
  frm_Main in 'frm_Main.pas' {frmMain},
  Vcl.Themes,
  Vcl.Styles,
  CSVFile in '..\CSVFile.pas',
  class_Frontend_Output_TStringGrid in '..\class_Frontend_Output_TStringGrid.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Aqua Light Slate');
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
