program Example4;

uses
  Vcl.Forms,
  frm_Main in 'frm_Main.pas' {frmMain},
  class_Frontend_Output_TStringlist in '..\class_Frontend_Output_TStringlist.pas',
  class_Frontend_Output_TVirtualTreeview in '..\class_Frontend_Output_TVirtualTreeview.pas',
  class_Frontend_Output_TStringGrid in '..\class_Frontend_Output_TStringGrid.pas',
  class_Frontend_Output_TListView in '..\class_Frontend_Output_TListView.pas',
  Vcl.Themes,
  Vcl.Styles,
  CSVFile in '..\CSVFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Sterling');
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
