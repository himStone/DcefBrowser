program CustomTab;

uses
  Vcl.Forms,
  DcefB.Dcef3.CefLib,
  UnitFrmMain in 'UnitFrmMain.pas' {MainForm};

{$R *.res}

begin
  CefSingleProcess := False;
  if not CefLoadLibDefault then
    Exit;

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


