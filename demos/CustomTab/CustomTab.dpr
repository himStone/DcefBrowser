program CustomTab;

uses
  Forms,
  DcefB.Core.App,
  UnitFrmMain in 'UnitFrmMain.pas' {MainForm};

{$R *.res}

begin
  DcefBApp.CefSingleProcess := False;
  if not DcefBApp.Init then
    Exit;

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


