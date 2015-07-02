program CustomTab;

uses
  Vcl.Forms,
  dcef3_ceflib,
  UnitFrmMain in 'UnitFrmMain.pas' {MainForm};

{$R *.res}

begin
  if not CefLoadLibDefault then
    Exit;

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


