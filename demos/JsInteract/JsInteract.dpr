program JsInteract;

uses
  Vcl.Forms,
  DcefB.Core.DcefBrowser,
  DcefB.Dcef3.CefLib,
  UnitFrmMain in 'UnitFrmMain.pas' {MainForm};

{$R *.res}

begin
  // CefSingleProcess := False;
  TDcefBrowser.CreateDefaultRenderProcess;
  if not CefLoadLibDefault then
    Exit;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.
