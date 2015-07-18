program JsInteract;

uses
  Vcl.Forms,
  dcefb_Browser,
  dcef3_ceflib,
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
