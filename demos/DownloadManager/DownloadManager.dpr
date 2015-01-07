program DownloadManager;

uses
  Vcl.Forms,
  dcef3_ceflib,
  dcefb_Browser,
  dcef3_ceffilescheme,
  UnitFrmMain in 'UnitFrmMain.pas' {MainForm};

{$R *.res}   

begin
  CefSingleProcess := False;
  CefRemoteDebuggingPort := 9000;
  if not CefLoadLibDefault then
    Exit;
  CefRegisterSchemeHandlerFactory('local', '', False, TFileScheme);

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  
end.
