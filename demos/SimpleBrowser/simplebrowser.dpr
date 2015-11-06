program simplebrowser;

uses
  Forms,
  DcefB.Core.App,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  DcefBApp.CefSingleProcess := False;
   if not DcefBApp.Init then
    Exit;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
