program JsInteract;

uses
  Forms,
  DcefB.Core.App,
  Unit1 in 'Unit1.pas' {Form2};

{$R *.res}

begin
  DcefBApp.CefSingleProcess  := False;
  if not DcefBApp.Init then
    Exit;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;

end.
