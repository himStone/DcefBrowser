program simplebrowser;

uses
  Vcl.Forms,
  dcef3_ceflib,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  if not CefLoadLibDefault then
    Exit;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
