unit DcefBrowserReg;

{$I ..\Source\DcefB.Dcef3.Cef.inc}

interface

procedure Register;

implementation

uses Classes, DcefB.Core.DcefBrowser;

procedure Register;
begin
  RegisterComponents('DcefBrowser', [TDcefBrowser]);
end;

end.
