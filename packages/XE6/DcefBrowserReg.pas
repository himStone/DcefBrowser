unit DcefBrowserReg;

{$I ..\..\Source\Dcef3_cef.inc}

interface

procedure Register;

implementation

uses Classes, DcefB_Browser;

procedure Register;
begin
  RegisterComponents('DcefBrowser', [TDcefBrowser]);
end;

end.
