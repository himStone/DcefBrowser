unit UnitDcefBrowserReg;

{$I ..\Source\dcef3_cef.inc}

interface

procedure Register;

implementation

uses System.Classes, dcefb_Browser;

procedure Register;
begin
  RegisterComponents('DcefBrowser', [TDcefBrowser]);
end;

end.
