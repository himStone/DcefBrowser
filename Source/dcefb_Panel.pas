unit dcefb_Panel;

interface

uses
  Winapi.Windows, System.Classes, Vcl.ComCtrls, Winapi.Messages, Vcl.ExtCtrls,
  System.SysUtils;

type
  TDcefB_Panel = class(TPanel)
  private
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  end;

implementation

{ TDcefB_Panel }

procedure TDcefB_Panel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result :=   Message.Result or DLGC_WANTARROWS or DLGC_WANTTAB;
end;

end.
