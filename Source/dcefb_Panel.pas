unit DcefB_Panel;

interface

uses
  Windows, Classes, ComCtrls, Messages, ExtCtrls, SysUtils;

type
  TDcefBPanel = class(TPanel)
  private
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  end;

implementation

{ TDcefBPanel }

procedure TDcefBPanel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTTAB;
end;

end.
