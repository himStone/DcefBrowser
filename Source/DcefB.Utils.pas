(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Utils;

interface

uses
  WinApi.Windows, System.Classes, DcefB.Dcef3.CefLib;

type
  TDcefBUtils = record
    class function GetCefParentWindow(aBrowser: ICefBrowser): HWND; static;
    class function SendMsg(aBrowser: ICefBrowser; Msg: UINT; LParam: LParam)
      : Boolean; static;
  end;

implementation

{ TDcefBUtils }

class function TDcefBUtils.GetCefParentWindow(aBrowser: ICefBrowser): HWND;
begin
  Result := GetParent(aBrowser.host.WindowHandle);
end;

class function TDcefBUtils.SendMsg(aBrowser: ICefBrowser; Msg: UINT;
  LParam: LParam): Boolean;
begin
  Result := SendMessage(GetCefParentWindow(aBrowser), Msg, WParam(@aBrowser),
    LParam) <> S_FALSE;
end;

end.
