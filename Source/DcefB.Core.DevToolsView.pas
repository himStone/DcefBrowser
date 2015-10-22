(*
  *                  Delphi Multi-tab Chromium Browser Frame
  *
  * Usage allowed under the restrictions of the Lesser GNU General Public License
  * or alternatively the restrictions of the Mozilla Public License 1.1
  *
  * Software distributed under the License is distributed on an "AS IS" basis,
  * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  * the specific language governing rights and limitations under the License.
  *
  * Unit owner : BccSafe <bccsafe5988@gmail.com>
  * QQ         : 1262807955
  * Web site   : http://www.bccsafe.com
  * Repository : https://github.com/bccsafe/DcefBrowser
  *
  * The code of DcefBrowser is based on DCEF3 by: Henri Gourvest <hgourvest@gmail.com>
  * code: https://github.com/hgourvest/dcef3
  *
  * Embarcadero Technologies, Inc is not permitted to use or redistribute
  * this source code without explicit permission.
  *
*)

unit DcefB.Core.DevToolsView;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Winapi.Messages, Vcl.ExtCtrls,
  Generics.Collections, Vcl.Forms,
  DcefB.Dcef3.CefLib, DcefB.res, DcefB.Locker;

type
  TDevToolsView = class(TWinControl)
  private
    FBrowserDic: TDictionary<Integer, ICefBrowser>;
    procedure CreateDevTools(const browser: ICefBrowser;
      inspectElementAt: PCefPoint = nil);
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure ShowDevTools(const browser: ICefBrowser;
      inspectElementAt: PCefPoint = nil);
    procedure CloseDevTools(const browser: ICefBrowser);
  end;

implementation

{ TDevToolsView }

procedure TDevToolsView.CloseDevTools(const browser: ICefBrowser);
begin
  if browser <> nil then
  begin
    // Winapi.Windows.SetParent(GetWindow(Handle, GW_CHILD), 0);
    browser.Host.CloseDevTools;
    DevToolsBroListLocker.Enter;
    try
      FBrowserDic.Remove(browser.identifier);
    finally
      DevToolsBroListLocker.Exit;
    end;
  end;
end;

constructor TDevToolsView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBrowserDic := TDictionary<Integer, ICefBrowser>.Create;
end;

procedure TDevToolsView.CreateDevTools(const browser: ICefBrowser;
  inspectElementAt: PCefPoint);
var
  info: TCefWindowInfo;
  setting: TCefBrowserSettings;
begin

  FillChar(info, SizeOf(info), 0);
  info.style := WS_POPUP or WS_VISIBLE or WS_CAPTION or WS_SYSMENU or
    WS_SIZEBOX or WS_MAXIMIZEBOX;
  info.x := 0;
  info.y := 0;
  info.Width := Screen.Width;
  info.Height := Screen.Height;
  info.window_name := CefString('DevTools');

  FillChar(setting, SizeOf(setting), 0);
  setting.size := SizeOf(setting);

  browser.Host.ShowDevTools(@info, TCefClientOwn.Create as ICefClient, @setting,
    inspectElementAt);
end;

destructor TDevToolsView.Destroy;
begin
  DevToolsBroListLocker.Enter;
  try
    FBrowserDic.Clear;
  finally
    DevToolsBroListLocker.Exit;
  end;
  inherited;
end;

procedure TDevToolsView.Resize;
var
  rect: TRect;
  hdwp: THandle;
  hndl: THandle;
begin
  inherited;
  hndl := GetWindow(Handle, GW_CHILD);
  if hndl = 0 then
    Exit;

  rect := GetClientRect;
  hdwp := BeginDeferWindowPos(1);
  try
    hdwp := DeferWindowPos(hdwp, hndl, 0, rect.left, rect.top,
      rect.right - rect.left, rect.bottom - rect.top, SWP_NOZORDER);
  finally
    EndDeferWindowPos(hdwp);
  end;
end;

procedure TDevToolsView.ShowDevTools(const browser: ICefBrowser;
  inspectElementAt: PCefPoint);
var
  MyBrowser: ICefBrowser;
begin
  if browser = nil then
    Exit;

  DevToolsBroListLocker.Enter;
  try
    FBrowserDic.TryGetValue(browser.identifier, MyBrowser);
    if MyBrowser <> nil then
    begin
      ShowWindow(MyBrowser.Host.WindowHandle, SW_SHOWNORMAL);
    end
    else
    begin
      CreateDevTools(browser, inspectElementAt);
      FBrowserDic.Add(browser.identifier, browser);
    end;
  finally
    DevToolsBroListLocker.Exit;
  end;
end;

procedure TDevToolsView.WndProc(var Message: TMessage);
var
  hndl: THandle;
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        hndl := GetWindow(Handle, GW_CHILD);
        if hndl <> 0 then
          PostMessage(hndl, WM_SETFOCUS, Message.WParam, 0);
        inherited WndProc(Message);
      end;
    WM_ERASEBKGND:
      begin
        hndl := GetWindow(Handle, GW_CHILD);
        if (csDesigning in ComponentState) or (hndl = 0) then
          inherited WndProc(Message);
      end;
    CM_WANTSPECIALKEY:
      if not(TWMKey(Message).CharCode in [VK_LEFT .. VK_DOWN]) then
        Message.Result := 1
      else
        inherited WndProc(Message);
    WM_GETDLGCODE:
      Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  else
    inherited WndProc(Message);
  end;
end;

end.
