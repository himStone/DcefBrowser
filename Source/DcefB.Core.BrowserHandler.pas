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
  * QQ: 1262807955
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

unit DcefB.Core.BrowserHandler;

interface

uses
  Windows, Classes, SysUtils, Controls, Winapi.Messages,

  DcefB.CefEvents, DcefB.Dcef3.CefLib, DcefB.res, DcefB.Events,
  // Handler Unit
  DcefB.Handler.Display, DcefB.Handler.Dialog, DcefB.Handler.Download,
  DcefB.Handler.Focus, DcefB.Handler.Geolocation, DcefB.Handler.JsDialog,
  DcefB.Handler.Keyboard, DcefB.Handler.LifeSpan, DcefB.Handler.Load,
  DcefB.Handler.Menu, DcefB.Handler.Request, DcefB.Handler.Drag,
  DcefB.Handler.Render, DcefB.Handler.Basic;

type
  TDcefBHandler = class(TCefClientOwn, ICefClientHandler)
  private
    FDcefBEvents: IDcefBEvents;

    FLoadHandler: ICefLoadHandler;
    FFocusHandler: ICefFocusHandler;
    FContextMenuHandler: ICefContextMenuHandler;
    FDialogHandler: ICefDialogHandler;
    FKeyboardHandler: ICefKeyboardHandler;
    FDisplayHandler: ICefDisplayHandler;
    FDownloadHandler: ICefDownloadHandler;
    FGeolocationHandler: ICefGeolocationHandler;
    FJsDialogHandler: ICefJsDialogHandler;
    FLifeSpanHandler: ICefLifeSpanHandler;
    FRenderHandler: ICefRenderHandler;
    FRequestHandler: ICefRequestHandler;
    FDragHandler: ICefDragHandler;
    function doOnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;
  protected
    function GetContextMenuHandler: ICefContextMenuHandler; override;
    function GetDialogHandler: ICefDialogHandler; override;
    function GetDisplayHandler: ICefDisplayHandler; override;
    function GetDownloadHandler: ICefDownloadHandler; override;
    function GetFocusHandler: ICefFocusHandler; override;
    function GetGeolocationHandler: ICefGeolocationHandler; override;
    function GetJsdialogHandler: ICefJsDialogHandler; override;
    function GetKeyboardHandler: ICefKeyboardHandler; override;
    function GetLifeSpanHandler: ICefLifeSpanHandler; override;
    function GetRenderHandler: ICefRenderHandler; override;
    function GetLoadHandler: ICefLoadHandler; override;
    function GetRequestHandler: ICefRequestHandler; override;
    function GetDragHandler: ICefDragHandler; override;

    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage)
      : Boolean; override;

    procedure Disconnect;
  public
    constructor Create(renderer: Boolean; aDcefBEvents: IDcefBEvents);
      reintroduce;
    destructor Destroy; override;
  end;

var
  DcefBHandler: TDcefBHandler;

implementation

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}

var
  CefInstances: Integer = 0;
  CefTimer: UINT = 0;

var
  looping: Boolean = False;

procedure TimerProc(HWND: HWND; uMsg: UINT; idEvent: Pointer;
  dwTime: DWORD); stdcall;
begin
  if looping then
    Exit;
  if CefInstances > 0 then
  begin
    looping := True;
    try
      CefDoMessageLoopWork;
    finally
      looping := False;
    end;
  end;
end;
{$ENDIF}

function GetCefParentWindow(aBrowser: ICefBrowser): HWND;
begin
  Result := GetParent(aBrowser.host.WindowHandle);
end;

function SendMsgToCefParentWin(aBrowser: ICefBrowser; Msg: UINT;
  LParam: LParam): LRESULT;
begin
  Result := SendMessage(GetCefParentWindow(aBrowser), Msg,
    WParam(@aBrowser), LParam);
end;

{ TDcefBHandler }

constructor TDcefBHandler.Create(renderer: Boolean; aDcefBEvents: IDcefBEvents);
begin
  inherited Create;
  FDcefBEvents := aDcefBEvents;
  FLoadHandler := TDcefBLoadHandler.Create(FDcefBEvents);
  FFocusHandler := TDcefBFocusHandler.Create(FDcefBEvents);
  FContextMenuHandler := TDcefBContextMenuHandler.Create(FDcefBEvents);
  FDialogHandler := TDcefBDialogHandler.Create(FDcefBEvents);
  FKeyboardHandler := TDcefBKeyboardHandler.Create(FDcefBEvents);
  FDisplayHandler := TDcefBDisplayHandler.Create(FDcefBEvents);
  FDownloadHandler := TDcefBDownloadHandler.Create(FDcefBEvents);
  FGeolocationHandler := TDcefBGeolocationHandler.Create(FDcefBEvents);
  FJsDialogHandler := TDcefBJsDialogHandler.Create(FDcefBEvents);
  FLifeSpanHandler := TDcefBLifeSpanHandler.Create(FDcefBEvents);
  FRequestHandler := TDcefBRequestHandler.Create(FDcefBEvents);
  if renderer then
    FRenderHandler := TDcefBRenderHandler.Create(FDcefBEvents)
  else
    FRenderHandler := nil;
  FDragHandler := TDcefBDragHandler.Create(FDcefBEvents);

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  if CefInstances = 0 then
    CefTimer := SetTimer(0, 0, 10, @TimerProc);
  InterlockedIncrement(CefInstances);
{$ENDIF}
end;

destructor TDcefBHandler.Destroy;
begin
  FDcefBEvents := nil;
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  InterlockedDecrement(CefInstances);
  if CefInstances = 0 then
  begin
    KillTimer(0, CefTimer);
  end;
{$ENDIF}
  inherited;
end;

procedure TDcefBHandler.Disconnect;
begin
  if FLoadHandler <> nil then
    FLoadHandler := nil;
  if FFocusHandler <> nil then
    FFocusHandler := nil;
  if FContextMenuHandler <> nil then
    FContextMenuHandler := nil;
  if FDialogHandler <> nil then
    FDialogHandler := nil;
  if FKeyboardHandler <> nil then
    FKeyboardHandler := nil;
  if FDisplayHandler <> nil then
    FDisplayHandler := nil;
  if FDownloadHandler <> nil then
    FDownloadHandler := nil;
  if FGeolocationHandler <> nil then
    FGeolocationHandler := nil;
  if FJsDialogHandler <> nil then
    FJsDialogHandler := nil;
  if FLifeSpanHandler <> nil then
    FLifeSpanHandler := nil;
  if FRequestHandler <> nil then
    FRequestHandler := nil;
  if FDragHandler <> nil then
    FDragHandler := nil;
  if FRenderHandler <> nil then
    FRenderHandler := nil;
end;

function TDcefBHandler.doOnProcessMessageReceived(const browser: ICefBrowser;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;
begin
  Result := False;
end;

function TDcefBHandler.GetContextMenuHandler: ICefContextMenuHandler;
begin
  Result := FContextMenuHandler;
end;

function TDcefBHandler.GetDialogHandler: ICefDialogHandler;
begin
  Result := FDialogHandler;
end;

function TDcefBHandler.GetDisplayHandler: ICefDisplayHandler;
begin
  Result := FDisplayHandler;
end;

function TDcefBHandler.GetDownloadHandler: ICefDownloadHandler;
begin
  Result := FDownloadHandler;
end;

function TDcefBHandler.GetDragHandler: ICefDragHandler;
begin
  Result := FDragHandler;
end;

function TDcefBHandler.GetRenderHandler: ICefRenderHandler;
begin
  Result := FRenderHandler;
end;

function TDcefBHandler.GetFocusHandler: ICefFocusHandler;
begin
  Result := FFocusHandler;
end;

function TDcefBHandler.GetGeolocationHandler: ICefGeolocationHandler;
begin
  Result := FGeolocationHandler;
end;

function TDcefBHandler.GetJsdialogHandler: ICefJsDialogHandler;
begin
  Result := FJsDialogHandler;
end;

function TDcefBHandler.GetKeyboardHandler: ICefKeyboardHandler;
begin
  Result := FKeyboardHandler;
end;

function TDcefBHandler.GetLifeSpanHandler: ICefLifeSpanHandler;
begin
  Result := FLifeSpanHandler;
end;

function TDcefBHandler.GetLoadHandler: ICefLoadHandler;
begin
  Result := FLoadHandler;
end;

function TDcefBHandler.GetRequestHandler: ICefRequestHandler;
begin
  Result := FRequestHandler;
end;

function TDcefBHandler.OnProcessMessageReceived(const browser: ICefBrowser;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;
begin
  Result := doOnProcessMessageReceived(browser, sourceProcess, message);
end;

end.
