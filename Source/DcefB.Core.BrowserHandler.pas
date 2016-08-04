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

unit DcefB.Core.BrowserHandler;

interface

uses
  Windows, Classes, SysUtils, Controls, Messages, ExtCtrls,

  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types,
  DcefB.res, DcefB.Events, DcefB.Utils, DcefB.BaseObject,
  // Handler Unit
  DcefB.Handler.Display, DcefB.Handler.Dialog, DcefB.Handler.Download,
  DcefB.Handler.Focus, DcefB.Handler.Geolocation, DcefB.Handler.JsDialog,
  DcefB.Handler.Keyboard, DcefB.Handler.LifeSpan, DcefB.Handler.Load,
  DcefB.Handler.Menu, DcefB.Handler.Request, DcefB.Handler.Drag,
  DcefB.Handler.Render, DcefB.Handler.Find, DcefB.Handler.Basic;

type
  TDcefBHandler = class(TCefClientOwn, ICefClientHandler)
  private
    FEvents: IDcefBrowser;

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
    FFindHandler: ICefFindHandler;
    function doOnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;
    procedure OnTimer(Sender: TObject);
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
    function GetFindHandler: ICefFindHandler; override;

    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage)
      : Boolean; override;

    procedure Disconnect;
  public
    constructor Create(renderer: Boolean; aDcefBrowser: IDcefBrowser);
      reintroduce;
    destructor Destroy; override;
    procedure StartTimer;
  end;

var
  DcefBHandler: TDcefBHandler;

implementation

uses
  DcefB.Core.App;

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
var
  CefInstances: Integer = 0;
  Timer: TTimer;
  looping: Boolean = False;
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

constructor TDcefBHandler.Create(renderer: Boolean; aDcefBrowser: IDcefBrowser);
begin
  inherited Create;

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  If not assigned(Timer) then
  begin
    Timer := TTimer.Create(nil);
    Timer.Interval := 15;
    Timer.Enabled := False;
    Timer.OnTimer := OnTimer;
  end;
  InterlockedIncrement(CefInstances);
{$ENDIF}

  FEvents := aDcefBrowser;

  FLoadHandler := TDcefBLoadHandler.Create(FEvents);
  FFocusHandler := TDcefBFocusHandler.Create(FEvents);
  FContextMenuHandler := TDcefBContextMenuHandler.Create(FEvents);
  FDialogHandler := TDcefBDialogHandler.Create(FEvents);
  FKeyboardHandler := TDcefBKeyboardHandler.Create(FEvents);
  FDisplayHandler := TDcefBDisplayHandler.Create(FEvents);
  FDownloadHandler := TDcefBDownloadHandler.Create(FEvents);
  FGeolocationHandler := TDcefBGeolocationHandler.Create(FEvents);
  FJsDialogHandler := TDcefBJsDialogHandler.Create(FEvents);
  FLifeSpanHandler := TDcefBLifeSpanHandler.Create(FEvents);
  FRequestHandler := TDcefBRequestHandler.Create(FEvents);
  if renderer then
    FRenderHandler := TDcefBRenderHandler.Create(FEvents)
  else
    FRenderHandler := nil;
  FDragHandler := TDcefBDragHandler.Create(FEvents);
  FFindHandler := TDcefBFindHandler.Create(FEvents);
end;

destructor TDcefBHandler.Destroy;
begin
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  InterlockedDecrement(CefInstances);
  If CefInstances = 0 then
  begin
    Timer.Enabled := False;

    FreeAndNil(Timer);
  end;
{$ENDIF}
  FEvents := nil;
  inherited;
end;

procedure TDcefBHandler.OnTimer(Sender: TObject);
begin
  {$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  If Looping then Exit;
  If CefInstances > 0 then
  begin
    Looping := True;
    try
      DcefBApp.CefDoMessageLoopWork;
    finally
      Looping := False;
    end;
  end;
  {$ENDIF}
end;

procedure TDcefBHandler.StartTimer;
begin
  {$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  If not Assigned(Timer) then Exit;

  Timer.Enabled := True;
  {$ENDIF}
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
  if FRequestHandler <> nil then
    FRequestHandler := nil;
  if FDragHandler <> nil then
    FDragHandler := nil;
  if FFindHandler <> nil then
    FFindHandler := nil;
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

function TDcefBHandler.GetFindHandler: ICefFindHandler;
begin
  Result := FFindHandler;
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
  procedure DoJsExtention;
  var
    PArgs: PJsExtentionArgs;
    aJsExtentionId: string;
    aJsResult: Variant;
    aExceptionStr: string;
  begin
    aJsExtentionId := message.ArgumentList.GetString(0);
    TDcefBUtils.SetVariantData(aJsResult, message.ArgumentList, 1);
    aExceptionStr := message.ArgumentList.GetString(2);
    New(PArgs);
    PArgs.JsExtentionId := @aJsExtentionId;
    PArgs.JsResult := @aJsResult;
    PArgs.ExceptionHint := @aExceptionStr;
    TDcefBUtils.SendMsg(browser, WM_JsExtention, LParam(PArgs));
    Dispose(PArgs);
  end;

begin
  if SameText(message.Name, JSEXTENTION_TOBROWSER_MEG) then
  begin
    DoJsExtention;
    Result := True;
  end
  else
    Result := doOnProcessMessageReceived(browser, sourceProcess, message);
end;

end.
