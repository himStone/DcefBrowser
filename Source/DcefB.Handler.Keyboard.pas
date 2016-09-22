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

unit DcefB.Handler.Keyboard;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBKeyboardHandler = class(TCefKeyboardHandlerOwn)
  private
    FEvents: IDcefBrowser;
  protected
    function OnPreKeyEvent(const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle;
      out isKeyboardShortcut: Boolean): Boolean; override;
    function OnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle): Boolean; override;
  public
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDcefBKeyboardHandler }

constructor TDcefBKeyboardHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
end;

destructor TDcefBKeyboardHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

function TDcefBKeyboardHandler.OnKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean;
{ var
  PArgs: PKeyEventArgs; }
begin
  { New(PArgs);
    PArgs.event := event;
    PArgs.osEvent := osEvent;
    PArgs.Result := False;
    if TDcefBUtils.SendMsg(browser, WM_KeyEvent, LParam(PArgs)) then
    Result := PArgs.Result
    else
    Result := False;
    Dispose(PArgs); }
  Result := False;
  FEvents.doOnKeyEvent(browser, event, osEvent, Result);
end;

function TDcefBKeyboardHandler.OnPreKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle;
  out isKeyboardShortcut: Boolean): Boolean;
var
  // PArgs: PPreKeyEventArgs;
  CancelDefaultEvent: Boolean;
begin
  { New(PArgs);
    PArgs.event := event;
    PArgs.osEvent := osEvent;
    PArgs.isKeyboardShortcut := isKeyboardShortcut;
    PArgs.Result := False;
    PArgs.CancelDefaultEvent := False;
    if TDcefBUtils.SendMsg(browser, WM_PreKeyEvent, LParam(PArgs)) then
    Result := PArgs.Result
    else
    Result := False;
    Dispose(PArgs); }
  Result := False;
  CancelDefaultEvent := False;
  FEvents.doOnPreKeyEvent(browser, event, osEvent, isKeyboardShortcut, Result,
    CancelDefaultEvent);
  if Not CancelDefaultEvent then
  begin
    if (event.native_key_code = 123) and (event.Kind = KEYEVENT_KEYUP) then
      TDcefBUtils.SendMsg(browser, WM_DevTools, 0); // F12

    if (event.native_key_code = 116) and (event.Kind = KEYEVENT_KEYUP) then
      TDcefBUtils.SendMsg(browser, WM_RefreshIgnoreCache, 0); // F5

    if (event.native_key_code = 70) and
      (EVENTFLAG_CONTROL_DOWN in event.modifiers) then
      TDcefBUtils.SendMsg(browser, WM_SearchText, 0); // Ctrl+F

    if (event.native_key_code = 115) and // Alt + F4
      (EVENTFLAG_ALT_DOWN in event.modifiers) then
      Result := True;
  end;
end;

end.
