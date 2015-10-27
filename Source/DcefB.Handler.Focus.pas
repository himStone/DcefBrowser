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

unit DcefB.Handler.Focus;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBFocusHandler = class(TCefFocusHandlerOwn)
  private
    FEvents: IDcefBrowser;
  protected
    procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); override;
    function OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource)
      : Boolean; override;
    procedure OnGotFocus(const browser: ICefBrowser); override;
  public
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDcefBFocusHandler }

constructor TDcefBFocusHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
end;

destructor TDcefBFocusHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

procedure TDcefBFocusHandler.OnGotFocus(const browser: ICefBrowser);
begin
  inherited;
  TDcefBUtils.SendMsg(browser, WM_GotFocus, 0);
end;

function TDcefBFocusHandler.OnSetFocus(const browser: ICefBrowser;
  source: TCefFocusSource): Boolean;
var
  // PArgs: PSetFocusArgs;
  CancelDefaultEvent: Boolean;
begin
  { New(PArgs);
    PArgs.source := @source;
    PArgs.CancelDefaultEvent := TDcefBUtils.SendMsg(browser, WM_SetFocus,
    LParam(PArgs)) and PArgs.CancelDefaultEvent;

    if Not PArgs.CancelDefaultEvent then
    Result := source = TCefFocusSource.FOCUS_SOURCE_NAVIGATION
    else
    Result := PArgs.Result;
    Dispose(PArgs); }

  CancelDefaultEvent := False;
  Result := False;
  FEvents.doOnSetFocus(browser, source, Result, CancelDefaultEvent);
  if Not CancelDefaultEvent then
    Result := source = TCefFocusSource.FOCUS_SOURCE_NAVIGATION;
end;

procedure TDcefBFocusHandler.OnTakeFocus(const browser: ICefBrowser;
  next: Boolean);
{ var
  PArgs: PTakeFocusArgs; }
begin
  inherited;
  { New(PArgs);
    PArgs.next := next;
    TDcefBUtils.SendMsg(browser, WM_TakeFocus, LParam(PArgs));
    Dispose(PArgs); }
  FEvents.doOnTakeFocus(browser, next);
end;

end.
