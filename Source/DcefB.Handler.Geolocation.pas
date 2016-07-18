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

unit DcefB.Handler.Geolocation;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBGeolocationHandler = class(TCefGeolocationHandlerOwn)
  private
    FEvents: IDcefBrowser;
  protected
    function OnRequestGeolocationPermission(const browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer;
      const callback: ICefGeolocationCallback): Boolean; override;
    procedure OnCancelGeolocationPermission(const browser: ICefBrowser;
      requestId: Integer); override;
  public
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomGeolocationHandler }

constructor TDcefBGeolocationHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
end;

destructor TDcefBGeolocationHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

procedure TDcefBGeolocationHandler.OnCancelGeolocationPermission
  (const browser: ICefBrowser; requestId: Integer);
{ var
  PArgs: PCancelGeolocationPermissionArgs; }
begin
  inherited;
  { New(PArgs);
    PArgs.requestingUrl := @requestingUrl;
    PArgs.requestId := requestId;
    TDcefBUtils.SendMsg(browser, WM_CancelGeolocationPermission, LParam(PArgs));
    Dispose(PArgs); }
  FEvents.doOnCancelGeolocationPermission(browser, requestId);
end;

function TDcefBGeolocationHandler.OnRequestGeolocationPermission
  (const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer;
  const callback: ICefGeolocationCallback): Boolean;
{ var
  PArgs: PRequestGeolocationPermissionArgs; }
begin
  { New(PArgs);
    PArgs.requestingUrl := @requestingUrl;
    PArgs.requestId := requestId;
    PArgs.callback := @callback;
    PArgs.Result := False;
    if TDcefBUtils.SendMsg(browser, WM_RequestGeolocationPermission, LParam(PArgs))
    then
    Result := PArgs.Result
    else
    Result := False;
    Dispose(PArgs); }
  Result := False;
  FEvents.doOnRequestGeolocationPermission(browser, requestingUrl, requestId,
    callback, Result);
end;

end.
