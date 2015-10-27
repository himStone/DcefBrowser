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

unit DcefB.Handler.Load;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBLoadHandler = class(TCefLoadHandlerOwn)
  private
    FEvents: IDcefBrowser;
  protected
    procedure OnLoadingStateChange(const browser: ICefBrowser;
      isLoading, canGoBack, canGoForward: Boolean); override;
    procedure OnLoadStart(const browser: ICefBrowser;
      const frame: ICefFrame); override;
    procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame;
      httpStatusCode: Integer); override;
    procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame;
      errorCode: Integer; const errorText, failedUrl: ustring); override;
  public
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomLoadHandler }

constructor TDcefBLoadHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
end;

destructor TDcefBLoadHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

procedure TDcefBLoadHandler.OnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
var
  PArgs: PLoadEndArgs;
begin
  inherited;
  New(PArgs);
  PArgs.frame := @frame;
  PArgs.httpStatusCode := httpStatusCode;
  TDcefBUtils.SendMsg(browser, WM_LoadEnd, LParam(PArgs));
  Dispose(PArgs);
end;

procedure TDcefBLoadHandler.OnLoadError(const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
var
  PArgs: PLoadErrorArgs;
begin
  inherited;
  New(PArgs);
  PArgs.frame := @frame;
  PArgs.errorCode := errorCode;
  PArgs.errorText := @errorText;
  PArgs.failedUrl := @failedUrl;
  PArgs.CancelDefaultEvent := False;
  TDcefBUtils.SendMsg(browser, WM_LoadError, LParam(PArgs));
  Dispose(PArgs);
end;

procedure TDcefBLoadHandler.OnLoadingStateChange(const browser: ICefBrowser;
  isLoading, canGoBack, canGoForward: Boolean);
var
  LoadingState: Integer;
begin
  inherited;
  LoadingState := 0;
  if isLoading then
    LoadingState := LoadingState or $001;
  if canGoBack then
    LoadingState := LoadingState or $002;
  if canGoForward then
    LoadingState := LoadingState or $004;

  TDcefBUtils.SendMsg(browser, WM_LoadingStateChange, LoadingState);
end;

procedure TDcefBLoadHandler.OnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  inherited;
  TDcefBUtils.SendMsg(browser, WM_LoadStart, LParam(@frame));
end;

end.
