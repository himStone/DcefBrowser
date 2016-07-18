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

unit DcefB.Handler.Request;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBRequestHandler = class(TCefRequestHandlerOwn)
  private
    FEvents: IDcefBrowser;
  protected
    function OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; isRedirect: Boolean): Boolean; override;
    function OnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame;
      const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean): Boolean; override;
    function OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue; override;
    function GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest): ICefResourceHandler; override;
    procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; var newUrl: ustring); override;
    function OnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse): Boolean; override;
    function GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse): ICefResponseFilter; override;
    procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus;
      receivedContentLength: Int64); override;
    function GetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame;
      isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback): Boolean; override;
    function OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring;
      newSize: Int64; const callback: ICefRequestCallback): Boolean; override;
    procedure OnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean); override;
    function OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode;
      const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean; override;
    procedure OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring); override;
    procedure OnRenderViewReady(const browser: ICefBrowser); override;
    procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); override;
  public
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomRequestHandler }

constructor TDcefBRequestHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
end;

destructor TDcefBRequestHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

// Called on the IO thread
function TDcefBRequestHandler.GetAuthCredentials(const browser: ICefBrowser;
  const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer;
  const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
var
  PArgs: PAuthCredentialsArgs;
begin
  inherited;
  result := true;
  New(PArgs);
  PArgs.frame := @frame;
  PArgs.isProxy := @isProxy;
  PArgs.host := @host;
  PArgs.port := @port;
  PArgs.realm := @realm;
  PArgs.scheme := @scheme;
  PArgs.callback := @callback;
  PArgs.CancelDefaultEvent := False;
  PArgs.Result := @Result;
  TDcefBUtils.SendMsg(browser, WM_GetAuthCredentials, LParam(PArgs));
  Dispose(PArgs);
end;

// Called on the IO thread
function TDcefBRequestHandler.GetResourceHandler(const browser: ICefBrowser;
  const frame: ICefFrame; const Request: ICefRequest): ICefResourceHandler;
begin
  FEvents.doOnGetResourceHandler(browser, frame, Request, Result);
end;

// Called on the IO thread
function TDcefBRequestHandler.GetResourceResponseFilter(
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse): ICefResponseFilter;
begin
  FEvents.doOnGetResourceResponseFilter(browser, frame, request, response, Result);
end;

// Called on the UI thread
function TDcefBRequestHandler.OnBeforeBrowse(const browser: ICefBrowser;
  const frame: ICefFrame; const Request: ICefRequest;
  isRedirect: Boolean): Boolean;
begin
  Result := False;
  FEvents.doOnBeforeBrowse(browser, frame, Request, isRedirect, Result);
end;

// Called on the IO thread
function TDcefBRequestHandler.OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue;
begin
  Result := TCefReturnValue.RV_CONTINUE;
  FEvents.doOnBeforeResourceLoad(browser, frame, request, callback, Result);
end;

// Called on the UI thread
function TDcefBRequestHandler.OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode;
      const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean;
begin
  Result := False;
  FEvents.doOnCertificateError(browser, certError, requestUrl, sslInfo, callback, Result);
end;

// Called on the UI thread
function TDcefBRequestHandler.OnOpenUrlFromTab(const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean;
begin
  inherited;
  Result := False;
  FEvents.doOnOpenUrlFromTab(browser, frame, targetUrl, targetDisposition, userGesture, Result);
end;

// Called on the browser process UI thread
procedure TDcefBRequestHandler.OnPluginCrashed(const browser: ICefBrowser;
  const pluginPath: ustring);
begin
  inherited;
  FEvents.doOnPluginCrashed(browser, pluginPath);
end;

// Called on the UI thread
procedure TDcefBRequestHandler.OnProtocolExecution(const browser: ICefBrowser;
  const url: ustring; out allowOsExecution: Boolean);
begin
  inherited;
  FEvents.doOnProtocolExecution(browser, url, allowOsExecution);
end;

// Called on the IO thread
function TDcefBRequestHandler.OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring;
      newSize: Int64; const callback: ICefRequestCallback): Boolean;
begin
  Result := False;
  FEvents.doOnQuotaRequest(browser, originUrl, newSize, callback, Result);
end;

// Called on the browser process UI thread
procedure TDcefBRequestHandler.OnRenderProcessTerminated(const browser
  : ICefBrowser; status: TCefTerminationStatus);
begin
  inherited;
  FEvents.doOnRenderProcessTerminated(browser, status);
end;

// Called on the browser process UI thread
procedure TDcefBRequestHandler.OnRenderViewReady(const browser: ICefBrowser);
begin
  inherited;
  //FEvents.doOnRenderViewReady(browser);
end;

// Called on the IO thread
procedure TDcefBRequestHandler.OnResourceLoadComplete(
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse;
  status: TCefUrlRequestStatus; receivedContentLength: Int64);
begin
  inherited;
  FEvents.doOnResourceLoadComplete(browser, frame, request, response, status, receivedContentLength);
end;

// Called on the IO thread
procedure TDcefBRequestHandler.OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; var newUrl: ustring);
begin
  inherited;
  FEvents.doOnResourceRedirect(browser, frame, request, newUrl);
end;

// Called on the IO thread
function TDcefBRequestHandler.OnResourceResponse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const response: ICefResponse): Boolean;
begin
  inherited;
  Result := False;
  FEvents.doOnResourceResponse(browser, frame, request, response, Result);
end;

end.
