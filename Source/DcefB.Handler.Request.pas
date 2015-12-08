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
      const Request: ICefRequest; isRedirect: Boolean): Boolean; override;
    function OnBeforeResourceLoad(const browser: ICefBrowser;
      const frame: ICefFrame; const Request: ICefRequest): Boolean; override;
    function GetResourceHandler(const browser: ICefBrowser;
      const frame: ICefFrame; const Request: ICefRequest)
      : ICefResourceHandler; override;
    procedure OnResourceRedirect(const browser: ICefBrowser;
      const frame: ICefFrame; const oldUrl: ustring;
      var newUrl: ustring); override;
    function GetAuthCredentials(const browser: ICefBrowser;
      const frame: ICefFrame; isProxy: Boolean; const host: ustring;
      port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback): Boolean; override;
    function OnQuotaRequest(const browser: ICefBrowser;
      const originUrl: ustring; newSize: Int64;
      const callback: ICefQuotaCallback): Boolean; override;
    procedure OnProtocolExecution(const browser: ICefBrowser;
      const url: ustring; out allowOsExecution: Boolean); override;
    function OnBeforePluginLoad(const browser: ICefBrowser; const url: ustring;
      const policyUrl: ustring; const info: ICefWebPluginInfo)
      : Boolean; override;
    function OnCertificateError(certError: TCefErrorCode;
      const requestUrl: ustring;
      const callback: ICefAllowCertificateErrorCallback): Boolean; override;
    procedure OnPluginCrashed(const browser: ICefBrowser;
      const pluginPath: ustring); override;
    procedure OnRenderProcessTerminated(const browser: ICefBrowser;
      status: TCefTerminationStatus); override;
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

function TDcefBRequestHandler.GetResourceHandler(const browser: ICefBrowser;
  const frame: ICefFrame; const Request: ICefRequest): ICefResourceHandler;
begin
  FEvents.doOnGetResourceHandler(browser, frame, Request, Result);
end;

function TDcefBRequestHandler.OnBeforeBrowse(const browser: ICefBrowser;
  const frame: ICefFrame; const Request: ICefRequest;
  isRedirect: Boolean): Boolean;
begin
  Result := False;
  FEvents.doOnBeforeBrowse(browser, frame, Request, isRedirect, Result);
end;

function TDcefBRequestHandler.OnBeforePluginLoad(const browser: ICefBrowser;
  const url, policyUrl: ustring; const info: ICefWebPluginInfo): Boolean;
begin
  Result := False;
  FEvents.doOnBeforePluginLoad(browser, url, policyUrl, info, Result);
end;

function TDcefBRequestHandler.OnBeforeResourceLoad(const browser: ICefBrowser;
  const frame: ICefFrame; const Request: ICefRequest): Boolean;
begin
  Result := False;
  FEvents.doOnBeforeResourceLoad(browser, frame, Request, Result);
end;

function TDcefBRequestHandler.OnCertificateError(certError: TCefErrorCode;
  const requestUrl: ustring;
  const callback: ICefAllowCertificateErrorCallback): Boolean;
begin
  Result := False;
  FEvents.doOnCertificateError(certError, requestUrl, callback, Result);
end;

procedure TDcefBRequestHandler.OnPluginCrashed(const browser: ICefBrowser;
  const pluginPath: ustring);
begin
  inherited;
  FEvents.doOnPluginCrashed(browser, pluginPath);
end;

procedure TDcefBRequestHandler.OnProtocolExecution(const browser: ICefBrowser;
  const url: ustring; out allowOsExecution: Boolean);
begin
  inherited;
  FEvents.doOnProtocolExecution(browser, url, allowOsExecution);
end;

function TDcefBRequestHandler.OnQuotaRequest(const browser: ICefBrowser;
  const originUrl: ustring; newSize: Int64;
  const callback: ICefQuotaCallback): Boolean;
begin
  Result := False;
  FEvents.doOnQuotaRequest(browser, originUrl, newSize, callback, Result);
end;

procedure TDcefBRequestHandler.OnRenderProcessTerminated(const browser
  : ICefBrowser; status: TCefTerminationStatus);
begin
  inherited;
  FEvents.doOnRenderProcessTerminated(browser, status);
end;

procedure TDcefBRequestHandler.OnResourceRedirect(const browser: ICefBrowser;
  const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring);
begin
  inherited;
  FEvents.doOnResourceRedirect(browser, frame, oldUrl, newUrl);
end;

end.
