(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Handler.Request;

interface

uses
  Winapi.Windows, System.Classes,
  DcefB.Dcef3.CefLib, DcefB.Events, DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBRequestHandler = class(TCefRequestHandlerOwn)
  private
    FEvents: IDcefBEvents;
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
    constructor Create(aDcefBEvents: IDcefBEvents); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomRequestHandler }

constructor TDcefBRequestHandler.Create(aDcefBEvents: IDcefBEvents);
begin
  inherited Create;
  FEvents := aDcefBEvents;
end;

destructor TDcefBRequestHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

function TDcefBRequestHandler.GetAuthCredentials(const browser: ICefBrowser;
  const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer;
  const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
begin
  Result := False;
  FEvents.doOnGetAuthCredentials(browser, frame, isProxy, host, port, realm,
    scheme, callback, Result);
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
