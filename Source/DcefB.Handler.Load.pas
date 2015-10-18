(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Handler.Load;

interface

uses
  Winapi.Windows, System.Classes,
  DcefB.Dcef3.CefLib, DcefB.Events, DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBLoadHandler = class(TCefLoadHandlerOwn)
  private
    FEvents: IDcefBEvents;
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
    constructor Create(aDcefBEvents: IDcefBEvents); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomLoadHandler }

constructor TDcefBLoadHandler.Create(aDcefBEvents: IDcefBEvents);
begin
  inherited Create;
  FEvents := aDcefBEvents;
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
