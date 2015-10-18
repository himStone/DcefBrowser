(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Handler.JsDialog;

interface

uses
  WinApi.Windows, System.Classes,
  DcefB.Dcef3.CefLib, DcefB.Events, DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBJsDialogHandler = class(TCefJsDialogHandlerOwn)
  private
    FEvents: IDcefBEvents;
  protected
    function OnJsdialog(const browser: ICefBrowser;
      const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
      const messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean)
      : Boolean; override;
    function OnBeforeUnloadDialog(const browser: ICefBrowser;
      const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback): Boolean; override;
    procedure OnResetDialogState(const browser: ICefBrowser); override;
    procedure OnDialogClosed(const browser: ICefBrowser); override;
  public
    constructor Create(aDcefBEvents: IDcefBEvents); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomJsDialogHandler }

constructor TDcefBJsDialogHandler.Create(aDcefBEvents: IDcefBEvents);
begin
  inherited Create;
  FEvents := aDcefBEvents;
end;

destructor TDcefBJsDialogHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

function TDcefBJsDialogHandler.OnBeforeUnloadDialog(const browser: ICefBrowser;
  const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback): Boolean;
var
  PArgs: PBeforeUnloadDialogArgs;
begin
  Result := False;
  New(PArgs);
  PArgs.messageText := @messageText;
  PArgs.isReload := isReload;
  PArgs.callback := @callback;
  PArgs.Result := @Result;
  PArgs.CancelDefaultEvent := False;
  Dispose(PArgs);
end;

procedure TDcefBJsDialogHandler.OnDialogClosed(const browser: ICefBrowser);
begin
  inherited;
  // TDcefBUtils.SendMsg(browser, WM_DialogClosed, 0);
  FEvents.doOnDialogClosed(browser);
end;

function TDcefBJsDialogHandler.OnJsdialog(const browser: ICefBrowser;
  const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
  const messageText, defaultPromptText: ustring; callback: ICefJsDialogCallback;
  out suppressMessage: Boolean): Boolean;
var
  PArgs: PJsdialogArgs;
begin
  Result := False;
  New(PArgs);
  PArgs.originUrl := @originUrl;
  PArgs.acceptLang := @acceptLang;
  PArgs.dialogType := @dialogType;
  PArgs.messageText := @messageText;
  PArgs.defaultPromptText := @defaultPromptText;
  PArgs.callback := @callback;
  PArgs.suppressMessage := @suppressMessage;
  PArgs.Result := @Result;
  PArgs.CancelDefaultEvent := False;
  Dispose(PArgs);
end;

procedure TDcefBJsDialogHandler.OnResetDialogState(const browser: ICefBrowser);
begin
  inherited;
  // TDcefBUtils.SendMsg(browser, WM_ResetDialogState, 0);
  FEvents.doOnResetDialogState(browser);
end;

end.
