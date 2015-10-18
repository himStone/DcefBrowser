(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Handler.Focus;

interface

uses
  Winapi.Windows, System.Classes,
  DcefB.Dcef3.CefLib, DcefB.Events, DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBFocusHandler = class(TCefFocusHandlerOwn)
  private
    FEvents: IDcefBEvents;
  protected
    procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); override;
    function OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource)
      : Boolean; override;
    procedure OnGotFocus(const browser: ICefBrowser); override;
  public
    constructor Create(aDcefBEvents: IDcefBEvents); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDcefBFocusHandler }

constructor TDcefBFocusHandler.Create(aDcefBEvents: IDcefBEvents);
begin
  inherited Create;
  FEvents := aDcefBEvents;
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
