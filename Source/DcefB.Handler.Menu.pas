(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Handler.Menu;

interface

uses
  Winapi.Windows, System.Classes,
  DcefB.Dcef3.CefLib, DcefB.Events, DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBContextMenuHandler = class(TCefContextMenuHandlerOwn)
  private
    FEvents: IDcefBEvents;
  protected
    procedure OnBeforeContextMenu(const browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      const model: ICefMenuModel); override;
    function OnContextMenuCommand(const browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      commandId: Integer; eventFlags: TCefEventFlags): Boolean; override;
    procedure OnContextMenuDismissed(const browser: ICefBrowser;
      const frame: ICefFrame); override;
  public
    constructor Create(aDcefBEvents: IDcefBEvents); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDcefBContextMenuHandler }

constructor TDcefBContextMenuHandler.Create(aDcefBEvents: IDcefBEvents);
begin
  inherited Create;
  FEvents := aDcefBEvents;
end;

destructor TDcefBContextMenuHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

procedure TDcefBContextMenuHandler.OnBeforeContextMenu(const browser
  : ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams;
  const model: ICefMenuModel);
{ var
  PArgs: PBeforeContextMenuArgs; }
begin
  inherited;
  { New(PArgs);
    PArgs.frame := @frame;
    PArgs.params := @params;
    PArgs.model := @model;
    TDcefBUtils.SendMsg(browser, WM_BeforeContextMenu, LParam(PArgs));
    Dispose(PArgs); }
  FEvents.doOnBeforeContextMenu(browser, frame, params, model);
end;

function TDcefBContextMenuHandler.OnContextMenuCommand(const browser
  : ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams;
  commandId: Integer; eventFlags: TCefEventFlags): Boolean;
var
  PArgs: PContextMenuCommandArgs;
begin
  New(PArgs);
  PArgs.frame := @frame;
  PArgs.params := @params;
  PArgs.commandId := commandId;
  PArgs.eventFlags := @eventFlags;
  PArgs.Result := @Result;
  TDcefBUtils.SendMsg(browser, WM_ContextMenuCommand, LParam(PArgs));
  Dispose(PArgs);
end;

procedure TDcefBContextMenuHandler.OnContextMenuDismissed
  (const browser: ICefBrowser; const frame: ICefFrame);
begin
  inherited;
  // TDcefBUtils.SendMsg(browser, WM_ContextMenuDismissed, @frame);

  FEvents.doOnContextMenuDismissed(browser, frame);
end;

end.
