(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Handler.Drag;

interface

uses
  Winapi.Windows, System.Classes,
  DcefB.Dcef3.CefLib, DcefB.Events, DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBDragHandler = class(TCefDragHandlerOwn)
  private
    FEvents: IDcefBEvents;
  protected
    function OnDragEnter(const browser: ICefBrowser;
      const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
      override;
  public
    constructor Create(aDcefBEvents: IDcefBEvents); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDcefBDragHandler }

constructor TDcefBDragHandler.Create(aDcefBEvents: IDcefBEvents);
begin
  inherited Create;
  FEvents := aDcefBEvents;
end;

destructor TDcefBDragHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

function TDcefBDragHandler.OnDragEnter(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
{ var
  PArgs: PDragEnterArgs; }
begin
  { New(PArgs);
    PArgs.dragData := @dragData;
    PArgs.mask := @mask;
    PArgs.Result := False;
    if TDcefBUtils.SendMsg(browser, WM_DragEnter, LParam(PArgs)) then
    Result := PArgs.Result
    else
    Result := False;
    Dispose(PArgs); }
  Result := False;
  FEvents.doOnDragEnter(browser, dragData, mask, Result);
end;

end.
