(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Handler.Render;

interface

uses
  WinApi.Windows, System.Classes,
  DcefB.Dcef3.CefLib, DcefB.Events, DcefB.res, DcefB.Utils;

type
  TDcefBRenderHandler = class(TCefRenderHandlerOwn)
  private
    FEvents: IDcefBEvents;
  protected
    function GetRootScreenRect(const browser: ICefBrowser; rect: PCefRect)
      : Boolean; override;
    function GetViewRect(const browser: ICefBrowser; rect: PCefRect)
      : Boolean; override;
    function GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer;
      screenX, screenY: PInteger): Boolean; override;
    procedure OnPopupShow(const browser: ICefBrowser; show: Boolean); override;
    procedure OnPopupSize(const browser: ICefBrowser;
      const rect: PCefRect); override;
    procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
      dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray;
      const buffer: Pointer; width, height: Integer); override;
    procedure OnCursorChange(const browser: ICefBrowser;
      cursor: TCefCursorHandle; cursorType: TCefCursorType;
      const customCursorInfo: PCefCursorInfo); override;
    function GetScreenInfo(const browser: ICefBrowser;
      screenInfo: PCefScreenInfo): Boolean; override;
    function OnStartDragging(const browser: ICefBrowser;
      const dragData: ICefDragData; allowedOps: TCefDragOperations;
      x, y: Integer): Boolean; override;
    procedure OnUpdateDragCursor(const browser: ICefBrowser;
      operation: TCefDragOperation); override;
    procedure OnScrollOffsetChanged(const browser: ICefBrowser); override;
  public
    constructor Create(aDcefBEvents: IDcefBEvents); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDcefBRenderHandler }

constructor TDcefBRenderHandler.Create(aDcefBEvents: IDcefBEvents);
begin
  FEvents := aDcefBEvents;
end;

destructor TDcefBRenderHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

function TDcefBRenderHandler.GetRootScreenRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

function TDcefBRenderHandler.GetScreenInfo(const browser: ICefBrowser;
  screenInfo: PCefScreenInfo): Boolean;
begin
  Result := False;
end;

function TDcefBRenderHandler.GetScreenPoint(const browser: ICefBrowser;
  viewX, viewY: Integer; screenX, screenY: PInteger): Boolean;
begin
  Result := False;
end;

function TDcefBRenderHandler.GetViewRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

procedure TDcefBRenderHandler.OnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle; cursorType: TCefCursorType;
  const customCursorInfo: PCefCursorInfo);
begin
  inherited;

end;

procedure TDcefBRenderHandler.OnPaint(const browser: ICefBrowser;
  kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer;
  width, height: Integer);
begin
  inherited;

end;

procedure TDcefBRenderHandler.OnPopupShow(const browser: ICefBrowser;
  show: Boolean);
begin
  inherited;

end;

procedure TDcefBRenderHandler.OnPopupSize(const browser: ICefBrowser;
  const rect: PCefRect);
begin
  inherited;

end;

procedure TDcefBRenderHandler.OnScrollOffsetChanged(const browser: ICefBrowser);
begin
  inherited;

end;

function TDcefBRenderHandler.OnStartDragging(const browser: ICefBrowser;
  const dragData: ICefDragData; allowedOps: TCefDragOperations;
  x, y: Integer): Boolean;
begin
  Result := False;
end;

procedure TDcefBRenderHandler.OnUpdateDragCursor(const browser: ICefBrowser;
  operation: TCefDragOperation);
begin
  inherited;

end;

end.
