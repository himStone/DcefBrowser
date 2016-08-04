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

unit DcefB.Handler.Render;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Cef3.Api,
  DcefB.Events, DcefB.res, DcefB.Utils;

type
  TDcefBRenderHandler = class(TCefRenderHandlerOwn)
  private
    FEvents: IDcefBrowser;
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
    procedure OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double); override;
  public
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDcefBRenderHandler }

constructor TDcefBRenderHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  FEvents := aDcefBrowser;
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

procedure TDcefBRenderHandler.OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
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
