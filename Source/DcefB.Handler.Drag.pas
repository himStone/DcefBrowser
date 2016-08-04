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

unit DcefB.Handler.Drag;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBDragHandler = class(TCefDragHandlerOwn)
  private
    FEvents: IDcefBrowser;
  protected
    function OnDragEnter(const browser: ICefBrowser;
      const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
      override;
    procedure OnDraggableRegionsChanged(const browser: ICefBrowser;
      regionsCount: NativeUInt; regions: PCefDraggableRegionArray); override;
  public
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDcefBDragHandler }

constructor TDcefBDragHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
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

procedure TDcefBDragHandler.OnDraggableRegionsChanged(
  const browser: ICefBrowser; regionsCount: NativeUInt;
  regions: PCefDraggableRegionArray);
begin
  inherited;
  FEvents.doOnDraggableRegionsChanged(browser, regionsCount, regions);
end;

end.
