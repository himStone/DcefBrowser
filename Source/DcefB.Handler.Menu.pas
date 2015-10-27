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

unit DcefB.Handler.Menu;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBContextMenuHandler = class(TCefContextMenuHandlerOwn)
  private
    FEvents: IDcefBrowser;
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
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDcefBContextMenuHandler }

constructor TDcefBContextMenuHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
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
