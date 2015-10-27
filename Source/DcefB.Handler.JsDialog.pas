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

unit DcefB.Handler.JsDialog;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBJsDialogHandler = class(TCefJsDialogHandlerOwn)
  private
    FEvents: IDcefBrowser;
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
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomJsDialogHandler }

constructor TDcefBJsDialogHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
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
  TDcefBUtils.SendMsg(browser, WM_BeforeUnloadDialog, LParam(PArgs));
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
  TDcefBUtils.SendMsg(browser, WM_Jsdialog, LParam(PArgs));
  Dispose(PArgs);
end;

procedure TDcefBJsDialogHandler.OnResetDialogState(const browser: ICefBrowser);
begin
  inherited;
  // TDcefBUtils.SendMsg(browser, WM_ResetDialogState, 0);
  FEvents.doOnResetDialogState(browser);
end;

end.
