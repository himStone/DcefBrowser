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

unit DcefB.Handler.LifeSpan;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject, DcefB.Cef3.Helper;

type
  TDcefBLifeSpanHandler = class(TCefLifeSpanHandlerOwn)
  private
    FEvents: IDcefBrowser;
  protected
    function OnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame;
      const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean): Boolean; override;
    procedure OnAfterCreated(const browser: ICefBrowser); override;
    procedure OnBeforeClose(const browser: ICefBrowser); override;
    function DoClose(const browser: ICefBrowser): Boolean; override;
  public
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomLifeSpanHandler }

constructor TDcefBLifeSpanHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
end;

destructor TDcefBLifeSpanHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

function TDcefBLifeSpanHandler.DoClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;
  TDcefBUtils.SendMsg(browser, WM_DoClose, LParam(@Result));
end;

procedure TDcefBLifeSpanHandler.OnAfterCreated(const browser: ICefBrowser);
begin
  inherited;
  TDcefBUtils.SendMsg(browser, WM_NewBrowser, LParam(browser.Identifier));
end;

procedure TDcefBLifeSpanHandler.OnBeforeClose(const browser: ICefBrowser);
begin
  inherited;
  TDcefBUtils.SendMsg(browser, WM_BeforeClose, 0);
end;

function TDcefBLifeSpanHandler.OnBeforePopup(const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var noJavascriptAccess: Boolean): Boolean;
var
  PArgs: PBeforePopupArgs;
begin
  new(PArgs);
  PArgs.frame := @frame;
  PArgs.targetUrl := @targetUrl;
  PArgs.targetFrameName := @targetFrameName;
  PArgs.targetDisposition := @targetDisposition;
  PArgs.userGesture := @userGesture;
  PArgs.popupFeatures := @popupFeatures;
  PArgs.windowInfo := @windowInfo;
  PArgs.client := @client;
  PArgs.settings := @settings;
  PArgs.noJavascriptAccess := @noJavascriptAccess;
  PArgs.Result := @Result;
  PArgs.CancelDefaultEvent := False;

  if Not TDcefBUtils.SendMsg(browser, WM_WindowCheck, LParam(PArgs)) then
  begin
    Result := True;
    Dispose(PArgs);
    Exit;
  end;

  if Not TDcefBUtils.SendMsg(browser, WM_CreateWindow, LParam(PArgs)) then
  begin
    Result := True;
    Exit;
  end;
  Dispose(PArgs);
  Result := False;
end;

end.
