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

unit DcefB.Handler.Display;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBDisplayHandler = class(TCefDisplayHandlerOwn)
  private
    FEvents: IDcefBrowser;
  protected
    procedure OnAddressChange(const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring); override;
    procedure OnTitleChange(const browser: ICefBrowser;
      const title: ustring); override;
    procedure OnFaviconUrlChange(const browser: ICefBrowser; iconUrls: TStrings); override;
    procedure OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean); override;
    function OnTooltip(const browser: ICefBrowser; var text: ustring)
      : Boolean; override;
    procedure OnStatusMessage(const browser: ICefBrowser;
      const value: ustring); override;
    function OnConsoleMessage(const browser: ICefBrowser;
      const message, source: ustring; line: Integer): Boolean; override;
  public
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomDisplayHandler }

constructor TDcefBDisplayHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
end;

destructor TDcefBDisplayHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

procedure TDcefBDisplayHandler.OnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin
  inherited;
  TDcefBUtils.SendMsg(browser, WM_AddressChange, LParam(@url));
end;

function TDcefBDisplayHandler.OnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: Integer): Boolean;
var
  PArgs: PConsoleMessageArgs;
begin
  Result := False;
  New(PArgs);
  PArgs.message := @message;
  PArgs.source := @source;
  PArgs.line := line;
  PArgs.Result := @Result;
  TDcefBUtils.SendMsg(browser, WM_ConsoleMessage, LParam(PArgs));
  Dispose(PArgs);
end;

procedure TDcefBDisplayHandler.OnFaviconUrlChange(const browser: ICefBrowser;
  iconUrls: TStrings);
begin
  inherited;
  TDcefBUtils.SendMsg(browser, WM_FaviconUrlChange, LParam(@iconUrls));
end;

procedure TDcefBDisplayHandler.OnFullScreenModeChange(
  const browser: ICefBrowser; fullscreen: Boolean);
begin
  inherited;
  TDcefBUtils.SendMsg(browser, WM_FullScreenModeChange, LParam(@fullscreen));
end;

procedure TDcefBDisplayHandler.OnStatusMessage(const browser: ICefBrowser;
  const value: ustring);
begin
  inherited;
  TDcefBUtils.SendMsg(browser, WM_StatusMessage, LParam(@value));
end;

procedure TDcefBDisplayHandler.OnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin
  inherited;
  TDcefBUtils.SendMsg(browser, WM_TitleChange, LParam(@title));
end;

function TDcefBDisplayHandler.OnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
var
  PArgs: PTooltipArgs;
begin
  Result := False;
  New(PArgs);
  PArgs.text := @text;
  PArgs.Result := @Result;
  TDcefBUtils.SendMsg(browser, WM_Tooltip, LParam(PArgs));
  Dispose(PArgs);
end;

end.
