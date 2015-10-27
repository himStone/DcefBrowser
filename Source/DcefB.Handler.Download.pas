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

unit DcefB.Handler.Download;

interface

uses
  Windows, Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBDownloadHandler = class(TCefDownloadHandlerOwn)
  private
    FEvents: IDcefBrowser;
  protected
    procedure OnBeforeDownload(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback); override;
    procedure OnDownloadUpdated(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback); override;
  public
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomDownloadHandler }

constructor TDcefBDownloadHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
end;

destructor TDcefBDownloadHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

procedure TDcefBDownloadHandler.OnBeforeDownload(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
var
  PArgs: PBeforeDownloadArgs;
  // CancelDefaultEvent: Boolean;
begin
  inherited;
  New(PArgs);
  PArgs.downloadItem := @downloadItem;
  PArgs.suggestedName := @suggestedName;
  PArgs.callback := @callback;
  PArgs.CancelDefaultEvent := False;
  TDcefBUtils.SendMsg(browser, WM_BeforeDownload, LParam(PArgs));
  Dispose(PArgs);
  { FEvents.doOnBeforeDownload(browser, downloadItem, suggestedName, callback,
    CancelDefaultEvent); }
end;

procedure TDcefBDownloadHandler.OnDownloadUpdated(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
{ var
  PArgs: PDownloadUpdatedArgs; }
begin
  inherited;
  { New(PArgs);
    PArgs.downloadItem := @downloadItem;
    PArgs.callback := @callback;
    TDcefBUtils.SendMsg(browser, WM_DownloadUpdated, LParam(PArgs));
    Dispose(PArgs); }
  FEvents.doOnDownloadUpdated(browser, downloadItem, callback);
end;

end.
