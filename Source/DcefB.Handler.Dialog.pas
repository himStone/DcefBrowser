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

unit DcefB.Handler.Dialog;

interface

uses
  Winapi.Windows, System.Classes,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.Events,
  DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBDialogHandler = class(TCefDialogHandlerOwn)
  private
    FEvents: IDcefBrowser;
  protected
    function OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title: ustring; const defaultFileName: ustring;
      acceptTypes: TStrings; const callback: ICefFileDialogCallback)
      : Boolean; override;
  public
    constructor Create(aDcefBrowser: IDcefBrowser); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDcefBDialogHandler }

constructor TDcefBDialogHandler.Create(aDcefBrowser: IDcefBrowser);
begin
  inherited Create;
  FEvents := aDcefBrowser;
end;

destructor TDcefBDialogHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

function TDcefBDialogHandler.OnFileDialog(const browser: ICefBrowser;
  mode: TCefFileDialogMode; const title, defaultFileName: ustring;
  acceptTypes: TStrings; const callback: ICefFileDialogCallback): Boolean;
var
  PArgs: PFileDialogArgs;
begin
  Result := False;
  new(PArgs);
  PArgs.mode := @mode;
  PArgs.title := @title;
  PArgs.defaultFileName := @defaultFileName;
  PArgs.acceptTypes := @acceptTypes;
  PArgs.callback := @callback;
  PArgs.Result := @Result;
  Dispose(PArgs);
end;

end.
