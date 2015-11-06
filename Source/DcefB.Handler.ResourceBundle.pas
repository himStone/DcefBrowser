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

unit DcefB.Handler.ResourceBundle;

interface

uses
  Classes, SysUtils, DcefB.Cef3.Interfaces, DcefB.Cef3.Classes,
  DcefB.Cef3.Types, DcefB.BaseObject, DcefB.res, DcefB.Events;

type
  TDcefBResourceBundleHandler = class(TCefResourceBundleHandlerOwn,
    IDcefBResourceBundleHandler)
  private
    FOnGetLocalizedString: TOnGetLocalizedString;
    FOnGetDataResource: TOnGetDataResource;
    procedure SetOnGetDataResource(const Value: TOnGetDataResource);
    procedure SetOnGetLocalizedString(const Value: TOnGetLocalizedString);
    function GetOnGetDataResource: TOnGetDataResource;
    function GetOnGetLocalizedString: TOnGetLocalizedString;
  protected
    function GetDataResource(resourceId: Integer; out data: Pointer;
      out dataSize: NativeUInt): Boolean; override;
    function GetLocalizedString(messageId: Integer; out stringVal: ustring)
      : Boolean; override;
  end;

implementation

{ TDcefBResourceBundleHandler }

function TDcefBResourceBundleHandler.GetDataResource(resourceId: Integer;
  out data: Pointer; out dataSize: NativeUInt): Boolean;
begin
  Result := False;
  if Assigned(FOnGetDataResource) then
    FOnGetDataResource(resourceId, data, dataSize, Result);
end;

function TDcefBResourceBundleHandler.GetLocalizedString(messageId: Integer;
  out stringVal: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnGetLocalizedString) then
    FOnGetLocalizedString(messageId, stringVal, Result);
end;

function TDcefBResourceBundleHandler.GetOnGetDataResource: TOnGetDataResource;
begin
  Result := FOnGetDataResource;
end;

function TDcefBResourceBundleHandler.GetOnGetLocalizedString
  : TOnGetLocalizedString;
begin
  Result := FOnGetLocalizedString;
end;

procedure TDcefBResourceBundleHandler.SetOnGetDataResource
  (const Value: TOnGetDataResource);
begin
  FOnGetDataResource := Value;
end;

procedure TDcefBResourceBundleHandler.SetOnGetLocalizedString
  (const Value: TOnGetLocalizedString);
begin
  FOnGetLocalizedString := Value;
end;

end.
