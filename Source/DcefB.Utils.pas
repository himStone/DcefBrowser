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

unit DcefB.Utils;

interface

uses
  WinApi.Windows, System.Classes, DcefB.Dcef3.CefLib;

type
  TDcefBUtils = record
    class function GetCefParentWindow(aBrowser: ICefBrowser): HWND; static;
    class function SendMsg(aBrowser: ICefBrowser; Msg: UINT; LParam: LParam)
      : Boolean; static;
  end;

implementation

{ TDcefBUtils }

class function TDcefBUtils.GetCefParentWindow(aBrowser: ICefBrowser): HWND;
begin
  Result := GetParent(aBrowser.host.WindowHandle);
end;

class function TDcefBUtils.SendMsg(aBrowser: ICefBrowser; Msg: UINT;
  LParam: LParam): Boolean;
begin
  Result := SendMessage(GetCefParentWindow(aBrowser), Msg, WParam(@aBrowser),
    LParam) <> S_FALSE;
end;

end.
