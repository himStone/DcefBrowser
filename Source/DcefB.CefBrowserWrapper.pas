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
  * QQ: 1262807955
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

unit DcefB.CefBrowserWrapper;

interface

uses
  System.Classes, Generics.Collections,
  DcefB.Dcef3.CefLib, DcefB.res;

type
  TCefBrowserWrapper = class
  private
    FBrowser: ICefBrowser;
    FLoadingState: Integer;
    FLastTitle: string;
    FLastAddress: string;
  public
    constructor Create(aBrowser: ICefBrowser);
    destructor Destroy;

    property LoadingState: Integer read FLoadingState write FLoadingState;
    property LastTitle: string read FLastTitle write FLastTitle;
    property LastAddress: string read FLastAddress write FLastAddress;
    property Browser: ICefBrowser read FBrowser;
  end;

  TBrowserWrapperDic = class(TDictionary<Integer, TCefBrowserWrapper>)
  public
    function Add(aBrowser: ICefBrowser): Integer;
  end;

implementation

{ ICefBrowserWapper }

constructor TCefBrowserWrapper.Create(aBrowser: ICefBrowser);
begin
  FBrowser := aBrowser;
  FLastTitle := SLoadingText;
  FLastAddress := '';
  FLoadingState := 0 or State_IsLoading;
end;

destructor TCefBrowserWrapper.Destroy;
begin
  FBrowser := nil;
end;

{ TBrowserWrapperDic }

function TBrowserWrapperDic.Add(aBrowser: ICefBrowser): Integer;
var
  aCefBrowserWrapper: TCefBrowserWrapper;
begin
  aCefBrowserWrapper := TCefBrowserWrapper.Create(aBrowser);
  inherited Add(aBrowser.Identifier, aCefBrowserWrapper);
end;

end.
