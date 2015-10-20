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

unit DcefB.Core.DefaultRenderHandler;

interface

uses
  System.Classes, System.SysUtils, DcefB.Dcef3.CefLib, DcefB.BaseObject,
  DcefB.res;

type
  TRenderProcessCallbackA = reference to procedure(aBrowser: ICefBrowser;
    aContext: ICefv8Context; aData: Pointer);

  TDefaultRenderProcessHandler = class(TCefRenderProcessHandlerOwn)
  protected
    procedure OnWebKitInitialized; override;
    function OnProcessMessageReceived(const Browser: ICefBrowser;
      sourceProcess: TCefProcessId; const Message: ICefProcessMessage)
      : Boolean; override;
  public
    class var Classes: TDynClassArr;
  end;

implementation

{ TDefaultRenderProcessHandler }

function TDefaultRenderProcessHandler.OnProcessMessageReceived
  (const Browser: ICefBrowser; sourceProcess: TCefProcessId;
  const Message: ICefProcessMessage): Boolean;
  procedure DoRunInRender;
  var
    aBrowser: ICefBrowser;
    ATemp: Pointer;
    AProc: TRenderProcessCallbackA;
    aData: Pointer;
  begin
    aBrowser := ICefBrowser
      (Pointer(StrToInt64('$' + message.ArgumentList.GetString(0))));
    ATemp := Pointer(StrToInt64('$' + message.ArgumentList.GetString(1)));
    AProc := TRenderProcessCallbackA(ATemp);
    aData := Pointer(StrToInt64('$' + message.ArgumentList.GetString(2)));
    if Assigned(AProc) then
      AProc(aBrowser, Browser.MainFrame.GetV8Context, aData);
    TRenderProcessCallbackA(ATemp) := nil;
  end;

begin
  if message.Name = '@dcefbrowser_runinrender' then
  begin
    DoRunInRender;
    Result := True;
  end
  else
    Result := False;
end;

procedure TDefaultRenderProcessHandler.OnWebKitInitialized;
var
  Index: Integer;
begin
  inherited;
  for Index := Low(Classes) to High(Classes) do
  begin
    TCefRTTIExtension.Register(Classes[Index].ClassName, Classes[Index]
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}, True {$ENDIF});
  end;
end;

end.
