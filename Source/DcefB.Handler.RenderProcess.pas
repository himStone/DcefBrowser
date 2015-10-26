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

unit DcefB.Handler.RenderProcess;

interface

uses
  System.Classes, System.SysUtils, DcefB.Cef3.Interfaces, DcefB.Cef3.Classes,
  DcefB.Cef3.Types, DcefB.Cef3.Api, DcefB.BaseObject, DcefB.Events,
  DcefB.res, Rtti, TypInfo, Variants;

type
  TDcefBRenderProcessHandler = class(TCefRenderProcessHandlerOwn)
  protected
    procedure OnRenderThreadCreated(const extraInfo: ICefListValue); override;
    procedure OnWebKitInitialized; override;
    procedure OnBrowserCreated(const browser: ICefBrowser); override;
    procedure OnBrowserDestroyed(const browser: ICefBrowser); override;
    function GetLoadHandler: PCefLoadHandler; override;
    function OnBeforeNavigation(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      navigationType: TCefNavigationType; isRedirect: Boolean)
      : Boolean; override;
    procedure OnContextCreated(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefv8Context); override;
    procedure OnContextReleased(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefv8Context); override;
    procedure OnUncaughtException(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefv8Context;
      const exception: ICefV8Exception;
      const stackTrace: ICefV8StackTrace); override;
    procedure OnFocusedNodeChanged(const browser: ICefBrowser;
      const frame: ICefFrame; const node: ICefDomNode); override;
    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage)
      : Boolean; override;
  public
    class var RegParList: Array of TRegExtentionPar;
  end;

implementation

uses
  DcefB.Core.App;

{ TDefaultRenderProcessHandler }

function TDcefBRenderProcessHandler.GetLoadHandler: PCefLoadHandler;
begin
  Result := nil;
end;

function TDcefBRenderProcessHandler.OnBeforeNavigation(const browser
  : ICefBrowser; const frame: ICefFrame; const request: ICefRequest;
  navigationType: TCefNavigationType; isRedirect: Boolean): Boolean;
begin
  Result := False;
end;

procedure TDcefBRenderProcessHandler.OnBrowserCreated(const browser
  : ICefBrowser);
begin
  inherited;

end;

procedure TDcefBRenderProcessHandler.OnBrowserDestroyed(const browser
  : ICefBrowser);
begin
  inherited;

end;

procedure TDcefBRenderProcessHandler.OnContextCreated(const browser
  : ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
begin
  inherited;

end;

procedure TDcefBRenderProcessHandler.OnContextReleased(const browser
  : ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
begin
  inherited;

end;

procedure TDcefBRenderProcessHandler.OnFocusedNodeChanged
  (const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
begin
  inherited;

end;

function TDcefBRenderProcessHandler.OnProcessMessageReceived
  (const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage): Boolean;
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


procedure TDcefBRenderProcessHandler.OnRenderThreadCreated(const extraInfo
  : ICefListValue);
begin
  inherited;

end;

procedure TDcefBRenderProcessHandler.OnUncaughtException(const browser
  : ICefBrowser; const frame: ICefFrame; const context: ICefv8Context;
  const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
begin
  inherited;

end;

procedure TDcefBRenderProcessHandler.OnWebKitInitialized;
var
  Index: Integer;
begin
  inherited;

  for Index := Low(RegParList) to High(RegParList) do
  begin
    if (RegParList[Index].code = '') and (RegParList[Index].Handler = nil) then
      DcefBApp.CefRegisterExtension(RegParList[Index].name,
        RegParList[Index].value
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}, True {$ENDIF})
    else
      DcefBApp.CefRegisterExtension(RegParList[Index].name,
        RegParList[Index].code, RegParList[Index].Handler);
  end;
end;

end.
