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

unit DcefB.Handler.BrowserProcess;

interface

uses
  Classes, SysUtils,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types,
  DcefB.Cef3.Api, DcefB.BaseObject, DcefB.res, DcefB.Events;

type
  TDcefBBrowserProcessHandler = class(TCefBrowserProcessHandlerOwn,
    IDcefBBrowserProcessHandler)
  private
    FOnBeforeChildProcessLaunch: TOnBeforeChildProcessLaunch;
    FOnRenderProcessThreadCreated: TOnRenderProcessThreadCreated;
    FOnContextInitialized: TOnContextInitialized;
    function GetOnBeforeChildProcessLaunch: TOnBeforeChildProcessLaunch;
    function GetOnRenderProcessThreadCreated: TOnRenderProcessThreadCreated;
    function GetOnContextInitialized: TOnContextInitialized;
    procedure SetOnBeforeChildProcessLaunch(const Value
      : TOnBeforeChildProcessLaunch);
    procedure SetOnContextInitialized(const Value: TOnContextInitialized);
    procedure SetOnRenderProcessThreadCreated(const Value
      : TOnRenderProcessThreadCreated);
  protected
    procedure OnContextInitialized; override;
    procedure OnBeforeChildProcessLaunch(const commandLine
      : ICefCommandLine); override;
    procedure OnRenderProcessThreadCreated(const extraInfo
      : ICefListValue); override;
  end;

implementation

{ TDcefBBrowserProcessHandler }

function TDcefBBrowserProcessHandler.GetOnBeforeChildProcessLaunch
  : TOnBeforeChildProcessLaunch;
begin
  Result := FOnBeforeChildProcessLaunch;
end;

function TDcefBBrowserProcessHandler.GetOnContextInitialized
  : TOnContextInitialized;
begin
  Result := FOnContextInitialized;
end;

function TDcefBBrowserProcessHandler.GetOnRenderProcessThreadCreated
  : TOnRenderProcessThreadCreated;
begin
  Result := FOnRenderProcessThreadCreated;
end;

procedure TDcefBBrowserProcessHandler.OnBeforeChildProcessLaunch
  (const commandLine: ICefCommandLine);
begin
  inherited;
  if Assigned(FOnBeforeChildProcessLaunch) then
    FOnBeforeChildProcessLaunch(commandLine);
end;

procedure TDcefBBrowserProcessHandler.OnContextInitialized;
begin
  inherited;
  if Assigned(FOnContextInitialized) then
    FOnContextInitialized();
end;

procedure TDcefBBrowserProcessHandler.OnRenderProcessThreadCreated
  (const extraInfo: ICefListValue);
begin
  inherited;
  if Assigned(FOnRenderProcessThreadCreated) then
    FOnRenderProcessThreadCreated(extraInfo);
end;

procedure TDcefBBrowserProcessHandler.SetOnBeforeChildProcessLaunch
  (const Value: TOnBeforeChildProcessLaunch);
begin
  FOnBeforeChildProcessLaunch := Value;
end;

procedure TDcefBBrowserProcessHandler.SetOnContextInitialized
  (const Value: TOnContextInitialized);
begin
  FOnContextInitialized := Value;
end;

procedure TDcefBBrowserProcessHandler.SetOnRenderProcessThreadCreated
  (const Value: TOnRenderProcessThreadCreated);
begin
  FOnRenderProcessThreadCreated := Value;
end;

end.
