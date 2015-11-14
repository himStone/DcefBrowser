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

unit DcefB.Locker;

interface

uses
  Windows;

type
  TDcefBLocker = class
  private
    FCS: TRTLCriticalSection;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Enter();
    procedure Exit();
  end;

var
  BrowserDicLocker: TDcefBLocker;
  DevToolsBroListLocker: TDcefBLocker;
  ClosedUrlListLocker: TDcefBLocker;
  JsExtention: TDcefBLocker;


implementation

var
  CpuCount: Integer;

function GetCPUCount: Integer;
{$IFDEF MSWINDOWS}
var
  si: SYSTEM_INFO;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetSystemInfo(si);
  Result := si.dwNumberOfProcessors;
{$ELSE}// Linux,MacOS,iOS,Andriod{POSIX}
{$IFDEF POSIX}
  Result := sysconf(_SC_NPROCESSORS_ONLN);
{$ELSE}// unkown system, default 1
  Result := 1;
{$ENDIF !POSIX}
{$ENDIF !MSWINDOWS}
end;

{ TDcefBLocker }

constructor TDcefBLocker.Create;
var
  IsSuccess: Boolean;
begin
  IsSuccess := (CpuCount > 1) and InitializeCriticalSectionAndSpinCount
    (FCS, 4000);

  if Not IsSuccess then
    InitializeCriticalSection(FCS);
end;

destructor TDcefBLocker.Destroy;
begin
  DeleteCriticalSection(FCS);
  inherited;
end;

procedure TDcefBLocker.Enter;
begin
  EnterCriticalSection(FCS);
end;

procedure TDcefBLocker.Exit;
begin
  LeaveCriticalSection(FCS);
end;

Initialization

CpuCount := GetCPUCount;
BrowserDicLocker := TDcefBLocker.Create;
DevToolsBroListLocker := TDcefBLocker.Create;
ClosedUrlListLocker := TDcefBLocker.Create;
JsExtention := TDcefBLocker.Create;

Finalization

BrowserDicLocker.Free;
DevToolsBroListLocker.Free;
ClosedUrlListLocker.Free;
JsExtention.Free;

end.
