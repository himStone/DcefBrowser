(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
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
  CltHandleDicLocker: TDcefBLocker;
  BrowserDicLocker: TDcefBLocker;
  ClosedUrlListLocker: TDcefBLocker;

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
CltHandleDicLocker := TDcefBLocker.Create;
BrowserDicLocker := TDcefBLocker.Create;
ClosedUrlListLocker := TDcefBLocker.Create;

Finalization

CltHandleDicLocker.Free;
BrowserDicLocker.Free;
ClosedUrlListLocker.Free;

end.
