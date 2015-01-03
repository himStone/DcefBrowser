unit dcefb_Options;

interface

uses
  System.Classes, System.StrUtils, System.SysUtils, Winapi.Windows;

type
  TDcefBrowserOptions = class(TPersistent)
  private
    FDefaultDownLoadPath: string; // 值为 APP路径\Download\
    FTerminateAppWhenAllPageClosed: Boolean; // 所有标签关闭后是否终止APP 默认True
    FTerminateAppWhenDownloading: Boolean;
    // 正在执行下载时是否终止APP并退出下载 只有FTerminateAppWhenAllPageClosed为True时这个设置才有效 默认为False
    FPopupNewWindow: Boolean;
    // 是否弹出新窗口 这样就和TChromium没区别了 这个新窗口将不受TDcefBrowser控制 默认为False
    FDebugToolAvailable: Boolean;
    //是否允许使用F12 DebugTool 默认为True
    FMainFormWinHandle: HWND;
    // APP主窗体的Handle 这个设置是必要的！ 否则在某些情况下焦点会有问题 默认为0

    // ---------------------------------------------------------------------------
    FAutoDown: Boolean; // 是否自动完成下载 默认True
    FDownLoadPath: string; // 下载路径 默认为FDefaultDownLoadPath
    procedure SetDownLoadPath(const value: string);
    procedure SetDebugToolAvailable(const Value: Boolean);
  public
    constructor Create;
  published
    property TerminateAppWhenAllPageClosed: Boolean
      read FTerminateAppWhenAllPageClosed write FTerminateAppWhenAllPageClosed;
    property TerminateAppWhenDownloading: Boolean
      read FTerminateAppWhenDownloading write FTerminateAppWhenDownloading;
    property PopupNewWindow: Boolean read FPopupNewWindow write FPopupNewWindow;
    property DebugToolAvailable: Boolean read FDebugToolAvailable write SetDebugToolAvailable;
    property MainFormWinHandle: HWND read FMainFormWinHandle
      write FMainFormWinHandle;
    property AutoDown: Boolean read FAutoDown write FAutoDown;
    property DownLoadPath: string read FDownLoadPath write SetDownLoadPath;
  end;

implementation

{ TDcefBrowserOptions }

constructor TDcefBrowserOptions.Create;
begin
  FTerminateAppWhenAllPageClosed := True;
  FTerminateAppWhenDownloading := False;
  FDebugToolAvailable := True;
  FPopupNewWindow := False;
  FMainFormWinHandle := 0;
  FAutoDown := True;
  FDefaultDownLoadPath := ExtractFilePath(Paramstr(0)) + 'Download\';
  FDownLoadPath := FDefaultDownLoadPath;
end;

procedure TDcefBrowserOptions.SetDebugToolAvailable(const Value: Boolean);
begin
  FDebugToolAvailable := Value;
end;

procedure TDcefBrowserOptions.SetDownLoadPath(const value: string);
begin
  if DirectoryExists(value) then
    FDownLoadPath := IfThen(SameText(Copy(value, Length(value), 1), '\'), value,
      value + '\')
  else
    FDownLoadPath := FDefaultDownLoadPath;
end;

end.
