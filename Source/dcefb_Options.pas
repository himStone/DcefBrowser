unit dcefb_Options;

interface

uses
  System.Classes, System.StrUtils, System.SysUtils, Winapi.Windows;

type
  TDcefBrowserOptions = class(TPersistent)
  private
    FDftDownPath: string; // 值为 APP路径\Download\
    FExitPagesClosed: Boolean; // 所有标签关闭后是否终止APP 默认True
    {
    FExitDownloading: Boolean;
    // 正在执行下载时是否终止APP并退出下载 只有FTerminateAppWhenAllPageClosed为True时这个设置才有效 默认为False
    }
    FPopupNewWin: Boolean;
    // 是否弹出新窗口 这样就和TChromium没区别了 这个新窗口将不受TDcefBrowser控制 默认为False
    FDevToolsEnable: Boolean;
    //是否允许使用F12 DebugTool 默认为True

    // ---------------------------------------------------------------------------
    FAutoDown: Boolean; // 是否自动下载至默认文件夹 默认False
    FDownLoadPath: string; // 下载路径 默认为FDefaultDownLoadPath
    procedure SetDownLoadPath(const value: string);
    procedure SetDevToolsEnable(const Value: Boolean);
  public
    constructor Create;
  published
    property ExitPagesClosed: Boolean
      read FExitPagesClosed write FExitPagesClosed;
    {property ExitDownloading: Boolean
      read FExitDownloading write FExitDownloading; }
    property PopupNewWin: Boolean read FPopupNewWin write FPopupNewWin;
    property DevToolsEnable: Boolean read FDevToolsEnable write SetDevToolsEnable;
    property AutoDown: Boolean read FAutoDown write FAutoDown;
    property DownLoadPath: string read FDownLoadPath write SetDownLoadPath;
  end;

implementation

{ TDcefBrowserOptions }

constructor TDcefBrowserOptions.Create;
begin
  FExitPagesClosed := True;
  //FExitDownloading := False;
  FDevToolsEnable := True;
  FPopupNewWin := False;
  FAutoDown := False;
  FDftDownPath := ExtractFilePath(Paramstr(0)) + 'Download\';
  FDownLoadPath := FDftDownPath;
end;

procedure TDcefBrowserOptions.SetDevToolsEnable(const Value: Boolean);
begin
  FDevToolsEnable := Value;
end;

procedure TDcefBrowserOptions.SetDownLoadPath(const value: string);
begin
  if DirectoryExists(value) then
    FDownLoadPath := IfThen(SameText(Copy(value, Length(value), 1), '\'), value,
      value + '\')
  else
    FDownLoadPath := FDftDownPath;
end;

end.
