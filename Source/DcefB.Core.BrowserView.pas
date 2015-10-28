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

unit DcefB.Core.BrowserView;

interface

uses
  Windows, Classes, Controls, ComCtrls, Forms, ExtCtrls, Dialogs, StrUtils,
  SysUtils, Messages, Math, Generics.Collections,

  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.Types, DcefB.BaseObject,
  DcefB.Locker, DcefB.Settings, DcefB.Events, DcefB.Handler.Focus,
  DcefB.CefBrowserWrapper, DcefB.Dcef3.CefErr, DcefB.Handler.Basic,
  DcefB.Core.BrowserHandler, DcefB.res, DcefB.Utils;

type
  TBrowserView = class(TWinControl)
  private
    FEvents: IDcefBrowser;
    FHandler: ICefClient;
    FBrowserDic: TBrowserWrapperDic;
    FClosedURL: TStringList;
    FDcefBOptions: TDcefBOptions;

    FActiveBrowser: ICefBrowser;
    FActiveBrowserId: Integer;

    FLoadingState: Integer;
    FLastTitle: string;
    FLastAddress: string;
    FIsActivating: Boolean;

    procedure OnSize(WParam: WParam; LParam: LParam);
    procedure OnLoadingStateChange(aBrowser: ICefBrowser; LParam: LParam);
    function OnWindowCheck(aBrowser: ICefBrowser; LParam: LParam): LRESULT;
    function OnCreateWindow(aBrowser: ICefBrowser; LParam: LParam): LRESULT;
    procedure OnNewBrowser(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnLoadStart(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnLoadEnd(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnLoadError(aBrowser: ICefBrowser; LParam: LParam);
    function OnFileDialog(aBrowser: ICefBrowser; LParam: LParam): LRESULT;
    procedure OnSetActive(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnGotFocus(aBrowser: ICefBrowser; LParam: LParam);
    function OnSetFocus(aBrowser: ICefBrowser; LParam: LParam): LRESULT;
    procedure OnTakeFocus(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnBeforeContextMenu(aBrowser: ICefBrowser; LParam: LParam);
    function OnContextMenuCommand(aBrowser: ICefBrowser;
      LParam: LParam): LRESULT;
    procedure OnContextMenuDismissed(aBrowser: ICefBrowser; LParam: LParam);
    function OnKeyEvent(aBrowser: ICefBrowser; LParam: LParam): LRESULT;
    function OnPreKeyEvent(aBrowser: ICefBrowser; LParam: LParam): LRESULT;
    procedure OnAddressChange(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnTitleChange(aBrowser: ICefBrowser; LParam: LParam);
    function OnTooltip(aBrowser: ICefBrowser; LParam: LParam): LRESULT;
    procedure OnStatusMessage(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnConsoleMessage(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnBeforeDownload(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnDownloadUpdated(aBrowser: ICefBrowser; LParam: LParam);
    function OnRequestGeolocationPermission(aBrowser: ICefBrowser;
      LParam: LParam): LRESULT;
    procedure OnCancelGeolocationPermission(aBrowser: ICefBrowser;
      LParam: LParam);
    function OnJsdialog(aBrowser: ICefBrowser; LParam: LParam): LRESULT;
    function OnBeforeUnloadDialog(aBrowser: ICefBrowser;
      LParam: LParam): LRESULT;
    procedure OnResetDialogState(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnDialogClosed(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnDoClose(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnBeforeClose(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnRunModal(aBrowser: ICefBrowser; LParam: LParam);
    function OnDragEnter(aBrowser: ICefBrowser; LParam: LParam): LRESULT;
    procedure OnDevTools(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnRefreshIgnoreCache(aBrowser: ICefBrowser; LParam: LParam);
    procedure OnSearchText(aBrowser: ICefBrowser; LParam: LParam);

    procedure HideCurrentBrowserWindow;
    function GetEmpty: Boolean;
    procedure ClearInterface;
    procedure CreateCefWindow(aUrl: string);

    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    function GetIsLoading: Boolean;
    procedure SetActiveBrowserAndID(const aBrowser: ICefBrowser);
    procedure SetIsActivating(const Value: Boolean);
    function GetIsActivating: Boolean;
    function GetUrl: string;
    function GetClosedUrlCount: Integer;
    function GetBrowserCount: Integer;
    function GetZoomLevel: string;
    function GetWrapperBrowsers(aBrowserId: Integer): TCefBrowserWrapper;
    function GetTitle: string;
  protected
    procedure WndProc(var Message: TMessage); override;
    function InnerCloseBrowser(const aCloseBrowserId: Integer;
      const aShowBrowserId: Integer;
      const aCloseBrowserType: TCloseBrowserType): Boolean;
  public
    constructor Create(AOwner: TComponent; aEvents: IDcefBrowser;
      aDcefBOptions: TDcefBOptions); reintroduce;
    destructor Destroy; override;

    procedure AddPage(const aUrl: string;
      const aDefaultUrl: string = SBlankPageUrl);
    procedure Load(aUrl: string);
    function ShowBrowser(const aBrowser: ICefBrowser): Boolean; overload;
    function ShowBrowser(const aBrowserId: Integer): Boolean; overload;
    function CloseBrowser(const aCloseBrowserId: Integer;
      const aShowBrowserId: Integer): Boolean;
    function CloseAllOtherBrowser(const aBrowserId: Integer): Boolean; overload;
    procedure CloseAllBrowser(const aIsTrigClosePageEvent: Boolean);
    procedure CopyBrowser(aBrowserId: Integer); overload;
    procedure CopyBrowser(aBrowser: ICefBrowser); overload;

    procedure DownloadFile(aFileUrl: string);
    procedure GoBack;
    procedure GoForward;
    procedure Reload;
    procedure ReloadIgnoreCache;
    procedure StopLoad;
    procedure Print;
    procedure ExecuteJavaScript(Const code: string);
    procedure GetSourceInNewPage;
    procedure AddZoomLevel;
    procedure ReduceZoomLevel;
    procedure ResetZoomLevel;
    procedure ReOpenClosedPage;
    procedure RunInRenderProcess(AProc: TRenderProcessCallbackA;
      aData: Pointer);

    // incomplete
    procedure SearchText;
    function GetSource(var SourceText: string;
      Const TimeOut: Integer = 1000): Boolean;
    function GetText(var aText: string; Const TimeOut: Integer = 1000): Boolean;
    // --------

    property ActiveBrowser: ICefBrowser read FActiveBrowser;
    property ActiveBrowserId: Integer read FActiveBrowserId;
    property IsLoading: Boolean read GetIsLoading;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
    property IsActivating: Boolean read GetIsActivating write SetIsActivating;
    property Empty: Boolean read GetEmpty;
    property Url: string read GetUrl;
    property Title: string read GetTitle;
    property ClosedUrlCount: Integer read GetClosedUrlCount;
    property BrowserCount: Integer read GetBrowserCount;
    property ZoomLevel: string read GetZoomLevel;
    property BrowserWrappers[aBrowserId: Integer]: TCefBrowserWrapper
      read GetWrapperBrowsers;
  end;

implementation

uses
  DcefB.Core.App;

{ TBrowserView }

procedure TBrowserView.AddPage(const aUrl, aDefaultUrl: string);
begin
  CreateCefWindow(IfThen(SameText(aUrl, ''), aDefaultUrl, aUrl));
end;

procedure TBrowserView.AddZoomLevel;
begin
  if (ActiveBrowser <> nil) and (ActiveBrowser.host.ZoomLevel < 9) then
    ActiveBrowser.host.ZoomLevel := ActiveBrowser.host.ZoomLevel + 1;
end;

procedure TBrowserView.ClearInterface;
begin
  BrowserDicLocker.Enter;
  try
    FBrowserDic.Clear;
  finally
    BrowserDicLocker.Exit;
  end;

  if FHandler <> nil then
    (FHandler as ICefClientHandler).Disconnect;
  FHandler := nil;
end;

function TBrowserView.CloseBrowser(const aCloseBrowserId, aShowBrowserId
  : Integer): Boolean;
begin
  Result := InnerCloseBrowser(aCloseBrowserId, aShowBrowserId,
    TCloseBrowserType.CLOSETYPE_DEFAULT);
end;

procedure TBrowserView.CloseAllBrowser(const aIsTrigClosePageEvent: Boolean);
var
  ClosePageArr: Array of Integer;
  Index, Id: Integer;
  BrowserWrapperArr: TArray<TCefBrowserWrapper>;
  NeedShowBrowserId: Integer;
begin
  NeedShowBrowserId := -1;
  BrowserDicLocker.Enter;
  ClosedUrlListLocker.Enter;
  try
    BrowserWrapperArr := FBrowserDic.Values.ToArray;
    SetLength(ClosePageArr, High(BrowserWrapperArr) + 1);
    for Index := High(BrowserWrapperArr) downto Low(BrowserWrapperArr) do
      ClosePageArr[Index] := BrowserWrapperArr[Index].Browser.Identifier;
    if aIsTrigClosePageEvent then
      FEvents.doOnBeforeCloseBrowser(ClosePageArr,
        TCloseBrowserType.CLOSETYPE_DEFAULT, NeedShowBrowserId);

    for Index := High(BrowserWrapperArr) downto Low(BrowserWrapperArr) do
    begin
      Id := BrowserWrapperArr[Index].Browser.Identifier;
      FClosedURL.Add(BrowserWrapperArr[Index].Browser.MainFrame.Url);
      BrowserWrapperArr[Index].Browser.StopLoad;
      DestroyWindow(BrowserWrapperArr[Index].Browser.host.WindowHandle);
      BrowserWrapperArr[Index].Free;
      FBrowserDic.Remove(Id);
    end;
    SetLength(BrowserWrapperArr, 0);

    if aIsTrigClosePageEvent then
      FEvents.doOnCloseBrowser(ClosePageArr, NeedShowBrowserId);
    SetLength(ClosePageArr, 0);

    if (FBrowserDic.Count = 0) and FDcefBOptions.CloseWPagesClosed then
      SendMessage(Application.Handle, WM_CLOSE, 0, 0);
  finally
    BrowserDicLocker.Exit;
    ClosedUrlListLocker.Exit;
  end;
end;

function TBrowserView.CloseAllOtherBrowser(const aBrowserId: Integer): Boolean;
var
  ClosePageArr: Array of Integer;
  Index, Id: Integer;
  BrowserWrapperArr: TArray<TCefBrowserWrapper>;
  NeedShowBrowserId: Integer;
begin
  Result := False;
  NeedShowBrowserId := aBrowserId;
  BrowserDicLocker.Enter;
  ClosedUrlListLocker.Enter;
  try
    BrowserWrapperArr := FBrowserDic.Values.ToArray;
    for Index := High(BrowserWrapperArr) downto Low(BrowserWrapperArr) do
    begin
      if BrowserWrapperArr[Index].Browser.Identifier <> NeedShowBrowserId then
      begin
        SetLength(ClosePageArr, Length(ClosePageArr) + 1);
        ClosePageArr[High(ClosePageArr)] := BrowserWrapperArr[Index]
          .Browser.Identifier;
      end;
    end;
    FEvents.doOnBeforeCloseBrowser(ClosePageArr,
      TCloseBrowserType.CLOSETYPE_DEFAULT, NeedShowBrowserId);

    for Index := High(BrowserWrapperArr) downto Low(BrowserWrapperArr) do
      if BrowserWrapperArr[Index].Browser.Identifier <> aBrowserId then
      begin
        Id := BrowserWrapperArr[Index].Browser.Identifier;
        FClosedURL.Add(BrowserWrapperArr[Index].Browser.MainFrame.Url);
        BrowserWrapperArr[Index].Browser.StopLoad;
        Result := DestroyWindow(BrowserWrapperArr[Index]
          .Browser.host.WindowHandle);
        BrowserWrapperArr[Index].Free;
        FBrowserDic.Remove(Id);
      end;
    SetLength(BrowserWrapperArr, 0);

    FEvents.doOnCloseBrowser(ClosePageArr, -1);
    SetLength(ClosePageArr, 0);
  finally
    BrowserDicLocker.Exit;
    ClosedUrlListLocker.Exit;
  end;
end;

procedure TBrowserView.CopyBrowser(aBrowserId: Integer);
var
  aCefBrowserWrapper: TCefBrowserWrapper;
begin
  BrowserDicLocker.Enter;
  try
    FBrowserDic.TryGetValue(aBrowserId, aCefBrowserWrapper);
    if Assigned(aCefBrowserWrapper) then
      CopyBrowser(aCefBrowserWrapper.Browser);
  finally
    BrowserDicLocker.Exit;
  end;
end;

procedure TBrowserView.CopyBrowser(aBrowser: ICefBrowser);
begin
  if aBrowser <> nil then
    CreateCefWindow(aBrowser.MainFrame.Url);
end;

constructor TBrowserView.Create(AOwner: TComponent; aEvents: IDcefBrowser;
  aDcefBOptions: TDcefBOptions);
begin
  inherited Create(AOwner);
  FEvents := aEvents;

  FDcefBOptions := aDcefBOptions;

  FActiveBrowserId := -1;
  FActiveBrowser := nil;
  FLastTitle := SLoadingText;
  FLastAddress := '';
  FIsActivating := True;
  FLoadingState := 0 or State_IsLoading;

  FBrowserDic := TBrowserWrapperDic.Create;
  FClosedURL := TStringList.Create;
end;

procedure TBrowserView.CreateCefWindow(aUrl: string);
var
  info: TCefWindowInfo;
  Settings: TCefBrowserSettings;
  Rect: TRect;
  aBrowser: ICefBrowser;
begin
  HideCurrentBrowserWindow;

  if FHandler = nil then
    FHandler := TDcefBHandler.Create(False, FEvents);
  FillChar(info, SizeOf(info), 0);
  Rect := ClientRect;
  info.Style := WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or
    WS_TABSTOP;
  info.parent_window := Handle;
  info.x := Left;
  info.y := Top;
  info.Width := Rect.Right - Rect.Left;
  info.Height := Rect.bottom - Rect.Top;
  FillChar(Settings, SizeOf(TCefBrowserSettings), 0);
  Settings.size := SizeOf(TCefBrowserSettings);
  FEvents.GetSettings(Settings);
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  CefBrowserHostCreate(@info, aClientHandler, FDefaultUrl, @Settings, nil);
{$ELSE}
  if DcefBApp.IsNeedInitInMainProcess then
    raise exception.Create(EXP_CEFNOTLOADINMAINPRO);
  DcefBApp.Init;
  aBrowser := DcefBApp.CefBrowserHostCreateSync(@info, FHandler, aUrl,
    @Settings, nil);
{$ENDIF}
end;

destructor TBrowserView.Destroy;
begin
  FEvents := nil;
  FActiveBrowser := nil;
  ClearInterface;
  FBrowserDic.Free;
  FClosedURL.Free;
  inherited;
end;

procedure TBrowserView.DownloadFile(aFileUrl: string);
begin
  if ActiveBrowser <> nil then
    ActiveBrowser.host.StartDownload(aFileUrl);
end;

procedure TBrowserView.ExecuteJavaScript(const code: string);
begin
  if ActiveBrowser <> nil then
    ActiveBrowser.MainFrame.ExecuteJavaScript(code, 'about:blank', 0);
end;

function TBrowserView.GetBrowserCount: Integer;
begin
  BrowserDicLocker.Enter;
  try
    Result := FBrowserDic.Count;
  finally
    BrowserDicLocker.Exit;
  end;
end;

function TBrowserView.GetCanGoBack: Boolean;
begin
  Result := FLoadingState and State_CanGoBack = State_CanGoBack;
end;

function TBrowserView.GetCanGoForward: Boolean;
begin
  Result := FLoadingState and State_CanGoForward = State_CanGoForward;
end;

function TBrowserView.GetClosedUrlCount: Integer;
begin
  ClosedUrlListLocker.Enter;
  try
    Result := FClosedURL.Count;
  finally
    ClosedUrlListLocker.Exit;
  end;
end;

function TBrowserView.GetEmpty: Boolean;
begin
  Result := FActiveBrowser = nil;
  if Not Result then
  begin
    BrowserDicLocker.Enter;
    try
      Result := FBrowserDic.Count <= 0;
    finally
      BrowserDicLocker.Exit;
    end;
  end;
end;

function TBrowserView.GetIsActivating: Boolean;
begin
  Result := FIsActivating;
end;

function TBrowserView.GetIsLoading: Boolean;
begin
  Result := FLoadingState and State_IsLoading = State_IsLoading;
end;

function TBrowserView.GetSource(var SourceText: string;
  const TimeOut: Integer): Boolean;
begin
  Result := False;
end;

procedure TBrowserView.GetSourceInNewPage;
begin
  if ActiveBrowser <> nil then
    CreateCefWindow('view-source:' + Url);
end;

function TBrowserView.GetText(var aText: string;
  const TimeOut: Integer): Boolean;
begin
  Result := False;
end;

function TBrowserView.GetTitle: string;
var
  aCefBrowserWrapper: TCefBrowserWrapper;
begin
  BrowserDicLocker.Enter;
  try
    FBrowserDic.TryGetValue(ActiveBrowserId, aCefBrowserWrapper);
    if Assigned(aCefBrowserWrapper) then
      Result := aCefBrowserWrapper.LastTitle
    else
      Result := '';
  finally
    BrowserDicLocker.Exit;
  end;
end;

function TBrowserView.GetUrl: string;
begin
  if ActiveBrowser <> nil then
    Result := ActiveBrowser.MainFrame.Url
  else
    Result := '';
end;

function TBrowserView.GetWrapperBrowsers(aBrowserId: Integer)
  : TCefBrowserWrapper;
begin
  BrowserDicLocker.Enter;
  try
    FBrowserDic.TryGetValue(aBrowserId, Result);
  finally
    BrowserDicLocker.Exit;
  end;
end;

function TBrowserView.GetZoomLevel: string;
const
  DoubleCases: Array [0 .. 15] of Double = (-6, -5, -4, -3, -2, -1, 0, 1, 2, 3,
    4, 5, 6, 7, 8, 9);
  ResultCases: Array [0 .. 15] of string = ('25%', '33%', '50%', '67%', '75%',
    '90%', '100%', '110%', '125%', '150%', '175%', '200%', '250%', '300%',
    '400%', '500%');
var
  ZoomIndex: Integer;

  function DoubleIndex(Const aDouble: Double; Const aCases: Array of Double;
    Var Index: Integer): Boolean;
  var
    LoopIndex: Integer;
  begin
    Result := False;
    for LoopIndex := 0 to Pred(Length(aCases)) do
      if aDouble = aCases[LoopIndex] then
      begin
        Index := LoopIndex;
        Result := True;
        Exit;
      end;
    Index := 0;
  end;

begin
  if ActiveBrowser <> nil then
    if DoubleIndex(ActiveBrowser.host.ZoomLevel, DoubleCases, ZoomIndex) then
      Result := ResultCases[ZoomIndex]
    else
      Result := FloatToStr(ActiveBrowser.host.ZoomLevel);
end;

procedure TBrowserView.GoBack;
begin
  if ActiveBrowser <> nil then
    ActiveBrowser.GoBack;
end;

procedure TBrowserView.GoForward;
begin
  if ActiveBrowser <> nil then
    ActiveBrowser.GoForward;
end;

procedure TBrowserView.HideCurrentBrowserWindow;
begin
  if (FActiveBrowser <> nil) and
    (FActiveBrowser.host.WindowHandle <> INVALID_HANDLE_VALUE) then
    ShowWindow(FActiveBrowser.host.WindowHandle, SW_HIDE);
end;

function TBrowserView.InnerCloseBrowser(const aCloseBrowserId,
  aShowBrowserId: Integer; const aCloseBrowserType: TCloseBrowserType): Boolean;
var
  aCloseBrowserWrapper, aShowBrowserWrapper: TCefBrowserWrapper;
  ClosePageArr: Array of Integer;
  NeedShowBrowserId: Integer;
begin
  Result := False;
  NeedShowBrowserId := aShowBrowserId;
  BrowserDicLocker.Enter;
  try
    FBrowserDic.TryGetValue(aCloseBrowserId, aCloseBrowserWrapper);
    FBrowserDic.TryGetValue(NeedShowBrowserId, aShowBrowserWrapper);
    if Assigned(aCloseBrowserWrapper) then
    begin
      ClosedUrlListLocker.Enter;
      try
        FClosedURL.Add(aCloseBrowserWrapper.Browser.MainFrame.Url);
      finally
        ClosedUrlListLocker.Exit;
      end;

      aCloseBrowserWrapper.Browser.StopLoad;
      SetLength(ClosePageArr, 1);
      ClosePageArr[0] := aCloseBrowserId;
      FEvents.doOnBeforeCloseBrowser(ClosePageArr, aCloseBrowserType,
        NeedShowBrowserId);
      Result := DestroyWindow(aCloseBrowserWrapper.Browser.host.WindowHandle);
      FEvents.doOnCloseBrowser(ClosePageArr, NeedShowBrowserId);
      SetLength(ClosePageArr, 0);

      aCloseBrowserWrapper.Free;
      FBrowserDic.Remove(aCloseBrowserId);
      if (FBrowserDic.Count = 0) and FDcefBOptions.CloseWPagesClosed then
        SendMessage(Application.Handle, WM_CLOSE, 0, 0);

      if Assigned(aShowBrowserWrapper) then
        ShowBrowser(aShowBrowserWrapper.Browser);
    end;
  finally
    BrowserDicLocker.Exit;
  end;
end;

procedure TBrowserView.Load(aUrl: string);
begin
  if Empty then
    CreateCefWindow(aUrl)
  else if Assigned(FActiveBrowser) then
    FActiveBrowser.MainFrame.LoadUrl(aUrl);
end;

procedure TBrowserView.OnAddressChange(aBrowser: ICefBrowser; LParam: LParam);
{ var
  PArgs: PAddressChangeArgs; }
begin
  // PArgs := PAddressChangeArgs(LParam);

  BrowserDicLocker.Enter;
  try
    FBrowserDic.Items[aBrowser.Identifier].LastAddress :=
      string(Pointer(LParam)^);
  finally
    BrowserDicLocker.Exit;
  end;

  if aBrowser.Identifier = ActiveBrowserId then
    FLastAddress := string(Pointer(LParam)^);

  FEvents.doOnStateChange(aBrowser, BrowserDataChange_Address,
    string(Pointer(LParam)^));
end;

procedure TBrowserView.OnBeforeClose(aBrowser: ICefBrowser; LParam: LParam);
begin
  //
end;

procedure TBrowserView.OnBeforeContextMenu(aBrowser: ICefBrowser;
  LParam: LParam);
var
  PArgs: PBeforeContextMenuArgs;
begin
  PArgs := PBeforeContextMenuArgs(LParam);
  FEvents.doOnBeforeContextMenu(aBrowser, PArgs.frame^, PArgs.params^,
    PArgs.model^);
end;

procedure TBrowserView.OnBeforeDownload(aBrowser: ICefBrowser; LParam: LParam);
var
  PArgs: PBeforeDownloadArgs;

  function RightPos(const SubStr, Str: string): Integer;
  var
    i, J, k, LenSub, LenS: Integer;
  begin
    Result := 0;
    LenSub := Length(SubStr);
    LenS := Length(Str);
    k := 0;
    if (LenSub = 0) or (LenS = 0) or (LenSub > LenS) then
      Exit;
    for i := LenS downto 1 do
    begin
      if Str[i] = SubStr[LenSub] then
      begin
        k := i - 1;
        for J := LenSub - 1 downto 1 do
        begin
          if Str[k] = SubStr[J] then
            Dec(k)
          else
            Break;
        end;
      end;
      if i - k = LenSub then
      begin
        Result := k + 1;
        Exit;
      end;
    end;
  end;

  function DealExistsFile(FilePath: string): string;
  var
    i: Integer;
    Temps, Path, FileExt: string;
  begin
    if FileExists(FilePath) then
    begin
      Path := ExtractFilePath(FilePath);
      Temps := ExtractFileName(FilePath);
      FileExt := Temps;
      Delete(FileExt, 1, RightPos('.', FileExt));
      for i := 1 to 99 do
      begin
        Result := Path + copy(Temps, 0, RightPos('.', Temps) - 1) + '(' +
          inttostr(i) + ').' + FileExt;
        if Not FileExists(Result) then
          Break;
      end;
    end
    else
      Result := FilePath;
  end;

begin
  PArgs := PBeforeDownloadArgs(LParam);
  FEvents.doOnBeforeDownload(aBrowser, PArgs.downloadItem^,
    PArgs.suggestedName^, PArgs.callback^, PArgs.CancelDefaultEvent);
  if Not PArgs.CancelDefaultEvent then
  begin
    if Not DirectoryExists(FDcefBOptions.DownLoadPath) then
      CreateDir(FDcefBOptions.DownLoadPath);
    PArgs.callback^.Cont(DealExistsFile(FDcefBOptions.DownLoadPath +
      PArgs.suggestedName^), Not FDcefBOptions.AutoDown);
  end;
end;

function TBrowserView.OnBeforeUnloadDialog(aBrowser: ICefBrowser;
  LParam: LParam): LRESULT;
var
  PArgs: PBeforeUnloadDialogArgs;
begin
  PArgs := PBeforeUnloadDialogArgs(LParam);
  FEvents.doOnBeforeUnloadDialog(aBrowser, PArgs.messageText^, PArgs.isReload,
    PArgs.callback^, PArgs.Result^, PArgs.CancelDefaultEvent);
  if Not PArgs.CancelDefaultEvent then
    PArgs.callback^.Cont(MessageBox(Self.Handle,
      PChar(PArgs.messageText^ + #13#10 + SUnloadDialogText),
      PChar(SUnloadDialogTitle), MB_OKCANCEL) = IDOK, '');
  Result := S_OK;
end;

procedure TBrowserView.OnCancelGeolocationPermission(aBrowser: ICefBrowser;
  LParam: LParam);
var
  PArgs: PCancelGeolocationPermissionArgs;
begin
  PArgs := PCancelGeolocationPermissionArgs(LParam);
  FEvents.doOnCancelGeolocationPermission(aBrowser, PArgs.requestingUrl^,
    PArgs.requestId);
end;

procedure TBrowserView.OnConsoleMessage(aBrowser: ICefBrowser; LParam: LParam);
var
  PArgs: PConsoleMessageArgs;
begin
  PArgs := PConsoleMessageArgs(LParam);
  FEvents.doOnConsoleMessage(aBrowser, PArgs.Message^, PArgs.source^,
    PArgs.line, PArgs.Result^);
end;

function TBrowserView.OnContextMenuCommand(aBrowser: ICefBrowser;
  LParam: LParam): LRESULT;
var
  PArgs: PContextMenuCommandArgs;
begin
  PArgs := PContextMenuCommandArgs(LParam);
  FEvents.doOnContextMenuCommand(aBrowser, PArgs.frame^, PArgs.params^,
    PArgs.commandId, PArgs.eventFlags^, PArgs.Result^);
  Result := S_OK;
end;

procedure TBrowserView.OnContextMenuDismissed(aBrowser: ICefBrowser;
  LParam: LParam);
begin
  FEvents.doOnContextMenuDismissed(aBrowser, ICefFrame(Pointer(LParam)^));
end;

function TBrowserView.OnCreateWindow(aBrowser: ICefBrowser;
  LParam: LParam): LRESULT;
var
  PArgs: PBeforePopupArgs;
begin
  PArgs := PBeforePopupArgs(LParam);
  FEvents.doOnBeforePopup(aBrowser, PArgs.frame^, PArgs.targetUrl^,
    PArgs.targetFrameName^, PArgs.popupFeatures^, PArgs.windowInfo^,
    PArgs.client^, PArgs.Settings^, PArgs.noJavascriptAccess^, PArgs.Result^,
    PArgs.CancelDefaultEvent);

  if Not PArgs.CancelDefaultEvent then
  begin
    PArgs.windowInfo.x := PArgs.popupFeatures.x;
    PArgs.windowInfo.y := PArgs.popupFeatures.y;
    PArgs.windowInfo.Width := PArgs.popupFeatures.Width;
    PArgs.windowInfo.Height := PArgs.popupFeatures.Height;
    PArgs.windowInfo.Style := WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN or
      WS_CLIPSIBLINGS or WS_TABSTOP;
    PArgs.windowInfo.parent_window := Self.Handle;
  end;
  Result := S_OK;
end;

procedure TBrowserView.OnDevTools(aBrowser: ICefBrowser; LParam: LParam);
begin
  if FDcefBOptions.DevToolsEnable then
    FEvents.ShowDevTools(aBrowser);
end;

procedure TBrowserView.OnDialogClosed(aBrowser: ICefBrowser; LParam: LParam);
begin
  FEvents.doOnDialogClosed(aBrowser);
end;

procedure TBrowserView.OnDoClose(aBrowser: ICefBrowser; LParam: LParam);
begin
  Boolean(Pointer(LParam)^) := InnerCloseBrowser(aBrowser.Identifier, -1,
    TCloseBrowserType.CLOSETYPE_JS);
end;

procedure TBrowserView.OnDownloadUpdated(aBrowser: ICefBrowser; LParam: LParam);
var
  PArgs: PDownloadUpdatedArgs;
begin
  PArgs := PDownloadUpdatedArgs(LParam);
  FEvents.doOnDownloadUpdated(aBrowser, PArgs.downloadItem^, PArgs.callback^);
end;

function TBrowserView.OnDragEnter(aBrowser: ICefBrowser;
  LParam: LParam): LRESULT;
var
  PArgs: PDragEnterArgs;
begin
  PArgs := PDragEnterArgs(LParam);
  FEvents.doOnDragEnter(aBrowser, PArgs.dragData^, PArgs.mask^, PArgs.Result^);
  Result := S_OK;
end;

function TBrowserView.OnFileDialog(aBrowser: ICefBrowser;
  LParam: LParam): LRESULT;
var
  PArgs: PFileDialogArgs;
begin
  PArgs := PFileDialogArgs(LParam);
  FEvents.doOnFileDialog(aBrowser, TCefFileDialogMode(PArgs.mode^),
    PArgs.Title^, PArgs.defaultFileName^, TStrings(PArgs.acceptTypes^),
    PArgs.callback^, PArgs.Result^);
  Result := S_OK;
end;

procedure TBrowserView.OnGotFocus(aBrowser: ICefBrowser; LParam: LParam);
var
  CancelDefaultEvent: Boolean;
begin
  CancelDefaultEvent := False;
  FEvents.doOnGotFocus(aBrowser, CancelDefaultEvent);
  if Not CancelDefaultEvent then
  begin
    if GetIsActivating then
      SetIsActivating(False)
    else
      TDcefBUtils.SendMsg(aBrowser, WM_SetActive, 0);
  end;
end;

function TBrowserView.OnJsdialog(aBrowser: ICefBrowser; LParam: LParam)
  : LRESULT;
var
  PArgs: PJsdialogArgs;
  messageText, originUrl, UserInput: string;
  dialogType: TCefJsDialogType;
begin
  PArgs := PJsdialogArgs(LParam);
  FEvents.doOnJsdialog(aBrowser, PArgs.originUrl^, PArgs.acceptLang^,
    PArgs.dialogType^, PArgs.messageText^, PArgs.defaultPromptText^,
    PArgs.callback^, PArgs.suppressMessage^, PArgs.Result^,
    PArgs.CancelDefaultEvent);
  if Not PArgs.CancelDefaultEvent then
  begin
    messageText := PArgs.messageText^;
    originUrl := PArgs.originUrl^;
    dialogType := PArgs.dialogType^;

    case dialogType of
      JSDIALOGTYPE_ALERT:
        begin
          PArgs.suppressMessage^ := True;
          PArgs.Result^ := False;
          ShowMessage(messageText);
        end;
      JSDIALOGTYPE_CONFIRM:
        begin
          PArgs.suppressMessage^ := False;
          PArgs.Result^ := True;
          PArgs.callback^.Cont(MessageBox(Self.Handle, PChar(messageText),
            PChar(originUrl + SDialogTitleSuffix), MB_OKCANCEL) = IDOK, '');
        end;
      JSDIALOGTYPE_PROMPT:
        begin
          PArgs.suppressMessage^ := False;
          PArgs.Result^ := True;
          UserInput := PArgs.defaultPromptText^;
          PArgs.callback^.Cont(InputQuery(PChar(originUrl + SDialogTitleSuffix),
            PChar(messageText), UserInput), UserInput);
        end;
    end;
  end;

  Result := S_OK;
end;

function TBrowserView.OnKeyEvent(aBrowser: ICefBrowser; LParam: LParam)
  : LRESULT;
var
  PArgs: PKeyEventArgs;
begin
  PArgs := PKeyEventArgs(LParam);
  FEvents.doOnKeyEvent(aBrowser, PArgs.event, PArgs.osEvent, PArgs.Result^);
  Result := S_OK;
end;

procedure TBrowserView.OnLoadEnd(aBrowser: ICefBrowser; LParam: LParam);
var
  PArgs: PLoadEndArgs;
begin
  SendMessage(Self.Handle, WM_Size, 0, 0);
  PArgs := PLoadEndArgs(LParam);
  FEvents.doOnLoadEnd(aBrowser, PArgs.frame^, PArgs.httpStatusCode);
end;

procedure TBrowserView.OnLoadError(aBrowser: ICefBrowser; LParam: LParam);
var
  PArgs: PLoadErrorArgs;
begin
  PArgs := PLoadErrorArgs(LParam);

  FEvents.doOnLoadError(aBrowser, PArgs.frame^, PArgs.errorCode,
    PArgs.errorText^, PArgs.failedUrl^, PArgs.CancelDefaultEvent);

  if Not PArgs.CancelDefaultEvent and (PArgs.errorCode <> NET_ERROR_ABORTED)
  then // NET_ERROR_ABORTED: user cancel download
    aBrowser.MainFrame.LoadString('<html><body><h2>Failed to load URL ' +
      PArgs.failedUrl^ + ' with error ' + PArgs.errorText^ + ' (' +
      inttostr(PArgs.errorCode) + ').</h2></body></html>', PArgs.failedUrl^);

end;

procedure TBrowserView.OnLoadingStateChange(aBrowser: ICefBrowser;
  LParam: LParam);
begin
  BrowserDicLocker.Enter;
  try
    FBrowserDic.Items[aBrowser.Identifier].LoadingState := LParam;
  finally
    BrowserDicLocker.Exit;
  end;

  if aBrowser.Identifier = ActiveBrowserId then
    FLoadingState := LParam;

  FEvents.doOnLoadingStateChange(aBrowser, IsLoading, CanGoBack, CanGoForward);
end;

procedure TBrowserView.OnLoadStart(aBrowser: ICefBrowser; LParam: LParam);
begin
  SendMessage(Self.Handle, WM_Size, 0, 0);
  FEvents.doOnLoadStart(aBrowser, ICefFrame(Pointer(LParam)^));
end;

procedure TBrowserView.OnNewBrowser(aBrowser: ICefBrowser; LParam: LParam);
begin
  ShowBrowser(aBrowser);
  BrowserDicLocker.Enter;
  try
    FBrowserDic.Add(aBrowser);
  finally
    BrowserDicLocker.Exit;
  end;
  FEvents.doOnAddBrowser(aBrowser);
end;

function TBrowserView.OnPreKeyEvent(aBrowser: ICefBrowser;
  LParam: LParam): LRESULT;
var
  PArgs: PPreKeyEventArgs;
begin
  PArgs := PPreKeyEventArgs(LParam);
  FEvents.doOnPreKeyEvent(aBrowser, PArgs.event, PArgs.osEvent,
    PArgs.isKeyboardShortcut, PArgs.Result^, PArgs.CancelDefaultEvent);
  if (Not PArgs.CancelDefaultEvent) and
    (aBrowser.Identifier = ActiveBrowser.Identifier) then
  begin
    if (PArgs.event.windows_key_code = 123) and
      (PArgs.event.Kind = KEYEVENT_KEYUP) and FDcefBOptions.DevToolsEnable then
      FEvents.ShowDevTools(aBrowser); // F12

    if (PArgs.event.windows_key_code = 116) and
      (PArgs.event.Kind = KEYEVENT_KEYUP) then
      ReloadIgnoreCache; // F5

    if (PArgs.event.windows_key_code = 70) and
      (EVENTFLAG_CONTROL_DOWN in PArgs.event.modifiers) then
      SearchText; // Ctrl+F

    if (PArgs.event.windows_key_code = 115) and // Alt + F4
      (EVENTFLAG_ALT_DOWN in PArgs.event.modifiers) then
      PArgs.Result^ := True;
  end;
  Result := S_OK;
end;

procedure TBrowserView.OnRefreshIgnoreCache(aBrowser: ICefBrowser;
  LParam: LParam);
begin
  ReloadIgnoreCache;
end;

function TBrowserView.OnRequestGeolocationPermission(aBrowser: ICefBrowser;
  LParam: LParam): LRESULT;
var
  PArgs: PRequestGeolocationPermissionArgs;
begin
  PArgs := PRequestGeolocationPermissionArgs(LParam);
  FEvents.doOnRequestGeolocationPermission(aBrowser, PArgs.requestingUrl^,
    PArgs.requestId, PArgs.callback^, PArgs.Result^);
  Result := S_OK;
end;

procedure TBrowserView.OnResetDialogState(aBrowser: ICefBrowser;
  LParam: LParam);
begin
  FEvents.doOnResetDialogState(aBrowser);
end;

procedure TBrowserView.OnRunModal(aBrowser: ICefBrowser; LParam: LParam);
begin
  // Boolean(Pointer(LParam)^) := False;
end;

procedure TBrowserView.OnSearchText(aBrowser: ICefBrowser; LParam: LParam);
begin
  SearchText;
end;

procedure TBrowserView.OnSetActive(aBrowser: ICefBrowser; LParam: LParam);
begin
  Self.SetFocus;
end;

function TBrowserView.OnSetFocus(aBrowser: ICefBrowser; LParam: LParam)
  : LRESULT;
var
  PArgs: PSetFocusArgs;
begin
  PArgs := PSetFocusArgs(LParam);
  FEvents.doOnSetFocus(aBrowser, PArgs.source^, PArgs.Result^,
    PArgs.CancelDefaultEvent);
  Result := S_OK;
end;

procedure TBrowserView.OnSize(WParam: WParam; LParam: LParam);
var
  WinHandle: HWND;
  Rect: TRect;
  hdwp: THandle;
begin
  if (not(csDesigning in ComponentState)) and (FActiveBrowser <> nil) then
  begin
    WinHandle := FActiveBrowser.host.WindowHandle;
    if WinHandle <> INVALID_HANDLE_VALUE then
    begin
      Rect := GetClientRect;
      FActiveBrowser.host.NotifyMoveOrResizeStarted;
      hdwp := BeginDeferWindowPos(1);
      try
        hdwp := DeferWindowPos(hdwp, FActiveBrowser.host.WindowHandle, 0,
          Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.bottom - Rect.Top,
          SWP_NOZORDER);
      finally
        EndDeferWindowPos(hdwp);
      end;
    end;
  end;
end;

procedure TBrowserView.OnStatusMessage(aBrowser: ICefBrowser; LParam: LParam);
begin
  FEvents.doOnStateChange(aBrowser, BrowserDataChange_StatusMessage,
    string(Pointer(LParam)^));
end;

procedure TBrowserView.OnTakeFocus(aBrowser: ICefBrowser; LParam: LParam);
var
  PArgs: PTakeFocusArgs;
begin
  PArgs := PTakeFocusArgs(LParam);
  FEvents.doOnTakeFocus(aBrowser, PArgs.next);
end;

procedure TBrowserView.OnTitleChange(aBrowser: ICefBrowser; LParam: LParam);
begin
  BrowserDicLocker.Enter;
  try
    FBrowserDic.Items[aBrowser.Identifier].LastTitle :=
      string(Pointer(LParam)^);
  finally
    BrowserDicLocker.Exit;
  end;

  if aBrowser.Identifier = ActiveBrowserId then
    FLastTitle := string(Pointer(LParam)^);
  FEvents.doOnStateChange(aBrowser, BrowserDataChange_Title, FLastTitle);
end;

function TBrowserView.OnTooltip(aBrowser: ICefBrowser; LParam: LParam): LRESULT;
var
  PArgs: PTooltipArgs;
begin
  PArgs := PTooltipArgs(LParam);
  FEvents.doOnTooltip(aBrowser, PArgs.text^, PArgs.Result^);
  Result := S_OK;
end;

function TBrowserView.OnWindowCheck(aBrowser: ICefBrowser;
  LParam: LParam): LRESULT;
var
  PArgs: PBeforePopupArgs;
begin
  PArgs := PBeforePopupArgs(LParam);
  if PArgs.popupFeatures.x = 1 then
  begin
    // make compile happy
  end;
  // only test
  Result := S_OK;
end;

procedure TBrowserView.Print;
begin
  if ActiveBrowser <> nil then
    ActiveBrowser.host.Print;
end;

procedure TBrowserView.ReduceZoomLevel;
begin
  if (ActiveBrowser <> nil) and (ActiveBrowser.host.ZoomLevel > -6) then
    ActiveBrowser.host.ZoomLevel := ActiveBrowser.host.ZoomLevel - 1;
end;

procedure TBrowserView.Reload;
begin
  if ActiveBrowser <> nil then
  begin
    ActiveBrowser.Reload;
    SetActiveBrowserAndID(ActiveBrowser);
  end;
end;

procedure TBrowserView.ReloadIgnoreCache;
begin
  if ActiveBrowser <> nil then
  begin
    ActiveBrowser.ReloadIgnoreCache;
    SetActiveBrowserAndID(ActiveBrowser);
  end;
end;

procedure TBrowserView.ReOpenClosedPage;
begin
  ClosedUrlListLocker.Enter;
  try
    if FClosedURL.Count > 0 then
    begin
      CreateCefWindow(FClosedURL[0]);
      FClosedURL.Delete(0);
    end;
  finally
    ClosedUrlListLocker.Exit;
  end;
end;

procedure TBrowserView.ResetZoomLevel;
begin
  if ActiveBrowser <> nil then
    ActiveBrowser.host.ZoomLevel := 0;
end;

procedure TBrowserView.RunInRenderProcess(AProc: TRenderProcessCallbackA;
  aData: Pointer);
var
  AMsg: ICefProcessMessage;
  ATemp: Pointer;
begin
  if DcefBApp.CefSingleProcess then
  begin
    if ActiveBrowser <> nil then
    begin
      AMsg := TCefProcessMessageRef.New('@dcefbrowser_runinrender');
      AMsg.ArgumentList.SetSize(3);
      // 这里使用字符串而不是整数，是因为JavaScript里没有64位整数
      AMsg.ArgumentList.SetString(0, IntToHex(IntPtr(ActiveBrowser),
        SizeOf(Pointer)));
      TRenderProcessCallbackA(ATemp) := AProc;
      AMsg.ArgumentList.SetString(1, IntToHex(IntPtr(Pointer(ATemp)),
        SizeOf(Pointer)));
      AMsg.ArgumentList.SetString(2, IntToHex(IntPtr(aData), SizeOf(Pointer)));
      ActiveBrowser.SendProcessMessage(PID_RENDERER, AMsg);
    end;
  end
  else
    raise exception.Create(SRunOnlyInSinglePro);
end;

procedure TBrowserView.SearchText;
begin

end;

procedure TBrowserView.SetActiveBrowserAndID(const aBrowser: ICefBrowser);
begin
  FActiveBrowser := aBrowser;
  FActiveBrowserId := aBrowser.Identifier;
  aBrowser.host.SetFocus(True);
end;

procedure TBrowserView.SetIsActivating(const Value: Boolean);
begin
  FIsActivating := Value;
end;

function TBrowserView.ShowBrowser(const aBrowser: ICefBrowser): Boolean;
var
  aCefBrowserWrapper: TCefBrowserWrapper;
begin
  Result := False;
  HideCurrentBrowserWindow;

  if (aBrowser <> nil) and (aBrowser.host.WindowHandle <> INVALID_HANDLE_VALUE)
  then
  begin
    ShowWindow(aBrowser.host.WindowHandle, SW_SHOWMAXIMIZED);

    SetActiveBrowserAndID(aBrowser);
    aCefBrowserWrapper := BrowserWrappers[aBrowser.Identifier];
    if Assigned(aCefBrowserWrapper) then
    begin
      TDcefBUtils.SendMsg(aBrowser, WM_AddressChange,
        LParam(@aCefBrowserWrapper.LastAddress));
      TDcefBUtils.SendMsg(aBrowser, WM_TitleChange,
        LParam(@aCefBrowserWrapper.LastTitle));
    end;
    Result := True;
  end;
end;

function TBrowserView.ShowBrowser(const aBrowserId: Integer): Boolean;
var
  aCefBrowserWrapper: TCefBrowserWrapper;
begin
  Result := False;
  BrowserDicLocker.Enter;
  try
    FBrowserDic.TryGetValue(aBrowserId, aCefBrowserWrapper);
    if Assigned(aCefBrowserWrapper) then
      Result := ShowBrowser(aCefBrowserWrapper.Browser);
  finally
    BrowserDicLocker.Exit;
  end;
end;

procedure TBrowserView.StopLoad;
begin
  if ActiveBrowser <> nil then
  begin
    ActiveBrowser.StopLoad;
    SetActiveBrowserAndID(ActiveBrowser);
  end;
end;

procedure TBrowserView.WndProc(var Message: TMessage);
  function GetCefBrowser: ICefBrowser;
  begin
    Result := ICefBrowser(Pointer(Message.WParam)^);
  end;

begin
  case Message.Msg of
    { WM_SETFOCUS:
      begin
      if (FActiveBrowser <> nil) and (FActiveBrowser.host.WindowHandle <> 0)
      then
      PostMessage(FActiveBrowser.host.WindowHandle, WM_SETFOCUS,
      Message.WParam, 0);
      inherited WndProc(Message);
      end; }
    WM_ERASEBKGND:
      if (csDesigning in ComponentState) or (FActiveBrowser = nil) then
        inherited WndProc(Message);
    CM_WANTSPECIALKEY:
      if not(TWMKey(Message).CharCode in [VK_LEFT .. VK_DOWN]) then
        Message.Result := 1
      else
        inherited WndProc(Message);
    WM_GETDLGCODE:
      Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
    WM_Size:
      OnSize(Message.WParam, Message.LParam);
    // ---------------- Custom Message
    WM_LoadingStateChange:
      OnLoadingStateChange(GetCefBrowser, Message.LParam);
    WM_WindowCheck:
      Message.Result := OnWindowCheck(GetCefBrowser, Message.LParam);
    WM_CreateWindow:
      Message.Result := OnCreateWindow(GetCefBrowser, Message.LParam);
    WM_NewBrowser:
      OnNewBrowser(GetCefBrowser, Message.LParam);
    WM_LoadStart:
      OnLoadStart(GetCefBrowser, Message.LParam);
    WM_LoadEnd:
      OnLoadEnd(GetCefBrowser, Message.LParam);
    WM_LoadError:
      OnLoadError(GetCefBrowser, Message.LParam);
    WM_FileDialog:
      Message.Result := OnFileDialog(GetCefBrowser, Message.LParam);
    WM_SetActive:
      OnSetActive(GetCefBrowser, Message.LParam);
    WM_GotFocus:
      OnGotFocus(GetCefBrowser, Message.LParam);
    WM_SetFocus:
      Message.Result := OnSetFocus(GetCefBrowser, Message.LParam);
    WM_TakeFocus:
      OnTakeFocus(GetCefBrowser, Message.LParam);
    WM_BeforeContextMenu:
      OnBeforeContextMenu(GetCefBrowser, Message.LParam);
    WM_ContextMenuCommand:
      Message.Result := OnContextMenuCommand(GetCefBrowser, Message.LParam);
    WM_ContextMenuDismissed:
      OnContextMenuDismissed(GetCefBrowser, Message.LParam);
    WM_KeyEvent:
      Message.Result := OnKeyEvent(GetCefBrowser, Message.LParam);
    WM_PreKeyEvent:
      Message.Result := OnPreKeyEvent(GetCefBrowser, Message.LParam);
    WM_AddressChange:
      OnAddressChange(GetCefBrowser, Message.LParam);
    WM_TitleChange:
      OnTitleChange(GetCefBrowser, Message.LParam);
    WM_Tooltip:
      Message.Result := OnTooltip(GetCefBrowser, Message.LParam);
    WM_StatusMessage:
      OnStatusMessage(GetCefBrowser, Message.LParam);
    WM_ConsoleMessage:
      OnConsoleMessage(GetCefBrowser, Message.LParam);
    WM_BeforeDownload:
      OnBeforeDownload(GetCefBrowser, Message.LParam);
    WM_DownloadUpdated:
      OnDownloadUpdated(GetCefBrowser, Message.LParam);
    WM_RequestGeolocationPermission:
      Message.Result := OnRequestGeolocationPermission(GetCefBrowser,
        Message.LParam);
    WM_CancelGeolocationPermission:
      OnCancelGeolocationPermission(GetCefBrowser, Message.LParam);
    WM_Jsdialog:
      Message.Result := OnJsdialog(GetCefBrowser, Message.LParam);
    WM_BeforeUnloadDialog:
      Message.Result := OnBeforeUnloadDialog(GetCefBrowser, Message.LParam);
    WM_ResetDialogState:
      OnResetDialogState(GetCefBrowser, Message.LParam);
    WM_DialogClosed:
      OnDialogClosed(GetCefBrowser, Message.LParam);
    WM_DoClose:
      OnDoClose(GetCefBrowser, Message.LParam);
    WM_BeforeClose:
      OnBeforeClose(GetCefBrowser, Message.LParam);
    WM_RunModal:
      OnRunModal(GetCefBrowser, Message.LParam);
    WM_DragEnter:
      Message.Result := OnDragEnter(GetCefBrowser, Message.LParam);
    WM_DevTools:
      OnDevTools(GetCefBrowser, Message.LParam);
    WM_RefreshIgnoreCache:
      OnRefreshIgnoreCache(GetCefBrowser, Message.LParam);
    WM_SearchText:
      OnSearchText(GetCefBrowser, Message.LParam);
  else
    inherited WndProc(Message);
  end;
end;

end.
