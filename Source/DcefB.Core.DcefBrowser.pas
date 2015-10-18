(*  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Core.DcefBrowser;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.ComCtrls, Vcl.Forms,
  Vcl.ExtCtrls, Vcl.Dialogs, System.StrUtils, System.SysUtils,
  Winapi.Messages, System.Math, Generics.Collections,

  DcefB.Dcef3.CefLib, DcefB.BaseObject, DcefB.Locker, DcefB.Settings,
  DcefB.Events, DcefB.Core.BrowserView, DcefB.Core.DefaultRenderHandler,
  DcefB.Handler.Main, DcefB.res;

type
  TCustomDcefBrowser = class(TWinControl, IDcefBEvents)
  private
    FBrowserView: TBrowserView;

    FDcefBOptions: TDcefBOptions;
    FChromiumOptions: TChromiumOptions;
    FChromiumFontOptions: TChromiumFontOptions;

    FDefaultUrl: ustring;
    FDefaultEncoding: ustring;
    FActivePageID: Integer;
    FIsMoving: Boolean;

    FOnLoadingStateChange: TOnLoadingStateChange;
    FOnStateChange: TOnStateChange;
    FOnAddBrowser: TOnAddBrowser;
    FOnCloseBrowser: TOnCloseBrowser;
    FOnLoadStart: TOnLoadStart;
    FOnLoadEnd: TOnLoadEnd;
    FOnLoadError: TOnLoadError;
    FOnBeforeBrowse: TOnBeforeBrowse;
    FOnPreKeyEvent: TOnPreKeyEvent;
    FOnKeyEvent: TOnKeyEvent;
    FOnBeforeResourceLoad: TOnBeforeResourceLoad;
    FOnGetResourceHandler: TOnGetResourceHandler;
    FOnResourceRedirect: TOnResourceRedirect;
    FOnGotFocus: TOnGotFocus;
    FOnSetFocus: TOnSetFocus;
    FOnTakeFocus: TOnTakeFocus;
    FOnBeforeContextMenu: TOnBeforeContextMenu;
    FOnContextMenuCommand: TOnContextMenuCommand;
    FOnContextMenuDismissed: TOnContextMenuDismissed;
    FOnJsdialog: TOnJsdialog;
    FOnBeforeUnloadDialog: TOnBeforeUnloadDialog;
    FOnDialogClosed: TOnDialogClosed;
    FOnPluginCrashed: TOnPluginCrashed;
    FOnBeforePluginLoad: TOnBeforePluginLoad;
    FOnBeforeDownload: TOnBeforeDownload;
    FOnDownloadUpdated: TOnDownloadUpdated;
    FOnGetAuthCredentials: TOnGetAuthCredentials;
    FOnConsoleMessage: TOnConsoleMessage;
    FOnProtocolExecution: TOnProtocolExecution;
    FOnFileDialog: TOnFileDialog;
    FOnRequestGeolocationPermission: TOnRequestGeolocationPermission;
    FOnCancelGeolocationPermission: TOnCancelGeolocationPermission;
    FOnQuotaRequest: TOnQuotaRequest;
    FOnCertificateError: TOnCertificateError;
    FOnDragEnter: TOnDragEnter;
    FOnStartDragging: TOnStartDragging;
    FOnUpdateDragCursor: TOnUpdateDragCursor;
    FOnCursorChange: TOnCursorChange;
    FOnTooltip: TOnTooltip;
    FOnResetDialogState: TOnResetDialogState;
    FOnRenderProcessTerminated: TOnRenderProcessTerminated;

    procedure GetSettings(var Settings: TCefBrowserSettings);
    procedure doOnLoadingStateChange(const Browser: ICefBrowser;
      IsLoading, CanGoBack, CanGoForward: Boolean);
    procedure doOnStateChange(const Browser: ICefBrowser;
      const Kind: TBrowserDataChangeKind; const value: string);
    procedure doOnAddBrowser(const Browser: ICefBrowser);
    procedure doOnCloseBrowser(const CloseBrowserIdArr: Array of Integer;
      Const ShowBrowserId: Integer);

    procedure doOnLoadStart(const Browser: ICefBrowser; const frame: ICefFrame);
    procedure doOnLoadEnd(const Browser: ICefBrowser; const frame: ICefFrame;
      httpStatusCode: Integer);
    procedure doOnLoadError(const Browser: ICefBrowser; const frame: ICefFrame;
      errorCode: Integer; const errorText, failedUrl: ustring;
      var CancelDefaultEvent: Boolean);
    procedure doOnBeforeBrowse(const Browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean;
      var Cancel: Boolean);

    procedure doOnPreKeyEvent(const Browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle;
      var isKeyboardShortcut: Boolean; var Cancel: Boolean;
      var CancelDefaultEvent: Boolean);
    procedure doOnKeyEvent(const Browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle; var Cancel: Boolean);

    procedure doOnBeforeResourceLoad(const Browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      var CancelLoad: Boolean);
    procedure doOnGetResourceHandler(const Browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      var ResourceHandler: ICefResourceHandler);
    procedure doOnResourceRedirect(const Browser: ICefBrowser;
      const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring);

    procedure doOnGotFocus(const Browser: ICefBrowser;
      var CancelDefaultEvent: Boolean);
    procedure doOnSetFocus(const Browser: ICefBrowser; source: TCefFocusSource;
      var Cancel: Boolean; var CancelDefaultEvent: Boolean);
    procedure doOnTakeFocus(const Browser: ICefBrowser; next: Boolean);

    procedure doOnBeforeContextMenu(const Browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      const model: ICefMenuModel);
    procedure doOnContextMenuCommand(const Browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      commandId: Integer; eventFlags: TCefEventFlags;
      var CancelDefaultEvent: Boolean);
    procedure doOnContextMenuDismissed(const Browser: ICefBrowser;
      const frame: ICefFrame);

    procedure doOnJsdialog(const Browser: ICefBrowser;
      const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
      const messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean;
      var Cancel: Boolean; var CancelDefaultEvent: Boolean);
    procedure doOnBeforeUnloadDialog(const Browser: ICefBrowser;
      const messageText: ustring; isReload: Boolean;
      callback: ICefJsDialogCallback; var Cancel: Boolean;
      var CancelDefaultEvent: Boolean);
    procedure doOnDialogClosed(const Browser: ICefBrowser);

    procedure doOnPluginCrashed(const Browser: ICefBrowser;
      const pluginPath: ustring);
    procedure doOnBeforePluginLoad(const Browser: ICefBrowser;
      const URL, policyUrl: ustring; const info: ICefWebPluginInfo;
      var CancelLoad: Boolean);

    { procedure doOnDownloadUpdated(Const DcefItemIndex: Integer;
      Const Kind: TBrowserDownloadUpdatedKind); }
    procedure doOnBeforeDownload(const Browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback;
      var CancelDefaultEvent: Boolean);
    procedure doOnDownloadUpdated(const Browser: ICefBrowser;
      const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback);
    procedure doOnGetAuthCredentials(const Browser: ICefBrowser;
      const frame: ICefFrame; isProxy: Boolean; const host: ustring;
      port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback; var CancelDefaultEvent: Boolean);
    procedure doOnConsoleMessage(const Browser: ICefBrowser;
      const message, source: ustring; line: Integer; var Cancel: Boolean);
    procedure doOnProtocolExecution(Browser: ICefBrowser; const URL: ustring;
      var allowOsExecution: Boolean);
    procedure doOnFileDialog(const Browser: ICefBrowser;
      mode: TCefFileDialogMode; const title, defaultFileName: ustring;
      acceptTypes: TStrings; const callback: ICefFileDialogCallback;
      out Result: Boolean);

    procedure doOnRequestGeolocationPermission(const Browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer;
      const callback: ICefGeolocationCallback; out Result: Boolean);
    procedure doOnCancelGeolocationPermission(const Browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer);

    procedure doOnQuotaRequest(const Browser: ICefBrowser;
      const originUrl: ustring; newSize: Int64;
      const callback: ICefQuotaCallback; var Cancel: Boolean);
    procedure doOnCertificateError(certError: TCefErrorCode;
      const requestUrl: ustring;
      const callback: ICefAllowCertificateErrorCallback; out Result: Boolean);

    procedure doOnDragEnter(const Browser: ICefBrowser;
      const dragData: ICefDragData; mask: TCefDragOperations;
      var Cancel: Boolean);
    procedure doOnStartDragging(const Browser: ICefBrowser;
      const dragData: ICefDragData; allowedOps: TCefDragOperations;
      x, y: Integer; out Result: Boolean);
    procedure doOnUpdateDragCursor(const Browser: ICefBrowser;
      operation: TCefDragOperation);
    procedure doOnCursorChange(const Browser: ICefBrowser;
      cursor: TCefCursorHandle; cursorType: TCefCursorType;
      const customCursorInfo: PCefCursorInfo);
    procedure doOnTooltip(const Browser: ICefBrowser; var text: ustring;
      var Cancel: Boolean);
    procedure doOnResetDialogState(const Browser: ICefBrowser);
    procedure doOnRenderProcessTerminated(const Browser: ICefBrowser;
      status: TCefTerminationStatus);

    function GetZoomLevel: string;
    function GetActiveBrowser: ICefBrowser;
    function GetActiveBrowserID: Integer;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    function GetIsLoading: Boolean;
    function GetClosedUrlCount: Integer;
    function GetBrowserCount: Integer;
  protected
    // swish changed:
    FLastWndProc: TWndMethod;
    FParentForm: TCustomForm;
    procedure HookWndProc;
    procedure UnhookWndProc;
    procedure WndProc(var AMsg: TMessage); override;
    procedure FormWndProc(var AMsg: TMessage);
    // changed end
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddPage(Const aUrl: string = '');
    procedure Load(Const aUrl: string);
    function ShowBrowser(const aBrowser: ICefBrowser): Boolean; overload;
    function ShowBrowser(const aBrowserId: Integer): Boolean; overload;
    function CloseBrowser(const aCloseBrowserId: Integer;
      const aShowBrowserId: Integer): Boolean; overload;
    function CloseBrowser(const aCloseBrowser: ICefBrowser;
      const aShowBrowser: ICefBrowser): Boolean; overload;
    function CloseAllOtherBrowser(const aBrowserId: Integer): Boolean; overload;
    function CloseAllOtherBrowser(const aBrowser: ICefBrowser)
      : Boolean; overload;
    procedure CloseAllBrowser(const aIsTrigClosePageEvent: Boolean);
    procedure CopyBrowser(aBrowserId: Integer); overload;
    procedure CopyBrowser(aBrowser: ICefBrowser); overload;

    procedure GoHome;
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
    procedure DevTools;
    procedure SearchText;
    function GetSource(var SourceText: string;
      Const TimeOut: Integer = 1000): Boolean;
    function GetText(var aText: string; Const TimeOut: Integer = 1000): Boolean;
    // --------

    class procedure CreateDefaultRenderProcess;
    class procedure RegisterClasses(const aObjList: array of TClass);

    property ActiveBrowser: ICefBrowser read GetActiveBrowser;
    property ActiveBrowserId: Integer read GetActiveBrowserID;
    property BrowserCount: Integer read GetBrowserCount;
    property ClosedUrlCount: Integer read GetClosedUrlCount;
    property DefaultURL: ustring read FDefaultUrl write FDefaultUrl;
    property DefaultEncoding: ustring read FDefaultEncoding
      write FDefaultEncoding;
    property DcefBOptions: TDcefBOptions read FDcefBOptions write FDcefBOptions;
    property ChromiumOptions: TChromiumOptions read FChromiumOptions
      write FChromiumOptions;
    property ChromiumFontOptions: TChromiumFontOptions
      read FChromiumFontOptions;

    property IsLoading: Boolean read GetIsLoading;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
    property ZoomLevel: string read GetZoomLevel;

    property OnLoadingStateChange: TOnLoadingStateChange
      read FOnLoadingStateChange write FOnLoadingStateChange;
    property OnStateChange: TOnStateChange read FOnStateChange
      write FOnStateChange;
    property OnAddBrowser: TOnAddBrowser read FOnAddBrowser write FOnAddBrowser;
    property OnCloseBrowser: TOnCloseBrowser read FOnCloseBrowser
      write FOnCloseBrowser;
    property OnLoadStart: TOnLoadStart read FOnLoadStart write FOnLoadStart;
    property OnLoadEnd: TOnLoadEnd read FOnLoadEnd write FOnLoadEnd;
    property OnLoadError: TOnLoadError read FOnLoadError write FOnLoadError;
    property OnBeforeBrowse: TOnBeforeBrowse read FOnBeforeBrowse
      write FOnBeforeBrowse;
    property OnPreKeyEvent: TOnPreKeyEvent read FOnPreKeyEvent
      write FOnPreKeyEvent;
    property OnKeyEvent: TOnKeyEvent read FOnKeyEvent write FOnKeyEvent;
    property OnBeforeResourceLoad: TOnBeforeResourceLoad
      read FOnBeforeResourceLoad write FOnBeforeResourceLoad;
    property OnGetResourceHandler: TOnGetResourceHandler
      read FOnGetResourceHandler write FOnGetResourceHandler;
    property OnResourceRedirect: TOnResourceRedirect read FOnResourceRedirect
      write FOnResourceRedirect;
    property OnGotFocus: TOnGotFocus read FOnGotFocus write FOnGotFocus;
    property OnSetFocus: TOnSetFocus read FOnSetFocus write FOnSetFocus;
    property OnTakeFocus: TOnTakeFocus read FOnTakeFocus write FOnTakeFocus;
    property OnBeforeContextMenu: TOnBeforeContextMenu read FOnBeforeContextMenu
      write FOnBeforeContextMenu;
    property OnContextMenuCommand: TOnContextMenuCommand
      read FOnContextMenuCommand write FOnContextMenuCommand;
    property OnContextMenuDismissed: TOnContextMenuDismissed
      read FOnContextMenuDismissed write FOnContextMenuDismissed;
    property OnJsdialog: TOnJsdialog read FOnJsdialog write FOnJsdialog;
    property OnBeforeUnloadDialog: TOnBeforeUnloadDialog
      read FOnBeforeUnloadDialog write FOnBeforeUnloadDialog;
    property OnDialogClosed: TOnDialogClosed read FOnDialogClosed
      write FOnDialogClosed;
    property OnPluginCrashed: TOnPluginCrashed read FOnPluginCrashed
      write FOnPluginCrashed;
    property OnBeforePluginLoad: TOnBeforePluginLoad read FOnBeforePluginLoad
      write FOnBeforePluginLoad;
    property OnBeforeDownload: TOnBeforeDownload read FOnBeforeDownload
      write FOnBeforeDownload;
    property OnDownloadUpdated: TOnDownloadUpdated read FOnDownloadUpdated
      write FOnDownloadUpdated;
    property OnGetAuthCredentials: TOnGetAuthCredentials
      read FOnGetAuthCredentials write FOnGetAuthCredentials;
    property OnConsoleMessage: TOnConsoleMessage read FOnConsoleMessage
      write FOnConsoleMessage;
    property OnProtocolExecution: TOnProtocolExecution read FOnProtocolExecution
      write FOnProtocolExecution;
    property OnFileDialog: TOnFileDialog read FOnFileDialog write FOnFileDialog;
    property OnRequestGeolocationPermission: TOnRequestGeolocationPermission
      read FOnRequestGeolocationPermission
      write FOnRequestGeolocationPermission;
    property OnCancelGeolocationPermission: TOnCancelGeolocationPermission
      read FOnCancelGeolocationPermission write FOnCancelGeolocationPermission;
    property OnQuotaRequest: TOnQuotaRequest read FOnQuotaRequest
      write FOnQuotaRequest;
    property OnCertificateError: TOnCertificateError read FOnCertificateError
      write FOnCertificateError;
    property OnDragEnter: TOnDragEnter read FOnDragEnter write FOnDragEnter;
    property OnStartDragging: TOnStartDragging read FOnStartDragging
      write FOnStartDragging;
    property OnUpdateDragCursor: TOnUpdateDragCursor read FOnUpdateDragCursor
      write FOnUpdateDragCursor;
    property OnCursorChange: TOnCursorChange read FOnCursorChange
      write FOnCursorChange;
    property OnTooltip: TOnTooltip read FOnTooltip write FOnTooltip;
    property OnResetDialogState: TOnResetDialogState read FOnResetDialogState
      write FOnResetDialogState;
    property OnRenderProcessTerminated: TOnRenderProcessTerminated
      read FOnRenderProcessTerminated write FOnRenderProcessTerminated;
  end;

  TDcefBrowser = class(TCustomDcefBrowser)
  published
    property Color;
    property Constraints;
    property Anchors;
    property TabStop;
    property TabOrder;
    property Align;
    property Visible;
    property DefaultURL;

    property DcefBOptions;
    property ChromiumOptions;
    property ChromiumFontOptions;

    property OnLoadingStateChange;
    property OnStateChange;
    property OnAddBrowser;
    property OnCloseBrowser;
    property OnLoadStart;
    property OnLoadEnd;
    property OnLoadError;
    property OnBeforeBrowse;
    property OnPreKeyEvent;
    property OnKeyEvent;
    property OnBeforeResourceLoad;
    property OnGetResourceHandler;
    property OnResourceRedirect;
    property OnGotFocus;
    property OnSetFocus;
    property OnTakeFocus;
    property OnBeforeContextMenu;
    property OnContextMenuCommand;
    property OnContextMenuDismissed;
    property OnJsdialog;
    property OnBeforeUnloadDialog;
    property OnDialogClosed;
    property OnPluginCrashed;
    property OnBeforePluginLoad;
    property OnBeforeDownload;
    property OnDownloadUpdated;
    property OnGetAuthCredentials;
    property OnConsoleMessage;
    property OnProtocolExecution;
    property OnFileDialog;
    property OnRequestGeolocationPermission;
    property OnCancelGeolocationPermission;
    property OnQuotaRequest;
    property OnCertificateError;
    property OnDragEnter;
    property OnStartDragging;
    property OnUpdateDragCursor;
    property OnCursorChange;
    property OnTooltip;
    property OnResetDialogState;
    property OnRenderProcessTerminated;
  end;

implementation

{ TDcefBrowser }

procedure TCustomDcefBrowser.AddPage(Const aUrl: string = '');
begin
  FBrowserView.AddPage(aUrl, FDefaultUrl);
end;

procedure TCustomDcefBrowser.Print;
begin
  FBrowserView.Print;
end;

procedure TCustomDcefBrowser.ReduceZoomLevel;
begin
  FBrowserView.ReduceZoomLevel;
end;

procedure TCustomDcefBrowser.Reload;
begin
  FBrowserView.Reload;
end;

procedure TCustomDcefBrowser.ReloadIgnoreCache;
begin
  FBrowserView.ReloadIgnoreCache;
end;

procedure TCustomDcefBrowser.ReOpenClosedPage;
begin
  FBrowserView.ReOpenClosedPage;
end;

procedure TCustomDcefBrowser.ResetZoomLevel;
begin
  FBrowserView.ResetZoomLevel;
end;

procedure TCustomDcefBrowser.RunInRenderProcess(AProc: TRenderProcessCallbackA;
  aData: Pointer);
begin
  FBrowserView.RunInRenderProcess(AProc, aData);
end;

procedure TCustomDcefBrowser.SearchText;
begin
  FBrowserView.SearchText;
end;

function TCustomDcefBrowser.ShowBrowser(const aBrowserId: Integer): Boolean;
begin
  Result := FBrowserView.ShowBrowser(aBrowserId);
end;

function TCustomDcefBrowser.ShowBrowser(const aBrowser: ICefBrowser): Boolean;
begin
  Result := FBrowserView.ShowBrowser(aBrowser);
end;

procedure TCustomDcefBrowser.StopLoad;
begin
  FBrowserView.StopLoad;
end;

procedure TCustomDcefBrowser.UnhookWndProc;
begin
  if Assigned(FParentForm) then
  begin
    FParentForm.WindowProc := FLastWndProc;
    FParentForm := nil;
  end;
end;

procedure TCustomDcefBrowser.WndProc(var AMsg: TMessage);
begin
  inherited;
  if AMsg.Msg = WM_CREATE then
    HookWndProc; // Added by swish;
end;

function TCustomDcefBrowser.CloseAllOtherBrowser(const aBrowserId
  : Integer): Boolean;
begin
  Result := FBrowserView.CloseAllOtherBrowser(aBrowserId);
end;

procedure TCustomDcefBrowser.CloseAllBrowser(const aIsTrigClosePageEvent
  : Boolean);
begin
  FBrowserView.CloseAllBrowser(aIsTrigClosePageEvent);
end;

function TCustomDcefBrowser.CloseAllOtherBrowser(const aBrowser
  : ICefBrowser): Boolean;
begin
  Result := FBrowserView.CloseAllOtherBrowser(aBrowser);
end;

function TCustomDcefBrowser.CloseBrowser(const aCloseBrowser,
  aShowBrowser: ICefBrowser): Boolean;
begin
  Result := FBrowserView.CloseBrowser(aCloseBrowser, aShowBrowser);
end;

function TCustomDcefBrowser.CloseBrowser(const aCloseBrowserId,
  aShowBrowserId: Integer): Boolean;
begin
  Result := FBrowserView.CloseBrowser(aCloseBrowserId, aShowBrowserId);
end;

procedure TCustomDcefBrowser.CopyBrowser(aBrowserId: Integer);
begin
  FBrowserView.CopyBrowser(aBrowserId);
end;

procedure TCustomDcefBrowser.CopyBrowser(aBrowser: ICefBrowser);
begin
  FBrowserView.CopyBrowser(aBrowser);
end;

constructor TCustomDcefBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultUrl := SBlankPageUrl;
  FActivePageID := -1;

  FDcefBOptions := TDcefBOptions.Create;
  FChromiumOptions := TChromiumOptions.Create;
  FChromiumFontOptions := TChromiumFontOptions.Create;

  FBrowserView := TBrowserView.Create(Self, Self, FDcefBOptions);
  FBrowserView.Parent := Self;
  FBrowserView.Align := alClient;
end;

class procedure TCustomDcefBrowser.CreateDefaultRenderProcess;
begin
  if not Assigned(CefRenderProcessHandler) then
    CefRenderProcessHandler := TDefaultRenderProcessHandler.Create;
end;

procedure TCustomDcefBrowser.DevTools;
begin
  FBrowserView.DevTools;
end;

class procedure TCustomDcefBrowser.RegisterClasses(const aObjList
  : array of TClass);
var
  i, J, C: Integer;
  AFound: Boolean;
begin
  CreateDefaultRenderProcess;

  C := Length(TDefaultRenderProcessHandler.Classes);
  SetLength(TDefaultRenderProcessHandler.Classes, C + Length(aObjList));
  for i := 0 to High(aObjList) do
  begin
    AFound := False;
    for J := 0 to C - 1 do
    begin
      if TDefaultRenderProcessHandler.Classes[J] = aObjList[i] then
      begin
        AFound := True;
        Break;
      end;
    end;
    if not AFound then
    begin
      TDefaultRenderProcessHandler.Classes[C] := aObjList[i];
      Inc(C);
    end;
  end;
  SetLength(TDefaultRenderProcessHandler.Classes, C);
end;

destructor TCustomDcefBrowser.Destroy;
begin
  FBrowserView.Free;
  FDcefBOptions.Free;
  FChromiumOptions.Free;
  FChromiumFontOptions.Free;
  UnhookWndProc;
  inherited;
end;

procedure TCustomDcefBrowser.doOnStateChange(const Browser: ICefBrowser;
  const Kind: TBrowserDataChangeKind; const value: string);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Browser, Kind, value);
end;

procedure TCustomDcefBrowser.doOnPluginCrashed(const Browser: ICefBrowser;
  const pluginPath: ustring);
begin
  if Assigned(FOnPluginCrashed) then
    FOnPluginCrashed(Browser, pluginPath);
end;

procedure TCustomDcefBrowser.DownloadFile(aFileUrl: string);
begin
  FBrowserView.DownloadFile(aFileUrl);
end;

procedure TCustomDcefBrowser.AddZoomLevel;
begin
  FBrowserView.AddZoomLevel;
end;

procedure TCustomDcefBrowser.ExecuteJavaScript(const code: string);
begin
  FBrowserView.ExecuteJavaScript(code)
end;

procedure TCustomDcefBrowser.FormWndProc(var AMsg: TMessage);
var
  isMovingMessage: Boolean;
  x, y: Integer;
  Rect: ^TRect;
begin
  if Not(csDestroying in ComponentState) then
  begin
    isMovingMessage := False;
    x := -1;
    y := -1;
    case AMsg.Msg of
      WM_MOVING:
        begin
          Rect := Pointer(AMsg.LParam);
          isMovingMessage := True;
          x := Rect.Left;
          y := Rect.Top;
        end;
      WM_MOVE:
        begin
          isMovingMessage := True;
          x := TWMMove(AMsg).XPos;
          y := TWMMove(AMsg).YPos;
        end;
      WM_ACTIVATE:
        begin
          if Assigned(FBrowserView) then
          begin
            if AMsg.LParam = 0 then // WA_INACTIVE
            begin
              FBrowserView.IsActivating := False;
            end
            else // WA_ACTIVE or WA_CLICKACTIVE
            begin
              if Assigned(FParentForm) then
                FBrowserView.IsActivating :=
                  FBrowserView = FParentForm.ActiveControl;
            end;
          end;
        end;
    end;

    if (isMovingMessage and Assigned(FParentForm) and
      FBrowserView.HandleAllocated and
      (FParentForm.WindowState = TWindowState.wsNormal) and
      ((FParentForm.Left <> x) or (FParentForm.Top <> y)) and (Not FIsMoving))
    then
    begin
      if (FParentForm.Left >= 0) and
        (screen.Monitors[screen.MonitorCount - 1].Width >= FParentForm.Left +
        FParentForm.Width) then
      begin
        FIsMoving := True;
        if Assigned(ActiveBrowser) then
          ActiveBrowser.host.NotifyMoveOrResizeStarted;
        FIsMoving := False;
      end;
    end;

    FLastWndProc(AMsg);
  end;
end;

procedure TCustomDcefBrowser.doOnDialogClosed(const Browser: ICefBrowser);
begin
  if Assigned(FOnDialogClosed) then
    FOnDialogClosed(Browser);
end;

procedure TCustomDcefBrowser.doOnDownloadUpdated(const Browser: ICefBrowser;
  const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin
  if Assigned(FOnDownloadUpdated) then
    FOnDownloadUpdated(Browser, downloadItem, callback);
end;

{
  procedure TCustomDcefBrowser.doOnDownloadUpdated(const DcefItemIndex: Integer;
  const Kind: TBrowserDownloadUpdatedKind);
  begin
  if Assigned(FOnDownloadUpdated) and (DcefItemIndex > -1) then
  FOnDownloadUpdated(DcefItemIndex, Kind);
  end;
}

procedure TCustomDcefBrowser.doOnDragEnter(const Browser: ICefBrowser;
  const dragData: ICefDragData; mask: TCefDragOperations; var Cancel: Boolean);
begin
  if Assigned(FOnDragEnter) then
    FOnDragEnter(Browser, dragData, mask, Cancel);
end;

procedure TCustomDcefBrowser.doOnFileDialog(const Browser: ICefBrowser;
  mode: TCefFileDialogMode; const title, defaultFileName: ustring;
  acceptTypes: TStrings; const callback: ICefFileDialogCallback;
  out Result: Boolean);
begin
  if Assigned(FOnFileDialog) then
    FOnFileDialog(Browser, mode, title, defaultFileName, acceptTypes,
      callback, Result);
end;

procedure TCustomDcefBrowser.doOnAddBrowser(const Browser: ICefBrowser);
begin
  if Assigned(FOnAddBrowser) then
    FOnAddBrowser(Browser);
end;

procedure TCustomDcefBrowser.doOnBeforeBrowse(const Browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean;
  var Cancel: Boolean);
begin
  if Assigned(FOnBeforeBrowse) then
    FOnBeforeBrowse(Browser, frame, request, isRedirect, Cancel);
end;

procedure TCustomDcefBrowser.doOnBeforeContextMenu(const Browser: ICefBrowser;
  const frame: ICefFrame; const params: ICefContextMenuParams;
  const model: ICefMenuModel);
begin
  if Assigned(FOnBeforeContextMenu) then
    FOnBeforeContextMenu(Browser, frame, params, model);
end;

procedure TCustomDcefBrowser.doOnBeforeDownload(const Browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback; var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnBeforeDownload) then
    FOnBeforeDownload(Browser, downloadItem, suggestedName, callback,
      CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnBeforePluginLoad(const Browser: ICefBrowser;
  const URL, policyUrl: ustring; const info: ICefWebPluginInfo;
  var CancelLoad: Boolean);
begin
  if Assigned(FOnBeforePluginLoad) then
    FOnBeforePluginLoad(Browser, URL, policyUrl, info, CancelLoad);
end;

procedure TCustomDcefBrowser.doOnBeforeResourceLoad(const Browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest; var CancelLoad: Boolean);
begin
  if Assigned(FOnBeforeResourceLoad) then
    FOnBeforeResourceLoad(Browser, frame, request, CancelLoad);
end;

procedure TCustomDcefBrowser.doOnBeforeUnloadDialog(const Browser: ICefBrowser;
  const messageText: ustring; isReload: Boolean; callback: ICefJsDialogCallback;
  var Cancel: Boolean; var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnBeforeUnloadDialog) then
    FOnBeforeUnloadDialog(Browser, messageText, isReload, callback, Cancel,
      CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnCancelGeolocationPermission
  (const Browser: ICefBrowser; const requestingUrl: ustring;
  requestId: Integer);
begin
  if Assigned(FOnCancelGeolocationPermission) then
    FOnCancelGeolocationPermission(Browser, requestingUrl, requestId);
end;

procedure TCustomDcefBrowser.doOnCertificateError(certError: TCefErrorCode;
  const requestUrl: ustring; const callback: ICefAllowCertificateErrorCallback;
  out Result: Boolean);
begin
  if Assigned(FOnCertificateError) then
    FOnCertificateError(certError, requestUrl, callback, Result);
end;

procedure TCustomDcefBrowser.doOnConsoleMessage(const Browser: ICefBrowser;
  const message, source: ustring; line: Integer; var Cancel: Boolean);
begin
  if Assigned(FOnConsoleMessage) then
    FOnConsoleMessage(Browser, message, source, line, Cancel);
end;

procedure TCustomDcefBrowser.doOnContextMenuCommand(const Browser: ICefBrowser;
  const frame: ICefFrame; const params: ICefContextMenuParams;
  commandId: Integer; eventFlags: TCefEventFlags;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnContextMenuCommand) then
    FOnContextMenuCommand(Browser, frame, params, commandId, eventFlags,
      CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnContextMenuDismissed(const Browser
  : ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnContextMenuDismissed) then
    FOnContextMenuDismissed(Browser, frame);
end;

procedure TCustomDcefBrowser.doOnCursorChange(const Browser: ICefBrowser;
  cursor: TCefCursorHandle; cursorType: TCefCursorType;
  const customCursorInfo: PCefCursorInfo);
begin
  if Assigned(FOnCursorChange) then
    FOnCursorChange(Browser, cursor, cursorType, customCursorInfo);
end;

procedure TCustomDcefBrowser.doOnCloseBrowser(const CloseBrowserIdArr
  : Array of Integer; Const ShowBrowserId: Integer);
begin
  if Assigned(FOnCloseBrowser) then
    FOnCloseBrowser(CloseBrowserIdArr, ShowBrowserId);
end;

procedure TCustomDcefBrowser.doOnLoadingStateChange(const Browser: ICefBrowser;
  IsLoading, CanGoBack, CanGoForward: Boolean);
begin
  if Assigned(FOnLoadingStateChange) then
    FOnLoadingStateChange(Browser, IsLoading, CanGoBack, CanGoForward);
end;

procedure TCustomDcefBrowser.doOnGetAuthCredentials(const Browser: ICefBrowser;
  const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer;
  const realm, scheme: ustring; const callback: ICefAuthCallback;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnGetAuthCredentials) then
    FOnGetAuthCredentials(Browser, frame, isProxy, host, port, realm, scheme,
      callback, CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnGetResourceHandler(const Browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  var ResourceHandler: ICefResourceHandler);
begin
  if Assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(Browser, frame, request, ResourceHandler);
end;

procedure TCustomDcefBrowser.doOnGotFocus(const Browser: ICefBrowser;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnGotFocus) then
    FOnGotFocus(Browser, CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnJsdialog(const Browser: ICefBrowser;
  const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
  const messageText, defaultPromptText: ustring; callback: ICefJsDialogCallback;
  out suppressMessage: Boolean; var Cancel: Boolean;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnJsdialog) then
    FOnJsdialog(Browser, originUrl, acceptLang, dialogType, messageText,
      defaultPromptText, callback, suppressMessage, Cancel, CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnKeyEvent(const Browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle; var Cancel: Boolean);
begin
  if Assigned(FOnKeyEvent) then
    FOnKeyEvent(Browser, event, osEvent, Cancel);
end;

procedure TCustomDcefBrowser.doOnLoadEnd(const Browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Browser, frame, httpStatusCode);
end;

procedure TCustomDcefBrowser.doOnLoadError(const Browser: ICefBrowser;
  const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring; var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnLoadError) then
    FOnLoadError(Browser, frame, errorCode, errorText, failedUrl,
      CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnLoadStart(const Browser: ICefBrowser;
  const frame: ICefFrame);
begin
  if Assigned(FOnLoadStart) then
    FOnLoadStart(Browser, frame);
end;

procedure TCustomDcefBrowser.doOnPreKeyEvent(const Browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle;
  var isKeyboardShortcut: Boolean; var Cancel: Boolean;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnPreKeyEvent) then
    FOnPreKeyEvent(Browser, event, osEvent, isKeyboardShortcut, Cancel,
      CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnProtocolExecution(Browser: ICefBrowser;
  const URL: ustring; var allowOsExecution: Boolean);
begin
  if Assigned(FOnProtocolExecution) then
    FOnProtocolExecution(Browser, URL, allowOsExecution);
end;

procedure TCustomDcefBrowser.doOnQuotaRequest(const Browser: ICefBrowser;
  const originUrl: ustring; newSize: Int64; const callback: ICefQuotaCallback;
  var Cancel: Boolean);
begin
  if Assigned(FOnQuotaRequest) then
    FOnQuotaRequest(Browser, originUrl, newSize, callback, Cancel);
end;

procedure TCustomDcefBrowser.doOnRenderProcessTerminated(const Browser
  : ICefBrowser; status: TCefTerminationStatus);
begin
  if Assigned(FOnRenderProcessTerminated) then
    FOnRenderProcessTerminated(Browser, status);
end;

procedure TCustomDcefBrowser.doOnRequestGeolocationPermission
  (const Browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer;
  const callback: ICefGeolocationCallback; out Result: Boolean);
begin
  if Assigned(FOnRequestGeolocationPermission) then
    FOnRequestGeolocationPermission(Browser, requestingUrl, requestId,
      callback, Result);
end;

procedure TCustomDcefBrowser.doOnResetDialogState(const Browser: ICefBrowser);
begin
  if Assigned(FOnResetDialogState) then
    FOnResetDialogState(Browser);
end;

procedure TCustomDcefBrowser.doOnResourceRedirect(const Browser: ICefBrowser;
  const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring);
begin
  if Assigned(FOnResourceRedirect) then
    FOnResourceRedirect(Browser, frame, oldUrl, newUrl);
end;

procedure TCustomDcefBrowser.doOnSetFocus(const Browser: ICefBrowser;
  source: TCefFocusSource; var Cancel: Boolean;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Browser, source, Cancel, CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnStartDragging(const Browser: ICefBrowser;
  const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer;
  out Result: Boolean);
begin
  if Assigned(FOnStartDragging) then
    FOnStartDragging(Browser, dragData, allowedOps, x, y, Result);
end;

procedure TCustomDcefBrowser.doOnTakeFocus(const Browser: ICefBrowser;
  next: Boolean);
begin
  if Assigned(FOnTakeFocus) then
    FOnTakeFocus(Browser, next);
end;

procedure TCustomDcefBrowser.doOnTooltip(const Browser: ICefBrowser;
  var text: ustring; var Cancel: Boolean);
begin
  if Assigned(FOnTooltip) then
    FOnTooltip(Browser, text, Cancel);
end;

procedure TCustomDcefBrowser.doOnUpdateDragCursor(const Browser: ICefBrowser;
  operation: TCefDragOperation);
begin
  if Assigned(FOnUpdateDragCursor) then
    FOnUpdateDragCursor(Browser, operation);
end;

function TCustomDcefBrowser.GetActiveBrowser: ICefBrowser;
begin
  if Assigned(FBrowserView) then
    Result := FBrowserView.ActiveBrowser
  else
    Result := nil;
end;

function TCustomDcefBrowser.GetActiveBrowserID: Integer;
begin
  if Assigned(FBrowserView) then
    Result := FBrowserView.ActiveBrowserId
  else
    Result := -1;
end;

function TCustomDcefBrowser.GetBrowserCount: Integer;
begin
  Result := FBrowserView.BrowserCount;
end;

function TCustomDcefBrowser.GetCanGoBack: Boolean;
begin
  if Assigned(FBrowserView) then
    Result := FBrowserView.CanGoBack
  else
    Result := False;
end;

function TCustomDcefBrowser.GetCanGoForward: Boolean;
begin
  if Assigned(FBrowserView) then
    Result := FBrowserView.CanGoForward
  else
    Result := False;
end;

function TCustomDcefBrowser.GetClosedUrlCount: Integer;
begin
  Result := FBrowserView.ClosedUrlCount;
end;

function TCustomDcefBrowser.GetIsLoading: Boolean;
begin
  if Assigned(FBrowserView) then
    Result := FBrowserView.IsLoading
  else
    Result := False;
end;

procedure TCustomDcefBrowser.GetSettings(var Settings: TCefBrowserSettings);
begin
  Assert(Settings.size >= SizeOf(Settings));
  Settings.windowless_frame_rate := FChromiumOptions.WindowlessFrameRate;

  Settings.standard_font_family :=
    CefString(FChromiumFontOptions.StandardFontFamily);
  Settings.fixed_font_family := CefString(FChromiumFontOptions.FixedFontFamily);
  Settings.serif_font_family := CefString(FChromiumFontOptions.SerifFontFamily);
  Settings.sans_serif_font_family :=
    CefString(FChromiumFontOptions.SansSerifFontFamily);
  Settings.cursive_font_family :=
    CefString(FChromiumFontOptions.CursiveFontFamily);
  Settings.fantasy_font_family :=
    CefString(FChromiumFontOptions.FantasyFontFamily);
  Settings.default_font_size := FChromiumFontOptions.DefaultFontSize;
  Settings.default_fixed_font_size := FChromiumFontOptions.DefaultFixedFontSize;
  Settings.minimum_font_size := FChromiumFontOptions.MinimumFontSize;
  Settings.minimum_logical_font_size :=
    FChromiumFontOptions.MinimumLogicalFontSize;
  Settings.remote_fonts := FChromiumFontOptions.RemoteFonts;
  Settings.default_encoding := CefString(DefaultEncoding);

  Settings.javascript := FChromiumOptions.javascript;
  Settings.javascript_open_windows := FChromiumOptions.JavascriptOpenWindows;
  Settings.javascript_close_windows := FChromiumOptions.JavascriptCloseWindows;
  Settings.javascript_access_clipboard :=
    FChromiumOptions.JavascriptAccessClipboard;
  Settings.javascript_dom_paste := FChromiumOptions.JavascriptDomPaste;
  Settings.caret_browsing := FChromiumOptions.CaretBrowsing;
  Settings.java := FChromiumOptions.java;
  Settings.plugins := FChromiumOptions.plugins;
  Settings.universal_access_from_file_urls :=
    FChromiumOptions.UniversalAccessFromFileUrls;
  Settings.file_access_from_file_urls :=
    FChromiumOptions.FileAccessFromFileUrls;
  Settings.web_security := FChromiumOptions.WebSecurity;
  Settings.image_loading := FChromiumOptions.ImageLoading;
  Settings.image_shrink_standalone_to_fit :=
    FChromiumOptions.ImageShrinkStandaloneToFit;
  Settings.text_area_resize := FChromiumOptions.TextAreaResize;
  Settings.tab_to_links := FChromiumOptions.TabToLinks;
  Settings.local_storage := FChromiumOptions.LocalStorage;
  Settings.databases := FChromiumOptions.databases;
  Settings.application_cache := FChromiumOptions.ApplicationCache;
  Settings.webgl := FChromiumOptions.webgl;
  Settings.background_color := FChromiumOptions.BackgroundColor;
end;

function TCustomDcefBrowser.GetSource(var SourceText: string;
  Const TimeOut: Integer = 1000): Boolean;
begin
  Result := FBrowserView.GetSource(SourceText, TimeOut);
end;

procedure TCustomDcefBrowser.GetSourceInNewPage;
begin
  FBrowserView.GetSourceInNewPage;
end;

function TCustomDcefBrowser.GetText(var aText: string;
  const TimeOut: Integer): Boolean;
begin
  Result := FBrowserView.GetText(aText, TimeOut);
end;

function TCustomDcefBrowser.GetZoomLevel: string;
begin
  Result := FBrowserView.ZoomLevel;
end;

procedure TCustomDcefBrowser.GoBack;
begin
  FBrowserView.GoBack;
end;

procedure TCustomDcefBrowser.GoForward;
begin
  FBrowserView.GoForward;
end;

procedure TCustomDcefBrowser.GoHome;
begin
  Load(FDefaultUrl);
end;

procedure TCustomDcefBrowser.HookWndProc;
var
  AForm: TCustomForm;
begin
  AForm := GetParentForm(Self);
  if Assigned(AForm) then
  begin
    if AForm <> FParentForm then
      UnhookWndProc;
    FParentForm := AForm;
    FLastWndProc := FParentForm.WindowProc;
    FParentForm.WindowProc := FormWndProc;
  end;
end;

procedure TCustomDcefBrowser.Load(const aUrl: string);
begin
  if not(csDestroying in ComponentState) then
    FBrowserView.Load(aUrl);
end;

end.