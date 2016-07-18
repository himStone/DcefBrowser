(*  *                  Delphi Multi-tab Chromium Browser Frame
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

unit DcefB.Core.DcefBrowser;

interface

uses
  Windows, Classes, Controls, ComCtrls, Forms, ExtCtrls, Dialogs, StrUtils,
  SysUtils, Messages, Math, Generics.Collections,

  DcefB.Cef3.Types, DcefB.Cef3.Interfaces, DcefB.Cef3.Classes,
  DcefB.Cef3.Helper, DcefB.BaseObject, DcefB.Locker, DcefB.Settings,
  DcefB.Events, DcefB.Core.BrowserView, DcefB.Core.BrowserHandler, DcefB.res,
  DcefB.CefBrowserWrapper, DcefB.Core.DevToolsView, DcefB.Core.JsExtention;

type
  TCustomDcefBrowser = class(TWinControl, IDcefBrowser)
  private
    FBrowserView: TBrowserView;
    FDevToolsView: TDevToolsView;
    FDefaultTabControl: TPageControl;

    FDcefBOptions: TDcefBOptions;
    FChromiumOptions: TChromiumOptions;
    FChromiumFontOptions: TChromiumFontOptions;

    FDefaultUrl: ustring;
    FDefaultEncoding: ustring;
    FActivePageID: Integer;
    FIsMoving: Boolean;

    FOnDefaultTabChanged: TOnDefaultTabChanged;
    FOnDefaultTabChanging: TOnDefaultTabChanging;
    // ---------------
    FOnLoadingStateChange: TOnLoadingStateChange;
    FOnStateChange: TOnStateChange;
    FOnFavIconUrlChange: TOnFavIconUrlChange;
    FOnFullScreenModeChange: TOnFullScreenModeChange;
    FOnAddBrowser: TOnAddBrowser;
    FOnCloseBrowser: TOnCloseBrowser;
    FOnBeforeCloseBrowser: TOnBeforeCloseBrowser;
    FOnLoadStart: TOnLoadStart;
    FOnLoadEnd: TOnLoadEnd;
    FOnLoadError: TOnLoadError;
    FOnBeforeBrowse: TOnBeforeBrowse;
    FOnOpenUrlFromTab: TOnOpenUrlFromTab;
    FOnPreKeyEvent: TOnPreKeyEvent;
    FOnKeyEvent: TOnKeyEvent;
    FOnBeforeResourceLoad: TOnBeforeResourceLoad;
    FOnGetResourceHandler: TOnGetResourceHandler;
    FOnResourceRedirect: TOnResourceRedirect;
    FOnResourceResponse: TOnResourceResponse;
    FOnGetResourceResponseFilter: TOnGetResourceResponseFilter;
    FOnResourceLoadComplete: TOnResourceLoadComplete;
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
    FOnDraggableRegionsChanged: TOnDraggableRegionsChanged;
    FOnUpdateDragCursor: TOnUpdateDragCursor;
    FOnScrollOffsetChanged: TOnScrollOffsetChanged;
    FOnCursorChange: TOnCursorChange;
    FOnTooltip: TOnTooltip;
    FOnResetDialogState: TOnResetDialogState;
    FOnRenderProcessTerminated: TOnRenderProcessTerminated;
    FOnBeforePopup: TOnBeforePopup;
    FOnFindResult: TOnFindResult;

    // ---------Default Tab control Event
    procedure doOnDefaultTabChanging(Sender: TObject; var Allow: Boolean);
    procedure doOnDefaultTabChanged(Sender: TObject);

    function GetDefaultTabByID(Const aBrowserID: Integer): TTabsheet;
    procedure OnDefaultAddBrowser(const browser: ICefBrowser);
    procedure OnDefaultCloseBrowser(const CloseBrowserIdArr: array of Integer;
      const ShowBrowserId: Integer);
    procedure OnDefaultStateChange(const browser: ICefBrowser;
      const Kind: TBrowserDataChangeKind; const Value: string);
    // ---------end
    procedure GetSettings(var Settings: TCefBrowserSettings);
    procedure doOnLoadingStateChange(const browser: ICefBrowser;
      IsLoading, CanGoBack, CanGoForward: Boolean);
    procedure doOnStateChange(const browser: ICefBrowser;
      const Kind: TBrowserDataChangeKind; const Value: string);
    procedure doOnFaviconUrlChange(const browser: ICefBrowser; iconUrls: TStrings);
    procedure doOnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
    procedure doOnAddBrowser(const browser: ICefBrowser);
    procedure doOnCloseBrowser(const CloseBrowserIdArr: Array of Integer;
      Const ShowBrowserId: Integer);
    procedure doOnBeforeCloseBrowser(const CloseBrowserIdArr: Array of Integer;
      const aCloseBrowserType: TCloseBrowserType; var ShowBrowserId: Integer);

    procedure doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame);
    procedure doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame;
      httpStatusCode: Integer);
    procedure doOnLoadError(const browser: ICefBrowser; const frame: ICefFrame;
      errorCode: Integer; const errorText, failedUrl: ustring;
      var CancelDefaultEvent: Boolean);
    procedure doOnBeforeBrowse(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean;
      var Cancel: Boolean);
    procedure doOnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame;
      const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean; out Cancel: Boolean);

    procedure doOnPreKeyEvent(const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle;
      var isKeyboardShortcut: Boolean; var Cancel: Boolean;
      var CancelDefaultEvent: Boolean);
    procedure doOnKeyEvent(const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle; var Cancel: Boolean);

    procedure doOnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const callback: ICefRequestCallback; out Result: TCefReturnValue);
    procedure doOnGetResourceHandler(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      var ResourceHandler: ICefResourceHandler);
    procedure doOnResourceRedirect(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; var newUrl: ustring);
    procedure doOnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse; out Cancel: Boolean);
    procedure doOnGetResourceResponseFilter(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse;
      out Result: ICefResponseFilter);
    procedure doOnResourceLoadComplete(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse;
      status: TCefUrlRequestStatus; receivedContentLength: Int64);

    procedure doOnGotFocus(const browser: ICefBrowser;
      var CancelDefaultEvent: Boolean);
    procedure doOnSetFocus(const browser: ICefBrowser; source: TCefFocusSource;
      var Cancel: Boolean; var CancelDefaultEvent: Boolean);
    procedure doOnTakeFocus(const browser: ICefBrowser; next: Boolean);

    procedure doOnBeforeContextMenu(const browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      const model: ICefMenuModel);
    procedure doOnContextMenuCommand(const browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      commandId: Integer; eventFlags: TCefEventFlags;
      var CancelDefaultEvent: Boolean);
    procedure doOnContextMenuDismissed(const browser: ICefBrowser;
      const frame: ICefFrame);

    procedure doOnJsdialog(const browser: ICefBrowser;
      const originUrl: ustring; dialogType: TCefJsDialogType;
      const messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean;
      var Cancel: Boolean; var CancelDefaultEvent: Boolean);
    procedure doOnBeforeUnloadDialog(const browser: ICefBrowser;
      const messageText: ustring; isReload: Boolean;
      callback: ICefJsDialogCallback; var Cancel: Boolean;
      var CancelDefaultEvent: Boolean);
    procedure doOnDialogClosed(const browser: ICefBrowser);

    procedure doOnPluginCrashed(const browser: ICefBrowser;
      const pluginPath: ustring);

    procedure doOnBeforeDownload(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback;
      var CancelDefaultEvent: Boolean);
    procedure doOnDownloadUpdated(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback);
    procedure doOnGetAuthCredentials(const browser: ICefBrowser;
      const frame: ICefFrame; isProxy: Boolean; const host: ustring;
      port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback; var CancelDefaultEvent: Boolean);
    procedure doOnConsoleMessage(const browser: ICefBrowser;
      const message, source: ustring; line: Integer; var Cancel: Boolean);
    procedure doOnProtocolExecution(browser: ICefBrowser; const URL: ustring;
      var allowOsExecution: Boolean);
    procedure doOnFileDialog(const browser: ICefBrowser;
    mode: TCefFileDialogMode; const title, defaultFilePath: ustring;
    acceptFilters: TStrings; selectedAcceptFilter: Integer;
    const callback: ICefFileDialogCallback; var Cancel: Boolean);

    procedure doOnRequestGeolocationPermission(const browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer;
      const callback: ICefGeolocationCallback; var Cancel: Boolean);
    procedure doOnCancelGeolocationPermission(const browser: ICefBrowser;
       requestId: Integer);

    procedure doOnQuotaRequest(const browser: ICefBrowser;
      const originUrl: ustring; newSize: Int64;
      const callback: ICefRequestCallback; var Cancel: Boolean);
    procedure doOnCertificateError(const browser: ICefBrowser;
      certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo;
      const callback: ICefRequestCallback; out Cancel: Boolean);

    procedure doOnDragEnter(const browser: ICefBrowser;
      const dragData: ICefDragData; mask: TCefDragOperations;
      var Cancel: Boolean);
    procedure doOnStartDragging(const browser: ICefBrowser;
      const dragData: ICefDragData; allowedOps: TCefDragOperations;
      x, y: Integer; var Cancel: Boolean);
    procedure doOnDraggableRegionsChanged(const browser: ICefBrowser;
      regionsCount: NativeUInt; regions: PCefDraggableRegionArray);
    procedure doOnUpdateDragCursor(const browser: ICefBrowser;
      operation: TCefDragOperation);
    procedure doOnCursorChange(const browser: ICefBrowser;
      cursor: TCefCursorHandle; cursorType: TCefCursorType;
      const customCursorInfo: PCefCursorInfo);
    procedure doOnTooltip(const browser: ICefBrowser; var text: ustring;
      var Cancel: Boolean);
    procedure doOnResetDialogState(const browser: ICefBrowser);
    procedure doOnRenderProcessTerminated(const browser: ICefBrowser;
      status: TCefTerminationStatus);
    procedure doOnBeforePopup(const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; var Cancel: Boolean;
      var CancelDefaultEvent: Boolean);
    procedure doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
    procedure doOnFindResult(const browser: ICefBrowser; identifier, count: Integer;
      const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean);

    function GetZoomLevel: string;
    function GetActiveBrowser: ICefBrowser;
    function GetActiveBrowserID: Integer;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    function GetIsLoading: Boolean;
    function GetClosedUrlCount: Integer;
    function GetBrowserCount: Integer;
    function GetWrapperBrowsers(aBrowserID: Integer): TCefBrowserWrapper;
    function GetTitle: string;
    function GetUrl: string;
    procedure CreateDevToolsView;
    function GetJsExtention: TDcefBJsExtention;
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
    function ShowBrowser(const aBrowserID: Integer): Boolean; overload;
    function CloseBrowser(const aCloseBrowserId: Integer;
      const aShowBrowserId: Integer): Boolean;
    function CloseAllOtherBrowser(const aBrowserID: Integer): Boolean; overload;
    procedure CloseAllBrowser(const aIsTrigClosePageEvent: Boolean);
    procedure CopyBrowser(aBrowserID: Integer); overload;
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
    procedure CreateDefaultTabsControl(const aHeight: Integer = 22);
    procedure ShowDevTools(const aBrowser: ICefBrowser = nil);
    procedure CloseDevTools(const aBrowser: ICefBrowser = nil);

    // incomplete
    procedure SearchText;
    function GetSource(var SourceText: string;
      Const TimeOut: Integer = 1000): Boolean;
    function GetText(var aText: string; Const TimeOut: Integer = 1000): Boolean;
    // --------

    property JsExtention: TDcefBJsExtention read GetJsExtention;
    property BrowserWrappers[aBrowserID: Integer]: TCefBrowserWrapper
      read GetWrapperBrowsers;
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
    property URL: string read GetUrl;
    property title: string read GetTitle;

    property OnDefaultTabChanged: TOnDefaultTabChanged read FOnDefaultTabChanged
      write FOnDefaultTabChanged;
    property OnDefaultTabChanging: TOnDefaultTabChanging
      read FOnDefaultTabChanging write FOnDefaultTabChanging;
    // -----------------------
    property OnLoadingStateChange: TOnLoadingStateChange
      read FOnLoadingStateChange write FOnLoadingStateChange;
    property OnStateChange: TOnStateChange read FOnStateChange
      write FOnStateChange;
    property OnFavIconUrlChange: TOnFavIconUrlChange read FOnFavIconUrlChange write FOnFavIconUrlChange;
    property OnFullScreenModeChange: TOnFullScreenModeChange read FOnFullScreenModeChange write FOnFullScreenModeChange;
    property OnAddBrowser: TOnAddBrowser read FOnAddBrowser write FOnAddBrowser;
    property OnCloseBrowser: TOnCloseBrowser read FOnCloseBrowser
      write FOnCloseBrowser;
    property OnBeforeCloseBrowser: TOnBeforeCloseBrowser
      read FOnBeforeCloseBrowser write FOnBeforeCloseBrowser;
    property OnLoadStart: TOnLoadStart read FOnLoadStart write FOnLoadStart;
    property OnLoadEnd: TOnLoadEnd read FOnLoadEnd write FOnLoadEnd;
    property OnLoadError: TOnLoadError read FOnLoadError write FOnLoadError;
    property OnBeforeBrowse: TOnBeforeBrowse read FOnBeforeBrowse
      write FOnBeforeBrowse;
    property OnOpenUrlFromTab: TOnOpenUrlFromTab read FOnOpenUrlFromTab write FOnOpenUrlFromTab;
    property OnPreKeyEvent: TOnPreKeyEvent read FOnPreKeyEvent
      write FOnPreKeyEvent;
    property OnKeyEvent: TOnKeyEvent read FOnKeyEvent write FOnKeyEvent;
    property OnBeforeResourceLoad: TOnBeforeResourceLoad
      read FOnBeforeResourceLoad write FOnBeforeResourceLoad;
    property OnGetResourceHandler: TOnGetResourceHandler
      read FOnGetResourceHandler write FOnGetResourceHandler;
    property OnResourceRedirect: TOnResourceRedirect read FOnResourceRedirect
      write FOnResourceRedirect;
    property OnResourceResponse: TOnResourceResponse read FOnResourceResponse write FOnResourceResponse;
    property OnGetResourceResponseFilter: TOnGetResourceResponseFilter read FOnGetResourceResponseFilter write FOnGetResourceResponseFilter;
    property OnResourceLoadComplete: TOnResourceLoadComplete read FOnResourceLoadComplete write FOnResourceLoadComplete;
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
    property OnDraggableRegionsChanged: TOnDraggableRegionsChanged read FOnDraggableRegionsChanged write FOnDraggableRegionsChanged;
    property OnUpdateDragCursor: TOnUpdateDragCursor read FOnUpdateDragCursor
      write FOnUpdateDragCursor;
    property OnScrollOffsetChanged: TOnScrollOffsetChanged read FOnScrollOffsetChanged write FOnScrollOffsetChanged;
    property OnCursorChange: TOnCursorChange read FOnCursorChange
      write FOnCursorChange;
    property OnTooltip: TOnTooltip read FOnTooltip write FOnTooltip;
    property OnResetDialogState: TOnResetDialogState read FOnResetDialogState
      write FOnResetDialogState;
    property OnRenderProcessTerminated: TOnRenderProcessTerminated
      read FOnRenderProcessTerminated write FOnRenderProcessTerminated;
    property OnBeforePopup: TOnBeforePopup read FOnBeforePopup
      write FOnBeforePopup;
    property OnFindResult: TOnFindResult read FOnFindResult write FOnFindResult;
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

    property OnDefaultTabChanged;
    property OnDefaultTabChanging;
    property OnLoadingStateChange;
    property OnStateChange;
    property OnFavIconUrlChange;
    property OnFullScreenModeChange;
    property OnAddBrowser;
    property OnCloseBrowser;
    property OnBeforeCloseBrowser;
    property OnLoadStart;
    property OnLoadEnd;
    property OnLoadError;
    property OnBeforeBrowse;
    property OnOpenUrlFromTab;
    property OnPreKeyEvent;
    property OnKeyEvent;
    property OnBeforeResourceLoad;
    property OnGetResourceHandler;
    property OnResourceRedirect;
    property OnResourceResponse;
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
    property OnDraggableRegionsChanged;
    property OnUpdateDragCursor;
    property OnCursorChange;
    property OnTooltip;
    property OnResetDialogState;
    property OnRenderProcessTerminated;
    property OnBeforePopup;
    property OnFindResult;
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

function TCustomDcefBrowser.ShowBrowser(const aBrowserID: Integer): Boolean;
begin
  Result := FBrowserView.ShowBrowser(aBrowserID);
end;

procedure TCustomDcefBrowser.ShowDevTools(const aBrowser: ICefBrowser = nil);
var
  aShowBrowser: ICefBrowser;
begin
  if aBrowser = nil then
    aShowBrowser := ActiveBrowser
  else
    aShowBrowser := aBrowser;

  if aShowBrowser <> nil then
  begin
    if Not Assigned(FDevToolsView) then
      CreateDevToolsView;

    FDevToolsView.ShowDevTools(aShowBrowser);
  end;
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

function TCustomDcefBrowser.CloseAllOtherBrowser(const aBrowserID
  : Integer): Boolean;
begin
  Result := FBrowserView.CloseAllOtherBrowser(aBrowserID);
end;

procedure TCustomDcefBrowser.CloseAllBrowser(const aIsTrigClosePageEvent
  : Boolean);
begin
  FBrowserView.CloseAllBrowser(aIsTrigClosePageEvent);
end;

function TCustomDcefBrowser.CloseBrowser(const aCloseBrowserId,
  aShowBrowserId: Integer): Boolean;
begin
  Result := FBrowserView.CloseBrowser(aCloseBrowserId, aShowBrowserId);
end;

procedure TCustomDcefBrowser.CloseDevTools(const aBrowser: ICefBrowser = nil);
var
  aCloseBrowser: ICefBrowser;
begin
  if aBrowser = nil then
    aCloseBrowser := ActiveBrowser
  else
    aCloseBrowser := aBrowser;

  if (aCloseBrowser <> nil) and Assigned(FDevToolsView) then
    FDevToolsView.CloseDevTools(aCloseBrowser);
end;

procedure TCustomDcefBrowser.CopyBrowser(aBrowserID: Integer);
begin
  FBrowserView.CopyBrowser(aBrowserID);
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

procedure TCustomDcefBrowser.CreateDefaultTabsControl(const aHeight: Integer);
begin
  if Not Assigned(FDefaultTabControl) then
  begin
    FDefaultTabControl := TPageControl.Create(Self);
    FDefaultTabControl.Parent := Self;
    FDefaultTabControl.Align := alTop;
    FDefaultTabControl.Height := aHeight;
    FDefaultTabControl.OnChange := doOnDefaultTabChanged;
    FDefaultTabControl.OnChanging := doOnDefaultTabChanging;
  end;
end;

procedure TCustomDcefBrowser.CreateDevToolsView;
begin
  FDevToolsView := TDevToolsView.Create(Self);
  { FDevToolsView.Parent := Self;
    FDevToolsView.Align := alBottom;
    FDevToolsView.Height := 10; }
end;

destructor TCustomDcefBrowser.Destroy;
begin
  FDevToolsView.Free;
  FBrowserView.Free;
  FDcefBOptions.Free;
  FChromiumOptions.Free;
  FChromiumFontOptions.Free;
  FDefaultTabControl.Free;
  UnhookWndProc;
  inherited;
end;

procedure TCustomDcefBrowser.doOnStateChange(const browser: ICefBrowser;
  const Kind: TBrowserDataChangeKind; const Value: string);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(browser, Kind, Value);

  if Assigned(FDefaultTabControl) then
    OnDefaultStateChange(browser, Kind, Value);
end;

procedure TCustomDcefBrowser.doOnPluginCrashed(const browser: ICefBrowser;
  const pluginPath: ustring);
begin
  if Assigned(FOnPluginCrashed) then
    FOnPluginCrashed(browser, pluginPath);
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

procedure TCustomDcefBrowser.doOnDefaultTabChanged(Sender: TObject);
begin
  ShowBrowser(FDefaultTabControl.ActivePage.Tag);

  if Assigned(FOnDefaultTabChanged) then
    FOnDefaultTabChanged(Sender);
end;

procedure TCustomDcefBrowser.doOnDefaultTabChanging(Sender: TObject;
  var Allow: Boolean);
begin
  if Assigned(FOnDefaultTabChanging) then
    FOnDefaultTabChanging(Sender, Allow);
end;

procedure TCustomDcefBrowser.doOnDialogClosed(const browser: ICefBrowser);
begin
  if Assigned(FOnDialogClosed) then
    FOnDialogClosed(browser);
end;

procedure TCustomDcefBrowser.doOnDownloadUpdated(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin
  if Assigned(FOnDownloadUpdated) then
    FOnDownloadUpdated(browser, downloadItem, callback);
end;

{
  procedure TCustomDcefBrowser.doOnDownloadUpdated(const DcefItemIndex: Integer;
  const Kind: TBrowserDownloadUpdatedKind);
  begin
  if Assigned(FOnDownloadUpdated) and (DcefItemIndex > -1) then
  FOnDownloadUpdated(DcefItemIndex, Kind);
  end;
}

procedure TCustomDcefBrowser.doOnDragEnter(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: TCefDragOperations; var Cancel: Boolean);
begin
  if Assigned(FOnDragEnter) then
    FOnDragEnter(browser, dragData, mask, Cancel);
end;

procedure TCustomDcefBrowser.doOnDraggableRegionsChanged(
  const browser: ICefBrowser; regionsCount: NativeUInt;
  regions: PCefDraggableRegionArray);
begin
   if Assigned(FOnDraggableRegionsChanged) then
    FOnDraggableRegionsChanged(browser, regionsCount, regions);
end;

procedure TCustomDcefBrowser.doOnFaviconUrlChange(const browser: ICefBrowser;
  iconUrls: TStrings);
begin
  if Assigned(FOnFaviconUrlChange) then
    FOnFaviconUrlChange(browser, iconUrls);
end;

procedure TCustomDcefBrowser.doOnFileDialog(const browser: ICefBrowser;
    mode: TCefFileDialogMode; const title, defaultFilePath: ustring;
    acceptFilters: TStrings; selectedAcceptFilter: Integer;
    const callback: ICefFileDialogCallback; var Cancel: Boolean);
begin
  if Assigned(FOnFileDialog) then
    FOnFileDialog(browser,  mode, title, defaultFilePath, acceptFilters, selectedAcceptFilter, callback, Cancel);
end;

procedure TCustomDcefBrowser.doOnFindResult(const browser: ICefBrowser;
  identifier, count: Integer; const selectionRect: PCefRect;
  activeMatchOrdinal: Integer; finalUpdate: Boolean);
begin
  if Assigned(FOnFindResult) then
    FOnFindResult(browser, identifier, count, selectionRect, activeMatchOrdinal, finalUpdate);
end;

procedure TCustomDcefBrowser.doOnFullScreenModeChange(
  const browser: ICefBrowser; fullscreen: Boolean);
begin
    if Assigned(FOnFullScreenModeChange) then
    FOnFullScreenModeChange(browser, fullscreen);
end;

procedure TCustomDcefBrowser.doOnAddBrowser(const browser: ICefBrowser);
begin
  if Assigned(FOnAddBrowser) then
    FOnAddBrowser(browser);

  if Assigned(FDefaultTabControl) then
    OnDefaultAddBrowser(browser);
end;

procedure TCustomDcefBrowser.doOnBeforeBrowse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean;
  var Cancel: Boolean);
begin
  if Assigned(FOnBeforeBrowse) then
    FOnBeforeBrowse(browser, frame, request, isRedirect, Cancel);
end;

procedure TCustomDcefBrowser.doOnBeforeCloseBrowser(const CloseBrowserIdArr
  : Array of Integer; const aCloseBrowserType: TCloseBrowserType;
  var ShowBrowserId: Integer);
begin
  if Assigned(FOnBeforeCloseBrowser) then
    FOnBeforeCloseBrowser(CloseBrowserIdArr, aCloseBrowserType, ShowBrowserId);
end;

procedure TCustomDcefBrowser.doOnBeforeContextMenu(const browser: ICefBrowser;
  const frame: ICefFrame; const params: ICefContextMenuParams;
  const model: ICefMenuModel);
begin
  if Assigned(FOnBeforeContextMenu) then
    FOnBeforeContextMenu(browser, frame, params, model);
end;

procedure TCustomDcefBrowser.doOnBeforeDownload(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback; var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnBeforeDownload) then
    FOnBeforeDownload(browser, downloadItem, suggestedName, callback,
      CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnBeforePopup(const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; var Cancel: Boolean;
      var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnBeforePopup) then
    FOnBeforePopup(browser, frame, targetUrl, targetFrameName, targetDisposition, userGesture, popupFeatures,
      windowInfo, client, Settings, noJavascriptAccess, Cancel,
      CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const callback: ICefRequestCallback; out Result: TCefReturnValue);
begin
  if Assigned(FOnBeforeResourceLoad) then
    FOnBeforeResourceLoad(browser, frame, request, callback, Result)
  else
    Result := RV_CONTINUE;
end;

procedure TCustomDcefBrowser.doOnBeforeUnloadDialog(const browser: ICefBrowser;
  const messageText: ustring; isReload: Boolean; callback: ICefJsDialogCallback;
  var Cancel: Boolean; var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnBeforeUnloadDialog) then
    FOnBeforeUnloadDialog(browser, messageText, isReload, callback, Cancel,
      CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnCancelGeolocationPermission
  (const browser: ICefBrowser;
  requestId: Integer);
begin
  if Assigned(FOnCancelGeolocationPermission) then
    FOnCancelGeolocationPermission(browser, requestId);
end;

procedure TCustomDcefBrowser.doOnCertificateError(const browser: ICefBrowser;
    certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo;
    const callback: ICefRequestCallback; out Cancel: Boolean);
begin
  if Assigned(FOnCertificateError) then
    FOnCertificateError(browser, certError, requestUrl, sslInfo, callback, Cancel);
end;

procedure TCustomDcefBrowser.doOnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: Integer; var Cancel: Boolean);
begin
  if Assigned(FOnConsoleMessage) then
    FOnConsoleMessage(browser, message, source, line, Cancel);
end;

procedure TCustomDcefBrowser.doOnContextMenuCommand(const browser: ICefBrowser;
  const frame: ICefFrame; const params: ICefContextMenuParams;
  commandId: Integer; eventFlags: TCefEventFlags;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnContextMenuCommand) then
    FOnContextMenuCommand(browser, frame, params, commandId, eventFlags,
      CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnContextMenuDismissed(const browser
  : ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnContextMenuDismissed) then
    FOnContextMenuDismissed(browser, frame);
end;

procedure TCustomDcefBrowser.doOnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle; cursorType: TCefCursorType;
  const customCursorInfo: PCefCursorInfo);
begin
  if Assigned(FOnCursorChange) then
    FOnCursorChange(browser, cursor, cursorType, customCursorInfo);
end;

procedure TCustomDcefBrowser.doOnCloseBrowser(const CloseBrowserIdArr
  : Array of Integer; Const ShowBrowserId: Integer);
begin
  if Assigned(FOnCloseBrowser) then
    FOnCloseBrowser(CloseBrowserIdArr, ShowBrowserId);

  if Assigned(FDefaultTabControl) then
    OnDefaultCloseBrowser(CloseBrowserIdArr, ShowBrowserId);
end;

procedure TCustomDcefBrowser.doOnLoadingStateChange(const browser: ICefBrowser;
  IsLoading, CanGoBack, CanGoForward: Boolean);
begin
  if Assigned(FOnLoadingStateChange) then
    FOnLoadingStateChange(browser, IsLoading, CanGoBack, CanGoForward);
end;

procedure TCustomDcefBrowser.doOnGetAuthCredentials(const browser: ICefBrowser;
  const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer;
  const realm, scheme: ustring; const callback: ICefAuthCallback;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnGetAuthCredentials) then
    FOnGetAuthCredentials(browser, frame, isProxy, host, port, realm, scheme,
      callback, CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnGetResourceHandler(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  var ResourceHandler: ICefResourceHandler);
begin
  if Assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(browser, frame, request, ResourceHandler);
end;

procedure TCustomDcefBrowser.doOnGetResourceResponseFilter(
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse;
  out Result: ICefResponseFilter);
begin
  if Assigned(FOnGetResourceResponseFilter) then
    FOnGetResourceResponseFilter(browser, frame, request, response, Result);
end;

procedure TCustomDcefBrowser.doOnGotFocus(const browser: ICefBrowser;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnGotFocus) then
    FOnGotFocus(browser, CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnJsdialog(const browser: ICefBrowser;
  const originUrl: ustring; dialogType: TCefJsDialogType;
  const messageText, defaultPromptText: ustring; callback: ICefJsDialogCallback;
  out suppressMessage: Boolean; var Cancel: Boolean;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnJsdialog) then
    FOnJsdialog(browser, originUrl, dialogType, messageText,
      defaultPromptText, callback, suppressMessage, Cancel, CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle; var Cancel: Boolean);
begin
  if Assigned(FOnKeyEvent) then
    FOnKeyEvent(browser, event, osEvent, Cancel);
end;

procedure TCustomDcefBrowser.doOnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(browser, frame, httpStatusCode);
end;

procedure TCustomDcefBrowser.doOnLoadError(const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring; var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnLoadError) then
    FOnLoadError(browser, frame, errorCode, errorText, failedUrl,
      CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  if Assigned(FOnLoadStart) then
    FOnLoadStart(browser, frame);
end;

procedure TCustomDcefBrowser.doOnOpenUrlFromTab(const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  out Cancel: Boolean);
begin
  if Assigned(FOnOpenUrlFromTab) then
    FOnOpenUrlFromTab(browser, frame, targetUrl, targetDisposition, userGesture, Cancel);
end;

procedure TCustomDcefBrowser.doOnPreKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle;
  var isKeyboardShortcut: Boolean; var Cancel: Boolean;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnPreKeyEvent) then
    FOnPreKeyEvent(browser, event, osEvent, isKeyboardShortcut, Cancel,
      CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnProtocolExecution(browser: ICefBrowser;
  const URL: ustring; var allowOsExecution: Boolean);
begin
  if Assigned(FOnProtocolExecution) then
    FOnProtocolExecution(browser, URL, allowOsExecution);
end;

procedure TCustomDcefBrowser.doOnQuotaRequest(const browser: ICefBrowser;
  const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback;
  var Cancel: Boolean);
begin
  if Assigned(FOnQuotaRequest) then
    FOnQuotaRequest(browser, originUrl, newSize, callback, Cancel);
end;

procedure TCustomDcefBrowser.doOnRenderProcessTerminated(const browser
  : ICefBrowser; status: TCefTerminationStatus);
begin
  if Assigned(FOnRenderProcessTerminated) then
    FOnRenderProcessTerminated(browser, status);
end;

procedure TCustomDcefBrowser.doOnRequestGeolocationPermission
  (const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer;
  const callback: ICefGeolocationCallback; var Cancel: Boolean);
begin
  if Assigned(FOnRequestGeolocationPermission) then
    FOnRequestGeolocationPermission(browser, requestingUrl, requestId,
      callback, Cancel);
end;

procedure TCustomDcefBrowser.doOnResetDialogState(const browser: ICefBrowser);
begin
  if Assigned(FOnResetDialogState) then
    FOnResetDialogState(browser);
end;

procedure TCustomDcefBrowser.doOnResourceLoadComplete(
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse;
  status: TCefUrlRequestStatus; receivedContentLength: Int64);
begin
  if Assigned(FOnResourceLoadComplete) then
    FOnResourceLoadComplete(browser, frame, request, response, status, receivedContentLength);
end;

procedure TCustomDcefBrowser.doOnResourceRedirect(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; var newUrl: ustring);
begin
  if Assigned(FOnResourceRedirect) then
    FOnResourceRedirect(browser, frame, request, newUrl);
end;

procedure TCustomDcefBrowser.doOnResourceResponse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const response: ICefResponse; out Cancel: Boolean);
begin
  if Assigned(FOnResourceResponse) then
    FOnResourceResponse(browser, frame, request, response, Cancel);
end;

procedure TCustomDcefBrowser.doOnScrollOffsetChanged(const browser: ICefBrowser;
  x, y: Double);
begin
   if Assigned(FOnScrollOffsetChanged) then
    FOnScrollOffsetChanged(browser, x, y);
end;

procedure TCustomDcefBrowser.doOnSetFocus(const browser: ICefBrowser;
  source: TCefFocusSource; var Cancel: Boolean;
  var CancelDefaultEvent: Boolean);
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(browser, source, Cancel, CancelDefaultEvent);
end;

procedure TCustomDcefBrowser.doOnStartDragging(const browser: ICefBrowser;
  const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer;
  var Cancel: Boolean);
begin
  if Assigned(FOnStartDragging) then
    FOnStartDragging(browser, dragData, allowedOps, x, y, Cancel);
end;

procedure TCustomDcefBrowser.doOnTakeFocus(const browser: ICefBrowser;
  next: Boolean);
begin
  if Assigned(FOnTakeFocus) then
    FOnTakeFocus(browser, next);
end;

procedure TCustomDcefBrowser.doOnTooltip(const browser: ICefBrowser;
  var text: ustring; var Cancel: Boolean);
begin
  if Assigned(FOnTooltip) then
    FOnTooltip(browser, text, Cancel);
end;

procedure TCustomDcefBrowser.doOnUpdateDragCursor(const browser: ICefBrowser;
  operation: TCefDragOperation);
begin
  if Assigned(FOnUpdateDragCursor) then
    FOnUpdateDragCursor(browser, operation);
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

function TCustomDcefBrowser.GetDefaultTabByID(const aBrowserID: Integer)
  : TTabsheet;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to FDefaultTabControl.PageCount - 1 do
    if FDefaultTabControl.Pages[Index].Tag = aBrowserID then
    begin
      Result := FDefaultTabControl.Pages[Index];
      Break;
    end;
end;

function TCustomDcefBrowser.GetIsLoading: Boolean;
begin
  if Assigned(FBrowserView) then
    Result := FBrowserView.IsLoading
  else
    Result := False;
end;

function TCustomDcefBrowser.GetJsExtention: TDcefBJsExtention;
begin
  Result := FBrowserView.JsExtention;
end;

procedure TCustomDcefBrowser.GetSettings(var Settings: TCefBrowserSettings);
begin
  Assert(Settings.size >= SizeOf(Settings));
  Settings.windowless_frame_rate := FChromiumOptions.WindowlessFrameRate;

  Settings.standard_font_family := TCef3Helper.CefString
    (FChromiumFontOptions.StandardFontFamily);
  Settings.fixed_font_family := TCef3Helper.CefString
    (FChromiumFontOptions.FixedFontFamily);
  Settings.serif_font_family := TCef3Helper.CefString
    (FChromiumFontOptions.SerifFontFamily);
  Settings.sans_serif_font_family := TCef3Helper.CefString
    (FChromiumFontOptions.SansSerifFontFamily);
  Settings.cursive_font_family := TCef3Helper.CefString
    (FChromiumFontOptions.CursiveFontFamily);
  Settings.fantasy_font_family := TCef3Helper.CefString
    (FChromiumFontOptions.FantasyFontFamily);
  Settings.default_font_size := FChromiumFontOptions.DefaultFontSize;
  Settings.default_fixed_font_size := FChromiumFontOptions.DefaultFixedFontSize;
  Settings.minimum_font_size := FChromiumFontOptions.MinimumFontSize;
  Settings.minimum_logical_font_size :=
    FChromiumFontOptions.MinimumLogicalFontSize;
  Settings.remote_fonts := FChromiumFontOptions.RemoteFonts;
  Settings.default_encoding := TCef3Helper.CefString(DefaultEncoding);

  Settings.javascript := FChromiumOptions.javascript;
  Settings.javascript_open_windows := FChromiumOptions.JavascriptOpenWindows;
  Settings.javascript_close_windows := FChromiumOptions.JavascriptCloseWindows;
  Settings.javascript_access_clipboard :=
    FChromiumOptions.JavascriptAccessClipboard;
  Settings.javascript_dom_paste := FChromiumOptions.JavascriptDomPaste;
  Settings.caret_browsing := FChromiumOptions.CaretBrowsing;
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

function TCustomDcefBrowser.GetTitle: string;
begin
  Result := FBrowserView.title;
end;

function TCustomDcefBrowser.GetUrl: string;
begin
  Result := FBrowserView.URL;
end;

function TCustomDcefBrowser.GetWrapperBrowsers(aBrowserID: Integer)
  : TCefBrowserWrapper;
begin
  Result := FBrowserView.BrowserWrappers[aBrowserID];
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

procedure TCustomDcefBrowser.OnDefaultAddBrowser(const browser: ICefBrowser);
var
  NewTab: TTabsheet;
begin
  NewTab := TTabsheet.Create(FDefaultTabControl);
  NewTab.Caption := '';
  NewTab.Tag := browser.Identifier;
  NewTab.PageControl := FDefaultTabControl;

  FDefaultTabControl.ActivePageIndex := NewTab.PageIndex;
end;

procedure TCustomDcefBrowser.OnDefaultCloseBrowser(const CloseBrowserIdArr
  : array of Integer; const ShowBrowserId: Integer);
var
  Index: Integer;
  MyShowTabsheet, MyCloseTabsheet: TTabsheet;
begin
  MyShowTabsheet := GetDefaultTabByID(ShowBrowserId);
  for Index := Low(CloseBrowserIdArr) to High(CloseBrowserIdArr) do
  begin
    MyCloseTabsheet := GetDefaultTabByID(Index);
    if MyCloseTabsheet <> nil then
      MyCloseTabsheet.Free;
  end;

  if (MyShowTabsheet <> nil) and
    (MyShowTabsheet <> FDefaultTabControl.ActivePage) then
    FDefaultTabControl.ActivePage := MyShowTabsheet;
end;

procedure TCustomDcefBrowser.OnDefaultStateChange(const browser: ICefBrowser;
  const Kind: TBrowserDataChangeKind; const Value: string);
var
  MyTabsheet: TTabsheet;
begin
  if BrowserDataChange_Title = Kind then
  begin
    MyTabsheet := GetDefaultTabByID(browser.Identifier);
    if MyTabsheet <> nil then
      MyTabsheet.Caption := Value;
  end;
end;

end.