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

unit DcefB.Events;

interface

uses
  Classes, Vcl.Graphics,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Types, DcefB.Cef3.Api;

type
  TBrowserDataChangeKind = (BrowserDataChange_StatusMessage,
    BrowserDataChange_Address, BrowserDataChange_Title);

  TCloseBrowserType = (
    // 默认，调用接口CloseBrowser
    CLOSETYPE_DEFAULT,
    // JS脚本
    CLOSETYPE_JS);

  TOnDefaultTabChanged = TNotifyEvent;
  TOnDefaultTabChanging = procedure(Sender: TObject; var Allow: Boolean)
    of object;
  TRenderProcessCallbackA = reference to procedure(aBrowser: ICefBrowser;
    aContext: ICefv8Context; aData: Pointer);

  TOnLoadingStateChange = procedure(const browser: ICefBrowser;
    isLoading, canGoBack, canGoForward: Boolean) of object;
  TOnStateChange = procedure(const browser: ICefBrowser;
    const Kind: TBrowserDataChangeKind; const Value: string) of object;
  TOnFavIconChange = procedure(const browser: ICefBrowser; const favUrl: string; const icon: TIcon) of object;
  TOnFullScreenModeChange = procedure(const browser: ICefBrowser; fullscreen: Boolean) of object;
  TOnAddBrowser = procedure(const browser: ICefBrowser) of object;
  TOnCloseBrowser = procedure(const CloseBrowserIdArr: Array of Integer;
    Const ShowBrowserId: Integer) of object;
  TOnBeforeCloseBrowser = procedure(const CloseBrowserIdArr: Array of Integer;
    const aCloseBrowserType: TCloseBrowserType; var ShowBrowserId: Integer)
    of object;

  TOnLoadStart = procedure(const browser: ICefBrowser; const frame: ICefFrame)
    of object;
  TOnLoadEnd = procedure(const browser: ICefBrowser; const frame: ICefFrame;
    httpStatusCode: Integer) of object;
  TOnLoadError = procedure(const browser: ICefBrowser; const frame: ICefFrame;
    errorCode: Integer; const errorText, failedUrl: ustring;
    var CancelDefaultEvent: Boolean) of object;
  TOnBeforeBrowse = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean;
    var Cancel: Boolean) of object;
  TOnOpenUrlFromTab = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
    userGesture: Boolean; out Cancel: Boolean) of Object;

  TOnPreKeyEvent = procedure(const browser: ICefBrowser;
    const event: PCefKeyEvent; osEvent: TCefEventHandle;
    var isKeyboardShortcut: Boolean; var Cancel: Boolean;
    var CancelDefaultEvent: Boolean) of object;
  TOnKeyEvent = procedure(const browser: ICefBrowser; const event: PCefKeyEvent;
    osEvent: TCefEventHandle; var Cancel: Boolean) of object;

  TOnBeforeResourceLoad = procedure(const browser: ICefBrowser; const frame: ICefFrame;
    const request: ICefRequest; const callback: ICefRequestCallback; out Result: TCefReturnValue) of object;

  TOnGetResourceHandler = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const request: ICefRequest;
    var ResourceHandler: ICefResourceHandler) of object;
  TOnResourceRedirect = procedure(const browser: ICefBrowser; const frame: ICefFrame;
    const request: ICefRequest; var newUrl: ustring) of object;
  TOnResourceResponse = procedure(const browser: ICefBrowser; const frame: ICefFrame;
    const request: ICefRequest; const response: ICefResponse; out Result: Boolean) of Object;
  TOnGetResourceResponseFilter = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse;
    out Result: ICefResponseFilter) of object;
  TOnResourceLoadComplete = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse;
    status: TCefUrlRequestStatus; receivedContentLength: Int64) of object;

  TOnGotFocus = procedure(const browser: ICefBrowser;
    var CancelDefaultEvent: Boolean) of object;
  TOnSetFocus = procedure(const browser: ICefBrowser; source: TCefFocusSource;
    var Cancel: Boolean; var CancelDefaultEvent: Boolean) of object;
  TOnTakeFocus = procedure(const browser: ICefBrowser; next: Boolean) of object;

  TOnBeforeContextMenu = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const params: ICefContextMenuParams;
    const model: ICefMenuModel) of object;
  TOnContextMenuCommand = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const params: ICefContextMenuParams;
    commandId: Integer; eventFlags: TCefEventFlags;
    var CancelDefaultEvent: Boolean) of object;
  TOnContextMenuDismissed = procedure(const browser: ICefBrowser;
    const frame: ICefFrame) of object;

  TOnJsdialog = procedure(const browser: ICefBrowser;
    const originUrl: ustring; dialogType: TCefJsDialogType;
    const messageText, defaultPromptText: ustring;
    callback: ICefJsDialogCallback; out suppressMessage: Boolean;
    var Cancel: Boolean; var CancelDefaultEvent: Boolean) of object;
  TOnBeforeUnloadDialog = procedure(const browser: ICefBrowser;
    const messageText: ustring; isReload: Boolean;
    callback: ICefJsDialogCallback; var Cancel: Boolean;
    var CancelDefaultEvent: Boolean) of object;
  TOnDialogClosed = procedure(const browser: ICefBrowser) of object;

  TOnBeforeDownload = procedure(const browser: ICefBrowser;
    const downloadItem: ICefDownloadItem; const suggestedName: ustring;
    const callback: ICefBeforeDownloadCallback; var CancelDefaultEvent: Boolean)
    of object;
  TOnDownloadUpdated = procedure(const browser: ICefBrowser;
    const downloadItem: ICefDownloadItem;
    const callback: ICefDownloadItemCallback) of object;
  TOnGetAuthCredentials = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; isProxy: Boolean; const host: ustring;
    port: Integer; const realm, scheme: ustring;
    const callback: ICefAuthCallback; var CancelDefaultEvent: Boolean)
    of object;
  TOnQuotaRequest = procedure(const browser: ICefBrowser;
    const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback;
    out Cancel: Boolean) of object;
  TOnConsoleMessage = procedure(const browser: ICefBrowser;
    const message, source: ustring; line: Integer; var Cancel: Boolean)
    of object;
  TOnProtocolExecution = procedure(browser: ICefBrowser; const url: ustring;
    var allowOsExecution: Boolean) of object;
  TOnCertificateError = procedure(const browser: ICefBrowser;
    certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo;
    const callback: ICefRequestCallback; out Cancel: Boolean) of Object;
  TOnPluginCrashed = procedure(const browser: ICefBrowser;
    const pluginPath: ustring) of object;
  TOnRenderViewReady = procedure(const browser: ICefBrowser) of Object;
  TOnBeforePluginLoad  = procedure(const browser: ICefBrowser;
      const URL, policyUrl: ustring; const info: ICefWebPluginInfo;
      var CancelLoad: Boolean) of Object;

  TOnFileDialog = procedure(const browser: ICefBrowser;
    mode: TCefFileDialogMode; const title, defaultFilePath: ustring;
    acceptFilters: TStrings; selectedAcceptFilter: Integer;
    const callback: ICefFileDialogCallback; var Cancel: Boolean) of Object;

  TOnRequestGeolocationPermission = procedure(const browser: ICefBrowser;
    const requestingUrl: ustring; requestId: Integer;
    const callback: ICefGeolocationCallback; var Cancel: Boolean) of object;
  TOnCancelGeolocationPermission = procedure(const browser: ICefBrowser;
    requestId: Integer) of object;

  TOnDragEnter = procedure(const browser: ICefBrowser;
    const dragData: ICefDragData; mask: TCefDragOperations; var Cancel: Boolean)
    of object;
  TOnStartDragging = procedure(const browser: ICefBrowser;
    const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer;
    var Cancel: Boolean) of object;
  TOnDraggableRegionsChanged = procedure(const browser: ICefBrowser;
    regionsCount: NativeUInt; regions: PCefDraggableRegionArray)of Object;

  TOnUpdateDragCursor = procedure(const browser: ICefBrowser;
    operation: TCefDragOperation) of object;
  TOnScrollOffsetChanged = procedure(const browser: ICefBrowser; x, y: Double) of Object;

  TOnCursorChange = procedure(const browser: ICefBrowser;
    cursor: TCefCursorHandle; cursorType: TCefCursorType;
    const customCursorInfo: PCefCursorInfo) of object;

  TOnTooltip = procedure(const browser: ICefBrowser; var text: ustring;
    var Cancel: Boolean) of object;
  TOnResetDialogState = procedure(const browser: ICefBrowser) of object;
  TOnRenderProcessTerminated = procedure(const browser: ICefBrowser;
    status: TCefTerminationStatus) of object;
  TOnFindResult = procedure(const browser: ICefBrowser; identifier,
    count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean) of Object;

  TOnBeforePopup = procedure(const browser: ICefBrowser; const frame: ICefFrame;
    const targetUrl, targetFrameName: ustring;
    targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
    var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
    var client: ICefClient; var settings: TCefBrowserSettings;
    var noJavascriptAccess: Boolean; var Cancel: Boolean;
    var CancelDefaultEvent: Boolean) of object;

  // -------------------------------------------



  // -------------------------------------------

  IDcefBrowser = interface
    procedure GetSettings(var settings: TCefBrowserSettings);
    procedure ShowDevTools(const aBrowser: ICefBrowser = nil);
    procedure CloseDevTools(const aBrowser: ICefBrowser = nil);

    procedure doOnLoadingStateChange(const browser: ICefBrowser;
      isLoading, canGoBack, canGoForward: Boolean);
    procedure doOnStateChange(const browser: ICefBrowser;
      const Kind: TBrowserDataChangeKind; const Value: string);
    procedure doOnFaviconChange(const browser: ICefBrowser; const favUrl: string; const icon: TIcon);
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
    procedure doOnGetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse; out Result: ICefResponseFilter);
    procedure doOnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus;
      receivedContentLength: Int64);

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
    procedure doOnProtocolExecution(browser: ICefBrowser; const url: ustring;
      var allowOsExecution: Boolean);
    procedure doOnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode;
      const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback; Out Cancel: Boolean);

    procedure doOnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title, defaultFilePath: ustring; acceptFilters: TStrings;
      selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback;
      var Cancel: Boolean);

    procedure doOnPluginCrashed(const browser: ICefBrowser;
      const pluginPath: ustring);

    procedure doOnRequestGeolocationPermission(const browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer;
      const callback: ICefGeolocationCallback; var Cancel: Boolean);
    procedure doOnCancelGeolocationPermission(const browser: ICefBrowser;
      requestId: Integer);

    procedure doOnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring;
      newSize: Int64; const callback: ICefRequestCallback; var Cancel: Boolean);

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
    procedure doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
    procedure doOnFindResult(const browser: ICefBrowser; identifier, count: Integer;
      const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean);

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
  end;

  // --------------------app events
  TOnGetDataResource = procedure(resourceId: Integer; out data: Pointer;
    var dataSize: NativeUInt; var Cancel: Boolean) of object;
  TOnGetLocalizedString = procedure(messageId: Integer; out stringVal: ustring;
    var Cancel: Boolean) of object;
  TOnGetDataResourceForScale = procedure(resourceId: Integer;
      scaleFactor: TCefScaleFactor; out data: Pointer;
      dataSize: NativeUInt; var Cancel: Boolean) of object;
  TOnContextInitialized = procedure() of object;
  TOnBeforeChildProcessLaunch = procedure(const commandLine: ICefCommandLine)
    of object;
  TOnRenderProcessThreadCreated = procedure(const extraInfo: ICefListValue)
    of object;
  TOnRenderThreadCreated = procedure(const extraInfo: ICefListValue) of object;
  TOnWebKitInitialized = procedure() of object;
  TOnBrowserCreated = procedure(const browser: ICefBrowser) of object;
  TOnBrowserDestroyed = procedure(const browser: ICefBrowser) of object;
  TOnBeforeNavigation = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const request: ICefRequest;
    navigationType: TCefNavigationType; isRedirect: Boolean;
    var Cancel: Boolean) of object;
  TOnContextCreated = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const context: ICefv8Context) of object;
  TOnContextReleased = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const context: ICefv8Context) of object;
  TOnUncaughtException = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const context: ICefv8Context;
    const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace)
    of object;
  TOnFocusedNodeChanged = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const node: ICefDomNode) of object;
  TOnProcessMessageReceived = procedure(const browser: ICefBrowser;
    sourceProcess: TCefProcessId; const message: ICefProcessMessage;
    var Cancel: Boolean) of object;

  IDcefBResourceBundleHandler = interface(ICefResourceBundleHandler)
    procedure SetOnGetDataResource(const Value: TOnGetDataResource);
    procedure SetOnGetLocalizedString(const Value: TOnGetLocalizedString);
    procedure SetOnGetDataResourceForScale(const Value: TOnGetDataResourceForScale);
    function GetOnGetDataResource: TOnGetDataResource;
    function GetOnGetLocalizedString: TOnGetLocalizedString;
    function GetOnGetDataResourceForScale: TOnGetDataResourceForScale;
  end;

  IDcefBBrowserProcessHandler = interface(ICefBrowserProcessHandler)
    function GetOnBeforeChildProcessLaunch: TOnBeforeChildProcessLaunch;
    function GetOnRenderProcessThreadCreated: TOnRenderProcessThreadCreated;
    function GetOnContextInitialized: TOnContextInitialized;
    procedure SetOnBeforeChildProcessLaunch(const Value
      : TOnBeforeChildProcessLaunch);
    procedure SetOnContextInitialized(const Value: TOnContextInitialized);
    procedure SetOnRenderProcessThreadCreated(const Value
      : TOnRenderProcessThreadCreated);
  end;

  IDcefBRenderProcessHandler = interface(ICefRenderProcessHandler)
    function GetOnBeforeNavigation: TOnBeforeNavigation;
    function GetOnBrowserCreated: TOnBrowserCreated;
    function GetOnBrowserDestroyed: TOnBrowserDestroyed;
    function GetOnContextCreated: TOnContextCreated;
    function GetOnContextReleased: TOnContextReleased;
    function GetOnFocusedNodeChanged: TOnFocusedNodeChanged;
    function GetOnProcessMessageReceived: TOnProcessMessageReceived;
    function GetOnRenderThreadCreated: TOnRenderThreadCreated;
    function GetOnUncaughtException: TOnUncaughtException;
    function GetOnWebKitInitialized: TOnWebKitInitialized;
    procedure SetOnBeforeNavigation(const Value: TOnBeforeNavigation);
    procedure SetOnBrowserCreated(const Value: TOnBrowserCreated);
    procedure SetOnBrowserDestroyed(const Value: TOnBrowserDestroyed);
    procedure SetOnContextCreated(const Value: TOnContextCreated);
    procedure SetOnContextReleased(const Value: TOnContextReleased);
    procedure SetOnFocusedNodeChanged(const Value: TOnFocusedNodeChanged);
    procedure SetOnProcessMessageReceived(const Value
      : TOnProcessMessageReceived);
    procedure SetOnRenderThreadCreated(const Value: TOnRenderThreadCreated);
    procedure SetOnUncaughtException(const Value: TOnUncaughtException);
    procedure SetOnWebKitInitialized(const Value: TOnWebKitInitialized);
  end;

implementation

end.
