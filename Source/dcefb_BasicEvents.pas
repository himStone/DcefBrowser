unit dcefb_BasicEvents;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, dcef3_cefgui, dcef3_ceflib;

type
  TOnProcessMessageReceived = procedure(const browser: ICefBrowser;
    sourceProcess: TCefProcessId; const message: ICefProcessMessage;
    out Result: Boolean) of object;

  TOnLoadingStateChange = procedure(const browser: ICefBrowser;
    isLoading, canGoBack, canGoForward: Boolean) of object;
  TOnLoadStart = procedure(const browser: ICefBrowser; const frame: ICefFrame)
    of object;
  TOnLoadEnd = procedure(const browser: ICefBrowser; const frame: ICefFrame;
    httpStatusCode: Integer) of object;
  TOnLoadError = procedure(const browser: ICefBrowser; const frame: ICefFrame;
    errorCode: Integer; const errorText, failedUrl: ustring) of object;
  TOnRenderProcessTerminated = procedure(const browser: ICefBrowser;
    status: TCefTerminationStatus) of object;
  TOnPluginCrashed = procedure(const browser: ICefBrowser;
    const pluginPath: ustring) of object;

  TOnTakeFocus = procedure(const browser: ICefBrowser; next: Boolean) of object;
  TOnSetFocus = procedure(const browser: ICefBrowser; source: TCefFocusSource;
    out Result: Boolean) of object;
  TOnGotFocus = procedure(const browser: ICefBrowser) of object;

  TOnBeforeContextMenu = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const params: ICefContextMenuParams;
    const model: ICefMenuModel) of object;
  TOnContextMenuCommand = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const params: ICefContextMenuParams;
    commandId: Integer; eventFlags: TCefEventFlags; out Result: Boolean)
    of object;
  TOnContextMenuDismissed = procedure(const browser: ICefBrowser;
    const frame: ICefFrame) of object;

  TOnPreKeyEvent = procedure(const browser: ICefBrowser;
    const event: PCefKeyEvent; osEvent: TCefEventHandle;
    out isKeyboardShortcut: Boolean; out Result: Boolean) of object;
  TOnKeyEvent = procedure(const browser: ICefBrowser; const event: PCefKeyEvent;
    osEvent: TCefEventHandle; out Result: Boolean) of object;

  TOnAddressChange = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const url: ustring) of object;
  TOnTitleChange = procedure(const browser: ICefBrowser; const title: ustring)
    of object;
  TOnTooltip = procedure(const browser: ICefBrowser; var text: ustring;
    out Result: Boolean) of object;
  TOnStatusMessage = procedure(const browser: ICefBrowser; const value: ustring)
    of object;
  TOnConsoleMessage = procedure(const browser: ICefBrowser;
    const message, source: ustring; line: Integer; out Result: Boolean)
    of object;

  TOnBeforeDownload = procedure(const browser: ICefBrowser;
    const downloadItem: ICefDownloadItem; const suggestedName: ustring;
    const callback: ICefBeforeDownloadCallback) of object;
  TOnDownloadUpdated = procedure(const browser: ICefBrowser;
    const downloadItem: ICefDownloadItem;
    const callback: ICefDownloadItemCallback) of object;

  TOnRequestGeolocationPermission = procedure(const browser: ICefBrowser;
    const requestingUrl: ustring; requestId: Integer;
    const callback: ICefGeolocationCallback; out Result: Boolean) of object;
  TOnCancelGeolocationPermission = procedure(const browser: ICefBrowser;
    const requestingUrl: ustring; requestId: Integer) of object;

  TOnJsdialog = procedure(const browser: ICefBrowser;
    const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
    const messageText, defaultPromptText: ustring;
    callback: ICefJsDialogCallback; out suppressMessage: Boolean;
    out Result: Boolean) of object;
  TOnBeforeUnloadDialog = procedure(const browser: ICefBrowser;
    const messageText: ustring; isReload: Boolean;
    const callback: ICefJsDialogCallback; out Result: Boolean) of object;
  TOnResetDialogState = procedure(const browser: ICefBrowser) of object;

  TOnDialogClosed = procedure(const browser: ICefBrowser) of object;
  TOnBeforePopup = procedure(const browser: ICefBrowser; const frame: ICefFrame;
    const targetUrl, targetFrameName: ustring;
    var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
    var client: ICefClient; var settings: TCefBrowserSettings;
    var noJavascriptAccess: Boolean; out Result: Boolean) of object;

  TOnAfterCreated = procedure(const browser: ICefBrowser) of object;
  TOnBeforeClose = procedure(const browser: ICefBrowser) of object;
  TOnRunModal = procedure(const browser: ICefBrowser; out Result: Boolean)
    of object;
  TOnClose = procedure(const browser: ICefBrowser; out Result: Boolean)
    of object;

  TOnBeforeBrowse = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean;
    out Result: Boolean) of object;
  TOnBeforeResourceLoad = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const request: ICefRequest; out Result: Boolean)
    of object;
  TOnGetResourceHandler = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const request: ICefRequest;
    out Result: ICefResourceHandler) of object;
  TOnResourceRedirect = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring)
    of object;
  TOnGetAuthCredentials = procedure(const browser: ICefBrowser;
    const frame: ICefFrame; isProxy: Boolean; const host: ustring;
    port: Integer; const realm, scheme: ustring;
    const callback: ICefAuthCallback; out Result: Boolean) of object;
  TOnQuotaRequest = procedure(const browser: ICefBrowser;
    const originUrl: ustring; newSize: Int64; const callback: ICefQuotaCallback;
    out Result: Boolean) of object;
  TOnProtocolExecution = procedure(const browser: ICefBrowser;
    const url: ustring; out allowOsExecution: Boolean) of object;

  TOnBeforePluginLoad = procedure(const browser: ICefBrowser;
    const url, policyUrl: ustring; const info: ICefWebPluginInfo;
    out Result: Boolean) of object;

  TOnFileDialog = procedure(const browser: ICefBrowser;
    mode: TCefFileDialogMode; const title, defaultFileName: ustring;
    acceptTypes: TStrings; const callback: ICefFileDialogCallback;
    out Result: Boolean) of object;

  TOnDragEnter = procedure(const browser: ICefBrowser;
    const dragData: ICefDragData; mask: TCefDragOperations; out Result: Boolean)
    of object;

  TOnStartDragging = procedure(const browser: ICefBrowser;
    const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer;
    out Result: Boolean) of object;
  TOnUpdateDragCursor = procedure(const browser: ICefBrowser;
    operation: TCefDragOperation) of object;

  TOnCertificateError = procedure(certError: TCefErrorCode;
    const requestUrl: ustring;
    const callback: ICefAllowCertificateErrorCallback; out Result: Boolean)
    of object;

  TOnCursorChange = procedure(const browser: ICefBrowser;
    cursor: TCefCursorHandle; cursorType: TCefCursorType;
    const customCursorInfo: PCefCursorInfo) of object;

  { TOnGetRootScreenRect = procedure(const browser: ICefBrowser;
    rect: PCefRect; out Result: Boolean);
    TOnGetViewRect = procedure(const browser: ICefBrowser;
    rect: PCefRect; out Result: Boolean);
    TOnGetScreenPoint = procedure(const browser: ICefBrowser;
    viewX, viewY: Integer; screenX, screenY: PInteger; out Result: Boolean);
    TOnGetScreenInfo = procedure(const browser: ICefBrowser;
    screenInfo: PCefScreenInfo; Result: Boolean);
    TOnPopupShow = procedure(const browser: ICefBrowser;
    show: Boolean);
    TOnPopupSize = procedure(const browser: ICefBrowser;
    const rect: PCefRect);
    TOnPaint = procedure(const browser: ICefBrowser;
    kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray;
    const buffer: Pointer; width, height: Integer);
    TOnScrollOffsetChanged = procedure(const browser: ICefBrowser); }

  TBasicDcefBrowserEvents = class(TInterfacedObject, IChromiumEvents)
  private
    FOnProcessMessageReceived: TOnProcessMessageReceived;
    FOnLoadStart: TOnLoadStart;
    FOnLoadEnd: TOnLoadEnd;
    FOnLoadError: TOnLoadError;
    FOnRenderProcessTerminated: TOnRenderProcessTerminated;
    FOnPluginCrashed: TOnPluginCrashed;
    FOnTakeFocus: TOnTakeFocus;
    FOnSetFocus: TOnSetFocus;
    FOnGotFocus: TOnGotFocus;
    FOnBeforeContextMenu: TOnBeforeContextMenu;
    FOnContextMenuCommand: TOnContextMenuCommand;
    FOnContextMenuDismissed: TOnContextMenuDismissed;
    FOnPreKeyEvent: TOnPreKeyEvent;
    FOnKeyEvent: TOnKeyEvent;
    FOnLoadingStateChange: TOnLoadingStateChange;
    FOnAddressChange: TOnAddressChange;

    FOnTitleChange: TOnTitleChange;
    FOnTooltip: TOnTooltip;
    FOnStatusMessage: TOnStatusMessage;
    FOnConsoleMessage: TOnConsoleMessage;
    FOnBeforeDownload: TOnBeforeDownload;
    FOnDownloadUpdated: TOnDownloadUpdated;
    FOnRequestGeolocationPermission: TOnRequestGeolocationPermission;
    FOnCancelGeolocationPermission: TOnCancelGeolocationPermission;
    FOnJsdialog: TOnJsdialog;

    FOnBeforeUnloadDialog: TOnBeforeUnloadDialog;
    FOnResetDialogState: TOnResetDialogState;
    FOnDialogClosed: TOnDialogClosed;
    FOnBeforePopup: TOnBeforePopup;
    FOnAfterCreated: TOnAfterCreated;
    FOnBeforeClose: TOnBeforeClose;
    FOnRunModal: TOnRunModal;
    FOnClose: TOnClose;
    FOnBeforeBrowse: TOnBeforeBrowse;
    FOnBeforeResourceLoad: TOnBeforeResourceLoad;
    FOnGetResourceHandler: TOnGetResourceHandler;
    FOnResourceRedirect: TOnResourceRedirect;
    FOnGetAuthCredentials: TOnGetAuthCredentials;
    FOnQuotaRequest: TOnQuotaRequest;
    FOnProtocolExecution: TOnProtocolExecution;

    FOnBeforePluginLoad: TOnBeforePluginLoad;

    FOnFileDialog: TOnFileDialog;

    FOnDragEnter: TOnDragEnter;

    FOnStartDragging: TOnStartDragging;
    FOnUpdateDragCursor: TOnUpdateDragCursor;

    FOnCertificateError: TOnCertificateError;
    FOnCursorChange: TOnCursorChange;
  protected
    procedure GetSettings(var settings: TCefBrowserSettings);
    function doOnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage)
      : Boolean; virtual;

    procedure doOnLoadStart(const browser: ICefBrowser;
      const frame: ICefFrame); virtual;
    procedure doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame;
      httpStatusCode: Integer); virtual;
    procedure doOnLoadError(const browser: ICefBrowser; const frame: ICefFrame;
      errorCode: Integer; const errorText, failedUrl: ustring); virtual;
    procedure doOnRenderProcessTerminated(const browser: ICefBrowser;
      status: TCefTerminationStatus); virtual;
    procedure doOnPluginCrashed(const browser: ICefBrowser;
      const pluginPath: ustring); virtual;

    procedure doOnTakeFocus(const browser: ICefBrowser; next: Boolean); virtual;
    function doOnSetFocus(const browser: ICefBrowser; source: TCefFocusSource)
      : Boolean; virtual;
    procedure doOnGotFocus(const browser: ICefBrowser); virtual;

    procedure doOnBeforeContextMenu(const browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      const model: ICefMenuModel); virtual;
    function doOnContextMenuCommand(const browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      commandId: Integer; eventFlags: TCefEventFlags): Boolean; virtual;
    procedure doOnContextMenuDismissed(const browser: ICefBrowser;
      const frame: ICefFrame); virtual;

    function doOnPreKeyEvent(const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle;
      out isKeyboardShortcut: Boolean): Boolean; virtual;
    function doOnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle): Boolean; virtual;

    procedure doOnLoadingStateChange(const browser: ICefBrowser;
      isLoading, canGoBack, canGoForward: Boolean); virtual;
    procedure doOnAddressChange(const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring); virtual;
    procedure doOnTitleChange(const browser: ICefBrowser;
      const title: ustring); virtual;
    function doOnTooltip(const browser: ICefBrowser; var text: ustring)
      : Boolean; virtual;
    procedure doOnStatusMessage(const browser: ICefBrowser;
      const value: ustring); virtual;
    function doOnConsoleMessage(const browser: ICefBrowser;
      const message, source: ustring; line: Integer): Boolean; virtual;

    procedure doOnBeforeDownload(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback); virtual;
    procedure doOnDownloadUpdated(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback); virtual;

    function doOnRequestGeolocationPermission(const browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer;
      const callback: ICefGeolocationCallback): Boolean; virtual;
    procedure doOnCancelGeolocationPermission(const browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer); virtual;

    function doOnJsdialog(const browser: ICefBrowser;
      const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
      const messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean)
      : Boolean; virtual;
    function doOnBeforeUnloadDialog(const browser: ICefBrowser;
      const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback): Boolean; virtual;
    procedure doOnResetDialogState(const browser: ICefBrowser); virtual;

    procedure doOnDialogClosed(const browser: ICefBrowser); virtual;

    function doOnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame;
      const targetUrl, targetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean): Boolean; virtual;
    procedure doOnAfterCreated(const browser: ICefBrowser); virtual;
    procedure doOnBeforeClose(const browser: ICefBrowser); virtual;
    function doOnRunModal(const browser: ICefBrowser): Boolean; virtual;
    function doOnClose(const browser: ICefBrowser): Boolean; virtual;

    function doOnBeforeBrowse(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean)
      : Boolean; virtual;
    function doOnBeforeResourceLoad(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest): Boolean; virtual;
    function doOnGetResourceHandler(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest)
      : ICefResourceHandler; virtual;
    procedure doOnResourceRedirect(const browser: ICefBrowser;
      const frame: ICefFrame; const oldUrl: ustring;
      var newUrl: ustring); virtual;
    function doOnGetAuthCredentials(const browser: ICefBrowser;
      const frame: ICefFrame; isProxy: Boolean; const host: ustring;
      port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback): Boolean; virtual;
    function doOnQuotaRequest(const browser: ICefBrowser;
      const originUrl: ustring; newSize: Int64;
      const callback: ICefQuotaCallback): Boolean; virtual;
    procedure doOnProtocolExecution(const browser: ICefBrowser;
      const url: ustring; out allowOsExecution: Boolean); virtual;

    function doOnBeforePluginLoad(const browser: ICefBrowser;
      const url, policyUrl: ustring; const info: ICefWebPluginInfo)
      : Boolean; virtual;

    function doOnFileDialog(const browser: ICefBrowser;
      mode: TCefFileDialogMode; const title, defaultFileName: ustring;
      acceptTypes: TStrings; const callback: ICefFileDialogCallback): Boolean;

    function doOnGetRootScreenRect(const browser: ICefBrowser;
      rect: PCefRect): Boolean;
    function doOnGetViewRect(const browser: ICefBrowser;
      rect: PCefRect): Boolean;
    function doOnGetScreenPoint(const browser: ICefBrowser;
      viewX, viewY: Integer; screenX, screenY: PInteger): Boolean;
    function doOnGetScreenInfo(const browser: ICefBrowser;
      screenInfo: PCefScreenInfo): Boolean;
    procedure doOnPopupShow(const browser: ICefBrowser; show: Boolean);
    procedure doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    procedure doOnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
      dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray;
      const buffer: Pointer; width, height: Integer);
    procedure doOnCursorChange(const browser: ICefBrowser;
      cursor: TCefCursorHandle; cursorType: TCefCursorType;
      const customCursorInfo: PCefCursorInfo);
    procedure doOnScrollOffsetChanged(const browser: ICefBrowser);

    function doOnDragEnter(const browser: ICefBrowser;
      const dragData: ICefDragData; mask: TCefDragOperations): Boolean;

    function doOnStartDragging(const browser: ICefBrowser;
      const dragData: ICefDragData; allowedOps: TCefDragOperations;
      x, y: Integer): Boolean;
    procedure doOnUpdateDragCursor(const browser: ICefBrowser;
      operation: TCefDragOperation);

    function doOnCertificateError(certError: TCefErrorCode;
      const requestUrl: ustring;
      const callback: ICefAllowCertificateErrorCallback): Boolean;
  public
    destructor Destroy; override;

    property OnProcessMessageReceived: TOnProcessMessageReceived
      read FOnProcessMessageReceived write FOnProcessMessageReceived;
    property OnLoadStart: TOnLoadStart read FOnLoadStart write FOnLoadStart;
    property OnLoadEnd: TOnLoadEnd read FOnLoadEnd write FOnLoadEnd;
    property OnLoadError: TOnLoadError read FOnLoadError write FOnLoadError;
    property OnRenderProcessTerminated: TOnRenderProcessTerminated
      read FOnRenderProcessTerminated write FOnRenderProcessTerminated;
    property OnPluginCrashed: TOnPluginCrashed read FOnPluginCrashed
      write FOnPluginCrashed;
    property OnSetFocus: TOnSetFocus read FOnSetFocus write FOnSetFocus;
    property OnGotFocus: TOnGotFocus read FOnGotFocus write FOnGotFocus;
    property OnTakeFocus: TOnTakeFocus read FOnTakeFocus write FOnTakeFocus;
    property OnBeforeContextMenu: TOnBeforeContextMenu read FOnBeforeContextMenu
      write FOnBeforeContextMenu;
    property OnContextMenuCommand: TOnContextMenuCommand
      read FOnContextMenuCommand write FOnContextMenuCommand;
    property OnContextMenuDismissed: TOnContextMenuDismissed
      read FOnContextMenuDismissed write FOnContextMenuDismissed;
    property OnPreKeyEvent: TOnPreKeyEvent read FOnPreKeyEvent
      write FOnPreKeyEvent;
    property OnKeyEvent: TOnKeyEvent read FOnKeyEvent write FOnKeyEvent;
    property OnLoadingStateChange: TOnLoadingStateChange
      read FOnLoadingStateChange write FOnLoadingStateChange;
    property OnAddressChange: TOnAddressChange read FOnAddressChange
      write FOnAddressChange;

    property OnTitleChange: TOnTitleChange read FOnTitleChange
      write FOnTitleChange;
    property OnTooltip: TOnTooltip read FOnTooltip write FOnTooltip;
    property OnStatusMessage: TOnStatusMessage read FOnStatusMessage
      write FOnStatusMessage;
    property OnConsoleMessage: TOnConsoleMessage read FOnConsoleMessage
      write FOnConsoleMessage;
    property OnBeforeDownload: TOnBeforeDownload read FOnBeforeDownload
      write FOnBeforeDownload;
    property OnDownloadUpdated: TOnDownloadUpdated read FOnDownloadUpdated
      write FOnDownloadUpdated;
    property OnRequestGeolocationPermission: TOnRequestGeolocationPermission
      read FOnRequestGeolocationPermission
      write FOnRequestGeolocationPermission;
    property OnCancelGeolocationPermission: TOnCancelGeolocationPermission
      read FOnCancelGeolocationPermission write FOnCancelGeolocationPermission;
    property OnJsdialog: TOnJsdialog read FOnJsdialog write FOnJsdialog;

    property OnBeforeUnloadDialog: TOnBeforeUnloadDialog
      read FOnBeforeUnloadDialog write FOnBeforeUnloadDialog;
    property OnResetDialogState: TOnResetDialogState read FOnResetDialogState
      write FOnResetDialogState;
    property OnDialogClosed: TOnDialogClosed read FOnDialogClosed
      write FOnDialogClosed;
    property OnBeforePopup: TOnBeforePopup read FOnBeforePopup
      write FOnBeforePopup;
    property OnAfterCreated: TOnAfterCreated read FOnAfterCreated
      write FOnAfterCreated;
    property OnBeforeClose: TOnBeforeClose read FOnBeforeClose
      write FOnBeforeClose;
    property OnRunModal: TOnRunModal read FOnRunModal write FOnRunModal;
    property OnClose: TOnClose read FOnClose write FOnClose;
    property OnBeforeBrowse: TOnBeforeBrowse read FOnBeforeBrowse
      write FOnBeforeBrowse;
    property OnBeforeResourceLoad: TOnBeforeResourceLoad
      read FOnBeforeResourceLoad write FOnBeforeResourceLoad;
    property OnGetResourceHandler: TOnGetResourceHandler
      read FOnGetResourceHandler write FOnGetResourceHandler;
    property OnResourceRedirect: TOnResourceRedirect read FOnResourceRedirect
      write FOnResourceRedirect;
    property OnGetAuthCredentials: TOnGetAuthCredentials
      read FOnGetAuthCredentials write FOnGetAuthCredentials;
    property OnQuotaRequest: TOnQuotaRequest read FOnQuotaRequest
      write FOnQuotaRequest;
    property OnProtocolExecution: TOnProtocolExecution read FOnProtocolExecution
      write FOnProtocolExecution;

    property OnBeforePluginLoad: TOnBeforePluginLoad read FOnBeforePluginLoad
      write FOnBeforePluginLoad;

    property OnFileDialog: TOnFileDialog read FOnFileDialog write FOnFileDialog;
    property OnDragEnter: TOnDragEnter read FOnDragEnter write FOnDragEnter;
    property OnStartDragging: TOnStartDragging read FOnStartDragging
      write FOnStartDragging;
    property OnUpdateDragCursor: TOnUpdateDragCursor read FOnUpdateDragCursor
      write FOnUpdateDragCursor;

    property OnCertificateError: TOnCertificateError read FOnCertificateError
      write FOnCertificateError;
    property OnCursorChange: TOnCursorChange read FOnCursorChange
      write FOnCursorChange;
  end;

implementation

{ TBrowserEvents }

procedure TBasicDcefBrowserEvents.GetSettings(var settings
  : TCefBrowserSettings);
begin

end;

destructor TBasicDcefBrowserEvents.Destroy;
begin
  inherited;
end;

procedure TBasicDcefBrowserEvents.doOnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin
  if Assigned(FOnAddressChange) then
    FOnAddressChange(browser, frame, url);
end;

procedure TBasicDcefBrowserEvents.doOnAfterCreated(const browser: ICefBrowser);
begin
  if Assigned(FOnAfterCreated) then
    FOnAfterCreated(browser);
end;

function TBasicDcefBrowserEvents.doOnBeforeBrowse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  isRedirect: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeBrowse) then
    FOnBeforeBrowse(browser, frame, request, isRedirect, Result);
end;

procedure TBasicDcefBrowserEvents.doOnBeforeClose(const browser: ICefBrowser);
begin
  if Assigned(FOnBeforeClose) then
    FOnBeforeClose(browser);
end;

procedure TBasicDcefBrowserEvents.doOnBeforeContextMenu(const browser
  : ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams;
  const model: ICefMenuModel);
begin
  if Assigned(FOnBeforeContextMenu) then
    FOnBeforeContextMenu(browser, frame, params, model);
end;

procedure TBasicDcefBrowserEvents.doOnBeforeDownload(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
begin
  if Assigned(FOnBeforeDownload) then
    FOnBeforeDownload(browser, downloadItem, suggestedName, callback);
end;

function TBasicDcefBrowserEvents.doOnBeforePluginLoad(const browser
  : ICefBrowser; const url, policyUrl: ustring;
  const info: ICefWebPluginInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforePluginLoad) then
    FOnBeforePluginLoad(browser, url, policyUrl, info, Result);
end;

function TBasicDcefBrowserEvents.doOnBeforePopup(const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var noJavascriptAccess: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforePopup) then
    FOnBeforePopup(browser, frame, targetUrl, targetFrameName, popupFeatures,
      windowInfo, client, settings, noJavascriptAccess, Result);
end;

function TBasicDcefBrowserEvents.doOnBeforeResourceLoad(const browser
  : ICefBrowser; const frame: ICefFrame; const request: ICefRequest): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeResourceLoad) then
    FOnBeforeResourceLoad(browser, frame, request, Result);
end;

function TBasicDcefBrowserEvents.doOnBeforeUnloadDialog(const browser
  : ICefBrowser; const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeUnloadDialog) then
    FOnBeforeUnloadDialog(browser, messageText, isReload, callback, Result);
end;

procedure TBasicDcefBrowserEvents.doOnCancelGeolocationPermission
  (const browser: ICefBrowser; const requestingUrl: ustring;
  requestId: Integer);
begin
  if Assigned(FOnCancelGeolocationPermission) then
    FOnCancelGeolocationPermission(browser, requestingUrl, requestId);
end;

function TBasicDcefBrowserEvents.doOnCertificateError(certError: TCefErrorCode;
  const requestUrl: ustring;
  const callback: ICefAllowCertificateErrorCallback): Boolean;
begin
  Result := False;
  if Assigned(FOnCertificateError) then
    FOnCertificateError(certError, requestUrl, callback, Result);
end;

function TBasicDcefBrowserEvents.doOnClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;
  if Assigned(FOnClose) then
    FOnClose(browser, Result);
end;

function TBasicDcefBrowserEvents.doOnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnConsoleMessage) then
    FOnConsoleMessage(browser, message, source, line, Result);
end;

function TBasicDcefBrowserEvents.doOnContextMenuCommand(const browser
  : ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams;
  commandId: Integer; eventFlags: TCefEventFlags): Boolean;
begin
  Result := False;
  if Assigned(FOnContextMenuCommand) then
    FOnContextMenuCommand(browser, frame, params, commandId,
      eventFlags, Result);
end;

procedure TBasicDcefBrowserEvents.doOnContextMenuDismissed
  (const browser: ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnContextMenuDismissed) then
    FOnContextMenuDismissed(browser, frame);
end;

procedure TBasicDcefBrowserEvents.doOnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle; cursorType: TCefCursorType;
  const customCursorInfo: PCefCursorInfo);
begin
  if Assigned(FOnCursorChange) then
    FOnCursorChange(browser, cursor, cursorType, customCursorInfo);
end;

procedure TBasicDcefBrowserEvents.doOnDialogClosed(const browser: ICefBrowser);
begin
  if Assigned(FOnDialogClosed) then
    FOnDialogClosed(browser);
end;

procedure TBasicDcefBrowserEvents.doOnDownloadUpdated(const browser
  : ICefBrowser; const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin
  if Assigned(FOnDownloadUpdated) then
    FOnDownloadUpdated(browser, downloadItem, callback);
end;

function TBasicDcefBrowserEvents.doOnDragEnter(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
begin
  Result := False;
  if Assigned(FOnDragEnter) then
    FOnDragEnter(browser, dragData, mask, Result);
end;

function TBasicDcefBrowserEvents.doOnFileDialog(const browser: ICefBrowser;
  mode: TCefFileDialogMode; const title, defaultFileName: ustring;
  acceptTypes: TStrings; const callback: ICefFileDialogCallback): Boolean;
begin
  Result := False;
  if Assigned(FOnFileDialog) then
    FOnFileDialog(browser, mode, title, defaultFileName, acceptTypes,
      callback, Result);
end;

function TBasicDcefBrowserEvents.doOnGetAuthCredentials(const browser
  : ICefBrowser; const frame: ICefFrame; isProxy: Boolean; const host: ustring;
  port: Integer; const realm, scheme: ustring;
  const callback: ICefAuthCallback): Boolean;
begin
  Result := False;
  if Assigned(FOnGetAuthCredentials) then
    FOnGetAuthCredentials(browser, frame, isProxy, host, port, realm, scheme,
      callback, Result);
end;

function TBasicDcefBrowserEvents.doOnGetResourceHandler(const browser
  : ICefBrowser; const frame: ICefFrame; const request: ICefRequest)
  : ICefResourceHandler;
begin
  if Assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(browser, frame, request, Result)
  else
    Result := nil;
end;

function TBasicDcefBrowserEvents.doOnGetRootScreenRect(const browser
  : ICefBrowser; rect: PCefRect): Boolean;
begin
  Result := False;
  { if Assigned(FOnGetRootScreenRect) then
    FOnGetRootScreenRect(Self, browser, rect, Result); }
end;

function TBasicDcefBrowserEvents.doOnGetScreenInfo(const browser: ICefBrowser;
  screenInfo: PCefScreenInfo): Boolean;
begin
  Result := False;
  { if Assigned(FOnGetScreenInfo) then
    FOnGetScreenInfo(Self, browser, screenInfo, Result); }
end;

function TBasicDcefBrowserEvents.doOnGetScreenPoint(const browser: ICefBrowser;
  viewX, viewY: Integer; screenX, screenY: PInteger): Boolean;
begin
  Result := False;
  { if Assigned(FOnGetScreenPoint) then
    FOnGetScreenPoint(Self, browser, viewX, viewY, screenX, screenY, Result); }
end;

function TBasicDcefBrowserEvents.doOnGetViewRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
  { if Assigned(FOnGetViewRect) then
    FOnGetViewRect(Self, browser, rect, Result); }
end;

procedure TBasicDcefBrowserEvents.doOnGotFocus(const browser: ICefBrowser);
begin
  if Assigned(FOnGotFocus) then
    FOnGotFocus(browser);
end;

function TBasicDcefBrowserEvents.doOnJsdialog(const browser: ICefBrowser;
  const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
  const messageText, defaultPromptText: ustring; callback: ICefJsDialogCallback;
  out suppressMessage: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnJsdialog) then
    FOnJsdialog(browser, originUrl, acceptLang, dialogType, messageText,
      defaultPromptText, callback, suppressMessage, Result);
end;

function TBasicDcefBrowserEvents.doOnKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean;
begin
  Result := False;
  if Assigned(FOnKeyEvent) then
    FOnKeyEvent(browser, event, osEvent, Result);
end;

procedure TBasicDcefBrowserEvents.doOnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(browser, frame, httpStatusCode);
end;

procedure TBasicDcefBrowserEvents.doOnLoadError(const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
begin
  if Assigned(FOnLoadError) then
    FOnLoadError(browser, frame, errorCode, errorText, failedUrl);
end;

procedure TBasicDcefBrowserEvents.doOnLoadingStateChange(const browser
  : ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if Assigned(FOnLoadingStateChange) then
    FOnLoadingStateChange(browser, isLoading, canGoBack, canGoForward);
end;

procedure TBasicDcefBrowserEvents.doOnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  if Assigned(FOnLoadStart) then
    FOnLoadStart(browser, frame);
end;

procedure TBasicDcefBrowserEvents.doOnPaint(const browser: ICefBrowser;
  kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer;
  width, height: Integer);
begin
  { if Assigned(FOnPaint) then
    FOnPaint(Self, browser, kind, dirtyRectsCount, dirtyRects, buffer, width, height); }
end;

procedure TBasicDcefBrowserEvents.doOnPluginCrashed(const browser: ICefBrowser;
  const pluginPath: ustring);
begin
  if Assigned(FOnPluginCrashed) then
    FOnPluginCrashed(browser, pluginPath);
end;

procedure TBasicDcefBrowserEvents.doOnPopupShow(const browser: ICefBrowser;
  show: Boolean);
begin
  { if Assigned(FOnPopupShow) then
    FOnPopupShow(Self, browser, show); }
end;

procedure TBasicDcefBrowserEvents.doOnPopupSize(const browser: ICefBrowser;
  const rect: PCefRect);
begin
  { if Assigned(FOnPopupSize) then
    FOnPopupSize(Self, browser, rect); }
end;

function TBasicDcefBrowserEvents.doOnPreKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle;
  out isKeyboardShortcut: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnPreKeyEvent) then
    FOnPreKeyEvent(browser, event, osEvent, isKeyboardShortcut, Result);
end;

function TBasicDcefBrowserEvents.doOnProcessMessageReceived
  (const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage): Boolean;
begin
  Result := False;
  if Assigned(FOnProcessMessageReceived) then
    FOnProcessMessageReceived(browser, sourceProcess, message, Result);
end;

procedure TBasicDcefBrowserEvents.doOnProtocolExecution(const browser
  : ICefBrowser; const url: ustring; out allowOsExecution: Boolean);
begin
  if Assigned(FOnProtocolExecution) then
    FOnProtocolExecution(browser, url, allowOsExecution);
end;

function TBasicDcefBrowserEvents.doOnQuotaRequest(const browser: ICefBrowser;
  const originUrl: ustring; newSize: Int64;
  const callback: ICefQuotaCallback): Boolean;
begin
  Result := False;
  if Assigned(FOnQuotaRequest) then
    FOnQuotaRequest(browser, originUrl, newSize, callback, Result);
end;

procedure TBasicDcefBrowserEvents.doOnRenderProcessTerminated
  (const browser: ICefBrowser; status: TCefTerminationStatus);
begin
  if Assigned(FOnRenderProcessTerminated) then
    FOnRenderProcessTerminated(browser, status);
end;

function TBasicDcefBrowserEvents.doOnRequestGeolocationPermission
  (const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer;
  const callback: ICefGeolocationCallback): Boolean;
begin
  Result := False;
  if Assigned(FOnRequestGeolocationPermission) then
    FOnRequestGeolocationPermission(browser, requestingUrl, requestId,
      callback, Result);
end;

procedure TBasicDcefBrowserEvents.doOnResetDialogState(const browser
  : ICefBrowser);
begin
  if Assigned(FOnResetDialogState) then
    FOnResetDialogState(browser);
end;

procedure TBasicDcefBrowserEvents.doOnResourceRedirect(const browser
  : ICefBrowser; const frame: ICefFrame; const oldUrl: ustring;
  var newUrl: ustring);
begin
  if Assigned(FOnResourceRedirect) then
    FOnResourceRedirect(browser, frame, oldUrl, newUrl);
end;

function TBasicDcefBrowserEvents.doOnRunModal(const browser
  : ICefBrowser): Boolean;
begin
  Result := False;
  if Assigned(FOnRunModal) then
    FOnRunModal(browser, Result);
end;

procedure TBasicDcefBrowserEvents.doOnScrollOffsetChanged
  (const browser: ICefBrowser);
begin
  { if Assigned(FOnScrollOffsetChanged) then
    FOnScrollOffsetChanged(Self, browser); }
end;

function TBasicDcefBrowserEvents.doOnSetFocus(const browser: ICefBrowser;
  source: TCefFocusSource): Boolean;
begin
  Result := False;
  if Assigned(FOnSetFocus) then
    FOnSetFocus(browser, source, Result);
end;

function TBasicDcefBrowserEvents.doOnStartDragging(const browser: ICefBrowser;
  const dragData: ICefDragData; allowedOps: TCefDragOperations;
  x, y: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnStartDragging) then
    FOnStartDragging(browser, dragData, allowedOps, x, y, Result);
end;

procedure TBasicDcefBrowserEvents.doOnStatusMessage(const browser: ICefBrowser;
  const value: ustring);
begin
  if Assigned(FOnStatusMessage) then
    FOnStatusMessage(browser, value);
end;

procedure TBasicDcefBrowserEvents.doOnTakeFocus(const browser: ICefBrowser;
  next: Boolean);
begin
  if Assigned(FOnTakeFocus) then
    FOnTakeFocus(browser, next);
end;

procedure TBasicDcefBrowserEvents.doOnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin
  if Assigned(FOnTitleChange) then
    FOnTitleChange(browser, title);
end;

function TBasicDcefBrowserEvents.doOnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnTooltip) then
    FOnTooltip(browser, text, Result);
end;

procedure TBasicDcefBrowserEvents.doOnUpdateDragCursor(const browser
  : ICefBrowser; operation: TCefDragOperation);
begin
  if Assigned(FOnUpdateDragCursor) then
    FOnUpdateDragCursor(browser, operation)
end;

end.
