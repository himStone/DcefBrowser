unit dcefb_Events;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, dcef3_ceflib;

type
  TBrowserDataChangeKind = (BrowserDataChange_StatusMessage,
    BrowserDataChange_Address, BrowserDataChange_Title);
  { TBrowserDownloadUpdatedKind = (BrowserDownloadUpdated_Start,
    BrowserDownloadUpdated_Progress, BrowserDownloadUpdated_End,
    BrowserDownloadUpdated_Canceled); }

  TOnPageLoadingStateChange = procedure(const PageID: Integer;
    const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean)
    of object;
  TOnPageStateChange = procedure(const PageID: Integer;
    const Kind: TBrowserDataChangeKind; const Value: string;
    const PageActived: Boolean) of object;
  TOnPageAdd = procedure(const PageID: Integer; Const AddAtLast: Boolean)
    of object;
  TOnPageClose = procedure(const ClosePageID, ShowPageID: Integer) of object;

  TOnLoadStart = procedure(const PageIndex: Integer; const browser: ICefBrowser;
    const frame: ICefFrame) of object;
  TOnLoadEnd = procedure(const PageIndex: Integer; const browser: ICefBrowser;
    const frame: ICefFrame; httpStatusCode: Integer) of object;
  TOnLoadError = procedure(const PageIndex: Integer; const browser: ICefBrowser;
    const frame: ICefFrame; errorCode: Integer;
    const errorText, failedUrl: ustring) of object;
  TOnBeforeBrowse = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const frame: ICefFrame;
    const request: ICefRequest; isRedirect: Boolean; var Cancel: Boolean)
    of object;

  TOnPreKeyEvent = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const event: PCefKeyEvent;
    osEvent: TCefEventHandle; var isKeyboardShortcut: Boolean;
    var Cancel: Boolean) of object;
  TOnKeyEvent = procedure(const PageIndex: Integer; const browser: ICefBrowser;
    const event: PCefKeyEvent; osEvent: TCefEventHandle; var Cancel: Boolean)
    of object;

  TOnBeforeResourceLoad = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const frame: ICefFrame;
    const request: ICefRequest; var CancelLoad: Boolean) of object;
  TOnGetResourceHandler = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const frame: ICefFrame;
    const request: ICefRequest; var ResourceHandler: ICefResourceHandler)
    of object;
  TOnResourceRedirect = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const frame: ICefFrame; const oldUrl: ustring;
    var newUrl: ustring) of object;

  TOnGotFocus = procedure(const PageIndex: Integer; const browser: ICefBrowser;
    var CancelEventBuiltIn: Boolean) of object;
  TOnSetFocus = procedure(const PageIndex: Integer; const browser: ICefBrowser;
    source: TCefFocusSource; var CancelFocus: Boolean) of object;
  TOnTakeFocus = procedure(const PageIndex: Integer; const browser: ICefBrowser;
    next: Boolean) of object;

  TOnBeforeContextMenu = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const frame: ICefFrame;
    const params: ICefContextMenuParams; const model: ICefMenuModel) of object;
  TOnContextMenuCommand = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const frame: ICefFrame;
    const params: ICefContextMenuParams; commandId: Integer;
    eventFlags: TCefEventFlags; var CancelEventBuiltIn: Boolean) of object;
  TOnContextMenuDismissed = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const frame: ICefFrame) of object;

  TOnJsdialog = procedure(const PageIndex: Integer; const browser: ICefBrowser;
    const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
    const messageText, defaultPromptText: ustring;
    callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean) of object;
  TOnBeforeUnloadDialog = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const messageText: ustring; isReload: Boolean;
    callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean) of object;
  TOnDialogClosed = procedure(const PageIndex: Integer;
    const browser: ICefBrowser) of object;

  { TOnDownloadUpdated = procedure(Const DcefItemIndex: Integer;
    Const Kind: TBrowserDownloadUpdatedKind) of object; }
  TOnDownloadUpdated = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
    const callback: ICefDownloadItemCallback) of object;
  TOnGetAuthCredentials = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean;
    const host: ustring; port: Integer; const realm, scheme: ustring;
    const callback: ICefAuthCallback; var CancelEventBuiltIn: Boolean)
    of object;
  TOnConsoleMessage = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const message, source: ustring; line: Integer)
    of object;
  TOnProtocolExecution = procedure(const PageIndex: Integer;
    browser: ICefBrowser; const url: ustring; var allowOsExecution: Boolean)
    of object;
  TOnFileDialog = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; mode: TCefFileDialogMode;
    const title, defaultFileName: ustring; acceptTypes: TStrings;
    const callback: ICefFileDialogCallback; out Result: Boolean) of object;

  TOnPluginCrashed = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const pluginPath: ustring) of object;
  TOnBeforePluginLoad = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const url, policyUrl: ustring;
    const info: ICefWebPluginInfo; var CancelLoad: Boolean) of object;

  TOnRequestGeolocationPermission = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const requestingUrl: ustring;
    requestId: Integer; const callback: ICefGeolocationCallback;
    out Result: Boolean) of object;
  TOnCancelGeolocationPermission = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const requestingUrl: ustring;
    requestId: Integer) of object;

  TOnQuotaRequest = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const originUrl: ustring; newSize: Int64;
    const callback: ICefQuotaCallback; out Result: Boolean) of object;

  TOnDragEnter = procedure(const PageIndex: Integer; const browser: ICefBrowser;
    const dragData: ICefDragData; mask: TCefDragOperations; out Result: Boolean)
    of object;
  TOnStartDragging = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; const dragData: ICefDragData;
    allowedOps: TCefDragOperations; x, y: Integer; out Result: Boolean)
    of object;
  TOnUpdateDragCursor = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; operation: TCefDragOperation) of object;

  TOnCertificateError = procedure(const PageIndex: Integer;
    certError: TCefErrorCode; const requestUrl: ustring;
    const callback: ICefAllowCertificateErrorCallback; out Result: Boolean)
    of object;

  TOnCursorChange = procedure(const PageIndex: Integer;
    const browser: ICefBrowser; cursor: TCefCursorHandle;
    cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo)
    of object;

  IDcefBrowserEvents = interface
    procedure doOnPageLoadingStateChange(const PageID: Integer;
      const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure doOnPageStateChange(const PageID: Integer;
      const Kind: TBrowserDataChangeKind; const Value: string;
      const PageActived: Boolean);
    procedure doOnPageAdd(const PageID: Integer; Const AddAtLast: Boolean);
    procedure doOnPageClose(const ClosePageID, ShowPageID: Integer);

    procedure doOnLoadStart(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame);
    procedure doOnLoadEnd(const PageIndex: Integer; const browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: Integer);
    procedure doOnLoadError(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
      const errorText, failedUrl: ustring);
    procedure doOnBeforeBrowse(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; isRedirect: Boolean; var Cancel: Boolean);

    procedure doOnPreKeyEvent(const PageIndex: Integer;
      const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle; var isKeyboardShortcut: Boolean;
      var Cancel: Boolean);
    procedure doOnKeyEvent(const PageIndex: Integer; const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle; var Cancel: Boolean);

    procedure doOnBeforeResourceLoad(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; var CancelLoad: Boolean);
    procedure doOnGetResourceHandler(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; var ResourceHandler: ICefResourceHandler);
    procedure doOnResourceRedirect(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame; const oldUrl: ustring;
      var newUrl: ustring);

    procedure doOnGotFocus(const PageIndex: Integer; const browser: ICefBrowser;
      var CancelEventBuiltIn: Boolean);
    procedure doOnSetFocus(const PageIndex: Integer; const browser: ICefBrowser;
      source: TCefFocusSource; var CancelFocus: Boolean);
    procedure doOnTakeFocus(const PageIndex: Integer;
      const browser: ICefBrowser; next: Boolean);

    procedure doOnBeforeContextMenu(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure doOnContextMenuCommand(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: TCefEventFlags; var CancelEventBuiltIn: Boolean);
    procedure doOnContextMenuDismissed(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame);

    procedure doOnJsdialog(const PageIndex: Integer; const browser: ICefBrowser;
      const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
      const messageText, defaultPromptText: ustring;
      const callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean);
    procedure doOnBeforeUnloadDialog(const PageIndex: Integer;
      const browser: ICefBrowser; const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean);
    procedure doOnDialogClosed(const PageIndex: Integer;
      const browser: ICefBrowser);

    procedure doOnPluginCrashed(const PageIndex: Integer;
      const browser: ICefBrowser; const pluginPath: ustring);
    procedure doOnBeforePluginLoad(const PageIndex: Integer;
      const browser: ICefBrowser; const url, policyUrl: ustring;
      const info: ICefWebPluginInfo; var CancelLoad: Boolean);

    { procedure doOnDownloadUpdated(Const DcefItemIndex: Integer;
      Const Kind: TBrowserDownloadUpdatedKind); }
    procedure doOnGetAuthCredentials(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean;
      const host: ustring; port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback; var CancelEventBuiltIn: Boolean);
    procedure doOnConsoleMessage(const PageIndex: Integer;
      const browser: ICefBrowser; const message, source: ustring;
      line: Integer);
    procedure doOnProtocolExecution(const PageIndex: Integer;
      browser: ICefBrowser; const url: ustring; var allowOsExecution: Boolean);
    procedure doOnFileDialog(const PageIndex: Integer;
      const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title, defaultFileName: ustring; acceptTypes: TStrings;
      const callback: ICefFileDialogCallback; out Result: Boolean);

    procedure doOnRequestGeolocationPermission(const PageIndex: Integer;
      const browser: ICefBrowser; const requestingUrl: ustring;
      requestId: Integer; const callback: ICefGeolocationCallback;
      out Result: Boolean);
    procedure doOnCancelGeolocationPermission(const PageIndex: Integer;
      const browser: ICefBrowser; const requestingUrl: ustring;
      requestId: Integer);

    procedure doOnQuotaRequest(const PageIndex: Integer;
      const browser: ICefBrowser; const originUrl: ustring; newSize: Int64;
      const callback: ICefQuotaCallback; out Result: Boolean);

    procedure doOnDragEnter(const PageIndex: Integer;
      const browser: ICefBrowser; const dragData: ICefDragData;
      mask: TCefDragOperations; out Result: Boolean);
    procedure doOnStartDragging(const PageIndex: Integer;
      const browser: ICefBrowser; const dragData: ICefDragData;
      allowedOps: TCefDragOperations; x, y: Integer; out Result: Boolean);
    procedure doOnUpdateDragCursor(const PageIndex: Integer;
      const browser: ICefBrowser; operation: TCefDragOperation);
    procedure doOnCertificateError(const PageIndex: Integer;
      certError: TCefErrorCode; const requestUrl: ustring;
      const callback: ICefAllowCertificateErrorCallback; out Result: Boolean);

    procedure doOnCursorChange(const PageIndex: Integer;
      const browser: ICefBrowser; cursor: TCefCursorHandle;
      cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
  end;

  TDcefBrowserEvents = class(TInterfacedObject, IDcefBrowserEvents)
  private
    FOnPageLoadingStateChange: TOnPageLoadingStateChange;
    FOnPageStateChange: TOnPageStateChange;
    FOnPageAdd: TOnPageAdd;
    FOnPageClose: TOnPageClose;

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
  protected
    procedure doOnPageLoadingStateChange(const PageID: Integer;
      const browser: ICefBrowser; isLoading, canGoBack,
      canGoForward: Boolean); virtual;
    procedure doOnPageStateChange(const PageID: Integer;
      const Kind: TBrowserDataChangeKind; const Value: string;
      const PageActived: Boolean); virtual;
    procedure doOnPageAdd(const PageID: Integer;
      Const AddAtLast: Boolean); virtual;
    procedure doOnPageClose(const ClosePageID, ShowPageID: Integer); virtual;

    procedure doOnLoadStart(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame); virtual;
    procedure doOnLoadEnd(const PageIndex: Integer; const browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: Integer); virtual;
    procedure doOnLoadError(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
      const errorText, failedUrl: ustring); virtual;
    procedure doOnBeforeBrowse(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; isRedirect: Boolean;
      var Cancel: Boolean); virtual;

    procedure doOnPreKeyEvent(const PageIndex: Integer;
      const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle; var isKeyboardShortcut: Boolean;
      var Cancel: Boolean); virtual;
    procedure doOnKeyEvent(const PageIndex: Integer; const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle;
      var Cancel: Boolean); virtual;

    procedure doOnBeforeResourceLoad(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; var CancelLoad: Boolean); virtual;
    procedure doOnGetResourceHandler(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest;
      var ResourceHandler: ICefResourceHandler); virtual;
    procedure doOnResourceRedirect(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame; const oldUrl: ustring;
      var newUrl: ustring); virtual;

    procedure doOnGotFocus(const PageIndex: Integer; const browser: ICefBrowser;
      var CancelEventBuiltIn: Boolean); virtual;
    procedure doOnSetFocus(const PageIndex: Integer; const browser: ICefBrowser;
      source: TCefFocusSource; var CancelFocus: Boolean); virtual;
    procedure doOnTakeFocus(const PageIndex: Integer;
      const browser: ICefBrowser; next: Boolean); virtual;

    procedure doOnBeforeContextMenu(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel); virtual;
    procedure doOnContextMenuCommand(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: TCefEventFlags; var CancelEventBuiltIn: Boolean); virtual;
    procedure doOnContextMenuDismissed(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame); virtual;

    procedure doOnJsdialog(const PageIndex: Integer; const browser: ICefBrowser;
      const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
      const messageText, defaultPromptText: ustring;
      const callback: ICefJsDialogCallback;
      var CancelEventBuiltIn: Boolean); virtual;
    procedure doOnBeforeUnloadDialog(const PageIndex: Integer;
      const browser: ICefBrowser; const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback;
      var CancelEventBuiltIn: Boolean); virtual;
    procedure doOnDialogClosed(const PageIndex: Integer;
      const browser: ICefBrowser); virtual;

    procedure doOnPluginCrashed(const PageIndex: Integer;
      const browser: ICefBrowser; const pluginPath: ustring); virtual;
    procedure doOnBeforePluginLoad(const PageIndex: Integer;
      const browser: ICefBrowser; const url, policyUrl: ustring;
      const info: ICefWebPluginInfo; var CancelLoad: Boolean); virtual;

    { procedure doOnDownloadUpdated(Const DcefItemIndex: Integer;
      Const Kind: TBrowserDownloadUpdatedKind); virtual; }
    procedure doOnDownloadUpdated(const PageIndex: Integer;
      const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback); virtual;
    procedure doOnGetAuthCredentials(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean;
      const host: ustring; port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback;
      var CancelEventBuiltIn: Boolean); virtual;
    procedure doOnConsoleMessage(const PageIndex: Integer;
      const browser: ICefBrowser; const message, source: ustring;
      line: Integer); virtual;
    procedure doOnProtocolExecution(const PageIndex: Integer;
      browser: ICefBrowser; const url: ustring;
      var allowOsExecution: Boolean); virtual;
    procedure doOnFileDialog(const PageIndex: Integer;
      const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title, defaultFileName: ustring; acceptTypes: TStrings;
      const callback: ICefFileDialogCallback; out Result: Boolean); virtual;

    procedure doOnRequestGeolocationPermission(const PageIndex: Integer;
      const browser: ICefBrowser; const requestingUrl: ustring;
      requestId: Integer; const callback: ICefGeolocationCallback;
      out Result: Boolean); virtual;
    procedure doOnCancelGeolocationPermission(const PageIndex: Integer;
      const browser: ICefBrowser; const requestingUrl: ustring;
      requestId: Integer); virtual;

    procedure doOnQuotaRequest(const PageIndex: Integer;
      const browser: ICefBrowser; const originUrl: ustring; newSize: Int64;
      const callback: ICefQuotaCallback; out Result: Boolean); virtual;
    procedure doOnCertificateError(const PageIndex: Integer;
      certError: TCefErrorCode; const requestUrl: ustring;
      const callback: ICefAllowCertificateErrorCallback;
      out Result: Boolean); virtual;

    procedure doOnDragEnter(const PageIndex: Integer;
      const browser: ICefBrowser; const dragData: ICefDragData;
      mask: TCefDragOperations; out Result: Boolean); virtual;
    procedure doOnStartDragging(const PageIndex: Integer;
      const browser: ICefBrowser; const dragData: ICefDragData;
      allowedOps: TCefDragOperations; x, y: Integer;
      out Result: Boolean); virtual;
    procedure doOnUpdateDragCursor(const PageIndex: Integer;
      const browser: ICefBrowser; operation: TCefDragOperation); virtual;
    procedure doOnCursorChange(const PageIndex: Integer;
      const browser: ICefBrowser; cursor: TCefCursorHandle;
      cursorType: TCefCursorType;
      const customCursorInfo: PCefCursorInfo); virtual;
  public
    destructor Destroy; override;
    property OnPageLoadingStateChange: TOnPageLoadingStateChange
      read FOnPageLoadingStateChange write FOnPageLoadingStateChange;
    property OnPageStateChange: TOnPageStateChange read FOnPageStateChange
      write FOnPageStateChange;
    property OnPageAdd: TOnPageAdd read FOnPageAdd write FOnPageAdd;
    property OnPageClose: TOnPageClose read FOnPageClose write FOnPageClose;
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
  end;

implementation

{ TDcefBrowserEvents }

destructor TDcefBrowserEvents.Destroy;
begin

  inherited;
end;

procedure TDcefBrowserEvents.doOnPageStateChange(const PageID: Integer;
  const Kind: TBrowserDataChangeKind; const Value: string;
  const PageActived: Boolean);
begin
  if Assigned(FOnPageStateChange) then
    FOnPageStateChange(PageID, Kind, Value, PageActived);
end;

procedure TDcefBrowserEvents.doOnPluginCrashed(const PageIndex: Integer;
  const browser: ICefBrowser; const pluginPath: ustring);
begin
  if Assigned(FOnPluginCrashed) then
    FOnPluginCrashed(PageIndex, browser, pluginPath);
end;

procedure TDcefBrowserEvents.doOnDialogClosed(const PageIndex: Integer;
  const browser: ICefBrowser);
begin
  if Assigned(FOnDialogClosed) then
    FOnDialogClosed(PageIndex, browser);
end;

procedure TDcefBrowserEvents.doOnDownloadUpdated(const PageIndex: Integer;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin
  if Assigned(FOnDownloadUpdated) then
    FOnDownloadUpdated(PageIndex, browser, downloadItem, callback);
end;

{
  procedure TDcefBrowserEvents.doOnDownloadUpdated(const DcefItemIndex: Integer;
  const Kind: TBrowserDownloadUpdatedKind);
  begin
  if Assigned(FOnDownloadUpdated) then
  FOnDownloadUpdated(DcefItemIndex, Kind);
  end;
}

procedure TDcefBrowserEvents.doOnDragEnter(const PageIndex: Integer;
  const browser: ICefBrowser; const dragData: ICefDragData;
  mask: TCefDragOperations; out Result: Boolean);
begin
  if Assigned(FOnDragEnter) then
    FOnDragEnter(PageIndex, browser, dragData, mask, Result);
end;

procedure TDcefBrowserEvents.doOnFileDialog(const PageIndex: Integer;
  const browser: ICefBrowser; mode: TCefFileDialogMode;
  const title, defaultFileName: ustring; acceptTypes: TStrings;
  const callback: ICefFileDialogCallback; out Result: Boolean);
begin
  if Assigned(FOnFileDialog) then
    FOnFileDialog(PageIndex, browser, mode, title, defaultFileName, acceptTypes,
      callback, Result);
end;

procedure TDcefBrowserEvents.doOnPageAdd(const PageID: Integer;
  Const AddAtLast: Boolean);
begin
  if Assigned(FOnPageAdd) then
    FOnPageAdd(PageID, AddAtLast);
end;

procedure TDcefBrowserEvents.doOnBeforeBrowse(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; isRedirect: Boolean; var Cancel: Boolean);
begin
  if Assigned(FOnBeforeBrowse) then
    FOnBeforeBrowse(PageIndex, browser, frame, request, isRedirect, Cancel);
end;

procedure TDcefBrowserEvents.doOnBeforeContextMenu(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  if Assigned(FOnBeforeContextMenu) then
    FOnBeforeContextMenu(PageIndex, browser, frame, params, model);
end;

procedure TDcefBrowserEvents.doOnBeforePluginLoad(const PageIndex: Integer;
  const browser: ICefBrowser; const url, policyUrl: ustring;
  const info: ICefWebPluginInfo; var CancelLoad: Boolean);
begin
  if Assigned(FOnBeforePluginLoad) then
    FOnBeforePluginLoad(PageIndex, browser, url, policyUrl, info, CancelLoad);
end;

procedure TDcefBrowserEvents.doOnBeforeResourceLoad(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; var CancelLoad: Boolean);
begin
  if Assigned(FOnBeforeResourceLoad) then
    FOnBeforeResourceLoad(PageIndex, browser, frame, request, CancelLoad);
end;

procedure TDcefBrowserEvents.doOnBeforeUnloadDialog(const PageIndex: Integer;
  const browser: ICefBrowser; const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnBeforeUnloadDialog) then
    FOnBeforeUnloadDialog(PageIndex, browser, messageText, isReload, callback,
      CancelEventBuiltIn);
end;

procedure TDcefBrowserEvents.doOnCancelGeolocationPermission(const PageIndex
  : Integer; const browser: ICefBrowser; const requestingUrl: ustring;
  requestId: Integer);
begin
  if Assigned(FOnCancelGeolocationPermission) then
    FOnCancelGeolocationPermission(PageIndex, browser, requestingUrl,
      requestId);
end;

procedure TDcefBrowserEvents.doOnCertificateError(const PageIndex: Integer;
  certError: TCefErrorCode; const requestUrl: ustring;
  const callback: ICefAllowCertificateErrorCallback; out Result: Boolean);
begin
  if Assigned(FOnCertificateError) then
    FOnCertificateError(PageIndex, certError, requestUrl, callback, Result);
end;

procedure TDcefBrowserEvents.doOnConsoleMessage(const PageIndex: Integer;
  const browser: ICefBrowser; const message, source: ustring; line: Integer);
begin
  if Assigned(FOnConsoleMessage) then
    FOnConsoleMessage(PageIndex, browser, message, source, line);
end;

procedure TDcefBrowserEvents.doOnContextMenuCommand(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: TCefEventFlags; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnContextMenuCommand) then
    FOnContextMenuCommand(PageIndex, browser, frame, params, commandId,
      eventFlags, CancelEventBuiltIn);
end;

procedure TDcefBrowserEvents.doOnContextMenuDismissed(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnContextMenuDismissed) then
    FOnContextMenuDismissed(PageIndex, browser, frame);
end;

procedure TDcefBrowserEvents.doOnCursorChange(const PageIndex: Integer;
  const browser: ICefBrowser; cursor: TCefCursorHandle;
  cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
begin
  if Assigned(FOnCursorChange) then
    FOnCursorChange(PageIndex, browser, cursor, cursorType, customCursorInfo);
end;

procedure TDcefBrowserEvents.doOnPageClose(const ClosePageID,
  ShowPageID: Integer);
begin
  if Assigned(FOnPageClose) then
    FOnPageClose(ClosePageID, ShowPageID);
end;

procedure TDcefBrowserEvents.doOnPageLoadingStateChange(const PageID: Integer;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if Assigned(FOnPageLoadingStateChange) then
    FOnPageLoadingStateChange(PageID, browser, isLoading, canGoBack,
      canGoForward);
end;

procedure TDcefBrowserEvents.doOnGetAuthCredentials(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean;
  const host: ustring; port: Integer; const realm, scheme: ustring;
  const callback: ICefAuthCallback; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnGetAuthCredentials) then
    FOnGetAuthCredentials(PageIndex, browser, frame, isProxy, host, port, realm,
      scheme, callback, CancelEventBuiltIn);
end;

procedure TDcefBrowserEvents.doOnGetResourceHandler(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; var ResourceHandler: ICefResourceHandler);
begin
  if Assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(PageIndex, browser, frame, request, ResourceHandler);
end;

procedure TDcefBrowserEvents.doOnGotFocus(const PageIndex: Integer;
  const browser: ICefBrowser; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnGotFocus) then
    FOnGotFocus(PageIndex, browser, CancelEventBuiltIn);
end;

procedure TDcefBrowserEvents.doOnJsdialog(const PageIndex: Integer;
  const browser: ICefBrowser; const originUrl, acceptLang: ustring;
  dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
  const callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnJsdialog) then
    FOnJsdialog(PageIndex, browser, originUrl, acceptLang, dialogType,
      messageText, defaultPromptText, callback, CancelEventBuiltIn);
end;

procedure TDcefBrowserEvents.doOnKeyEvent(const PageIndex: Integer;
  const browser: ICefBrowser; const event: PCefKeyEvent;
  osEvent: TCefEventHandle; var Cancel: Boolean);
begin
  if Assigned(FOnKeyEvent) then
    FOnKeyEvent(PageIndex, browser, event, osEvent, Cancel);
end;

procedure TDcefBrowserEvents.doOnLoadEnd(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(PageIndex, browser, frame, httpStatusCode);
end;

procedure TDcefBrowserEvents.doOnLoadError(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
begin
  if Assigned(FOnLoadError) then
    FOnLoadError(PageIndex, browser, frame, errorCode, errorText, failedUrl);
end;

procedure TDcefBrowserEvents.doOnLoadStart(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnLoadStart) then
    FOnLoadStart(PageIndex, browser, frame);
end;

procedure TDcefBrowserEvents.doOnPreKeyEvent(const PageIndex: Integer;
  const browser: ICefBrowser; const event: PCefKeyEvent;
  osEvent: TCefEventHandle; var isKeyboardShortcut: Boolean;
  var Cancel: Boolean);
begin
  if Assigned(FOnPreKeyEvent) then
    FOnPreKeyEvent(PageIndex, browser, event, osEvent,
      isKeyboardShortcut, Cancel);
end;

procedure TDcefBrowserEvents.doOnProtocolExecution(const PageIndex: Integer;
  browser: ICefBrowser; const url: ustring; var allowOsExecution: Boolean);
begin
  if Assigned(FOnProtocolExecution) then
    FOnProtocolExecution(PageIndex, browser, url, allowOsExecution);
end;

procedure TDcefBrowserEvents.doOnQuotaRequest(const PageIndex: Integer;
  const browser: ICefBrowser; const originUrl: ustring; newSize: Int64;
  const callback: ICefQuotaCallback; out Result: Boolean);
begin
  if Assigned(FOnQuotaRequest) then
    FOnQuotaRequest(PageIndex, browser, originUrl, newSize, callback, Result);
end;

procedure TDcefBrowserEvents.doOnRequestGeolocationPermission(const PageIndex
  : Integer; const browser: ICefBrowser; const requestingUrl: ustring;
  requestId: Integer; const callback: ICefGeolocationCallback;
  out Result: Boolean);
begin
  if Assigned(FOnRequestGeolocationPermission) then
    FOnRequestGeolocationPermission(PageIndex, browser, requestingUrl,
      requestId, callback, Result);
end;

procedure TDcefBrowserEvents.doOnResourceRedirect(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame; const oldUrl: ustring;
  var newUrl: ustring);
begin
  if Assigned(FOnResourceRedirect) then
    FOnResourceRedirect(PageIndex, browser, frame, oldUrl, newUrl);
end;

procedure TDcefBrowserEvents.doOnSetFocus(const PageIndex: Integer;
  const browser: ICefBrowser; source: TCefFocusSource;
  var CancelFocus: Boolean);
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(PageIndex, browser, source, CancelFocus);
end;

procedure TDcefBrowserEvents.doOnStartDragging(const PageIndex: Integer;
  const browser: ICefBrowser; const dragData: ICefDragData;
  allowedOps: TCefDragOperations; x, y: Integer; out Result: Boolean);
begin
  if Assigned(FOnStartDragging) then
    FOnStartDragging(PageIndex, browser, dragData, allowedOps, x, y, Result);
end;

procedure TDcefBrowserEvents.doOnTakeFocus(const PageIndex: Integer;
  const browser: ICefBrowser; next: Boolean);
begin
  if Assigned(FOnTakeFocus) then
    FOnTakeFocus(PageIndex, browser, next);
end;

procedure TDcefBrowserEvents.doOnUpdateDragCursor(const PageIndex: Integer;
  const browser: ICefBrowser; operation: TCefDragOperation);
begin
  if Assigned(FOnUpdateDragCursor) then
    FOnUpdateDragCursor(PageIndex, browser, operation);
end;

end.
