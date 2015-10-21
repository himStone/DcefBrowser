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

unit DcefB.CefEvents;

interface

uses
  Classes, DcefB.Dcef3.CefLib;

type
  TOnProcessMessageReceived = procedure(Sender: TObject;
    const browser: ICefBrowser; sourceProcess: TCefProcessId;
    const message: ICefProcessMessage; out Result: Boolean) of object;

  TOnLoadingStateChange = procedure(Sender: TObject; const browser: ICefBrowser;
    isLoading, canGoBack, canGoForward: Boolean) of object;
  TOnLoadStart = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame) of object;
  TOnLoadEnd = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; httpStatusCode: Integer) of object;
  TOnLoadError = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; errorCode: Integer;
    const errorText, failedUrl: ustring) of object;

  TOnTakeFocus = procedure(Sender: TObject; const browser: ICefBrowser;
    next: Boolean) of object;
  TOnSetFocus = procedure(Sender: TObject; const browser: ICefBrowser;
    source: TCefFocusSource; out Result: Boolean) of object;
  TOnGotFocus = procedure(Sender: TObject; const browser: ICefBrowser)
    of object;

  TOnBeforeContextMenu = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; const params: ICefContextMenuParams;
    const model: ICefMenuModel) of object;
  TOnContextMenuCommand = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; const params: ICefContextMenuParams;
    commandId: Integer; eventFlags: TCefEventFlags; out Result: Boolean)
    of object;
  TOnContextMenuDismissed = procedure(Sender: TObject;
    const browser: ICefBrowser; const frame: ICefFrame) of object;

  TOnPreKeyEvent = procedure(Sender: TObject; const browser: ICefBrowser;
    const event: PCefKeyEvent; osEvent: TCefEventHandle;
    out isKeyboardShortcut: Boolean; out Result: Boolean) of object;
  TOnKeyEvent = procedure(Sender: TObject; const browser: ICefBrowser;
    const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean)
    of object;

  TOnAddressChange = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; const url: ustring) of object;
  TOnTitleChange = procedure(Sender: TObject; const browser: ICefBrowser;
    const title: ustring) of object;
  TOnTooltip = procedure(Sender: TObject; const browser: ICefBrowser;
    var text: ustring; out Result: Boolean) of object;
  TOnStatusMessage = procedure(Sender: TObject; const browser: ICefBrowser;
    const value: ustring) of object;
  TOnConsoleMessage = procedure(Sender: TObject; const browser: ICefBrowser;
    const message, source: ustring; line: Integer; out Result: Boolean)
    of object;

  TOnBeforeDownload = procedure(Sender: TObject; const browser: ICefBrowser;
    const downloadItem: ICefDownloadItem; const suggestedName: ustring;
    const callback: ICefBeforeDownloadCallback) of object;
  TOnDownloadUpdated = procedure(Sender: TObject; const browser: ICefBrowser;
    const downloadItem: ICefDownloadItem;
    const callback: ICefDownloadItemCallback) of object;

  TOnRequestGeolocationPermission = procedure(Sender: TObject;
    const browser: ICefBrowser; const requestingUrl: ustring;
    requestId: Integer; const callback: ICefGeolocationCallback;
    out Result: Boolean) of object;
  TOnCancelGeolocationPermission = procedure(Sender: TObject;
    const browser: ICefBrowser; const requestingUrl: ustring;
    requestId: Integer) of object;

  TOnJsdialog = procedure(Sender: TObject; const browser: ICefBrowser;
    const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
    const messageText, defaultPromptText: ustring;
    callback: ICefJsDialogCallback; out suppressMessage: Boolean;
    out Result: Boolean) of object;
  TOnBeforeUnloadDialog = procedure(Sender: TObject; const browser: ICefBrowser;
    const messageText: ustring; isReload: Boolean;
    const callback: ICefJsDialogCallback; out Result: Boolean) of object;
  TOnResetDialogState = procedure(Sender: TObject; const browser: ICefBrowser)
    of object;

  TOnDialogClosed = procedure(Sender: TObject; const browser: ICefBrowser)
    of object;
  TOnBeforePopup = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
    var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
    var client: ICefClient; var settings: TCefBrowserSettings;
    var noJavascriptAccess: Boolean; out Result: Boolean) of object;

  TOnAfterCreated = procedure(Sender: TObject; const browser: ICefBrowser)
    of object;
  TOnBeforeClose = procedure(Sender: TObject; const browser: ICefBrowser)
    of object;
  TOnRunModal = procedure(Sender: TObject; const browser: ICefBrowser;
    out Result: Boolean) of object;
  TOnClose = procedure(Sender: TObject; const browser: ICefBrowser;
    out Result: Boolean) of object;

  TOnBeforeBrowse = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean;
    out Result: Boolean) of object;
  TOnBeforeResourceLoad = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; const request: ICefRequest; out Result: Boolean)
    of object;
  TOnGetResourceHandler = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; const request: ICefRequest;
    out Result: ICefResourceHandler) of object;
  TOnResourceRedirect = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring)
    of object;
  TOnGetAuthCredentials = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; isProxy: Boolean; const host: ustring;
    port: Integer; const realm, scheme: ustring;
    const callback: ICefAuthCallback; out Result: Boolean) of object;
  TOnQuotaRequest = procedure(Sender: TObject; const browser: ICefBrowser;
    const originUrl: ustring; newSize: Int64; const callback: ICefQuotaCallback;
    out Result: Boolean) of object;
  TOnProtocolExecution = procedure(Sender: TObject; const browser: ICefBrowser;
    const url: ustring; out allowOsExecution: Boolean) of object;

  TOnBeforePluginLoad = procedure(Sender: TObject; const browser: ICefBrowser;
    const url, policyUrl: ustring; const info: ICefWebPluginInfo;
    out Result: Boolean) of Object;
  TOnCertificateError = procedure(Sender: TObject; certError: TCefErrorCode;
    const requestUrl: ustring;
    const callback: ICefAllowCertificateErrorCallback; out Result: Boolean)
    of Object;
  TOnPluginCrashed = procedure(Sender: TObject; const browser: ICefBrowser;
    const pluginPath: ustring) of object;
  TOnRenderProcessTerminated = procedure(Sender: TObject;
    const browser: ICefBrowser; status: TCefTerminationStatus) of object;

  TOnFileDialog = procedure(Sender: TObject; const browser: ICefBrowser;
    mode: TCefFileDialogMode; const title, defaultFileName: ustring;
    acceptTypes: TStrings; const callback: ICefFileDialogCallback;
    out Result: Boolean) of Object;

  TOnGetRootScreenRect = procedure(Sender: TObject; const browser: ICefBrowser;
    rect: PCefRect; out Result: Boolean) of Object;
  TOnGetViewRect = procedure(Sender: TObject; const browser: ICefBrowser;
    rect: PCefRect; out Result: Boolean) of Object;
  TOnGetScreenPoint = procedure(Sender: TObject; const browser: ICefBrowser;
    viewX, viewY: Integer; screenX, screenY: PInteger; out Result: Boolean)
    of Object;
  TOnGetScreenInfo = procedure(Sender: TObject; const browser: ICefBrowser;
    screenInfo: PCefScreenInfo; Result: Boolean) of Object;
  TOnPopupShow = procedure(Sender: TObject; const browser: ICefBrowser;
    show: Boolean) of Object;
  TOnPopupSize = procedure(Sender: TObject; const browser: ICefBrowser;
    const rect: PCefRect) of Object;
  TOnPaint = procedure(Sender: TObject; const browser: ICefBrowser;
    kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
    const dirtyRects: PCefRectArray; const buffer: Pointer;
    width, height: Integer) of Object;
  TOnCursorChange = procedure(Sender: TObject; const browser: ICefBrowser;
    cursor: TCefCursorHandle; cursorType: TCefCursorType;
    const customCursorInfo: PCefCursorInfo) of Object;
  TOnStartDragging = procedure(Sender: TObject; const browser: ICefBrowser;
    const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer;
    out Result: Boolean) of Object;
  TOnUpdateDragCursor = procedure(Sender: TObject; const browser: ICefBrowser;
    operation: TCefDragOperation) of Object;
  TOnScrollOffsetChanged = procedure(Sender: TObject;
    const browser: ICefBrowser) of Object;

  TOnDragEnter = procedure(Sender: TObject; const browser: ICefBrowser;
    const dragData: ICefDragData; mask: TCefDragOperations; out Result: Boolean)
    of Object;

implementation

end.
