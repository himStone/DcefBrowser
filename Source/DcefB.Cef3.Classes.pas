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

unit DcefB.Cef3.Classes;

{$IFDEF FPC}
{$MODE DELPHI}{$H+}
{$ENDIF}
{$I DcefB.Dcef3.cef.inc}

interface

uses
{$IFDEF DELPHI14_UP}
  Rtti, TypInfo, Variants,
{$ENDIF}
  SysUtils, Classes,
  DcefB.Cef3.Types, DcefB.Cef3.Api, DcefB.Cef3.Interfaces, DcefB.Cef3.Helper;

type
  TCefBaseOwn = class(TInterfacedObject, ICefBase)
  private
    FData: Pointer;
  public
    function Wrap: Pointer;
    constructor CreateData(size: Cardinal; owned: Boolean = False); virtual;
    destructor Destroy; override;
  end;

  TCefBaseRef = class(TInterfacedObject, ICefBase)
  private
    FData: Pointer;
  public
    constructor create(data: Pointer); virtual;
    destructor Destroy; override;
    function Wrap: Pointer;
    class function UnWrap(data: Pointer): ICefBase;
  end;

  TCefRunFileDialogCallbackOwn = class(TCefBaseOwn, ICefRunFileDialogCallback)
  protected
    procedure OnFileDialogDismissed(selectedAcceptFilter: Integer; filePaths: TStrings); virtual;
  public
    constructor Create;
  end;

  TCefFastRunFileDialogCallback = class(TCefRunFileDialogCallbackOwn)
  private
    FCallback: TCefRunFileDialogCallbackProc;
  protected
    procedure OnFileDialogDismissed(selectedAcceptFilter: Integer; filePaths: TStrings); override;
  public
    constructor Create(callback: TCefRunFileDialogCallbackProc); reintroduce; virtual;
  end;

  TCefBrowserHostRef = class(TCefBaseRef, ICefBrowserHost)
  protected
    function GetBrowser: ICefBrowser;
    procedure CloseBrowser(forceClose: Boolean);
    procedure SetFocus(focus: Boolean);
    procedure SetWindowVisibility(visible: Boolean);
    function GetWindowHandle: TCefWindowHandle;
    function GetOpenerWindowHandle: TCefWindowHandle;
    function GetRequestContext: ICefRequestContext;
    function GetZoomLevel: Double;
    procedure SetZoomLevel(zoomLevel: Double);
    procedure RunFileDialog(mode: TCefFileDialogMode; const title, defaultFilePath: ustring;
      acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefRunFileDialogCallback);
    procedure RunFileDialogProc(mode: TCefFileDialogMode; const title, defaultFilePath: ustring;
      acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: TCefRunFileDialogCallbackProc);
    procedure StartDownload(const url: ustring);
    procedure Print;
    procedure PrintToPdf(const path: ustring; settings: PCefPdfPrintSettings; const callback: ICefPdfPrintCallback);
    procedure PrintToPdfProc(const path: ustring; settings: PCefPdfPrintSettings; const callback: TOnPdfPrintFinishedProc);
    procedure Find(identifier: Integer; const searchText: ustring; forward, matchCase, findNext: Boolean);
    procedure StopFinding(clearSelection: Boolean);
    procedure ShowDevTools(const windowInfo: PCefWindowInfo; const client: ICefClient;
      const settings: PCefBrowserSettings; inspectElementAt: PCefPoint);
    procedure CloseDevTools;
    procedure GetNavigationEntries(const visitor: ICefNavigationEntryVisitor; currentOnly: Boolean);
    procedure GetNavigationEntriesProc(const proc: TCefNavigationEntryVisitorProc; currentOnly: Boolean);
    procedure SetMouseCursorChangeDisabled(disabled: Boolean);
    function IsMouseCursorChangeDisabled: Boolean;
    procedure ReplaceMisspelling(const word: ustring);
    procedure AddWordToDictionary(const word: ustring);
    function IsWindowRenderingDisabled: Boolean;
    procedure WasResized;
    procedure NotifyScreenInfoChanged;
    procedure WasHidden(hidden: Boolean);
    procedure Invalidate(kind: TCefPaintElementType);
    procedure SendKeyEvent(const event: PCefKeyEvent);
    procedure SendMouseClickEvent(const event: PCefMouseEvent;
      kind: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
    procedure SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
    procedure SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
    procedure SendFocusEvent(setFocus: Boolean);
    procedure SendCaptureLostEvent;
    procedure NotifyMoveOrResizeStarted;
    function GetWindowlessFrameRate(): Integer;
    procedure SetWindowlessFrameRate(frameRate: Integer);
    function GetNsTextInputContext: TCefTextInputContext;
    procedure HandleKeyEventBeforeTextInputClient(keyEvent: TCefEventHandle);
    procedure HandleKeyEventAfterTextInputClient(keyEvent: TCefEventHandle);

    procedure DragTargetDragEnter(const dragData: ICefDragData;
      const event: PCefMouseEvent; allowedOps: TCefDragOperations);
    procedure DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
    procedure DragTargetDragLeave;
    procedure DragTargetDrop(event: PCefMouseEvent);
    procedure DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
    procedure DragSourceSystemDragEnded;
  public
    class function UnWrap(data: Pointer): ICefBrowserHost;
  end;

  TCefBrowserRef = class(TCefBaseRef, ICefBrowser)
  protected
    function GetHost: ICefBrowserHost;
    function canGoBack: Boolean;
    procedure GoBack;
    function canGoForward: Boolean;
    procedure GoForward;
    function isLoading: Boolean;
    procedure reload;
    procedure ReloadIgnoreCache;
    procedure StopLoad;
    function GetIdentifier: Integer;
    function IsSame(const that: ICefBrowser): Boolean;
    function IsPopup: Boolean;
    function HasDocument: Boolean;
    function GetMainFrame: ICefFrame;
    function GetFocusedFrame: ICefFrame;
    function GetFrameByident(identifier: Int64): ICefFrame;
    function GetFrame(const name: ustring): ICefFrame;
    function GetFrameCount: NativeUInt;
    procedure GetFrameIdentifiers(count: PNativeUInt; identifiers: PInt64);
    procedure GetFrameNames(names: TStrings);
    function SendProcessMessage(targetProcess: TCefProcessId;
      message: ICefProcessMessage): Boolean;
  public
    class function UnWrap(data: Pointer): ICefBrowser;
  end;

  TCefFrameRef = class(TCefBaseRef, ICefFrame)
  protected
    function IsValid: Boolean;
    procedure undo;
    procedure redo;
    procedure cut;
    procedure copy;
    procedure paste;
    procedure del;
    procedure SelectAll;
    procedure ViewSource;
    procedure GetSource(const visitor: ICefStringVisitor);
    procedure GetSourceProc(const proc: TCefStringVisitorProc);
    procedure GetText(const visitor: ICefStringVisitor);
    procedure GetTextProc(const proc: TCefStringVisitorProc);
    procedure LoadRequest(const request: ICefRequest);
    procedure LoadUrl(const url: ustring);
    procedure LoadString(const str, url: ustring);
    procedure ExecuteJavaScript(const code, scriptUrl: ustring;
      startLine: Integer);
    function IsMain: Boolean;
    function IsFocused: Boolean;
    function GetName: ustring;
    function GetIdentifier: Int64;
    function GetParent: ICefFrame;
    function GetUrl: ustring;
    function GetBrowser: ICefBrowser;
    function GetV8Context: ICefv8Context;
    procedure VisitDom(const visitor: ICefDomVisitor);
    procedure VisitDomProc(const proc: TCefDomVisitorProc);
  public
    class function UnWrap(data: Pointer): ICefFrame;
  end;

  TCefPostDataRef = class(TCefBaseRef, ICefPostData)
  protected
    function IsReadOnly: Boolean;
    function HasExcludedElements: Boolean;
    function GetCount: NativeUInt;
    function GetElements(count: NativeUInt): IInterfaceList;
    // ICefPostDataElement
    function RemoveElement(const element: ICefPostDataElement): Integer;
    function AddElement(const element: ICefPostDataElement): Integer;
    procedure RemoveElements;
  public
    class function UnWrap(data: Pointer): ICefPostData;
    class function New: ICefPostData;
  end;

  TCefPostDataElementRef = class(TCefBaseRef, ICefPostDataElement)
  protected
    function IsReadOnly: Boolean;
    procedure SetToEmpty;
    procedure SetToFile(const fileName: ustring);
    procedure SetToBytes(size: NativeUInt; bytes: Pointer);
    function GetType: TCefPostDataElementType;
    function GetFile: ustring;
    function GetBytesCount: NativeUInt;
    function GetBytes(size: NativeUInt; bytes: Pointer): NativeUInt;
  public
    class function UnWrap(data: Pointer): ICefPostDataElement;
    class function New: ICefPostDataElement;
  end;

  TCefRequestRef = class(TCefBaseRef, ICefRequest)
  protected
    function IsReadOnly: Boolean;
    function GetUrl: ustring;
    function GetMethod: ustring;
    function GetPostData: ICefPostData;
    procedure GetHeaderMap(const headerMap: ICefStringMultimap);
    procedure SetUrl(const value: ustring);
    procedure SetMethod(const value: ustring);
    procedure SetReferrer(const referrerUrl: string; policy: TCefReferrerPolicy);
    function GetReferrerUrl: string;
    function GetReferrerPolicy: TCefReferrerPolicy;
    procedure SetPostData(const value: ICefPostData);
    procedure SetHeaderMap(const headerMap: ICefStringMultimap);
    function GetFlags: TCefUrlRequestFlags;
    procedure SetFlags(flags: TCefUrlRequestFlags);
    function GetFirstPartyForCookies: ustring;
    procedure SetFirstPartyForCookies(const url: ustring);
    procedure Assign(const url, method: ustring; const postData: ICefPostData;
      const headerMap: ICefStringMultimap);
    function GetResourceType: TCefResourceType;
    function GetTransitionType: TCefTransitionType;
    function GetIdentifier: UInt64;
  public
    class function UnWrap(data: Pointer): ICefRequest;
    class function New: ICefRequest;
  end;

  TCefStreamReaderRef = class(TCefBaseRef, ICefStreamReader)
  protected
    function read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function seek(offset: Int64; whence: Integer): Integer;
    function tell: Int64;
    function eof: Boolean;
    function MayBlock: Boolean;
  public
    class function UnWrap(data: Pointer): ICefStreamReader;
    class function CreateForFile(const fileName: ustring): ICefStreamReader;
    class function CreateForCustomStream(const stream: ICefCustomStreamReader)
      : ICefStreamReader;
    class function CreateForStream(const stream: TSTream; owned: Boolean)
      : ICefStreamReader;
    class function CreateForData(data: Pointer; size: NativeUInt)
      : ICefStreamReader;
  end;

  TCefWriteHandlerOwn = class(TCefBaseOwn, ICefWriteHandler)
  protected
    function write(const ptr: Pointer; size, n: NativeUInt)
      : NativeUInt; virtual;
    function seek(offset: Int64; whence: Integer): Integer; virtual;
    function tell: Int64; virtual;
    function flush: Integer; virtual;
    function MayBlock: Boolean; virtual;
  public
    constructor create; virtual;
  end;

  TCefStreamWriterRef = class(TCefBaseRef, ICefStreamWriter)
  protected
    function write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function seek(offset: Int64; whence: Integer): Integer;
    function tell: Int64;
    function flush: Integer;
    function MayBlock: Boolean;
  public
    class function UnWrap(data: Pointer): ICefStreamWriter;
    class function CreateForFile(const fileName: ustring): ICefStreamWriter;
    class function CreateForHandler(const handler: ICefWriteHandler)
      : ICefStreamWriter;
  end;

  TCefV8AccessorGetterProc = {$IFDEF DELPHI12_UP} reference to
{$ENDIF} function(const name: ustring; const obj: ICefv8Value;
    out value: ICefv8Value; const exception: ustring): Boolean;

  TCefV8AccessorSetterProc = {$IFDEF DELPHI12_UP}reference to
{$ENDIF} function(const name: ustring; const obj, value: ICefv8Value;
    const exception: ustring): Boolean;

  TCefv8ValueRef = class(TCefBaseRef, ICefv8Value)
  protected
    function IsValid: Boolean;
    function IsUndefined: Boolean;
    function IsNull: Boolean;
    function IsBool: Boolean;
    function IsInt: Boolean;
    function IsUInt: Boolean;
    function IsDouble: Boolean;
    function IsDate: Boolean;
    function IsString: Boolean;
    function IsObject: Boolean;
    function IsArray: Boolean;
    function IsFunction: Boolean;
    function IsSame(const that: ICefv8Value): Boolean;
    function GetBoolValue: Boolean;
    function GetIntValue: Integer;
    function GetUIntValue: Cardinal;
    function GetDoubleValue: Double;
    function GetDateValue: TDateTime;
    function GetStringValue: ustring;
    function IsUserCreated: Boolean;
    function HasException: Boolean;
    function GetException: ICefV8Exception;
    function ClearException: Boolean;
    function WillRethrowExceptions: Boolean;
    function SetRethrowExceptions(rethrow: Boolean): Boolean;
    function HasValueByKey(const key: ustring): Boolean;
    function HasValueByIndex(index: Integer): Boolean;
    function DeleteValueByKey(const key: ustring): Boolean;
    function DeleteValueByIndex(index: Integer): Boolean;
    function GetValueByKey(const key: ustring): ICefv8Value;
    function GetValueByIndex(index: Integer): ICefv8Value;
    function SetValueByKey(const key: ustring; const value: ICefv8Value;
      attribute: TCefV8PropertyAttributes): Boolean;
    function SetValueByIndex(index: Integer; const value: ICefv8Value): Boolean;
    function SetValueByAccessor(const key: ustring;
      settings: TCefV8AccessControls;
      attribute: TCefV8PropertyAttributes): Boolean;
    function GetKeys(const keys: TStrings): Integer;
    function SetUserData(const data: ICefv8Value): Boolean;
    function GetUserData: ICefv8Value;
    function GetExternallyAllocatedMemory: Integer;
    function AdjustExternallyAllocatedMemory(changeInBytes: Integer): Integer;
    function GetArrayLength: Integer;
    function GetFunctionName: ustring;
    function GetFunctionHandler: ICefv8Handler;
    function ExecuteFunction(const obj: ICefv8Value;
      const arguments: TCefv8ValueArray): ICefv8Value;
    function ExecuteFunctionWithContext(const context: ICefv8Context;
      const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;
  public
    class function UnWrap(data: Pointer): ICefv8Value;
    class function NewUndefined: ICefv8Value;
    class function NewNull: ICefv8Value;
    class function NewBool(value: Boolean): ICefv8Value;
    class function NewInt(value: Integer): ICefv8Value;
    class function NewUInt(value: Cardinal): ICefv8Value;
    class function NewDouble(value: Double): ICefv8Value;
    class function NewDate(value: TDateTime): ICefv8Value;
    class function NewString(const str: ustring): ICefv8Value;
    class function NewObject(const Accessor: ICefV8Accessor): ICefv8Value;
    class function NewObjectProc(const getter: TCefV8AccessorGetterProc;
      const setter: TCefV8AccessorSetterProc): ICefv8Value;
    class function NewArray(len: Integer): ICefv8Value;
    class function NewFunction(const name: ustring;
      const handler: ICefv8Handler): ICefv8Value;
  end;

  TCefv8ContextRef = class(TCefBaseRef, ICefv8Context)
  protected
    function GetTaskRunner: ICefTaskRunner;
    function IsValid: Boolean;
    function GetBrowser: ICefBrowser;
    function GetFrame: ICefFrame;
    function GetGlobal: ICefv8Value;
    function enter: Boolean;
    function exit: Boolean;
    function IsSame(const that: ICefv8Context): Boolean;
    function eval(const code: ustring; var retval: ICefv8Value;
      var exception: ICefV8Exception): Boolean;
  public
    class function UnWrap(data: Pointer): ICefv8Context;
    class function current: ICefv8Context;
    class function Entered: ICefv8Context;
  end;

  TCefV8StackFrameRef = class(TCefBaseRef, ICefV8StackFrame)
  protected
    function IsValid: Boolean;
    function GetScriptName: ustring;
    function GetScriptNameOrSourceUrl: ustring;
    function GetFunctionName: ustring;
    function GetLineNumber: Integer;
    function GetColumn: Integer;
    function IsEval: Boolean;
    function IsConstructor: Boolean;
  public
    class function UnWrap(data: Pointer): ICefV8StackFrame;
  end;

  TCefV8StackTraceRef = class(TCefBaseRef, ICefV8StackTrace)
  protected
    function IsValid: Boolean;
    function GetFrameCount: Integer;
    function GetFrame(index: Integer): ICefV8StackFrame;
  public
    class function UnWrap(data: Pointer): ICefV8StackTrace;
    class function current(frameLimit: Integer): ICefV8StackTrace;
  end;

  TCefv8HandlerRef = class(TCefBaseRef, ICefv8Handler)
  protected
    function execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean;
  public
    class function UnWrap(data: Pointer): ICefv8Handler;
  end;

  TCefClientOwn = class(TCefBaseOwn, ICefClient)
  protected
    function GetContextMenuHandler: ICefContextMenuHandler; virtual;
    function GetDialogHandler: ICefDialogHandler; virtual;
    function GetDisplayHandler: ICefDisplayHandler; virtual;
    function GetDownloadHandler: ICefDownloadHandler; virtual;
    function GetDragHandler: ICefDragHandler; virtual;
    function GetFindHandler: ICefFindHandler; virtual;
    function GetFocusHandler: ICefFocusHandler; virtual;
    function GetGeolocationHandler: ICefGeolocationHandler; virtual;
    function GetJsdialogHandler: ICefJsDialogHandler; virtual;
    function GetKeyboardHandler: ICefKeyboardHandler; virtual;
    function GetLifeSpanHandler: ICefLifeSpanHandler; virtual;
    function GetRenderHandler: ICefRenderHandler; virtual;
    function GetLoadHandler: ICefLoadHandler; virtual;
    function GetRequestHandler: ICefRequestHandler; virtual;
    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage)
      : Boolean; virtual;
  public
    constructor create; virtual;
  end;

  TCefGeolocationHandlerOwn = class(TCefBaseOwn, ICefGeolocationHandler)
  protected
    function OnRequestGeolocationPermission(const browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer;
      const callback: ICefGeolocationCallback): Boolean; virtual;
    procedure OnCancelGeolocationPermission(const browser: ICefBrowser;
      requestId: Integer); virtual;
  public
    constructor create; virtual;
  end;

  TCefLifeSpanHandlerOwn = class(TCefBaseOwn, ICefLifeSpanHandler)
  protected
    function OnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame;
      const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean; var popupFeatures: TCefPopupFeatures;
      var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean): Boolean; virtual;
    procedure OnAfterCreated(const browser: ICefBrowser); virtual;
    procedure OnBeforeClose(const browser: ICefBrowser); virtual;
    function RunModal(const browser: ICefBrowser): Boolean; virtual;
    function DoClose(const browser: ICefBrowser): Boolean; virtual;
  public
    constructor create; virtual;
  end;

  TCefLoadHandlerOwn = class(TCefBaseOwn, ICefLoadHandler)
  protected
    procedure OnLoadingStateChange(const browser: ICefBrowser;
      isLoading, canGoBack, canGoForward: Boolean); virtual;
    procedure OnLoadStart(const browser: ICefBrowser;
      const frame: ICefFrame); virtual;
    procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame;
      httpStatusCode: Integer); virtual;
    procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame;
      errorCode: Integer; const errorText, failedUrl: ustring); virtual;
  public
    constructor create; virtual;
  end;

  TCefRequestCallbackRef = class(TCefBaseRef, ICefRequestCallback)
  protected
    procedure Cont(allow: Boolean);
    procedure Cancel;
  public
     class function UnWrap(data: Pointer): ICefRequestCallback;
  end;

  TCefRequestHandlerOwn = class(TCefBaseOwn, ICefRequestHandler)
  protected
    function OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; isRedirect: Boolean): Boolean; virtual;
    function OnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame;
      const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean): Boolean; virtual;
    function OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue; virtual;
    function GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest): ICefResourceHandler; virtual;
    procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; var newUrl: ustring); virtual;
    function OnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse): Boolean; virtual;
    function GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse): ICefResponseFilter; virtual;
    procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus;
      receivedContentLength: Int64); virtual;
    function GetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame;
      isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback): Boolean; virtual;
    function OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring;
      newSize: Int64; const callback: ICefRequestCallback): Boolean; virtual;
    function GetCookieManager(const browser: ICefBrowser; const mainUrl: ustring): ICefCookieManager; virtual;
    procedure OnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean); virtual;
    function OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode;
      const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean; virtual;
    procedure OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring); virtual;
    procedure OnRenderViewReady(const browser: ICefBrowser); virtual;
    procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); virtual;
  public
    constructor Create; virtual;
  end;

  TCefDisplayHandlerOwn = class(TCefBaseOwn, ICefDisplayHandler)
  protected
    procedure OnAddressChange(const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring); virtual;
    procedure OnTitleChange(const browser: ICefBrowser;
      const title: ustring); virtual;
    procedure OnFaviconUrlChange(const browser: ICefBrowser; iconUrls: TStrings); virtual;
    procedure OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean); virtual;
    function OnTooltip(const browser: ICefBrowser; var text: ustring)
      : Boolean; virtual;
    procedure OnStatusMessage(const browser: ICefBrowser;
      const value: ustring); virtual;
    function OnConsoleMessage(const browser: ICefBrowser;
      const message, source: ustring; line: Integer): Boolean; virtual;
  public
    constructor create; virtual;
  end;

  TCefFocusHandlerOwn = class(TCefBaseOwn, ICefFocusHandler)
  protected
    procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); virtual;
    function OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource)
      : Boolean; virtual;
    procedure OnGotFocus(const browser: ICefBrowser); virtual;
  public
    constructor create; virtual;
  end;

  TCefKeyboardHandlerOwn = class(TCefBaseOwn, ICefKeyboardHandler)
  protected
    function OnPreKeyEvent(const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle;
      out isKeyboardShortcut: Boolean): Boolean; virtual;
    function OnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle): Boolean; virtual;
  public
    constructor create; virtual;
  end;

  TCefJsDialogHandlerOwn = class(TCefBaseOwn, ICefJsDialogHandler)
  protected
    function OnJsdialog(const browser: ICefBrowser;
      const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
      const messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean)
      : Boolean; virtual;
    function OnBeforeUnloadDialog(const browser: ICefBrowser;
      const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback): Boolean; virtual;
    procedure OnResetDialogState(const browser: ICefBrowser); virtual;
    procedure OnDialogClosed(const browser: ICefBrowser); virtual;
  public
    constructor create; virtual;
  end;

  TCefContextMenuHandlerOwn = class(TCefBaseOwn, ICefContextMenuHandler)
  protected
    procedure OnBeforeContextMenu(const browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      const model: ICefMenuModel); virtual;
    function RunContextMenu(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel;
      const callback: ICefRunContextMenuCallback): Boolean; virtual;
    function OnContextMenuCommand(const browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      commandId: Integer; eventFlags: TCefEventFlags): Boolean; virtual;
    procedure OnContextMenuDismissed(const browser: ICefBrowser;
      const frame: ICefFrame); virtual;
  public
    constructor create; virtual;
  end;

  TCefDialogHandlerOwn = class(TCefBaseOwn, ICefDialogHandler)
  protected
    function OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title, defaultFilePath: ustring; acceptFilters: TStrings;
      selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean; virtual;
  public
    constructor create; virtual;
  end;

  TCefDownloadHandlerOwn = class(TCefBaseOwn, ICefDownloadHandler)
  protected
    procedure OnBeforeDownload(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback); virtual;
    procedure OnDownloadUpdated(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback); virtual;
  public
    constructor create; virtual;
  end;

  TCefCustomStreamReader = class(TCefBaseOwn, ICefCustomStreamReader)
  private
    FStream: TSTream;
    FOwned: Boolean;
  protected
    function read(ptr: Pointer; size, n: NativeUInt): NativeUInt; virtual;
    function seek(offset: Int64; whence: Integer): Integer; virtual;
    function tell: Int64; virtual;
    function eof: Boolean; virtual;
    function MayBlock: Boolean; virtual;
  public
    constructor create(stream: TSTream; owned: Boolean); overload; virtual;
    constructor create(const fileName: string); overload; virtual;
    destructor Destroy; override;
  end;

  TCefPostDataElementOwn = class(TCefBaseOwn, ICefPostDataElement)
  private
    FDataType: TCefPostDataElementType;
    FValueByte: Pointer;
    FValueStr: TCefString;
    FSize: NativeUInt;
    FReadOnly: Boolean;
    procedure clear;
  protected
    function IsReadOnly: Boolean; virtual;
    procedure SetToEmpty; virtual;
    procedure SetToFile(const fileName: ustring); virtual;
    procedure SetToBytes(size: NativeUInt; bytes: Pointer); virtual;
    function GetType: TCefPostDataElementType; virtual;
    function GetFile: ustring; virtual;
    function GetBytesCount: NativeUInt; virtual;
    function GetBytes(size: NativeUInt; bytes: Pointer): NativeUInt; virtual;
  public
    constructor create(readonly: Boolean); virtual;
  end;

  TCefCallbackRef = class(TCefBaseRef, ICefCallback)
  protected
    procedure cont;
    procedure cancel;
  public
    class function UnWrap(data: Pointer): ICefCallback;
  end;

  TCefCompletionCallbackOwn = class(TCefBaseOwn, ICefCompletionCallback)
  protected
    procedure OnComplete; virtual;
  public
    constructor create; virtual;
  end;

  TCefFastCompletionCallback = class(TCefCompletionCallbackOwn)
  private
    FProc: TCefCompletionCallbackProc;
  protected
    procedure OnComplete; override;
  public
    constructor create(const proc: TCefCompletionCallbackProc); reintroduce;
  end;

  TCefResourceHandlerOwn = class(TCefBaseOwn, ICefResourceHandler)
  protected
    function ProcessRequest(const request: ICefRequest;
      const callback: ICefCallback): Boolean; virtual;
    procedure GetResponseHeaders(const response: ICefResponse;
      out responseLength: Int64; out redirectUrl: ustring); virtual;
    function ReadResponse(const dataOut: Pointer; bytesToRead: Integer;
      var bytesRead: Integer; const callback: ICefCallback): Boolean; virtual;
    function CanGetCookie(const cookie: PCefCookie): Boolean; virtual;
    function CanSetCookie(const cookie: PCefCookie): Boolean; virtual;
    procedure cancel; virtual;
  public
    constructor create(const browser: ICefBrowser; const frame: ICefFrame;
      const schemeName: ustring; const request: ICefRequest); virtual;
  end;

  TCefResourceHandlerClass = class of TCefResourceHandlerOwn;

  TCefSchemeHandlerFactoryOwn = class(TCefBaseOwn, ICefSchemeHandlerFactory)
  private
    FClass: TCefResourceHandlerClass;
  protected
    function New(const browser: ICefBrowser; const frame: ICefFrame;
      const schemeName: ustring; const request: ICefRequest)
      : ICefResourceHandler; virtual;
  public
    constructor create(const AClass: TCefResourceHandlerClass); virtual;
  end;

  TCefv8HandlerOwn = class(TCefBaseOwn, ICefv8Handler)
  protected
    function execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean; virtual;
  public
    constructor create; virtual;
  end;

  TCefTaskOwn = class(TCefBaseOwn, ICefTask)
  protected
    procedure execute; virtual;
  public
    constructor create; virtual;
  end;

  TCefTaskRef = class(TCefBaseRef, ICefTask)
  protected
    procedure execute; virtual;
  public
    class function UnWrap(data: Pointer): ICefTask;
  end;

  TCefTaskRunnerRef = class(TCefBaseRef, ICefTaskRunner)
  protected
    function IsSame(const that: ICefTaskRunner): Boolean;
    function BelongsToCurrentThread: Boolean;
    function BelongsToThread(threadId: TCefThreadId): Boolean;
    function PostTask(const task: ICefTask): Boolean; stdcall;
    function PostDelayedTask(const task: ICefTask; delayMs: Int64): Boolean;
  public
    class function UnWrap(data: Pointer): ICefTaskRunner;
    class function GetForCurrentThread: ICefTaskRunner;
    class function GetForThread(threadId: TCefThreadId): ICefTaskRunner;
  end;

  TCefStringMapOwn = class(TInterfacedObject, ICefStringMap)
  private
    FStringMap: TCefStringMap;
  protected
    function GetHandle: TCefStringMap; virtual;
    function GetSize: Integer; virtual;
    function find(const key: ustring): ustring; virtual;
    function GetKey(index: Integer): ustring; virtual;
    function GetValue(index: Integer): ustring; virtual;
    procedure Append(const key, value: ustring); virtual;
    procedure clear; virtual;
  public
    constructor create; virtual;
    destructor Destroy; override;
  end;

  TCefStringMultimapOwn = class(TInterfacedObject, ICefStringMultimap)
  private
    FStringMap: TCefStringMultimap;
  protected
    function GetHandle: TCefStringMultimap; virtual;
    function GetSize: Integer; virtual;
    function FindCount(const key: ustring): Integer; virtual;
    function GetEnumerate(const key: ustring; ValueIndex: Integer)
      : ustring; virtual;
    function GetKey(index: Integer): ustring; virtual;
    function GetValue(index: Integer): ustring; virtual;
    procedure Append(const key, value: ustring); virtual;
    procedure clear; virtual;
  public
    constructor create; virtual;
    destructor Destroy; override;
  end;

  TCefXmlReaderRef = class(TCefBaseRef, ICefXmlReader)
  protected
    function MoveToNextNode: Boolean;
    function close: Boolean;
    function HasError: Boolean;
    function GetError: ustring;
    function GetType: TCefXmlNodeType;
    function GetDepth: Integer;
    function GetLocalName: ustring;
    function GetPrefix: ustring;
    function GetQualifiedName: ustring;
    function GetNamespaceUri: ustring;
    function GetBaseUri: ustring;
    function GetXmlLang: ustring;
    function IsEmptyElement: Boolean;
    function HasValue: Boolean;
    function GetValue: ustring;
    function HasAttributes: Boolean;
    function GetAttributeCount: NativeUInt;
    function GetAttributeByIndex(index: Integer): ustring;
    function GetAttributeByQName(const qualifiedName: ustring): ustring;
    function GetAttributeByLName(const localName, namespaceURI
      : ustring): ustring;
    function GetInnerXml: ustring;
    function GetOuterXml: ustring;
    function GetLineNumber: Integer;
    function MoveToAttributeByIndex(index: Integer): Boolean;
    function MoveToAttributeByQName(const qualifiedName: ustring): Boolean;
    function MoveToAttributeByLName(const localName, namespaceURI
      : ustring): Boolean;
    function MoveToFirstAttribute: Boolean;
    function MoveToNextAttribute: Boolean;
    function MoveToCarryingElement: Boolean;
  public
    class function UnWrap(data: Pointer): ICefXmlReader;
    class function New(const stream: ICefStreamReader;
      encodingType: TCefXmlEncodingType; const URI: ustring): ICefXmlReader;
  end;

  TCefZipReaderRef = class(TCefBaseRef, ICefZipReader)
  protected
    function MoveToFirstFile: Boolean;
    function MoveToNextFile: Boolean;
    function MoveToFile(const fileName: ustring;
      caseSensitive: Boolean): Boolean;
    function close: Boolean;
    function GetFileName: ustring;
    function GetFileSize: Int64;
    function GetFileLastModified: TCefTime;
    function OpenFile(const password: ustring): Boolean;
    function CloseFile: Boolean;
    function ReadFile(buffer: Pointer; bufferSize: NativeUInt): Integer;
    function tell: Int64;
    function eof: Boolean;
  public
    class function UnWrap(data: Pointer): ICefZipReader;
    class function New(const stream: ICefStreamReader): ICefZipReader;
  end;

  TCefDomVisitorOwn = class(TCefBaseOwn, ICefDomVisitor)
  protected
    procedure visit(const document: ICefDomDocument); virtual;
  public
    constructor create; virtual;
  end;

  TCefFastDomVisitor = class(TCefDomVisitorOwn)
  private
    FProc: TCefDomVisitorProc;
  protected
    procedure visit(const document: ICefDomDocument); override;
  public
    constructor create(const proc: TCefDomVisitorProc); reintroduce; virtual;
  end;

  TCefDomDocumentRef = class(TCefBaseRef, ICefDomDocument)
  protected
    function GetType: TCefDomDocumentType;
    function GetDocument: ICefDomNode;
    function GetBody: ICefDomNode;
    function GetHead: ICefDomNode;
    function GetTitle: ustring;
    function GetElementById(const id: ustring): ICefDomNode;
    function GetFocusedNode: ICefDomNode;
    function HasSelection: Boolean;
    function GetSelectionStartOffset: Integer;
    function GetSelectionEndOffset: Integer;
    function GetSelectionAsMarkup: ustring;
    function GetSelectionAsText: ustring;
    function GetBaseUrl: ustring;
    function GetCompleteUrl(const partialURL: ustring): ustring;
  public
    class function UnWrap(data: Pointer): ICefDomDocument;
  end;

  TCefDomNodeRef = class(TCefBaseRef, ICefDomNode)
  protected
    function GetType: TCefDomNodeType;
    function IsText: Boolean;
    function IsElement: Boolean;
    function IsEditable: Boolean;
    function IsFormControlElement: Boolean;
    function GetFormControlElementType: ustring;
    function IsSame(const that: ICefDomNode): Boolean;
    function GetName: ustring;
    function GetValue: ustring;
    function SetValue(const value: ustring): Boolean;
    function GetAsMarkup: ustring;
    function GetDocument: ICefDomDocument;
    function GetParent: ICefDomNode;
    function GetPreviousSibling: ICefDomNode;
    function GetNextSibling: ICefDomNode;
    function HasChildren: Boolean;
    function GetFirstChild: ICefDomNode;
    function GetLastChild: ICefDomNode;
    function GetElementTagName: ustring;
    function HasElementAttributes: Boolean;
    function HasElementAttribute(const attrName: ustring): Boolean;
    function GetElementAttribute(const attrName: ustring): ustring;
    procedure GetElementAttributes(const attrMap: ICefStringMap);
    function SetElementAttribute(const attrName, value: ustring): Boolean;
    function GetElementInnerText: ustring;
  public
    class function UnWrap(data: Pointer): ICefDomNode;
  end;

  TCefResponseRef = class(TCefBaseRef, ICefResponse)
  protected
    function IsReadOnly: Boolean;
    function GetStatus: Integer;
    procedure SetStatus(status: Integer);
    function GetStatusText: ustring;
    procedure SetStatusText(const statusText: ustring);
    function GetMimeType: ustring;
    procedure SetMimeType(const mimeType: ustring);
    function GetHeader(const name: ustring): ustring;
    procedure GetHeaderMap(const headerMap: ICefStringMultimap);
    procedure SetHeaderMap(const headerMap: ICefStringMultimap);
  public
    class function UnWrap(data: Pointer): ICefResponse;
    class function New: ICefResponse;
  end;

  TCefFastTaskProc = {$IFDEF DELPHI12_UP}reference to {$ENDIF} procedure;

  TCefFastTask = class(TCefTaskOwn)
  private
    FMethod: TCefFastTaskProc;
  protected
    procedure execute; override;
  public
    class procedure New(threadId: TCefThreadId; const method: TCefFastTaskProc);
    class procedure NewDelayed(threadId: TCefThreadId; Delay: Int64;
      const method: TCefFastTaskProc);
    constructor create(const method: TCefFastTaskProc); reintroduce;
  end;

  TCefV8AccessorOwn = class(TCefBaseOwn, ICefV8Accessor)
  protected
    function get(const name: ustring; const obj: ICefv8Value;
      out value: ICefv8Value; const exception: ustring): Boolean; virtual;
    function put(const name: ustring; const obj, value: ICefv8Value;
      const exception: ustring): Boolean; virtual;
  public
    constructor create; virtual;
  end;

  TCefFastV8Accessor = class(TCefV8AccessorOwn)
  private
    FGetter: TCefV8AccessorGetterProc;
    FSetter: TCefV8AccessorSetterProc;
  protected
    function get(const name: ustring; const obj: ICefv8Value;
      out value: ICefv8Value; const exception: ustring): Boolean; override;
    function put(const name: ustring; const obj, value: ICefv8Value;
      const exception: ustring): Boolean; override;
  public
    constructor create(const getter: TCefV8AccessorGetterProc;
      const setter: TCefV8AccessorSetterProc); reintroduce;
  end;

  TCefCookieVisitorOwn = class(TCefBaseOwn, ICefCookieVisitor)
  protected
    function visit(const name, value, domain, path: ustring;
      secure, httponly, hasExpires: Boolean; const creation, lastAccess,
      expires: TDateTime; count, total: Integer; out deleteCookie: Boolean)
      : Boolean; virtual;
  public
    constructor create; virtual;
  end;

  TCefFastCookieVisitor = class(TCefCookieVisitorOwn)
  private
    FVisitor: TCefCookieVisitorProc;
  protected
    function visit(const name, value, domain, path: ustring;
      secure, httponly, hasExpires: Boolean; const creation, lastAccess,
      expires: TDateTime; count, total: Integer; out deleteCookie: Boolean)
      : Boolean; override;
  public
    constructor create(const visitor: TCefCookieVisitorProc); reintroduce;
  end;

  TCefV8ExceptionRef = class(TCefBaseRef, ICefV8Exception)
  protected
    function GetMessage: ustring;
    function GetSourceLine: ustring;
    function GetScriptResourceName: ustring;
    function GetLineNumber: Integer;
    function GetStartPosition: Integer;
    function GetEndPosition: Integer;
    function GetStartColumn: Integer;
    function GetEndColumn: Integer;
  public
    class function UnWrap(data: Pointer): ICefV8Exception;
  end;

  TCefResourceBundleHandlerOwn = class(TCefBaseOwn, ICefResourceBundleHandler)
  protected
    function GetDataResource(stringId: Integer; out data: Pointer;
      out dataSize: NativeUInt): Boolean; virtual; abstract;
    function GetLocalizedString(messageId: Integer; out stringVal: ustring)
      : Boolean; virtual; abstract;
    function GetDataResourceForScale(resourceId: Integer;
      scaleFactor: TCefScaleFactor; out data: Pointer;
      dataSize: NativeUInt): Boolean; virtual; abstract;
  public
    constructor create; virtual;
  end;

  TCefFastResourceBundle = class(TCefResourceBundleHandlerOwn)
  private
    FGetDataResource: TGetDataResource;
    FGetLocalizedString: TGetLocalizedString;
    FGetDataResourceForScale: TGetDataResourceForScale;
  protected
    function GetDataResource(resourceId: Integer; out data: Pointer;
      out dataSize: NativeUInt): Boolean; override;
    function GetLocalizedString(stringId: Integer;
      out stringVal: ustring): Boolean; override;
    function GetDataResourceForScale(resourceId: Integer;
      scaleFactor: TCefScaleFactor; out data: Pointer;
      dataSize: NativeUInt): Boolean; override;
  public
    constructor Create(
      AGetDataResource: TGetDataResource;
      AGetLocalizedString: TGetLocalizedString;
      AGetDataResourceForScale: TGetDataResourceForScale); reintroduce;
  end;

  TCefAppOwn = class(TCefBaseOwn, ICefApp)
  protected
    procedure OnBeforeCommandLineProcessing(const processType: ustring;
      const commandLine: ICefCommandLine); virtual; abstract;
    procedure OnRegisterCustomSchemes(const registrar: ICefSchemeRegistrar);
      virtual; abstract;
    function GetResourceBundleHandler: ICefResourceBundleHandler;
      virtual; abstract;
    function GetBrowserProcessHandler: ICefBrowserProcessHandler;
      virtual; abstract;
    function GetRenderProcessHandler: ICefRenderProcessHandler;
      virtual; abstract;
  public
    constructor create; virtual;
  end;

  TCefSetCookieCallbackOwn = class(TCefBaseOwn, ICefSetCookieCallback)
  protected
    procedure OnComplete(success: Boolean); virtual; abstract;
  public
    constructor Create; virtual;
  end;

  TCefFastSetCookieCallback = class(TCefSetCookieCallbackOwn)
  private
    FCallback: TCefSetCookieCallbackProc;
  protected
    procedure OnComplete(success: Boolean); override;
  public
    constructor Create(const callback: TCefSetCookieCallbackProc); reintroduce;
  end;

  TCefDeleteCookiesCallbackOwn = class(TCefBaseOwn, ICefDeleteCookiesCallback)
  protected
    procedure OnComplete(numDeleted: Integer); virtual; abstract;
  public
    constructor Create; virtual;
  end;

  TCefFastDeleteCookiesCallback = class(TCefDeleteCookiesCallbackOwn)
  private
    FCallback: TCefDeleteCookiesCallbackProc;
  protected
    procedure OnComplete(numDeleted: Integer); override;
  public
    constructor Create(const callback: TCefDeleteCookiesCallbackProc); reintroduce;
  end;

  TCefCookieManagerRef = class(TCefBaseRef, ICefCookieManager)
  protected
    procedure SetSupportedSchemes(schemes: TStrings; const callback: ICefCompletionCallback);
    procedure SetSupportedSchemesProc(schemes: TStrings; const callback: TCefCompletionCallbackProc);
    function VisitAllCookies(const visitor: ICefCookieVisitor): Boolean;
    function VisitAllCookiesProc(const visitor: TCefCookieVisitorProc): Boolean;
    function VisitUrlCookies(const url: ustring;
      includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean;
    function VisitUrlCookiesProc(const url: ustring;
      includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean;
    function SetCookie(const url: ustring; const name, value, domain, path: ustring; secure, httponly,
      hasExpires: Boolean; const creation, lastAccess, expires: TDateTime;
      const callback: ICefSetCookieCallback): Boolean;
    function SetCookieProc(const url: ustring; const name, value, domain, path: ustring; secure, httponly,
      hasExpires: Boolean; const creation, lastAccess, expires: TDateTime;
      const callback: TCefSetCookieCallbackProc): Boolean;
    function DeleteCookies(const url, cookieName: ustring; const callback: ICefDeleteCookiesCallback): Boolean;
    function DeleteCookiesProc(const url, cookieName: ustring; const callback: TCefDeleteCookiesCallbackProc): Boolean;
    function SetStoragePath(const path: ustring; persistSessionCookies: Boolean; const callback: ICefCompletionCallback): Boolean;
    function SetStoragePathProc(const path: ustring; persistSessionCookies: Boolean; const callback: TCefCompletionCallbackProc): Boolean;
    function FlushStore(const handler: ICefCompletionCallback): Boolean;
    function FlushStoreProc(const proc: TCefCompletionCallbackProc): Boolean;
  public
    class function UnWrap(data: Pointer): ICefCookieManager;
    class function Global(const callback: ICefCompletionCallback): ICefCookieManager;
    class function GlobalProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
    class function New(const path: ustring; persistSessionCookies: Boolean;
      const callback: ICefCompletionCallback): ICefCookieManager;
    class function NewProc(const path: ustring; persistSessionCookies: Boolean;
      const callback: TCefCompletionCallbackProc): ICefCookieManager;
  end;

  TCefWebPluginInfoRef = class(TCefBaseRef, ICefWebPluginInfo)
  protected
    function GetName: ustring;
    function GetPath: ustring;
    function GetVersion: ustring;
    function GetDescription: ustring;
  public
    class function UnWrap(data: Pointer): ICefWebPluginInfo;
  end;

  TCefProcessMessageRef = class(TCefBaseRef, ICefProcessMessage)
  protected
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function copy: ICefProcessMessage;
    function GetName: ustring;
    function GetArgumentList: ICefListValue;
  public
    class function UnWrap(data: Pointer): ICefProcessMessage;
    class function New(const name: ustring): ICefProcessMessage;
  end;

  TCefStringVisitorOwn = class(TCefBaseOwn, ICefStringVisitor)
  protected
    procedure visit(const str: ustring); virtual;
  public
    constructor create; virtual;
  end;

  TCefFastStringVisitor = class(TCefStringVisitorOwn, ICefStringVisitor)
  private
    FVisit: TCefStringVisitorProc;
  protected
    procedure visit(const str: ustring); override;
  public
    constructor create(const callback: TCefStringVisitorProc); reintroduce;
  end;

  TCefDownLoadItemRef = class(TCefBaseRef, ICefDownloadItem)
  protected
    function IsValid: Boolean;
    function IsInProgress: Boolean;
    function IsComplete: Boolean;
    function IsCanceled: Boolean;
    function GetCurrentSpeed: Int64;
    function GetPercentComplete: Integer;
    function GetTotalBytes: Int64;
    function GetReceivedBytes: Int64;
    function GetStartTime: TDateTime;
    function GetEndTime: TDateTime;
    function GetFullPath: ustring;
    function GetId: Cardinal;
    function GetUrl: ustring;
    function GetOriginalUrl: ustring;
    function GetSuggestedFileName: ustring;
    function GetContentDisposition: ustring;
    function GetMimeType: ustring;
  public
    class function UnWrap(data: Pointer): ICefDownloadItem;
  end;

  TCefBeforeDownloadCallbackRef = class(TCefBaseRef, ICefBeforeDownloadCallback)
  protected
    procedure cont(const downloadPath: ustring; showDialog: Boolean);
  public
    class function UnWrap(data: Pointer): ICefBeforeDownloadCallback;
  end;

  TCefDownloadItemCallbackRef = class(TCefBaseRef, ICefDownloadItemCallback)
  protected
    procedure Cancel;
    procedure Pause;
    procedure Resume;
  public
    class function UnWrap(data: Pointer): ICefDownloadItemCallback;
  end;

  TCefAuthCallbackRef = class(TCefBaseRef, ICefAuthCallback)
  protected
    procedure cont(const username, password: ustring);
    procedure cancel;
  public
    class function UnWrap(data: Pointer): ICefAuthCallback;
  end;

  TCefJsDialogCallbackRef = class(TCefBaseRef, ICefJsDialogCallback)
  protected
    procedure cont(success: Boolean; const userInput: ustring);
  public
    class function UnWrap(data: Pointer): ICefJsDialogCallback;
  end;

  TCefCommandLineRef = class(TCefBaseRef, ICefCommandLine)
  protected
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function copy: ICefCommandLine;
    procedure InitFromArgv(argc: Integer; const argv: PPAnsiChar);
    procedure InitFromString(const commandLine: ustring);
    procedure reset;
    function GetCommandLineString: ustring;
    procedure GetArgv(args: TStrings);
    function GetProgram: ustring;
    procedure SetProgram(const prog: ustring);
    function HasSwitches: Boolean;
    function HasSwitch(const name: ustring): Boolean;
    function GetSwitchValue(const name: ustring): ustring;
    procedure GetSwitches(switches: TStrings);
    procedure AppendSwitch(const name: ustring);
    procedure AppendSwitchWithValue(const name, value: ustring);
    function HasArguments: Boolean;
    procedure GetArguments(arguments: TStrings);
    procedure AppendArgument(const argument: ustring);
    procedure PrependWrapper(const wrapper: ustring);
  public
    class function UnWrap(data: Pointer): ICefCommandLine;
    class function New: ICefCommandLine;
    class function Global: ICefCommandLine;
  end;

  TCefSchemeRegistrarRef = class(TCefBaseRef, ICefSchemeRegistrar)
  protected
    function AddCustomScheme(const schemeName: ustring;
      IsStandard, IsLocal, IsDisplayIsolated: Boolean): Boolean; stdcall;
  public
    class function UnWrap(data: Pointer): ICefSchemeRegistrar;
  end;

  TCefGeolocationCallbackRef = class(TCefBaseRef, ICefGeolocationCallback)
  protected
    procedure cont(allow: Boolean);
  public
    class function UnWrap(data: Pointer): ICefGeolocationCallback;
  end;

  TCefContextMenuParamsRef = class(TCefBaseRef, ICefContextMenuParams)
  protected
    function GetXCoord: Integer;
    function GetYCoord: Integer;
    function GetTypeFlags: TCefContextMenuTypeFlags;
    function GetLinkUrl: ustring;
    function GetUnfilteredLinkUrl: ustring;
    function GetSourceUrl: ustring;
    function HasImageContents: Boolean;
    function GetPageUrl: ustring;
    function GetFrameUrl: ustring;
    function GetFrameCharset: ustring;
    function GetMediaType: TCefContextMenuMediaType;
    function GetMediaStateFlags: TCefContextMenuMediaStateFlags;
    function GetSelectionText: ustring;
    function GetMisspelledWord: ustring;
    function GetDictionarySuggestions(const suggestions: TStringList): Boolean;
    function IsEditable: Boolean;
    function IsSpellCheckEnabled: Boolean;
    function GetEditStateFlags: TCefContextMenuEditStateFlags;
    function IsCustomMenu: Boolean;
    function IsPepperMenu: Boolean;
  public
    class function UnWrap(data: Pointer): ICefContextMenuParams;
  end;

  TCefMenuModelRef = class(TCefBaseRef, ICefMenuModel)
  protected
    function clear: Boolean;
    function GetCount: Integer;
    function AddSeparator: Boolean;
    function AddItem(commandId: Integer; const text: ustring): Boolean;
    function AddCheckItem(commandId: Integer; const text: ustring): Boolean;
    function AddRadioItem(commandId: Integer; const text: ustring;
      groupId: Integer): Boolean;
    function AddSubMenu(commandId: Integer; const text: ustring): ICefMenuModel;
    function InsertSeparatorAt(index: Integer): Boolean;
    function InsertItemAt(index, commandId: Integer;
      const text: ustring): Boolean;
    function InsertCheckItemAt(index, commandId: Integer;
      const text: ustring): Boolean;
    function InsertRadioItemAt(index, commandId: Integer; const text: ustring;
      groupId: Integer): Boolean;
    function InsertSubMenuAt(index, commandId: Integer; const text: ustring)
      : ICefMenuModel;
    function remove(commandId: Integer): Boolean;
    function RemoveAt(index: Integer): Boolean;
    function GetIndexOf(commandId: Integer): Integer;
    function GetCommandIdAt(index: Integer): Integer;
    function SetCommandIdAt(index, commandId: Integer): Boolean;
    function GetLabel(commandId: Integer): ustring;
    function GetLabelAt(index: Integer): ustring;
    function SetLabel(commandId: Integer; const text: ustring): Boolean;
    function SetLabelAt(index: Integer; const text: ustring): Boolean;
    function GetType(commandId: Integer): TCefMenuItemType;
    function GetTypeAt(index: Integer): TCefMenuItemType;
    function GetGroupId(commandId: Integer): Integer;
    function GetGroupIdAt(index: Integer): Integer;
    function SetGroupId(commandId, groupId: Integer): Boolean;
    function SetGroupIdAt(index, groupId: Integer): Boolean;
    function GetSubMenu(commandId: Integer): ICefMenuModel;
    function GetSubMenuAt(index: Integer): ICefMenuModel;
    function IsVisible(commandId: Integer): Boolean;
    function isVisibleAt(index: Integer): Boolean;
    function SetVisible(commandId: Integer; visible: Boolean): Boolean;
    function SetVisibleAt(index: Integer; visible: Boolean): Boolean;
    function IsEnabled(commandId: Integer): Boolean;
    function IsEnabledAt(index: Integer): Boolean;
    function SetEnabled(commandId: Integer; enabled: Boolean): Boolean;
    function SetEnabledAt(index: Integer; enabled: Boolean): Boolean;
    function IsChecked(commandId: Integer): Boolean;
    function IsCheckedAt(index: Integer): Boolean;
    function setChecked(commandId: Integer; checked: Boolean): Boolean;
    function setCheckedAt(index: Integer; checked: Boolean): Boolean;
    function HasAccelerator(commandId: Integer): Boolean;
    function HasAcceleratorAt(index: Integer): Boolean;
    function SetAccelerator(commandId, keyCode: Integer;
      shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function SetAcceleratorAt(index, keyCode: Integer;
      shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function RemoveAccelerator(commandId: Integer): Boolean;
    function RemoveAcceleratorAt(index: Integer): Boolean;
    function GetAccelerator(commandId: Integer; out keyCode: Integer;
      out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function GetAcceleratorAt(index: Integer; out keyCode: Integer;
      out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
  public
    class function UnWrap(data: Pointer): ICefMenuModel;
  end;

  TCefListValueRef = class(TCefBaseRef, ICefListValue)
  protected
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function IsReadOnly: Boolean;
    function IsSame(const that: ICefListValue): Boolean;
    function IsEqual(const that: ICefListValue): Boolean;
    function Copy: ICefListValue;
    function SetSize(size: NativeUInt): Boolean;
    function GetSize: NativeUInt;
    function Clear: Boolean;
    function Remove(index: Integer): Boolean;
    function GetType(index: Integer): TCefValueType;
    function GetValue(index: Integer): ICefValue;
    function GetBool(index: Integer): Boolean;
    function GetInt(index: Integer): Integer;
    function GetDouble(index: Integer): Double;
    function GetString(index: Integer): ustring;
    function GetBinary(index: Integer): ICefBinaryValue;
    function GetDictionary(index: Integer): ICefDictionaryValue;
    function GetList(index: Integer): ICefListValue;
    function SetValue(index: Integer; const value: ICefValue): Boolean;
    function SetNull(index: Integer): Boolean;
    function SetBool(index: Integer; value: Boolean): Boolean;
    function SetInt(index, value: Integer): Boolean;
    function SetDouble(index: Integer; value: Double): Boolean;
    function SetString(index: Integer; const value: ustring): Boolean;
    function SetBinary(index: Integer; const value: ICefBinaryValue): Boolean;
    function SetDictionary(index: Integer; const value: ICefDictionaryValue): Boolean;
    function SetList(index: Integer; const value: ICefListValue): Boolean;
  public
    class function UnWrap(data: Pointer): ICefListValue;
    class function New: ICefListValue;
  end;

  TCefValueRef = class(TCefBaseRef, ICefValue)
  protected
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function IsReadOnly: Boolean;
    function IsSame(const that: ICefValue): Boolean;
    function IsEqual(const that: ICefValue): Boolean;
    function Copy: ICefValue;
    function GetType: TCefValueType;
    function GetBool: Boolean;
    function GetInt: Integer;
    function GetDouble: Double;
    function GetString: ustring;
    function GetBinary: ICefBinaryValue;
    function GetDictionary: ICefDictionaryValue;
    function GetList: ICefListValue;
    function SetNull: Boolean;
    function SetBool(value: Integer): Boolean;
    function SetInt(value: Integer): Boolean;
    function SetDouble(value: Double): Boolean;
    function SetString(const value: ustring): Boolean;
    function SetBinary(const value: ICefBinaryValue): Boolean;
    function SetDictionary(const value: ICefDictionaryValue): Boolean;
    function SetList(const value: ICefListValue): Boolean;
  public
    class function UnWrap(data: Pointer): ICefValue;
    class function New: ICefValue;
  end;

  TCefBinaryValueRef = class(TCefBaseRef, ICefBinaryValue)
  protected
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function IsSame(const that: ICefBinaryValue): Boolean;
    function IsEqual(const that: ICefBinaryValue): Boolean;
    function copy: ICefBinaryValue;
    function GetSize: NativeUInt;
    function GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt)
      : NativeUInt;
  public
    class function UnWrap(data: Pointer): ICefBinaryValue;
    class function New(const data: Pointer; dataSize: NativeUInt)
      : ICefBinaryValue;
  end;

  TCefDictionaryValueRef = class(TCefBaseRef, ICefDictionaryValue)
  protected
    function IsValid: Boolean;
    function isOwned: Boolean;
    function IsReadOnly: Boolean;
    function IsSame(const that: ICefDictionaryValue): Boolean;
    function IsEqual(const that: ICefDictionaryValue): Boolean;
    function Copy(excludeEmptyChildren: Boolean): ICefDictionaryValue;
    function GetSize: NativeUInt;
    function Clear: Boolean;
    function HasKey(const key: ustring): Boolean;
    function GetKeys(const keys: TStrings): Boolean;
    function Remove(const key: ustring): Boolean;
    function GetType(const key: ustring): TCefValueType;
    function GetValue(const key: ustring): ICefValue;
    function GetBool(const key: ustring): Boolean;
    function GetInt(const key: ustring): Integer;
    function GetDouble(const key: ustring): Double;
    function GetString(const key: ustring): ustring;
    function GetBinary(const key: ustring): ICefBinaryValue;
    function GetDictionary(const key: ustring): ICefDictionaryValue;
    function GetList(const key: ustring): ICefListValue;
    function SetValue(const key: ustring; const value: ICefValue): Boolean;
    function SetNull(const key: ustring): Boolean;
    function SetBool(const key: ustring; value: Boolean): Boolean;
    function SetInt(const key: ustring; value: Integer): Boolean;
    function SetDouble(const key: ustring; value: Double): Boolean;
    function SetString(const key, value: ustring): Boolean;
    function SetBinary(const key: ustring; const value: ICefBinaryValue): Boolean;
    function SetDictionary(const key: ustring; const value: ICefDictionaryValue): Boolean;
    function SetList(const key: ustring; const value: ICefListValue): Boolean;
  public
    class function UnWrap(data: Pointer): ICefDictionaryValue;
    class function New: ICefDictionaryValue;
  end;

  TCefBrowserProcessHandlerOwn = class(TCefBaseOwn, ICefBrowserProcessHandler)
  protected
    procedure OnContextInitialized; virtual;
    procedure OnBeforeChildProcessLaunch(const commandLine
      : ICefCommandLine); virtual;
    procedure OnRenderProcessThreadCreated(const extraInfo
      : ICefListValue); virtual;
  public
    constructor create; virtual;
  end;

  TCefRenderProcessHandlerOwn = class(TCefBaseOwn, ICefRenderProcessHandler)
  protected
    procedure OnRenderThreadCreated(const extraInfo: ICefListValue); virtual;
    procedure OnWebKitInitialized; virtual;
    procedure OnBrowserCreated(const browser: ICefBrowser); virtual;
    procedure OnBrowserDestroyed(const browser: ICefBrowser); virtual;
    function GetLoadHandler: PCefLoadHandler; virtual;
    function OnBeforeNavigation(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      navigationType: TCefNavigationType; isRedirect: Boolean)
      : Boolean; virtual;
    procedure OnContextCreated(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefv8Context); virtual;
    procedure OnContextReleased(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefv8Context); virtual;
    procedure OnUncaughtException(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefv8Context;
      const exception: ICefV8Exception;
      const stackTrace: ICefV8StackTrace); virtual;
    procedure OnFocusedNodeChanged(const browser: ICefBrowser;
      const frame: ICefFrame; const node: ICefDomNode); virtual;
    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage)
      : Boolean; virtual;
  public
    constructor create; virtual;
  end;

  TCefUrlrequestClientOwn = class(TCefBaseOwn, ICefUrlrequestClient)
  protected
    procedure OnRequestComplete(const request: ICefUrlRequest); virtual;
    procedure OnUploadProgress(const request: ICefUrlRequest;
      current, total: UInt64); virtual;
    procedure OnDownloadProgress(const request: ICefUrlRequest;
      current, total: UInt64); virtual;
    procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer;
      dataLength: NativeUInt); virtual;
    function OnGetAuthCredentials(isProxy: Boolean; const host: ustring;
      port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback): Boolean;
  public
    constructor create; virtual;
  end;

  TCefUrlRequestRef = class(TCefBaseRef, ICefUrlRequest)
  protected
    function GetRequest: ICefRequest;
    function GetRequestStatus: TCefUrlRequestStatus;
    function GetRequestError: Integer;
    function GetResponse: ICefResponse;
    procedure cancel;
  public
    class function UnWrap(data: Pointer): ICefUrlRequest;
    class function New(const request: ICefRequest; const client: ICefUrlRequestClient;
      const requestContext: ICefRequestContext): ICefUrlRequest;
  end;

  TCefWebPluginInfoVisitorOwn = class(TCefBaseOwn, ICefWebPluginInfoVisitor)
  protected
    function visit(const info: ICefWebPluginInfo; count, total: Integer)
      : Boolean; virtual;
  public
    constructor create; virtual;
  end;

  TCefWebPluginInfoVisitorProc = {$IFDEF DELPHI12_UP}reference to
{$ENDIF} function(const info: ICefWebPluginInfo; count, total: Integer)
    : Boolean;
  TCefWebPluginIsUnstableProc = {$IFDEF DELPHI12_UP}reference to
{$ENDIF} procedure(const path: ustring; unstable: Boolean);

  TCefFastWebPluginInfoVisitor = class(TCefWebPluginInfoVisitorOwn)
  private
    FProc: TCefWebPluginInfoVisitorProc;
  protected
    function visit(const info: ICefWebPluginInfo; count, total: Integer)
      : Boolean; override;
  public
    constructor create(const proc: TCefWebPluginInfoVisitorProc); reintroduce;
  end;

  TCefWebPluginUnstableCallbackOwn = class(TCefBaseOwn,
    ICefWebPluginUnstableCallback)
  protected
    procedure IsUnstable(const path: ustring; unstable: Boolean); virtual;
  public
    constructor create; virtual;
  end;

  TCefFastWebPluginUnstableCallback = class(TCefWebPluginUnstableCallbackOwn)
  private
    FCallback: TCefWebPluginIsUnstableProc;
  protected
    procedure IsUnstable(const path: ustring; unstable: Boolean); override;
  public
    constructor create(const callback: TCefWebPluginIsUnstableProc);
      reintroduce;
  end;

  TCefEndTracingCallbackOwn = class(TCefBaseOwn, ICefEndTracingCallback)
  protected
    procedure OnEndTracingComplete(const tracingFile: ustring); virtual;
  public
    constructor create; virtual;
  end;

  TCefGetGeolocationCallbackOwn = class(TCefBaseOwn, ICefGetGeolocationCallback)
  protected
    procedure OnLocationUpdate(const position: PCefGeoposition); virtual;
  public
    constructor create; virtual;
  end;

  TOnLocationUpdate = {$IFDEF DELPHI12_UP}reference to
{$ENDIF} procedure(const position: PCefGeoposition);

  TCefFastGetGeolocationCallback = class(TCefGetGeolocationCallbackOwn)
  private
    FCallback: TOnLocationUpdate;
  protected
    procedure OnLocationUpdate(const position: PCefGeoposition); override;
  public
    constructor create(const callback: TOnLocationUpdate); reintroduce;
  end;

  TCefFileDialogCallbackRef = class(TCefBaseRef, ICefFileDialogCallback)
  protected
    procedure Cont(selectedAcceptFilter: Integer; filePaths: TStrings);
    procedure cancel;
  public
    class function UnWrap(data: Pointer): ICefFileDialogCallback;
  end;

  TCefRenderHandlerOwn = class(TCefBaseOwn, ICefRenderHandler)
  protected

    function GetRootScreenRect(const browser: ICefBrowser; rect: PCefRect)
      : Boolean; virtual;
    function GetViewRect(const browser: ICefBrowser; rect: PCefRect)
      : Boolean; virtual;
    function GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer;
      screenX, screenY: PInteger): Boolean; virtual;
    function GetScreenInfo(const browser: ICefBrowser;
      screenInfo: PCefScreenInfo): Boolean; virtual;
    procedure OnPopupShow(const browser: ICefBrowser; show: Boolean); virtual;
    procedure OnPopupSize(const browser: ICefBrowser;
      const rect: PCefRect); virtual;
    procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
      dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray;
      const buffer: Pointer; width, height: Integer); virtual;
    procedure OnCursorChange(const browser: ICefBrowser;
      cursor: TCefCursorHandle; CursorType: TCefCursorType;
      const customCursorInfo: PCefCursorInfo); virtual;
    function OnStartDragging(const browser: ICefBrowser;
      const dragData: ICefDragData; allowedOps: TCefDragOperations;
      x, y: Integer): Boolean; virtual;
    procedure OnUpdateDragCursor(const browser: ICefBrowser;
      operation: TCefDragOperation); virtual;
    procedure OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double); virtual;
  public
    constructor create; virtual;
  end;

  TCefDragDataRef = class(TCefBaseRef, ICefDragData)
  protected
    function clone: ICefDragData;
    function IsReadOnly: Boolean;
    function IsLink: Boolean;
    function IsFragment: Boolean;
    function IsFile: Boolean;
    function GetLinkUrl: ustring;
    function GetLinkTitle: ustring;
    function GetLinkMetadata: ustring;
    function GetFragmentText: ustring;
    function GetFragmentHtml: ustring;
    function GetFragmentBaseUrl: ustring;
    function GetFileName: ustring;
    function GetFileContents(const writer: ICefStreamWriter): NativeUInt;
    function GetFileNames(names: TStrings): Integer;
    procedure SetLinkUrl(const url: ustring);
    procedure SetLinkTitle(const title: ustring);
    procedure SetLinkMetadata(const data: ustring);
    procedure SetFragmentText(const text: ustring);
    procedure SetFragmentHtml(const html: ustring);
    procedure SetFragmentBaseUrl(const BaseUrl: ustring);
    procedure ResetFileContents;
    procedure AddFile(const path, displayName: ustring);
  public
    class function UnWrap(data: Pointer): ICefDragData;
    class function New: ICefDragData;
  end;

  TCefDragHandlerOwn = class(TCefBaseOwn, ICefDragHandler)
  protected
    function OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData;
      mask: TCefDragOperations): Boolean; virtual;
    procedure OnDraggableRegionsChanged(const browser: ICefBrowser;
      regionsCount: NativeUInt; regions: PCefDraggableRegionArray); virtual;
  public
    constructor Create; virtual;
  end;

  TCefFindHandlerOwn = class(TCefBaseOwn, ICefFindHandler)
  protected
    procedure OnFindResult(const browser: ICefBrowser;
      identifier, count: Integer; const selectionRect: PCefRect;
      activeMatchOrdinal: Integer; finalUpdate: Boolean); virtual; abstract;
  public
    constructor Create; virtual;
  end;

  TCefResolveCallbackOwn = class(TCefBaseOwn, ICefResolveCallback)
  protected
    procedure OnResolveCompleted(result: TCefErrorCode; resolvedIps: TStrings); virtual; abstract;
  public
    constructor Create; virtual;
  end;

  TCefRequestContextRef = class(TCefBaseRef, ICefRequestContext)
  protected
    function IsSame(const other: ICefRequestContext): Boolean;
    function IsSharingWith(const other: ICefRequestContext): Boolean;
    function IsGlobal: Boolean;
    function GetHandler: ICefRequestContextHandler;
    function GetCachePath: ustring;
    function GetDefaultCookieManager(const callback: ICefCompletionCallback): ICefCookieManager;
    function GetDefaultCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
    function RegisterSchemeHandlerFactory(const schemeName, domainName: ustring;
        const factory: ICefSchemeHandlerFactory): Boolean;
    function ClearSchemeHandlerFactories: Boolean;
    procedure PurgePluginListCache(reloadPages: Boolean);

    function HasPreference(const name: ustring): Boolean;
    function GetPreference(const name: ustring): ICefValue;
    function GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
    function CanSetPreference(const name: ustring): Boolean;
    function SetPreference(const name: ustring; const value: ICefValue; out error: ustring): Boolean;
    procedure ClearCertificateExceptions(const callback: ICefCompletionCallback);
    procedure CloseAllConnections(const callback: ICefCompletionCallback);
    procedure ResolveHost(const origin: ustring; const callback: ICefResolveCallback);
    function ResolveHostCached(const origin: ustring; resolvedIps: TStrings): TCefErrorCode;
  public
    class function UnWrap(data: Pointer): ICefRequestContext;
    class function Global: ICefRequestContext;
    class function New(const settings: PCefRequestContextSettings;
      const handler: ICefRequestContextHandler): ICefRequestContext;
    class function Shared(const other: ICefRequestContext;
      const handler: ICefRequestContextHandler): ICefRequestContext;
  end;

  TCefRequestContextHandlerRef = class(TCefBaseRef, ICefRequestContextHandler)
  protected
    function GetCookieManager: ICefCookieManager;
    function OnBeforePluginLoad(const mimeType, pluginUrl, topOriginUrl: ustring;
      const pluginInfo: ICefWebPluginInfo; pluginPolicy: PCefPluginPolicy): Boolean;
  public
    class function UnWrap(data: Pointer): ICefRequestContextHandler;
  end;

  TCefRequestContextHandlerOwn = class(TCefBaseOwn, ICefRequestContextHandler)
  protected
    function GetCookieManager: ICefCookieManager; virtual;
    function OnBeforePluginLoad(const mimeType, pluginUrl, topOriginUrl: ustring;
      const pluginInfo: ICefWebPluginInfo; pluginPolicy: PCefPluginPolicy): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefRequestContextHandlerProc = {$IFDEF DELPHI12_UP}reference to
{$ENDIF} function: ICefCookieManager;

  TCefFastRequestContextHandler = class(TCefRequestContextHandlerOwn)
  private
    FProc: TCefRequestContextHandlerProc;
  protected
    function GetCookieManager: ICefCookieManager; override;
  public
    constructor create(const proc: TCefRequestContextHandlerProc); reintroduce;
  end;

  TCefPrintSettingsRef = class(TCefBaseRef, ICefPrintSettings)
  protected
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function copy: ICefPrintSettings;
    procedure SetOrientation(landscape: Boolean);
    function IsLandscape: Boolean;
    procedure SetPrinterPrintableArea(const physicalSizeDeviceUnits: PCefSize;
      const printableAreaDeviceUnits: PCefRect;
      landscapeNeedsFlip: Boolean); stdcall;
    procedure SetDeviceName(const name: ustring);
    function GetDeviceName: ustring;
    procedure SetDpi(dpi: Integer);
    function GetDpi: Integer;
    procedure SetPageRanges(const ranges: TCefPageRangeArray);
    function GetPageRangesCount: NativeUInt;
    procedure GetPageRanges(out ranges: TCefPageRangeArray);
    procedure SetSelectionOnly(selectionOnly: Boolean);
    function IsSelectionOnly: Boolean;
    procedure SetCollate(collate: Boolean);
    function WillCollate: Boolean;
    procedure SetColorModel(model: TCefColorModel);
    function GetColorModel: TCefColorModel;
    procedure SetCopies(copies: Integer);
    function GetCopies: Integer;
    procedure SetDuplexMode(mode: TCefDuplexMode);
    function GetDuplexMode: TCefDuplexMode;
  public
    class function New: ICefPrintSettings;
    class function UnWrap(data: Pointer): ICefPrintSettings;
  end;

  TCefNavigationEntryRef = class(TCefBaseRef, ICefNavigationEntry)
  protected
    function IsValid: Boolean;
    function GetUrl: ustring;
    function GetDisplayUrl: ustring;
    function GetOriginalUrl: ustring;
    function GetTitle: ustring;
    function GetTransitionType: TCefTransitionType;
    function HasPostData: Boolean;
    function GetCompletionTime: TDateTime;
    function GetHttpStatusCode: Integer;
  public
    class function UnWrap(data: Pointer): ICefNavigationEntry;
  end;

  TCefNavigationEntryVisitorOwn = class(TCefBaseOwn, ICefNavigationEntryVisitor)
  protected
    function visit(const entry: ICefNavigationEntry; current: Boolean;
      index, total: Integer): Boolean; virtual;
  public
    constructor create;
  end;

  TCefFastNavigationEntryVisitor = class(TCefNavigationEntryVisitorOwn)
  private
    FVisitor: TCefNavigationEntryVisitorProc;
  protected
    function visit(const entry: ICefNavigationEntry; current: Boolean;
      index, total: Integer): Boolean; override;
  public
    constructor create(const proc: TCefNavigationEntryVisitorProc); reintroduce;
  end;

  TCefSslCertPrincipalRef = class(TCefBaseRef, ICefSslCertPrincipal)
  protected
    function GetDisplayName: ustring;
    function GetCommonName: ustring;
    function GetLocalityName: ustring;
    function GetStateOrProvinceName: ustring;
    function GetCountryName: ustring;
    procedure GetStreetAddresses(addresses: TStrings);
    procedure GetOrganizationNames(names: TStrings);
    procedure GetOrganizationUnitNames(names: TStrings);
    procedure GetDomainComponents(components: TStrings);
  public
    class function UnWrap(data: Pointer): ICefSslCertPrincipal;
  end;

  TCefSslInfoRef = class(TCefBaseRef, ICefSslInfo)
  protected
    function GetCertStatus: TCefCertStatus;
    function IsCertStatusError: Boolean;
    function IsCertStatusMinorError: Boolean;
    function GetSubject: ICefSslCertPrincipal;
    function GetIssuer: ICefSslCertPrincipal;
    function GetSerialNumber: ICefBinaryValue;
    function GetValidStart: TCefTime;
    function GetValidExpiry: TCefTime;
    function GetDerEncoded: ICefBinaryValue;
    function GetPemEncoded: ICefBinaryValue;
    function GetIssuerChainSize: NativeUInt;
    function GetDEREncodedIssuerChain(chainCount: NativeUInt): IInterfaceList;
    function GetPEMEencodedIssuerChain(chainCount: NativeUInt): IInterfaceList;
  public
    class function UnWrap(data: Pointer): ICefSslInfo;
  end;

  TCefPdfPrintCallbackOwn = class(TCefBaseOwn, ICefPdfPrintCallback)
  protected
    procedure OnPdfPrintFinished(const path: ustring; ok: Boolean); virtual; abstract;
  public
    constructor Create; virtual;
  end;

  TCefFastPdfPrintCallback = class(TCefPdfPrintCallbackOwn)
  private
    FProc: TOnPdfPrintFinishedProc;
  protected
    procedure OnPdfPrintFinished(const path: ustring; ok: Boolean); override;
  public
    constructor Create(const proc: TOnPdfPrintFinishedProc); reintroduce;
  end;

  TCefRunContextMenuCallbackRef = class(TCefBaseRef, ICefRunContextMenuCallback)
  protected
    procedure Cont(commandId: Integer; eventFlags: TCefEventFlags);
    procedure Cancel;
  public
    class function UnWrap(data: Pointer): ICefRunContextMenuCallback;
  end;

  TCefResourceBundleRef = class(TCefBaseRef, ICefResourceBundle)
  protected
    function GetLocalizedString(stringId: Integer): ustring;
    function GetDataResource(resourceId: Integer;
      out data: Pointer; out dataSize: NativeUInt): Boolean;
    function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor;
      out data: Pointer; out dataSize: NativeUInt): Boolean;
  public
    class function UnWrap(data: Pointer): ICefResourceBundle;
    class function Global: ICefResourceBundle;
  end;

  TCefResponseFilterOwn = class(TCefBaseOwn, ICefResponseFilter)
  protected
    function InitFilter: Boolean; virtual; abstract;
    function Filter(dataIn: Pointer; dataInSize, dataInRead: NativeUInt; dataOut: Pointer;
      dataOutSize, dataOutWritten: NativeUInt): TCefResponseFilterStatus; virtual; abstract;
  public
    constructor Create; virtual;
  end;

  ECefException = class(exception)
  end;

{$IFDEF DELPHI14_UP}

  TCefRTTIExtension = class(TCefv8HandlerOwn)
  private
    FValue: TValue;
    FCtx: TRttiContext;
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    FSyncMainThread: Boolean;
{$ENDIF}
    function GetValue(pi: PTypeInfo; const v: ICefv8Value;
      var ret: TValue): Boolean;
    function SetValue(const v: TValue; var ret: ICefv8Value): Boolean;
{$IFDEF CPUX64}
    class function StrToPtr(const str: ustring): Pointer;
    class function PtrToStr(p: Pointer): ustring;
{$ENDIF}
  protected
    function execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean; override;
  public
    constructor create(const value: TValue
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
      ; SyncMainThread: Boolean
{$ENDIF}
      ); reintroduce;
    destructor Destroy; override;
  end;
{$ENDIF}

implementation

{ cef_base }

procedure cef_base_add_ref(self: PCefBase); stdcall;
begin
  TCefBaseOwn(TCef3Helper.CefGetObject(self))._AddRef;
end;

function cef_base_release(self: PCefBase): Integer; stdcall;
begin
  Result := TCefBaseOwn(TCef3Helper.CefGetObject(self))._Release;
end;

function cef_base_has_one_ref(self: PCefBase): Integer; stdcall;
begin
  Result := ord(TCefBaseOwn(TCef3Helper.CefGetObject(self)).FRefCount = 1);
end;

procedure cef_base_add_ref_owned(self: PCefBase); stdcall;
begin

end;

function cef_base_release_owned(self: PCefBase): Integer; stdcall;
begin
  Result := 1;
end;

function cef_base_has_one_ref_owned(self: PCefBase): Integer; stdcall;
begin
  Result := 1;
end;

{ cef_client }

function cef_client_get_context_menu_handler(self: PCefClient)
  : PCefContextMenuHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetContextMenuHandler);
end;

function cef_client_get_dialog_handler(self: PCefClient)
  : PCefDialogHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetDialogHandler);
end;

function cef_client_get_display_handler(self: PCefClient)
  : PCefDisplayHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetDisplayHandler);
end;

function cef_client_get_download_handler(self: PCefClient)
  : PCefDownloadHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetDownloadHandler);
end;

function cef_client_get_drag_handler(self: PCefClient)
  : PCefDragHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetDragHandler);
end;

function cef_client_get_find_handler(self: PCefClient): PCefFindHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetFindHandler);
end;

function cef_client_get_focus_handler(self: PCefClient)
  : PCefFocusHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetFocusHandler);
end;

function cef_client_get_geolocation_handler(self: PCefClient)
  : PCefGeolocationHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetGeolocationHandler);
end;

function cef_client_get_jsdialog_handler(self: PCefClient)
  : PCefJsDialogHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetJsdialogHandler);
end;

function cef_client_get_keyboard_handler(self: PCefClient)
  : PCefKeyboardHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetKeyboardHandler);
end;

function cef_client_get_life_span_handler(self: PCefClient)
  : PCefLifeSpanHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetLifeSpanHandler);
end;

function cef_client_get_load_handler(self: PCefClient)
  : PCefLoadHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetLoadHandler);
end;

function cef_client_get_get_render_handler(self: PCefClient)
  : PCefRenderHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetRenderHandler);
end;

function cef_client_get_request_handler(self: PCefClient)
  : PCefRequestHandler; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetRequestHandler);
end;

function cef_client_on_process_message_received(self: PCefClient;
  browser: PCefBrowser; source_process: TCefProcessId;
  message: PCefProcessMessage): Integer; stdcall;
begin
  with TCefClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnProcessMessageReceived(TCefBrowserRef.UnWrap(browser),
      source_process, TCefProcessMessageRef.UnWrap(message)));
end;

{ cef_geolocation_handler }

function cef_geolocation_handler_on_request_geolocation_permission
  (self: PCefGeolocationHandler; browser: PCefBrowser;
  const requesting_url: PCefString; request_id: Integer;
  callback: PCefGeolocationCallback): Integer; stdcall;
begin
  with TCefGeolocationHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnRequestGeolocationPermission(TCefBrowserRef.UnWrap(browser),
      TCef3Helper.CefString(requesting_url), request_id,
      TCefGeolocationCallbackRef.UnWrap(callback)));
end;

procedure cef_geolocation_handler_on_cancel_geolocation_permission
  (self: PCefGeolocationHandler; browser: PCefBrowser;
  request_id: Integer); stdcall;
begin
  with TCefGeolocationHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnCancelGeolocationPermission(TCefBrowserRef.UnWrap(browser),
      request_id);
end;

{ cef_life_span_handler }

function cef_life_span_handler_on_before_popup(self: PCefLifeSpanHandler;
  browser: PCefBrowser; frame: PCefFrame; const target_url, target_frame_name: PCefString;
  target_disposition: TCefWindowOpenDisposition; user_gesture: Integer;
  const popupFeatures: PCefPopupFeatures; windowInfo: PCefWindowInfo; var client: PCefClient;
  settings: PCefBrowserSettings; no_javascript_access: PInteger): Integer; stdcall;
var
  _url, _frame: ustring;
  _client: ICefClient;
  _nojs: Boolean;
begin
  _url := TCef3Helper.CefString(target_url);
  _frame := TCef3Helper.CefString(target_frame_name);
  _client := TCefClientOwn(TCef3Helper.CefGetObject(client)) as ICefClient;
  _nojs := no_javascript_access^ <> 0;
  with TCefLifeSpanHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := Ord(OnBeforePopup(
      TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame),
      _url,
      _frame,
      target_disposition,
      user_gesture <> 0,
      popupFeatures^,
      windowInfo^,
      _client,
      settings^,
      _nojs
    ));
  TCef3Helper.CefStringSet(target_url, _url);
  TCef3Helper.CefStringSet(target_frame_name, _frame);
  client := TCef3Helper.CefGetData(_client);
  no_javascript_access^ := Ord(_nojs);
  _client := nil;
end;

procedure cef_life_span_handler_on_after_created(self: PCefLifeSpanHandler;
  browser: PCefBrowser); stdcall;
begin
  with TCefLifeSpanHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnAfterCreated(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_life_span_handler_on_before_close(self: PCefLifeSpanHandler;
  browser: PCefBrowser); stdcall;
begin
  with TCefLifeSpanHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnBeforeClose(TCefBrowserRef.UnWrap(browser));
end;

function cef_life_span_handler_run_modal(self: PCefLifeSpanHandler; browser: PCefBrowser): Integer; stdcall;
begin
  with TCefLifeSpanHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := Ord(RunModal(TCefBrowserRef.UnWrap(browser)));
end;

function cef_life_span_handler_do_close(self: PCefLifeSpanHandler;
  browser: PCefBrowser): Integer; stdcall;
begin
  with TCefLifeSpanHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(DoClose(TCefBrowserRef.UnWrap(browser)));
end;

{ cef_load_handler }

procedure cef_load_handler_on_loading_state_change(self: PCefLoadHandler;
  browser: PCefBrowser; isLoading, canGoBack, canGoForward: Integer); stdcall;
begin
  with TCefLoadHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnLoadingStateChange(TCefBrowserRef.UnWrap(browser), isLoading <> 0,
      canGoBack <> 0, canGoForward <> 0);
end;

procedure cef_load_handler_on_load_start(self: PCefLoadHandler;
  browser: PCefBrowser; frame: PCefFrame); stdcall;
begin
  with TCefLoadHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnLoadStart(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame));
end;

procedure cef_load_handler_on_load_end(self: PCefLoadHandler;
  browser: PCefBrowser; frame: PCefFrame; httpStatusCode: Integer); stdcall;
begin
  with TCefLoadHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnLoadEnd(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      httpStatusCode);
end;

procedure cef_load_handler_on_load_error(self: PCefLoadHandler;
  browser: PCefBrowser; frame: PCefFrame; errorCode: Integer;
  const errorText, failedUrl: PCefString); stdcall;
begin
  with TCefLoadHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnLoadError(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      errorCode, TCef3Helper.CefString(errorText),
      TCef3Helper.CefString(failedUrl));
end;

{ cef_request_handler }

function cef_request_handler_on_before_browse(self: PCefRequestHandler; browser: PCefBrowser;
  frame: PCefFrame; request: PCefRequest; isRedirect: Integer): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := Ord(OnBeforeBrowse(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCefRequestRef.UnWrap(request), isRedirect <> 0));
end;

function cef_request_handler_on_open_urlfrom_tab(self: PCefRequestHandler; browser: PCefBrowser;
  frame: PCefFrame; const target_url: PCefString; target_disposition: TCefWindowOpenDisposition;
  user_gesture: Integer): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := Ord(OnOpenUrlFromTab(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCef3Helper.CefString(target_url), target_disposition, user_gesture <> 0));
end;

function cef_request_handler_on_before_resource_load(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; request: PCefRequest;
  callback: PCefRequestCallback): TCefReturnValue; stdcall;
begin
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := OnBeforeResourceLoad(
      TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame),
      TCefRequestRef.UnWrap(request),
      TcefRequestCallbackRef.UnWrap(callback));
end;

function cef_request_handler_get_resource_handler(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): PCefResourceHandler; stdcall;
begin
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetResourceHandler(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefRequestRef.UnWrap(request)));
end;

procedure cef_request_handler_on_resource_redirect(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; const request: PCefRequest; new_url: PCefString); stdcall;
var
  url: ustring;
begin
  url := TCef3Helper.CefString(new_url);
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnResourceRedirect(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCefRequestRef.UnWrap(request), url);
  if url <> '' then
    TCef3Helper.CefStringSet(new_url, url);
end;

function cef_request_handler_on_resource_response(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; request: PCefRequest;
  response: PCefResponse): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := Ord(OnResourceResponse(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCefRequestRef.UnWrap(request), TCefResponseRef.UnWrap(response)));
end;

function cef_request_handler_get_resource_response_filter(self: PCefRequestHandler; browser: PCefBrowser;
  frame: PCefFrame; request: PCefRequest; response: PCefResponse): PCefResponseFilter; stdcall;
begin
   with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
     Result := TCef3Helper.CefGetData(GetResourceResponseFilter(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefRequestRef.UnWrap(request),
      TCefResponseRef.UnWrap(response)));
end;

procedure cef_request_handler_on_resource_load_complete(self: PCefRequestHandler; browser: PCefBrowser;
  frame: PCefFrame; request: PCefRequest; response: PCefResponse;
  status: TCefUrlRequestStatus; received_content_length: Int64); stdcall;
begin
   with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
     OnResourceLoadComplete(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefRequestRef.UnWrap(request),
      TCefResponseRef.UnWrap(response), status, received_content_length);
end;

function cef_request_handler_get_auth_credentials(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; isProxy: Integer; const host: PCefString;
  port: Integer; const realm, scheme: PCefString; callback: PCefAuthCallback): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := Ord(GetAuthCredentials(
      TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame), isProxy <> 0,
      TCef3Helper.CefString(host), port, TCef3Helper.CefString(realm), TCef3Helper.CefString(scheme), TCefAuthCallbackRef.UnWrap(callback)));
end;

function cef_request_handler_on_quota_request(self: PCefRequestHandler; browser: PCefBrowser;
  const origin_url: PCefString; new_size: Int64; callback: PCefRequestCallback): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := Ord(OnQuotaRequest(TCefBrowserRef.UnWrap(browser),
      TCef3Helper.CefString(origin_url), new_size, TCefRequestCallbackRef.UnWrap(callback)));
end;

procedure cef_request_handler_on_protocol_execution(self: PCefRequestHandler;
  browser: PCefBrowser; const url: PCefString; allow_os_execution: PInteger); stdcall;
var
  allow: Boolean;
begin
  allow := allow_os_execution^ <> 0;
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnProtocolExecution(
      TCefBrowserRef.UnWrap(browser),
      TCef3Helper.CefString(url), allow);
  allow_os_execution^ := Ord(allow);
end;

function cef_request_handler_on_certificate_error(self: PCefRequestHandler;
  browser: PCefBrowser; cert_error: TCefErrorcode; const request_url: PCefString;
  ssl_info: PCefSslInfo; callback: PCefRequestCallback): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := Ord(OnCertificateError(TCefBrowserRef.UnWrap(browser), cert_error,
      TCef3Helper.CefString(request_url), TCefSslInfoRef.UnWrap(ssl_info),
      TCefRequestCallbackRef.UnWrap(callback)));
end;

procedure cef_request_handler_on_plugin_crashed(self: PCefRequestHandler;
  browser: PCefBrowser; const plugin_path: PCefString); stdcall;
begin
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnPluginCrashed(TCefBrowserRef.UnWrap(browser), TCef3Helper.CefString(plugin_path));
end;

procedure cef_request_handler_on_render_view_ready(self: PCefRequestHandler;
  browser: PCefBrowser); stdcall;
begin
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnRenderViewReady(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_request_handler_on_render_process_terminated(self: PCefRequestHandler;
  browser: PCefBrowser; status: TCefTerminationStatus); stdcall;
begin
  with TCefRequestHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnRenderProcessTerminated(TCefBrowserRef.UnWrap(browser), status);
end;



{ cef_display_handler }

procedure cef_display_handler_on_address_change(self: PCefDisplayHandler;
  browser: PCefBrowser; frame: PCefFrame; const url: PCefString); stdcall;
begin
  with TCefDisplayHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnAddressChange(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCef3Helper.CefString(url))
end;

procedure cef_display_handler_on_title_change(self: PCefDisplayHandler;
  browser: PCefBrowser; const title: PCefString); stdcall;
begin
  with TCefDisplayHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnTitleChange(TCefBrowserRef.UnWrap(browser), TCef3Helper.CefString(title));
end;

procedure cef_display_handler_on_favicon_urlchange(self: PCefDisplayHandler;
  browser: PCefBrowser; icon_urls: TCefStringList); stdcall;
var
  list: TStringList;
  i: Integer;
  str: TCefString;
begin
  list := TStringList.Create;
  try
    for i := 0 to cef_string_list_size(icon_urls) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(icon_urls, i, @str);
      list.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
    with TCefDisplayHandlerOwn(TCef3Helper.CefGetObject(self)) do
      OnFaviconUrlChange(TCefBrowserRef.UnWrap(browser), list);
  finally
    list.Free;
  end;
end;

procedure cef_display_handler_on_fullscreen_mode_change(self: PCefDisplayHandler;
  browser: PCefBrowser; fullscreen: Integer); stdcall;
begin
  with TCefDisplayHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnFullScreenModeChange(TCefBrowserRef.UnWrap(browser), fullscreen <> 0);
end;

function cef_display_handler_on_tooltip(self: PCefDisplayHandler;
  browser: PCefBrowser; text: PCefString): Integer; stdcall;
var
  t: ustring;
begin
  t := TCef3Helper.CefStringClearAndGet(text^);
  with TCefDisplayHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnTooltip(TCefBrowserRef.UnWrap(browser), t));
  text^ := TCef3Helper.CefStringAlloc(t);
end;

procedure cef_display_handler_on_status_message(self: PCefDisplayHandler;
  browser: PCefBrowser; const value: PCefString); stdcall;
begin
  with TCefDisplayHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnStatusMessage(TCefBrowserRef.UnWrap(browser),
      TCef3Helper.CefString(value));
end;

function cef_display_handler_on_console_message(self: PCefDisplayHandler;
  browser: PCefBrowser; const message: PCefString; const source: PCefString;
  line: Integer): Integer; stdcall;
begin
  with TCefDisplayHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnConsoleMessage(TCefBrowserRef.UnWrap(browser),
      TCef3Helper.CefString(message), TCef3Helper.CefString(source), line));
end;

{ cef_focus_handler }

procedure cef_focus_handler_on_take_focus(self: PCefFocusHandler;
  browser: PCefBrowser; next: Integer); stdcall;
begin
  with TCefFocusHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnTakeFocus(TCefBrowserRef.UnWrap(browser), next <> 0);
end;

function cef_focus_handler_on_set_focus(self: PCefFocusHandler;
  browser: PCefBrowser; source: TCefFocusSource): Integer; stdcall;
begin
  with TCefFocusHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnSetFocus(TCefBrowserRef.UnWrap(browser), source))
end;

procedure cef_focus_handler_on_got_focus(self: PCefFocusHandler;
  browser: PCefBrowser); stdcall;
begin
  with TCefFocusHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnGotFocus(TCefBrowserRef.UnWrap(browser));
end;

{ cef_keyboard_handler }

function cef_keyboard_handler_on_pre_key_event(self: PCefKeyboardHandler;
  browser: PCefBrowser; const event: PCefKeyEvent; os_event: TCefEventHandle;
  is_keyboard_shortcut: PInteger): Integer; stdcall;
var
  ks: Boolean;
begin
  ks := is_keyboard_shortcut^ <> 0;
  with TCefKeyboardHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnPreKeyEvent(TCefBrowserRef.UnWrap(browser), event,
      os_event, ks));
  is_keyboard_shortcut^ := ord(ks);
end;

function cef_keyboard_handler_on_key_event(self: PCefKeyboardHandler;
  browser: PCefBrowser; const event: PCefKeyEvent; os_event: TCefEventHandle)
  : Integer; stdcall;
begin
  with TCefKeyboardHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnKeyEvent(TCefBrowserRef.UnWrap(browser), event, os_event));
end;

{ cef_jsdialog_handler }

function cef_jsdialog_handler_on_jsdialog(self: PCefJsDialogHandler;
  browser: PCefBrowser; const origin_url, accept_lang: PCefString;
  dialog_type: TCefJsDialogType; const message_text, default_prompt_text
  : PCefString; callback: PCefJsDialogCallback; suppress_message: PInteger)
  : Integer; stdcall;
var
  sm: Boolean;
begin
  sm := suppress_message^ <> 0;
  with TCefJsDialogHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnJsdialog(TCefBrowserRef.UnWrap(browser),
      TCef3Helper.CefString(origin_url),
	  TCef3Helper.CefString(accept_lang),
      dialog_type, TCef3Helper.CefString(message_text),
      TCef3Helper.CefString(default_prompt_text),
      TCefJsDialogCallbackRef.UnWrap(callback), sm));
  suppress_message^ := ord(sm);
end;

function cef_jsdialog_handler_on_before_unload_dialog(self: PCefJsDialogHandler;
  browser: PCefBrowser; const message_text: PCefString; is_reload: Integer;
  callback: PCefJsDialogCallback): Integer; stdcall;
begin
  with TCefJsDialogHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnBeforeUnloadDialog(TCefBrowserRef.UnWrap(browser),
      TCef3Helper.CefString(message_text), is_reload <> 0,
      TCefJsDialogCallbackRef.UnWrap(callback)));
end;

procedure cef_jsdialog_handler_on_reset_dialog_state(self: PCefJsDialogHandler;
  browser: PCefBrowser); stdcall;
begin
  with TCefJsDialogHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnResetDialogState(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_jsdialog_handler_on_dialog_closed(self: PCefJsDialogHandler;
  browser: PCefBrowser); stdcall;
begin
  with TCefJsDialogHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnDialogClosed(TCefBrowserRef.UnWrap(browser));
end;

{ cef_context_menu_handler }

procedure cef_context_menu_handler_on_before_context_menu
  (self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame;
  params: PCefContextMenuParams; model: PCefMenuModel); stdcall;
begin
  with TCefContextMenuHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnBeforeContextMenu(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefContextMenuParamsRef.UnWrap(params),
      TCefMenuModelRef.UnWrap(model));
end;

function cef_context_menu_handler_run_context_menu(self: PCefContextMenuHandler;
  browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams;
  model: PCefMenuModel; callback: PCefRunContextMenuCallback): Integer; stdcall;
begin
  with TCefContextMenuHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := Ord(RunContextMenu(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCefContextMenuParamsRef.UnWrap(params), TCefMenuModelRef.UnWrap(model),
      TCefRunContextMenuCallbackRef.UnWrap(callback)));
end;

function cef_context_menu_handler_on_context_menu_command
  (self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame;
  params: PCefContextMenuParams; command_id: Integer; event_flags: Integer)
  : Integer; stdcall;
begin
  with TCefContextMenuHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnContextMenuCommand(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefContextMenuParamsRef.UnWrap(params),
      command_id, TCefEventFlags(Pointer(@event_flags)^)));
end;

procedure cef_context_menu_handler_on_context_menu_dismissed
  (self: PCefContextMenuHandler; browser: PCefBrowser;
  frame: PCefFrame); stdcall;
begin
  with TCefContextMenuHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnContextMenuDismissed(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame));
end;

{ cef_stream_reader }

function cef_stream_reader_read(self: PCefReadHandler; ptr: Pointer;
  size, n: NativeUInt): NativeUInt; stdcall;
begin
  with TCefCustomStreamReader(TCef3Helper.CefGetObject(self)) do
    Result := Read(ptr, size, n);
end;

function cef_stream_reader_seek(self: PCefReadHandler; offset: Int64;
  whence: Integer): Integer; stdcall;
begin
  with TCefCustomStreamReader(TCef3Helper.CefGetObject(self)) do
    Result := seek(offset, whence);
end;

function cef_stream_reader_tell(self: PCefReadHandler): Int64; stdcall;
begin
  with TCefCustomStreamReader(TCef3Helper.CefGetObject(self)) do
    Result := tell;
end;

function cef_stream_reader_eof(self: PCefReadHandler): Integer; stdcall;
begin
  with TCefCustomStreamReader(TCef3Helper.CefGetObject(self)) do
    Result := ord(eof);
end;

function cef_stream_reader_may_block(self: PCefReadHandler): Integer; stdcall;
begin
  with TCefCustomStreamReader(TCef3Helper.CefGetObject(self)) do
    Result := ord(MayBlock);
end;

{ cef_post_data_element }

function cef_post_data_element_is_read_only(self: PCefPostDataElement)
  : Integer; stdcall;
begin
  with TCefPostDataElementOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(IsReadOnly)
end;

procedure cef_post_data_element_set_to_empty
  (self: PCefPostDataElement); stdcall;
begin
  with TCefPostDataElementOwn(TCef3Helper.CefGetObject(self)) do
    SetToEmpty;
end;

procedure cef_post_data_element_set_to_file(self: PCefPostDataElement;
  const fileName: PCefString); stdcall;
begin
  with TCefPostDataElementOwn(TCef3Helper.CefGetObject(self)) do
    SetToFile(TCef3Helper.CefString(fileName));
end;

procedure cef_post_data_element_set_to_bytes(self: PCefPostDataElement;
  size: NativeUInt; const bytes: Pointer); stdcall;
begin
  with TCefPostDataElementOwn(TCef3Helper.CefGetObject(self)) do
    SetToBytes(size, bytes);
end;

function cef_post_data_element_get_type(self: PCefPostDataElement)
  : TCefPostDataElementType; stdcall;
begin
  with TCefPostDataElementOwn(TCef3Helper.CefGetObject(self)) do
    Result := GetType;
end;

function cef_post_data_element_get_file(self: PCefPostDataElement)
  : PCefStringUserFree; stdcall;
begin
  with TCefPostDataElementOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefUserFreeString(GetFile);
end;

function cef_post_data_element_get_bytes_count(self: PCefPostDataElement)
  : NativeUInt; stdcall;
begin
  with TCefPostDataElementOwn(TCef3Helper.CefGetObject(self)) do
    Result := GetBytesCount;
end;

function cef_post_data_element_get_bytes(self: PCefPostDataElement;
  size: NativeUInt; bytes: Pointer): NativeUInt; stdcall;
begin
  with TCefPostDataElementOwn(TCef3Helper.CefGetObject(self)) do
    Result := GetBytes(size, bytes)
end;

{ cef_v8_handler }

function cef_v8_handler_execute(self: PCefv8Handler; const name: PCefString;
  obj: PCefv8Value; argumentsCount: NativeUInt; const arguments: PPCefV8Value;
  var retval: PCefv8Value; var exception: TCefString): Integer; stdcall;
var
  args: TCefv8ValueArray;
  i: NativeInt;
  ret: ICefv8Value;
  exc: ustring;
begin
  SetLength(args, argumentsCount);
  for i := 0 to argumentsCount - 1 do
    args[i] := TCefv8ValueRef.UnWrap(arguments[i]);

  Result := -ord(TCefv8HandlerOwn(TCef3Helper.CefGetObject(self))
    .execute(TCef3Helper.CefString(name), TCefv8ValueRef.UnWrap(obj), args,
    ret, exc));
  retval := TCef3Helper.CefGetData(ret);
  ret := nil;
  exception := TCef3Helper.CefString(exc);
end;

{ cef_task }

procedure cef_task_execute(self: PCefTask); stdcall;
begin
  TCefTaskOwn(TCef3Helper.CefGetObject(self)).execute();
end;

{ cef_download_handler }

procedure cef_download_handler_on_before_download(self: PCefDownloadHandler;
  browser: PCefBrowser; download_item: PCefDownloadItem;
  const suggested_name: PCefString;
  callback: PCefBeforeDownloadCallback); stdcall;
begin
  TCefDownloadHandlerOwn(TCef3Helper.CefGetObject(self))
    .OnBeforeDownload(TCefBrowserRef.UnWrap(browser),
    TCefDownLoadItemRef.UnWrap(download_item),
    TCef3Helper.CefString(suggested_name),
    TCefBeforeDownloadCallbackRef.UnWrap(callback));
end;

procedure cef_download_handler_on_download_updated(self: PCefDownloadHandler;
  browser: PCefBrowser; download_item: PCefDownloadItem;
  callback: PCefDownloadItemCallback); stdcall;
begin
  TCefDownloadHandlerOwn(TCef3Helper.CefGetObject(self))
    .OnDownloadUpdated(TCefBrowserRef.UnWrap(browser),
    TCefDownLoadItemRef.UnWrap(download_item),
    TCefDownloadItemCallbackRef.UnWrap(callback));
end;

{ cef_dom_visitor }

procedure cef_dom_visitor_visite(self: PCefDomVisitor;
  document: PCefDomDocument); stdcall;
begin
  TCefDomVisitorOwn(TCef3Helper.CefGetObject(self))
    .visit(TCefDomDocumentRef.UnWrap(document));
end;

{ cef_v8_accessor }

function cef_v8_accessor_get(self: PCefV8Accessor; const name: PCefString;
  obj: PCefv8Value; out retval: PCefv8Value; exception: PCefString)
  : Integer; stdcall;
var
  ret: ICefv8Value;
begin
  Result := ord(TCefV8AccessorOwn(TCef3Helper.CefGetObject(self))
    .get(TCef3Helper.CefString(name), TCefv8ValueRef.UnWrap(obj), ret,
    TCef3Helper.CefString(exception)));
  retval := TCef3Helper.CefGetData(ret);
end;

function cef_v8_accessor_put(self: PCefV8Accessor; const name: PCefString;
  obj: PCefv8Value; value: PCefv8Value; exception: PCefString)
  : Integer; stdcall;
begin
  Result := ord(TCefV8AccessorOwn(TCef3Helper.CefGetObject(self))
    .put(TCef3Helper.CefString(name), TCefv8ValueRef.UnWrap(obj),
    TCefv8ValueRef.UnWrap(value), TCef3Helper.CefString(exception)));
end;

{ cef_cookie_visitor }

function cef_cookie_visitor_visit(self: PCefCookieVisitor;
  const cookie: PCefCookie; count, total: Integer; deleteCookie: PInteger)
  : Integer; stdcall;
var
  delete: Boolean;
  exp: TDateTime;
begin
  delete := False;
  if cookie.has_expires <> 0 then
    exp := TCef3Helper.CefTimeToDateTime(cookie.expires)
  else
    exp := 0;
  Result := ord(TCefCookieVisitorOwn(TCef3Helper.CefGetObject(self))
    .visit(TCef3Helper.CefString(@cookie.name),
    TCef3Helper.CefString(@cookie.value), TCef3Helper.CefString(@cookie.domain),
    TCef3Helper.CefString(@cookie.path), Boolean(cookie.secure),
    Boolean(cookie.httponly), Boolean(cookie.has_expires),
    TCef3Helper.CefTimeToDateTime(cookie.creation),
    TCef3Helper.CefTimeToDateTime(cookie.last_access), exp, count,
    total, delete));
  deleteCookie^ := ord(delete);
end;

{ cef_resource_bundle_handler }

function cef_resource_bundle_handler_get_localized_string
  (self: PCefResourceBundleHandler; string_id: Integer; string_val: PCefString)
  : Integer; stdcall;
var
  str: ustring;
begin
  Result := ord(TCefResourceBundleHandlerOwn(TCef3Helper.CefGetObject(self))
    .GetLocalizedString(string_id, str));
  if Result <> 0 then
    string_val^ := TCef3Helper.CefString(str);
end;

function cef_resource_bundle_handler_get_data_resource
  (self: PCefResourceBundleHandler; resource_id: Integer; var data: Pointer;
  var data_size: NativeUInt): Integer; stdcall;
begin
  Result := ord(TCefResourceBundleHandlerOwn(TCef3Helper.CefGetObject(self))
    .GetDataResource(resource_id, data, data_size));
end;

function cef_resource_bundle_handler_get_data_resource_for_scale(
  self: PCefResourceBundleHandler; resource_id: Integer; scale_factor: TCefScaleFactor;
  out data: Pointer; data_size: NativeUInt): Integer; stdcall;
begin
  Result := Ord(TCefResourceBundleHandlerOwn(TCef3Helper.CefGetObject(self)).
    GetDataResourceForScale(resource_id, scale_factor, data, data_size));
end;

{ cef_app }

procedure cef_app_on_before_command_line_processing(self: PCefApp;
  const process_type: PCefString; command_line: PCefCommandLine); stdcall;
begin
  with TCefAppOwn(TCef3Helper.CefGetObject(self)) do
    OnBeforeCommandLineProcessing(TCef3Helper.CefString(process_type),
      TCefCommandLineRef.UnWrap(command_line));
end;

procedure cef_app_on_register_custom_schemes(self: PCefApp;
  registrar: PCefSchemeRegistrar); stdcall;
begin
  with TCefAppOwn(TCef3Helper.CefGetObject(self)) do
    OnRegisterCustomSchemes(TCefSchemeRegistrarRef.UnWrap(registrar));
end;

function cef_app_get_resource_bundle_handler(self: PCefApp)
  : PCefResourceBundleHandler; stdcall;
begin
  Result := TCef3Helper.CefGetData(TCefAppOwn(TCef3Helper.CefGetObject(self))
    .GetResourceBundleHandler());
end;

function cef_app_get_browser_process_handler(self: PCefApp)
  : PCefBrowserProcessHandler; stdcall;
begin
  Result := TCef3Helper.CefGetData(TCefAppOwn(TCef3Helper.CefGetObject(self))
    .GetBrowserProcessHandler());
end;

function cef_app_get_render_process_handler(self: PCefApp)
  : PCefRenderProcessHandler; stdcall;
begin
  Result := TCef3Helper.CefGetData(TCefAppOwn(TCef3Helper.CefGetObject(self))
    .GetRenderProcessHandler());
end;

{ cef_string_visitor_visit }

procedure cef_string_visitor_visit(self: PCefStringVisitor;
  const str: PCefString); stdcall;
begin
  TCefStringVisitorOwn(TCef3Helper.CefGetObject(self))
    .visit(TCef3Helper.CefString(str));
end;

{ cef_browser_process_handler }

procedure cef_browser_process_handler_on_context_initialized
  (self: PCefBrowserProcessHandler); stdcall;
begin
  with TCefBrowserProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnContextInitialized;
end;

procedure cef_browser_process_handler_on_before_child_process_launch
  (self: PCefBrowserProcessHandler; command_line: PCefCommandLine); stdcall;
begin
  with TCefBrowserProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnBeforeChildProcessLaunch(TCefCommandLineRef.UnWrap(command_line));
end;

procedure cef_browser_process_handler_on_render_process_thread_created
  (self: PCefBrowserProcessHandler; extra_info: PCefListValue); stdcall;
begin
  with TCefBrowserProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnRenderProcessThreadCreated(TCefListValueRef.UnWrap(extra_info));
end;

{ cef_render_process_handler }

procedure cef_render_process_handler_on_render_thread_created
  (self: PCefRenderProcessHandler; extra_info: PCefListValue); stdcall;
begin
  with TCefRenderProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnRenderThreadCreated(TCefListValueRef.UnWrap(extra_info));
end;

procedure cef_render_process_handler_on_web_kit_initialized
  (self: PCefRenderProcessHandler); stdcall;
begin
  with TCefRenderProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnWebKitInitialized;
end;

procedure cef_render_process_handler_on_browser_created
  (self: PCefRenderProcessHandler; browser: PCefBrowser); stdcall;
begin
  with TCefRenderProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnBrowserCreated(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_render_process_handler_on_browser_destroyed
  (self: PCefRenderProcessHandler; browser: PCefBrowser); stdcall;
begin
  with TCefRenderProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnBrowserDestroyed(TCefBrowserRef.UnWrap(browser));
end;

function cef_render_process_handler_get_load_handler
  (self: PCefRenderProcessHandler): PCefLoadHandler; stdcall;
begin
  with TCefRenderProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := GetLoadHandler();
end;

function cef_render_process_handler_on_before_navigation
  (self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame;
  request: PCefRequest; navigation_type: TCefNavigationType;
  is_redirect: Integer): Integer; stdcall;
begin
  with TCefRenderProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnBeforeNavigation(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefRequestRef.UnWrap(request),
      navigation_type, is_redirect <> 0));
end;

procedure cef_render_process_handler_on_context_created
  (self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame;
  context: PCefv8Context); stdcall;
begin
  with TCefRenderProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnContextCreated(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCefv8ContextRef.UnWrap(context));
end;

procedure cef_render_process_handler_on_context_released
  (self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame;
  context: PCefv8Context); stdcall;
begin
  with TCefRenderProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnContextReleased(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefv8ContextRef.UnWrap(context));
end;

procedure cef_render_process_handler_on_uncaught_exception
  (self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame;
  context: PCefv8Context; exception: PCefV8Exception;
  stackTrace: PCefV8StackTrace); stdcall;
begin
  with TCefRenderProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnUncaughtException(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefv8ContextRef.UnWrap(context),
      TCefV8ExceptionRef.UnWrap(exception),
      TCefV8StackTraceRef.UnWrap(stackTrace));
end;

procedure cef_render_process_handler_on_focused_node_changed
  (self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame;
  node: PCefDomNode); stdcall;
begin
  with TCefRenderProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnFocusedNodeChanged(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefDomNodeRef.UnWrap(node));
end;

function cef_render_process_handler_on_process_message_received
  (self: PCefRenderProcessHandler; browser: PCefBrowser;
  source_process: TCefProcessId; message: PCefProcessMessage): Integer; stdcall;
begin
  with TCefRenderProcessHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnProcessMessageReceived(TCefBrowserRef.UnWrap(browser),
      source_process, TCefProcessMessageRef.UnWrap(message)));
end;

{ cef_url_request_client }

procedure cef_url_request_client_on_request_complete(self: PCefUrlRequestClient;
  request: PCefUrlRequest); stdcall;
begin
  with TCefUrlrequestClientOwn(TCef3Helper.CefGetObject(self)) do
    OnRequestComplete(TCefUrlRequestRef.UnWrap(request));
end;

procedure cef_url_request_client_on_upload_progress(self: PCefUrlRequestClient;
  request: PCefUrlRequest; current, total: UInt64); stdcall;
begin
  with TCefUrlrequestClientOwn(TCef3Helper.CefGetObject(self)) do
    OnUploadProgress(TCefUrlRequestRef.UnWrap(request), current, total);
end;

procedure cef_url_request_client_on_download_progress
  (self: PCefUrlRequestClient; request: PCefUrlRequest;
  current, total: UInt64); stdcall;
begin
  with TCefUrlrequestClientOwn(TCef3Helper.CefGetObject(self)) do
    OnDownloadProgress(TCefUrlRequestRef.UnWrap(request), current, total);
end;

procedure cef_url_request_client_on_download_data(self: PCefUrlRequestClient;
  request: PCefUrlRequest; const data: Pointer;
  data_length: NativeUInt); stdcall;
begin
  with TCefUrlrequestClientOwn(TCef3Helper.CefGetObject(self)) do
    OnDownloadData(TCefUrlRequestRef.UnWrap(request), data, data_length);
end;

function cef_url_request_client_get_auth_credentials(self: PCefUrlRequestClient;
  isProxy: Integer; const host: PCefString; port: Integer;
  const realm, scheme: PCefString; callback: PCefAuthCallback)
  : Integer; stdcall;
begin
  with TCefUrlrequestClientOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnGetAuthCredentials(isProxy <> 0,
      TCef3Helper.CefString(host), port, TCef3Helper.CefString(realm),
      TCef3Helper.CefString(scheme), TCefAuthCallbackRef.UnWrap(callback)));
end;

{ cef_scheme_handler_factory }

function cef_scheme_handler_factory_create(self: PCefSchemeHandlerFactory;
  browser: PCefBrowser; frame: PCefFrame; const scheme_name: PCefString;
  request: PCefRequest): PCefResourceHandler; stdcall;
begin

  with TCefSchemeHandlerFactoryOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(New(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCef3Helper.CefString(scheme_name),
      TCefRequestRef.UnWrap(request)));
end;

{ cef_resource_handler }

function cef_resource_handler_process_request(self: PCefResourceHandler;
  request: PCefRequest; callback: PCefCallback): Integer; stdcall;
begin
  with TCefResourceHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(ProcessRequest(TCefRequestRef.UnWrap(request),
      TCefCallbackRef.UnWrap(callback)));
end;

procedure cef_resource_handler_get_response_headers(self: PCefResourceHandler;
  response: PCefResponse; response_length: PInt64;
  redirectUrl: PCefString); stdcall;
var
  ru: ustring;
begin
  ru := '';
  with TCefResourceHandlerOwn(TCef3Helper.CefGetObject(self)) do
    GetResponseHeaders(TCefResponseRef.UnWrap(response), response_length^, ru);
  if ru <> '' then
    TCef3Helper.CefStringSet(redirectUrl, ru);
end;

function cef_resource_handler_read_response(self: PCefResourceHandler;
  data_out: Pointer; bytes_to_read: Integer; bytes_read: PInteger;
  callback: PCefCallback): Integer; stdcall;
begin
  with TCefResourceHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(ReadResponse(data_out, bytes_to_read, bytes_read^,
      TCefCallbackRef.UnWrap(callback)));
end;

function cef_resource_handler_can_get_cookie(self: PCefResourceHandler;
  const cookie: PCefCookie): Integer; stdcall;
begin

  with TCefResourceHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(CanGetCookie(cookie));
end;

function cef_resource_handler_can_set_cookie(self: PCefResourceHandler;
  const cookie: PCefCookie): Integer; stdcall;
begin
  with TCefResourceHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(CanSetCookie(cookie));
end;

procedure cef_resource_handler_cancel(self: PCefResourceHandler); stdcall;
begin
  with TCefResourceHandlerOwn(TCef3Helper.CefGetObject(self)) do
    cancel;
end;

{ cef_web_plugin_info_visitor }

function cef_web_plugin_info_visitor_visit(self: PCefWebPluginInfoVisitor;

  info: PCefWebPluginInfo; count, total: Integer): Integer; stdcall;
begin
  with TCefWebPluginInfoVisitorOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(visit(TCefWebPluginInfoRef.UnWrap(info), count, total));
end;

{ cef_web_plugin_unstable_callback }

procedure cef_web_plugin_unstable_callback_is_unstable
  (self: PCefWebPluginUnstableCallback; const path: PCefString;
  unstable: Integer); stdcall;
begin
  with TCefWebPluginUnstableCallbackOwn(TCef3Helper.CefGetObject(self)) do
    IsUnstable(TCef3Helper.CefString(path), unstable <> 0);
end;

{ cef_run_file_dialog_callback }

procedure cef_run_file_dialog_callback_on_file_dialog_dismissed(
  self: PCefRunFileDialogCallback; selected_accept_filter: Integer;
  file_paths: TCefStringList); stdcall;
var
  list: TStringList;
  i: Integer;
  str: TCefString;
begin
  list := TStringList.Create;
  try
    for i := 0 to cef_string_list_size(file_paths) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(file_paths, i, @str);
      list.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
    with TCefRunFileDialogCallbackOwn(TCef3Helper.CefGetObject(self)) do
      OnFileDialogDismissed(selected_accept_filter, list);
  finally
    list.Free;
  end;
end;

{ cef_end_tracing_callback }

procedure cef_end_tracing_callback_on_end_tracing_complete
  (self: PCefEndTracingCallback; const tracing_file: PCefString); stdcall;
begin
  with TCefEndTracingCallbackOwn(TCef3Helper.CefGetObject(self)) do
    OnEndTracingComplete(TCef3Helper.CefString(tracing_file));
end;

{ cef_get_geolocation_callback }

procedure cef_get_geolocation_callback_on_location_update
  (self: PCefGetGeolocationCallback; const position: PCefGeoposition); stdcall;
begin
  with TCefGetGeolocationCallbackOwn(TCef3Helper.CefGetObject(self)) do
    OnLocationUpdate(position);
end;

{ cef_dialog_handler }

function cef_dialog_handler_on_file_dialog(self: PCefDialogHandler; browser: PCefBrowser;
  mode: TCefFileDialogMode; const title, default_file_path: PCefString;
  accept_filters: TCefStringList; selected_accept_filter: Integer;
  callback: PCefFileDialogCallback): Integer; stdcall;
var
  list: TStringList;
  i: Integer;
  str: TCefString;
begin
  list := TStringList.Create;
  try
    for i := 0 to cef_string_list_size(accept_filters) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(accept_filters, i, @str);
      list.Add(TCef3Helper.CefStringClearAndGet(str));
    end;

    with TCefDialogHandlerOwn(TCef3Helper.CefGetObject(self)) do
      Result := Ord(OnFileDialog(TCefBrowserRef.UnWrap(browser), mode, TCef3Helper.CefString(title),
        TCef3Helper.CefString(default_file_path), list, selected_accept_filter,
        TCefFileDialogCallbackRef.UnWrap(callback)));
  finally
    list.Free;
  end;
end;

{ cef_render_handler }

function cef_render_handler_get_root_screen_rect(self: PCefRenderHandler;
  browser: PCefBrowser; rect: PCefRect): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(GetRootScreenRect(TCefBrowserRef.UnWrap(browser), rect));
end;

function cef_render_handler_get_view_rect(self: PCefRenderHandler;
  browser: PCefBrowser; rect: PCefRect): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(GetViewRect(TCefBrowserRef.UnWrap(browser), rect));
end;

function cef_render_handler_get_screen_point(self: PCefRenderHandler;
  browser: PCefBrowser; viewX, viewY: Integer; screenX, screenY: PInteger)
  : Integer; stdcall;
begin
  with TCefRenderHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(GetScreenPoint(TCefBrowserRef.UnWrap(browser), viewX, viewY,
      screenX, screenY));
end;

function cef_render_handler_get_screen_info(self: PCefRenderHandler;
  browser: PCefBrowser; screen_info: PCefScreenInfo): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(GetScreenInfo(TCefBrowserRef.UnWrap(browser), screen_info));
end;

procedure cef_render_handler_on_popup_show(self: PCefRenderProcessHandler;
  browser: PCefBrowser; show: Integer); stdcall;
begin
  with TCefRenderHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnPopupShow(TCefBrowserRef.UnWrap(browser), show <> 0);
end;

procedure cef_render_handler_on_popup_size(self: PCefRenderProcessHandler;
  browser: PCefBrowser; const rect: PCefRect); stdcall;
begin
  with TCefRenderHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnPopupSize(TCefBrowserRef.UnWrap(browser), rect);
end;

procedure cef_render_handler_on_paint(self: PCefRenderProcessHandler;
  browser: PCefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer;
  width, height: Integer); stdcall;
begin
  with TCefRenderHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnPaint(TCefBrowserRef.UnWrap(browser), kind, dirtyRectsCount, dirtyRects,
      buffer, width, height);
end;

procedure cef_render_handler_on_cursor_change(self: PCefRenderProcessHandler;
  browser: PCefBrowser; cursor: TCefCursorHandle; type_: TCefCursorType;
  const custom_cursor_info: PCefCursorInfo); stdcall;
begin
  with TCefRenderHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnCursorChange(TCefBrowserRef.UnWrap(browser), cursor, type_,
      custom_cursor_info);
end;

function cef_render_handler_start_dragging(self: PCefRenderProcessHandler;
  browser: PCefBrowser; drag_data: PCefDragData;
  allowed_ops: TCefDragOperations; x, y: Integer): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnStartDragging(TCefBrowserRef.UnWrap(browser),
      TCefDragDataRef.UnWrap(drag_data), allowed_ops, x, y));
end;

procedure cef_render_handler_update_drag_cursor(self: PCefRenderProcessHandler;
  browser: PCefBrowser; operation: TCefDragOperation); stdcall;
begin
  with TCefRenderHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnUpdateDragCursor(TCefBrowserRef.UnWrap(browser), operation);
end;

procedure cef_render_handler_on_scroll_offset_changed(self: PCefRenderProcessHandler;
  browser: PCefBrowser; x, y: Double); stdcall;
begin
  with TCefRenderHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnScrollOffsetChanged(TCefBrowserRef.UnWrap(browser), x, y);
end;

{ cef_completion_callback }

procedure cef_completion_callback_on_complete
  (self: PCefCompletionCallback); stdcall;
begin
  with TCefCompletionCallbackOwn(TCef3Helper.CefGetObject(self)) do
    OnComplete();
end;

{ cef_drag_handler }

function cef_drag_handler_on_drag_enter(self: PCefDragHandler;
  browser: PCefBrowser; dragData: PCefDragData; mask: TCefDragOperations)
  : Integer; stdcall;
begin
  with TCefDragHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(OnDragEnter(TCefBrowserRef.UnWrap(browser),
      TCefDragDataRef.UnWrap(dragData), mask));
end;

procedure cef_drag_handler_on_draggable_regions_changed(self: PCefDragHandler;
  browser: PCefBrowser; regionsCount: NativeUInt; regions: PCefDraggableRegionArray); stdcall;
begin
  with TCefDragHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnDraggableRegionsChanged(TCefBrowserRef.UnWrap(browser), regionsCount, regions);
end;

{ cef_find_handler }

procedure cef_find_handler_on_find_result(self: PCefFindHandler; browser: PCefBrowser; identifier,
  count: Integer; const selection_rect: PCefRect; active_match_ordinal,
  final_update: Integer); stdcall;
begin
  with TCefFindHandlerOwn(TCef3Helper.CefGetObject(self)) do
    OnFindResult(TCefBrowserRef.UnWrap(browser), identifier, count, selection_rect,
      active_match_ordinal, final_update <> 0);
end;

{ cef_request_context_handler }

function cef_request_context_handler_get_cookie_manager
  (self: PCefRequestContextHandler): PCefCookieManager; stdcall;
begin
  with TCefRequestContextHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := TCef3Helper.CefGetData(GetCookieManager());
end;

function cef_request_context_handler_on_before_plugin_load(self: PCefRequestContextHandler;
  const mime_type, plugin_url, top_origin_url: PCefString;
  plugin_info: PCefWebPluginInfo; plugin_policy: PCefPluginPolicy): Integer; stdcall;
begin
  with TCefRequestContextHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := Ord(OnBeforePluginLoad(TCef3Helper.CefString(mime_type), TCef3Helper.CefString(plugin_url),
      TCef3Helper.CefString(top_origin_url), TCefWebPluginInfoRef.UnWrap(plugin_info), plugin_policy));
end;

{ cef_write_handler_ }

function cef_write_handler_write(self: PCefWriteHandler; const ptr: Pointer;
  size, n: NativeUInt): NativeUInt; stdcall;
begin
  with TCefWriteHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := Write(ptr, size, n);
end;

function cef_write_handler_seek(self: PCefWriteHandler; offset: Int64;
  whence: Integer): Integer; stdcall;
begin
  with TCefWriteHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := seek(offset, whence);
end;

function cef_write_handler_tell(self: PCefWriteHandler): Int64; stdcall;
begin
  with TCefWriteHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := tell();
end;

function cef_write_handler_flush(self: PCefWriteHandler): Integer; stdcall;
begin
  with TCefWriteHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := flush();
end;

function cef_write_handler_may_block(self: PCefWriteHandler): Integer; stdcall;
begin
  with TCefWriteHandlerOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(MayBlock);
end;

{ cef_navigation_entry_visitor }

function cef_navigation_entry_visitor_visit(self: PCefNavigationEntryVisitor;
  entry: PCefNavigationEntry; current, index, total: Integer): Integer; stdcall;
begin
  with TCefNavigationEntryVisitorOwn(TCef3Helper.CefGetObject(self)) do
    Result := ord(visit(TCefNavigationEntryRef.UnWrap(entry), current <> 0,
      index, total));
end;

{ cef_set_cookie_callback }

procedure cef_set_cookie_callback_on_complete(self: PCefSetCookieCallback; success: Integer); stdcall;
begin
  with TCefSetCookieCallbackOwn(TCef3Helper.CefGetObject(self)) do
    OnComplete(success <> 0);
end;

{ cef_delete_cookie_callback }

procedure cef_delete_cookie_callback_on_complete(self: PCefDeleteCookiesCallback; num_deleted: Integer); stdcall;
begin
  with TCefDeleteCookiesCallbackOwn(TCef3Helper.CefGetObject(self)) do
    OnComplete(num_deleted);
end;

{ cef_pdf_print_callback }

procedure cef_pdf_print_callback_on_pdf_print_finished(self: PCefPdfPrintCallback; const path: PCefString; ok: Integer); stdcall;
begin
  with TCefPdfPrintCallbackOwn(TCef3Helper.CefGetObject(self)) do
    OnPdfPrintFinished(TCef3Helper.CefString(path), ok <> 0);
end;

{ cef_resolve_callback }

procedure cef_resolve_callback_on_resolve_completed(self: PCefResolveCallback;
  result: TCefErrorCode; resolved_ips: TCefStringList); stdcall;
var
  list: TStringList;
  i: Integer;
  str: TCefString;
begin
  list := TStringList.Create;
  try
    for i := 0 to cef_string_list_size(resolved_ips) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(resolved_ips, i, @str);
      list.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
    with TCefResolveCallbackOwn(TCef3Helper.CefGetObject(self)) do
      OnResolveCompleted(result, list);
  finally
    list.Free;
  end;
end;

{ cef_response_filter }

function cef_response_filter_init_filter(self: PCefResponseFilter): Integer; stdcall;
begin
  with TCefResponseFilterOwn(TCef3Helper.CefGetObject(self)) do
    Result := Ord(InitFilter());
end;

function cef_response_filter_filter(self: PCefResponseFilter; data_in: Pointer; data_in_size, data_in_read: NativeUInt;
  data_out: Pointer; data_out_size, data_out_written: NativeUInt): TCefResponseFilterStatus; stdcall;
begin
  with TCefResponseFilterOwn(TCef3Helper.CefGetObject(self)) do
    Result := Filter(data_in, data_in_size, data_in_read, data_out, data_out_size, data_out_written);
end;

{ TCefBaseOwn }

constructor TCefBaseOwn.CreateData(size: Cardinal; owned: Boolean);
begin
  GetMem(FData, size + SizeOf(Pointer));
  PPointer(FData)^ := self;
  Inc(PByte(FData), SizeOf(Pointer));
  FillChar(FData^, size, 0);
  PCefBase(FData)^.size := size;
  if owned then
  begin
    PCefBase(FData)^.add_ref := cef_base_add_ref_owned;
    PCefBase(FData)^.release := cef_base_release_owned;
    PCefBase(FData)^.has_one_ref := cef_base_has_one_ref_owned;
  end
  else
  begin
    PCefBase(FData)^.add_ref := cef_base_add_ref;
    PCefBase(FData)^.release := cef_base_release;
    PCefBase(FData)^.has_one_ref := cef_base_has_one_ref;
  end;
end;

destructor TCefBaseOwn.Destroy;
begin
  Dec(PByte(FData), SizeOf(Pointer));
  FreeMem(FData);
  inherited;
end;

function TCefBaseOwn.Wrap: Pointer;
begin
  Result := FData;
  if Assigned(PCefBase(FData)^.add_ref) then
    PCefBase(FData)^.add_ref(PCefBase(FData));
end;

{ TCefBaseRef }

constructor TCefBaseRef.create(data: Pointer);
begin
  Assert(data <> nil);
  FData := data;
end;

destructor TCefBaseRef.Destroy;
begin
  if Assigned(PCefBase(FData)^.release) then
    PCefBase(FData)^.release(PCefBase(FData));
  inherited;
end;

class function TCefBaseRef.UnWrap(data: Pointer): ICefBase;
begin
  if data <> nil then
    Result := create(data) as ICefBase
  else
    Result := nil;
end;

function TCefBaseRef.Wrap: Pointer;
begin
  Result := FData;
  if Assigned(PCefBase(FData)^.add_ref) then
    PCefBase(FData)^.add_ref(PCefBase(FData));
end;

{ TCefBrowserRef }

function TCefBrowserRef.GetHost: ICefBrowserHost;
begin
  Result := TCefBrowserHostRef.UnWrap(PCefBrowser(FData)
    ^.get_host(PCefBrowser(FData)));
end;

function TCefBrowserRef.canGoBack: Boolean;
begin
  Result := PCefBrowser(FData)^.can_go_back(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.canGoForward: Boolean;
begin
  Result := PCefBrowser(FData)^.can_go_forward(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.GetFocusedFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)
    ^.get_focused_frame(PCefBrowser(FData)))
end;

function TCefBrowserRef.GetFrameByident(identifier: Int64): ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)
    ^.get_frame_byident(PCefBrowser(FData), identifier));
end;

function TCefBrowserRef.GetFrame(const name: ustring): ICefFrame;
var
  n: TCefString;
begin
  n := TCef3Helper.CefString(name);
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)
    ^.get_frame(PCefBrowser(FData), @n));
end;

function TCefBrowserRef.GetFrameCount: NativeUInt;
begin
  Result := PCefBrowser(FData)^.get_frame_count(PCefBrowser(FData));
end;

procedure TCefBrowserRef.GetFrameIdentifiers(count: PNativeUInt;
  identifiers: PInt64);
begin
  PCefBrowser(FData)^.get_frame_identifiers(PCefBrowser(FData), count,
    identifiers);
end;

procedure TCefBrowserRef.GetFrameNames(names: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefBrowser(FData)^.get_frame_names(PCefBrowser(FData), list);
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      names.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefBrowserRef.SendProcessMessage(targetProcess: TCefProcessId;
  message: ICefProcessMessage): Boolean;
begin
  Result := PCefBrowser(FData)^.send_process_message(PCefBrowser(FData),
    targetProcess, TCef3Helper.CefGetData(message)) <> 0;
end;

function TCefBrowserRef.GetMainFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)
    ^.get_main_frame(PCefBrowser(FData)))
end;

procedure TCefBrowserRef.GoBack;
begin
  PCefBrowser(FData)^.go_back(PCefBrowser(FData));
end;

procedure TCefBrowserRef.GoForward;
begin
  PCefBrowser(FData)^.go_forward(PCefBrowser(FData));
end;

function TCefBrowserRef.isLoading: Boolean;
begin
  Result := PCefBrowser(FData)^.is_loading(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.HasDocument: Boolean;
begin
  Result := PCefBrowser(FData)^.has_document(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.IsPopup: Boolean;
begin
  Result := PCefBrowser(FData)^.is_popup(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.IsSame(const that: ICefBrowser): Boolean;
begin
  Result := PCefBrowser(FData)^.is_same(PCefBrowser(FData),
    TCef3Helper.CefGetData(that)) <> 0;
end;

procedure TCefBrowserRef.reload;
begin
  PCefBrowser(FData)^.reload(PCefBrowser(FData));
end;

procedure TCefBrowserRef.ReloadIgnoreCache;
begin
  PCefBrowser(FData)^.reload_ignore_cache(PCefBrowser(FData));
end;

procedure TCefBrowserRef.StopLoad;
begin
  PCefBrowser(FData)^.stop_load(PCefBrowser(FData));
end;

function TCefBrowserRef.GetIdentifier: Integer;
begin
  Result := PCefBrowser(FData)^.get_identifier(PCefBrowser(FData));
end;

class function TCefBrowserRef.UnWrap(data: Pointer): ICefBrowser;
begin
  if data <> nil then
    Result := create(data) as ICefBrowser
  else
    Result := nil;
end;

{ TCefFrameRef }

function TCefFrameRef.IsValid: Boolean;
begin
  Result := PCefFrame(FData)^.is_valid(PCefFrame(FData)) <> 0;
end;

procedure TCefFrameRef.copy;
begin
  PCefFrame(FData)^.copy(PCefFrame(FData));
end;

procedure TCefFrameRef.cut;
begin
  PCefFrame(FData)^.cut(PCefFrame(FData));
end;

procedure TCefFrameRef.del;
begin
  PCefFrame(FData)^.del(PCefFrame(FData));
end;

procedure TCefFrameRef.ExecuteJavaScript(const code, scriptUrl: ustring;
  startLine: Integer);
var
  j, s: TCefString;
begin
  j := TCef3Helper.CefString(code);
  s := TCef3Helper.CefString(scriptUrl);
  PCefFrame(FData)^.execute_java_script(PCefFrame(FData), @j, @s, startLine);
end;

function TCefFrameRef.GetBrowser: ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefFrame(FData)
    ^.get_browser(PCefFrame(FData)));
end;

function TCefFrameRef.GetIdentifier: Int64;
begin
  Result := PCefFrame(FData)^.get_identifier(PCefFrame(FData));
end;

function TCefFrameRef.GetName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet
    (PCefFrame(FData)^.get_name(PCefFrame(FData)));
end;

function TCefFrameRef.GetParent: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefFrame(FData)^.get_parent(PCefFrame(FData)));
end;

procedure TCefFrameRef.GetSource(const visitor: ICefStringVisitor);
begin
  PCefFrame(FData)^.get_source(PCefFrame(FData),
    TCef3Helper.CefGetData(visitor));
end;

procedure TCefFrameRef.GetSourceProc(const proc: TCefStringVisitorProc);
begin
  GetSource(TCefFastStringVisitor.create(proc));
end;

procedure TCefFrameRef.GetText(const visitor: ICefStringVisitor);
begin
  PCefFrame(FData)^.get_text(PCefFrame(FData), TCef3Helper.CefGetData(visitor));
end;

procedure TCefFrameRef.GetTextProc(const proc: TCefStringVisitorProc);
begin
  GetText(TCefFastStringVisitor.create(proc));
end;

function TCefFrameRef.GetUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet
    (PCefFrame(FData)^.get_url(PCefFrame(FData)));
end;

function TCefFrameRef.GetV8Context: ICefv8Context;
begin
  Result := TCefv8ContextRef.UnWrap(PCefFrame(FData)
    ^.get_v8context(PCefFrame(FData)));
end;

function TCefFrameRef.IsFocused: Boolean;
begin
  Result := PCefFrame(FData)^.is_focused(PCefFrame(FData)) <> 0;
end;

function TCefFrameRef.IsMain: Boolean;
begin
  Result := PCefFrame(FData)^.is_main(PCefFrame(FData)) <> 0;
end;

procedure TCefFrameRef.LoadRequest(const request: ICefRequest);
begin
  PCefFrame(FData)^.load_request(PCefFrame(FData),
    TCef3Helper.CefGetData(request));
end;

procedure TCefFrameRef.LoadString(const str, url: ustring);
var
  s, u: TCefString;
begin
  s := TCef3Helper.CefString(str);
  u := TCef3Helper.CefString(url);
  PCefFrame(FData)^.load_string(PCefFrame(FData), @s, @u);
end;

procedure TCefFrameRef.LoadUrl(const url: ustring);
var
  u: TCefString;
begin
  u := TCef3Helper.CefString(url);
  PCefFrame(FData)^.load_url(PCefFrame(FData), @u);

end;

procedure TCefFrameRef.paste;
begin
  PCefFrame(FData)^.paste(PCefFrame(FData));
end;

procedure TCefFrameRef.redo;
begin
  PCefFrame(FData)^.redo(PCefFrame(FData));
end;

procedure TCefFrameRef.SelectAll;
begin
  PCefFrame(FData)^.select_all(PCefFrame(FData));
end;

procedure TCefFrameRef.undo;
begin
  PCefFrame(FData)^.undo(PCefFrame(FData));
end;

procedure TCefFrameRef.ViewSource;
begin
  PCefFrame(FData)^.view_source(PCefFrame(FData));
end;

procedure TCefFrameRef.VisitDom(const visitor: ICefDomVisitor);
begin
  PCefFrame(FData)^.visit_dom(PCefFrame(FData),
    TCef3Helper.CefGetData(visitor));
end;

procedure TCefFrameRef.VisitDomProc(const proc: TCefDomVisitorProc);
begin
  VisitDom(TCefFastDomVisitor.create(proc) as ICefDomVisitor);
end;

class function TCefFrameRef.UnWrap(data: Pointer): ICefFrame;
begin
  if data <> nil then
    Result := create(data) as ICefFrame
  else
    Result := nil;
end;

{ TCefCustomStreamReader }

constructor TCefCustomStreamReader.create(stream: TSTream; owned: Boolean);
begin
  inherited CreateData(SizeOf(TCefReadHandler));
  FStream := stream;
  FOwned := owned;
  with PCefReadHandler(FData)^ do
  begin
    read := cef_stream_reader_read;
    seek := cef_stream_reader_seek;
    tell := cef_stream_reader_tell;
    eof := cef_stream_reader_eof;
    may_block := cef_stream_reader_may_block;
  end;
end;

constructor TCefCustomStreamReader.create(const fileName: string);
begin
  create(TFileStream.create(fileName, fmOpenRead or fmShareDenyWrite), True);
end;

destructor TCefCustomStreamReader.Destroy;
begin
  if FOwned then
    FStream.Free;
  inherited;
end;

function TCefCustomStreamReader.eof: Boolean;
begin
  Result := FStream.position = FStream.size;
end;

function TCefCustomStreamReader.MayBlock: Boolean;
begin
  Result := False;
end;

function TCefCustomStreamReader.read(ptr: Pointer; size, n: NativeUInt)
  : NativeUInt;
begin
  Result := NativeUInt(FStream.read(ptr^, n * size)) div size;
end;

function TCefCustomStreamReader.seek(offset: Int64; whence: Integer): Integer;
begin
  Result := FStream.seek(offset, TSeekOrigin(whence));
end;

function TCefCustomStreamReader.tell: Int64;
begin
  Result := FStream.position;
end;

{ TCefPostDataRef }

function TCefPostDataRef.IsReadOnly: Boolean;
begin
  Result := PCefPostData(FData)^.is_read_only(PCefPostData(FData)) <> 0;
end;

function TCefPostDataRef.AddElement(const element: ICefPostDataElement)
  : Integer;
begin
  Result := PCefPostData(FData)^.add_element(PCefPostData(FData),
    TCef3Helper.CefGetData(element));
end;

function TCefPostDataRef.GetCount: NativeUInt;
begin
  Result := PCefPostData(FData)^.get_element_count(PCefPostData(FData))
end;

function TCefPostDataRef.GetElements(count: NativeUInt): IInterfaceList;
var
  items: PCefPostDataElementArray;
  i: Integer;
begin
  Result := TInterfaceList.create;
  GetMem(items, SizeOf(PCefPostDataElement) * count);
  FillChar(items^, SizeOf(PCefPostDataElement) * count, 0);
  try
    PCefPostData(FData)^.get_elements(PCefPostData(FData), @count, items);
    for i := 0 to count - 1 do
      Result.Add(TCefPostDataElementRef.UnWrap(items[i]));
  finally
    FreeMem(items);
  end;
end;

function TCefPostDataRef.HasExcludedElements: Boolean;
begin
  Result := PCefPostData(FData)^.has_excluded_elements(PCefPostData(FData)) <> 0;
end;

class function TCefPostDataRef.New: ICefPostData;
begin
  Result := UnWrap(cef_post_data_create);
end;

function TCefPostDataRef.RemoveElement(const element
  : ICefPostDataElement): Integer;
begin
  Result := PCefPostData(FData)^.remove_element(PCefPostData(FData),
    TCef3Helper.CefGetData(element));
end;

procedure TCefPostDataRef.RemoveElements;
begin
  PCefPostData(FData)^.remove_elements(PCefPostData(FData));
end;

class function TCefPostDataRef.UnWrap(data: Pointer): ICefPostData;
begin
  if data <> nil then
    Result := create(data) as ICefPostData
  else
    Result := nil;
end;

{ TCefPostDataElementRef }

function TCefPostDataElementRef.IsReadOnly: Boolean;
begin
  Result := PCefPostDataElement(FData)
    ^.is_read_only(PCefPostDataElement(FData)) <> 0;
end;

function TCefPostDataElementRef.GetBytes(size: NativeUInt; bytes: Pointer)
  : NativeUInt;
begin
  Result := PCefPostDataElement(FData)^.get_bytes(PCefPostDataElement(FData),
    size, bytes);
end;

function TCefPostDataElementRef.GetBytesCount: NativeUInt;
begin
  Result := PCefPostDataElement(FData)^.get_bytes_count
    (PCefPostDataElement(FData));
end;

function TCefPostDataElementRef.GetFile: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefPostDataElement(FData)
    ^.get_file(PCefPostDataElement(FData)));
end;

function TCefPostDataElementRef.GetType: TCefPostDataElementType;
begin
  Result := PCefPostDataElement(FData)^.get_type(PCefPostDataElement(FData));
end;

class function TCefPostDataElementRef.New: ICefPostDataElement;
begin
  Result := UnWrap(cef_post_data_element_create);
end;

procedure TCefPostDataElementRef.SetToBytes(size: NativeUInt; bytes: Pointer);
begin
  PCefPostDataElement(FData)^.set_to_bytes(PCefPostDataElement(FData),
    size, bytes);
end;

procedure TCefPostDataElementRef.SetToEmpty;
begin
  PCefPostDataElement(FData)^.set_to_empty(PCefPostDataElement(FData));
end;

procedure TCefPostDataElementRef.SetToFile(const fileName: ustring);
var
  f: TCefString;
begin
  f := TCef3Helper.CefString(fileName);
  PCefPostDataElement(FData)^.set_to_file(PCefPostDataElement(FData), @f);
end;

class function TCefPostDataElementRef.UnWrap(data: Pointer)
  : ICefPostDataElement;
begin
  if data <> nil then
    Result := create(data) as ICefPostDataElement
  else
    Result := nil;
end;

{ TCefPostDataElementOwn }

procedure TCefPostDataElementOwn.clear;
begin
  case FDataType of
    PDE_TYPE_BYTES:
      if (FValueByte <> nil) then
      begin
        FreeMem(FValueByte);
        FValueByte := nil;
      end;
    PDE_TYPE_FILE:
      TCef3Helper.CefStringFree(@FValueStr)
  end;
  FDataType := PDE_TYPE_EMPTY;
  FSize := 0;
end;

constructor TCefPostDataElementOwn.create(readonly: Boolean);
begin
  inherited CreateData(SizeOf(TCefPostDataElement));
  FReadOnly := readonly;
  FDataType := PDE_TYPE_EMPTY;
  FValueByte := nil;
  FillChar(FValueStr, SizeOf(FValueStr), 0);
  FSize := 0;
  with PCefPostDataElement(FData)^ do
  begin
    is_read_only := cef_post_data_element_is_read_only;
    set_to_empty := cef_post_data_element_set_to_empty;
    set_to_file := cef_post_data_element_set_to_file;
    set_to_bytes := cef_post_data_element_set_to_bytes;
    get_type := cef_post_data_element_get_type;
    get_file := cef_post_data_element_get_file;
    get_bytes_count := cef_post_data_element_get_bytes_count;
    get_bytes := cef_post_data_element_get_bytes;
  end;
end;

function TCefPostDataElementOwn.GetBytes(size: NativeUInt; bytes: Pointer)
  : NativeUInt;
begin
  if (FDataType = PDE_TYPE_BYTES) and (FValueByte <> nil) then
  begin
    if size > FSize then
      Result := FSize
    else
      Result := size;
    Move(FValueByte^, bytes^, Result);
  end
  else
    Result := 0;
end;

function TCefPostDataElementOwn.GetBytesCount: NativeUInt;
begin
  if (FDataType = PDE_TYPE_BYTES) then
    Result := FSize
  else
    Result := 0;
end;

function TCefPostDataElementOwn.GetFile: ustring;
begin
  if (FDataType = PDE_TYPE_FILE) then
    Result := TCef3Helper.CefString(@FValueStr)
  else
    Result := '';
end;

function TCefPostDataElementOwn.GetType: TCefPostDataElementType;
begin
  Result := FDataType;
end;

function TCefPostDataElementOwn.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TCefPostDataElementOwn.SetToBytes(size: NativeUInt; bytes: Pointer);
begin
  clear;
  if (size > 0) and (bytes <> nil) then
  begin
    GetMem(FValueByte, size);
    Move(bytes^, FValueByte, size);
    FSize := size;
  end
  else
  begin
    FValueByte := nil;
    FSize := 0;
  end;
  FDataType := PDE_TYPE_BYTES;
end;

procedure TCefPostDataElementOwn.SetToEmpty;
begin
  clear;
end;

procedure TCefPostDataElementOwn.SetToFile(const fileName: ustring);
begin
  clear;
  FSize := 0;
  FValueStr := TCef3Helper.CefStringAlloc(fileName);
  FDataType := PDE_TYPE_FILE;
end;

{ TCefRequestRef }

function TCefRequestRef.IsReadOnly: Boolean;
begin
  Result := PCefRequest(FData).is_read_only(PCefRequest(FData)) <> 0;
end;

procedure TCefRequestRef.Assign(const url, method: ustring;
  const postData: ICefPostData; const headerMap: ICefStringMultimap);
var
  u, m: TCefString;
begin
  u := TCef3Helper.CefString(url);
  m := TCef3Helper.CefString(method);
  PCefRequest(FData).set_(PCefRequest(FData), @u, @m,
    TCef3Helper.CefGetData(postData), headerMap.Handle);
end;

function TCefRequestRef.GetFirstPartyForCookies: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefRequest(FData)
    .get_first_party_for_cookies(PCefRequest(FData)));
end;

function TCefRequestRef.GetFlags: TCefUrlRequestFlags;
begin
  Byte(Result) := PCefRequest(FData)^.get_flags(PCefRequest(FData));
end;

procedure TCefRequestRef.GetHeaderMap(const headerMap: ICefStringMultimap);
begin
  PCefRequest(FData)^.get_header_map(PCefRequest(FData), headerMap.Handle);
end;

function TCefRequestRef.GetIdentifier: UInt64;
begin
  Result := PCefRequest(FData)^.get_identifier(PCefRequest(FData));
end;

function TCefRequestRef.GetMethod: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefRequest(FData)
    ^.get_method(PCefRequest(FData)))
end;

function TCefRequestRef.GetPostData: ICefPostData;
begin
  Result := TCefPostDataRef.UnWrap(PCefRequest(FData)
    ^.get_post_data(PCefRequest(FData)));
end;

function TCefRequestRef.GetReferrerPolicy: TCefReferrerPolicy;
begin
  Result := PCefRequest(FData)^.get_referrer_policy(PCefRequest(FData));
end;

function TCefRequestRef.GetReferrerUrl: string;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefRequest(FData)^.get_referrer_url(PCefRequest(FData)));
end;

function TCefRequestRef.GetResourceType: TCefResourceType;
begin
  Result := PCefRequest(FData).get_resource_type(FData);
end;

function TCefRequestRef.GetTransitionType: TCefTransitionType;
begin
  Result := PCefRequest(FData).get_transition_type(FData);
end;

function TCefRequestRef.GetUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefRequest(FData)
    ^.get_url(PCefRequest(FData)))
end;

class function TCefRequestRef.New: ICefRequest;
begin
  Result := UnWrap(cef_request_create);
end;

procedure TCefRequestRef.SetFirstPartyForCookies(const url: ustring);
var
  str: TCefString;
begin
  str := TCef3Helper.CefString(url);
  PCefRequest(FData).set_first_party_for_cookies(PCefRequest(FData), @str);
end;

procedure TCefRequestRef.SetFlags(flags: TCefUrlRequestFlags);
begin
  PCefRequest(FData)^.set_flags(PCefRequest(FData), PByte(@flags)^);
end;

procedure TCefRequestRef.SetHeaderMap(const headerMap: ICefStringMultimap);
begin
  PCefRequest(FData)^.set_header_map(PCefRequest(FData), headerMap.Handle);
end;

procedure TCefRequestRef.SetMethod(const value: ustring);
var
  v: TCefString;
begin
  v := TCef3Helper.CefString(value);
  PCefRequest(FData)^.set_method(PCefRequest(FData), @v);
end;

procedure TCefRequestRef.SetPostData(const value: ICefPostData);
begin
  if value <> nil then
    PCefRequest(FData)^.set_post_data(PCefRequest(FData),
      TCef3Helper.CefGetData(value));
end;

procedure TCefRequestRef.SetReferrer(const referrerUrl: string;
  policy: TCefReferrerPolicy);
var
  u: TCefString;
begin
  u := TCef3Helper.CefString(referrerUrl);
  PCefRequest(FData)^.set_referrer(PCefRequest(FData), @u, policy);
end;

procedure TCefRequestRef.SetUrl(const value: ustring);
var
  v: TCefString;
begin
  v := TCef3Helper.CefString(value);
  PCefRequest(FData)^.set_url(PCefRequest(FData), @v);
end;

class function TCefRequestRef.UnWrap(data: Pointer): ICefRequest;
begin
  if data <> nil then
    Result := create(data) as ICefRequest
  else
    Result := nil;
end;

{ TCefStreamReaderRef }

class function TCefStreamReaderRef.CreateForCustomStream
  (const stream: ICefCustomStreamReader): ICefStreamReader;
begin
  Result := UnWrap(cef_stream_reader_create_for_handler
    (TCef3Helper.CefGetData(stream)))
end;

class function TCefStreamReaderRef.CreateForData(data: Pointer;
  size: NativeUInt): ICefStreamReader;
begin
  Result := UnWrap(cef_stream_reader_create_for_data(data, size))
end;

class function TCefStreamReaderRef.CreateForFile(const fileName: ustring)
  : ICefStreamReader;
var
  f: TCefString;
begin
  f := TCef3Helper.CefString(fileName);
  Result := UnWrap(cef_stream_reader_create_for_file(@f))
end;

class function TCefStreamReaderRef.CreateForStream(const stream: TSTream;
  owned: Boolean): ICefStreamReader;
begin
  Result := CreateForCustomStream(TCefCustomStreamReader.create(stream, owned)
    as ICefCustomStreamReader);
end;

function TCefStreamReaderRef.eof: Boolean;
begin
  Result := PCefStreamReader(FData)^.eof(PCefStreamReader(FData)) <> 0;
end;

function TCefStreamReaderRef.MayBlock: Boolean;
begin
  Result := PCefStreamReader(FData)^.may_block(FData) <> 0;
end;

function TCefStreamReaderRef.read(ptr: Pointer; size, n: NativeUInt)
  : NativeUInt;
begin
  Result := PCefStreamReader(FData)^.read(PCefStreamReader(FData), ptr,
    size, n);
end;

function TCefStreamReaderRef.seek(offset: Int64; whence: Integer): Integer;
begin
  Result := PCefStreamReader(FData)^.seek(PCefStreamReader(FData),
    offset, whence);
end;

function TCefStreamReaderRef.tell: Int64;
begin
  Result := PCefStreamReader(FData)^.tell(PCefStreamReader(FData));
end;

class function TCefStreamReaderRef.UnWrap(data: Pointer): ICefStreamReader;
begin
  if data <> nil then
    Result := create(data) as ICefStreamReader
  else
    Result := nil;
end;

{ TCefv8ValueRef }

function TCefv8ValueRef.AdjustExternallyAllocatedMemory(changeInBytes
  : Integer): Integer;
begin
  Result := PCefv8Value(FData)^.adjust_externally_allocated_memory
    (PCefv8Value(FData), changeInBytes);
end;

class function TCefv8ValueRef.NewArray(len: Integer): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_array(len));
end;

class function TCefv8ValueRef.NewBool(value: Boolean): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_bool(ord(value)));
end;

class function TCefv8ValueRef.NewDate(value: TDateTime): ICefv8Value;
var
  dt: TCefTime;
begin
  dt := TCef3Helper.DateTimeToCefTime(value);
  Result := UnWrap(cef_v8value_create_date(@dt));
end;

class function TCefv8ValueRef.NewDouble(value: Double): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_double(value));
end;

class function TCefv8ValueRef.NewFunction(const name: ustring;
  const handler: ICefv8Handler): ICefv8Value;
var
  n: TCefString;
begin
  n := TCef3Helper.CefString(name);
  Result := UnWrap(cef_v8value_create_function(@n,
    TCef3Helper.CefGetData(handler)));
end;

class function TCefv8ValueRef.NewInt(value: Integer): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_int(value));
end;

class function TCefv8ValueRef.NewUInt(value: Cardinal): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_uint(value));
end;

class function TCefv8ValueRef.NewNull: ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_null);
end;

class function TCefv8ValueRef.NewObject(const Accessor: ICefV8Accessor)
  : ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_object(TCef3Helper.CefGetData(Accessor)));
end;

class function TCefv8ValueRef.NewObjectProc(const getter
  : TCefV8AccessorGetterProc; const setter: TCefV8AccessorSetterProc)
  : ICefv8Value;
begin
  Result := NewObject(TCefFastV8Accessor.create(getter, setter)
    as ICefV8Accessor);
end;

class function TCefv8ValueRef.NewString(const str: ustring): ICefv8Value;
var
  s: TCefString;
begin
  s := TCef3Helper.CefString(str);
  Result := UnWrap(cef_v8value_create_string(@s));
end;

class function TCefv8ValueRef.NewUndefined: ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_undefined);
end;

function TCefv8ValueRef.DeleteValueByIndex(index: Integer): Boolean;
begin
  Result := PCefv8Value(FData)^.delete_value_byindex(PCefv8Value(FData),
    index) <> 0;
end;

function TCefv8ValueRef.DeleteValueByKey(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefv8Value(FData)^.delete_value_bykey(PCefv8Value(FData), @k) <> 0;
end;

function TCefv8ValueRef.ExecuteFunction(const obj: ICefv8Value;
  const arguments: TCefv8ValueArray): ICefv8Value;
var
  args: PPCefV8Value;
  i: Integer;
begin
  GetMem(args, SizeOf(PCefv8Value) * length(arguments));
  try
    for i := 0 to length(arguments) - 1 do
      args[i] := TCef3Helper.CefGetData(arguments[i]);
    Result := TCefv8ValueRef.UnWrap(PCefv8Value(FData)
      ^.execute_function(PCefv8Value(FData), TCef3Helper.CefGetData(obj),
      length(arguments), args));
  finally
    FreeMem(args);
  end;
end;

function TCefv8ValueRef.ExecuteFunctionWithContext(const context: ICefv8Context;
  const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;
var
  args: PPCefV8Value;
  i: Integer;
begin
  GetMem(args, SizeOf(PCefv8Value) * length(arguments));
  try
    for i := 0 to length(arguments) - 1 do
      args[i] := TCef3Helper.CefGetData(arguments[i]);
    Result := TCefv8ValueRef.UnWrap(PCefv8Value(FData)
      ^.execute_function_with_context(PCefv8Value(FData),
      TCef3Helper.CefGetData(context), TCef3Helper.CefGetData(obj),
      length(arguments), args));
  finally
    FreeMem(args);
  end;
end;

function TCefv8ValueRef.GetArrayLength: Integer;
begin
  Result := PCefv8Value(FData)^.get_array_length(PCefv8Value(FData));
end;

function TCefv8ValueRef.GetBoolValue: Boolean;
begin
  Result := PCefv8Value(FData)^.get_bool_value(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.GetDateValue: TDateTime;
begin
  Result := TCef3Helper.CefTimeToDateTime(PCefv8Value(FData)
    ^.get_date_value(PCefv8Value(FData)));
end;

function TCefv8ValueRef.GetDoubleValue: Double;
begin
  Result := PCefv8Value(FData)^.get_double_value(PCefv8Value(FData));
end;

function TCefv8ValueRef.GetExternallyAllocatedMemory: Integer;
begin
  Result := PCefv8Value(FData)^.get_externally_allocated_memory
    (PCefv8Value(FData));
end;

function TCefv8ValueRef.GetFunctionHandler: ICefv8Handler;
begin
  Result := TCefv8HandlerRef.UnWrap(PCefv8Value(FData)^.get_function_handler
    (PCefv8Value(FData)));
end;

function TCefv8ValueRef.GetFunctionName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefv8Value(FData)
    ^.get_function_name(PCefv8Value(FData)))
end;

function TCefv8ValueRef.GetIntValue: Integer;
begin
  Result := PCefv8Value(FData)^.get_int_value(PCefv8Value(FData))
end;

function TCefv8ValueRef.GetUIntValue: Cardinal;
begin
  Result := PCefv8Value(FData)^.get_uint_value(PCefv8Value(FData))
end;

function TCefv8ValueRef.GetKeys(const keys: TStrings): Integer;
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    Result := PCefv8Value(FData)^.get_keys(PCefv8Value(FData), list);
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      keys.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefv8ValueRef.SetUserData(const data: ICefv8Value): Boolean;
begin
  Result := PCefv8Value(FData)^.set_user_data(PCefv8Value(FData),
    TCef3Helper.CefGetData(data)) <> 0;
end;

function TCefv8ValueRef.GetStringValue: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefv8Value(FData)
    ^.get_string_value(PCefv8Value(FData)));
end;

function TCefv8ValueRef.IsUserCreated: Boolean;
begin
  Result := PCefv8Value(FData)^.is_user_created(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsValid: Boolean;
begin
  Result := PCefv8Value(FData)^.is_valid(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.HasException: Boolean;
begin
  Result := PCefv8Value(FData)^.has_exception(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.GetException: ICefV8Exception;
begin
  Result := TCefV8ExceptionRef.UnWrap(PCefv8Value(FData)
    ^.get_exception(PCefv8Value(FData)));
end;

function TCefv8ValueRef.ClearException: Boolean;
begin
  Result := PCefv8Value(FData)^.clear_exception(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.WillRethrowExceptions: Boolean;
begin
  Result := PCefv8Value(FData)^.will_rethrow_exceptions
    (PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.SetRethrowExceptions(rethrow: Boolean): Boolean;
begin
  Result := PCefv8Value(FData)^.set_rethrow_exceptions(PCefv8Value(FData),
    ord(rethrow)) <> 0;
end;

function TCefv8ValueRef.GetUserData: ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefv8Value(FData)
    ^.get_user_data(PCefv8Value(FData)));
end;

function TCefv8ValueRef.GetValueByIndex(index: Integer): ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefv8Value(FData)
    ^.get_value_byindex(PCefv8Value(FData), index))
end;

function TCefv8ValueRef.GetValueByKey(const key: ustring): ICefv8Value;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := TCefv8ValueRef.UnWrap(PCefv8Value(FData)
    ^.get_value_bykey(PCefv8Value(FData), @k))
end;

function TCefv8ValueRef.HasValueByIndex(index: Integer): Boolean;
begin
  Result := PCefv8Value(FData)^.has_value_byindex(PCefv8Value(FData),
    index) <> 0;
end;

function TCefv8ValueRef.HasValueByKey(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefv8Value(FData)^.has_value_bykey(PCefv8Value(FData), @k) <> 0;
end;

function TCefv8ValueRef.IsArray: Boolean;
begin
  Result := PCefv8Value(FData)^.is_array(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsBool: Boolean;
begin
  Result := PCefv8Value(FData)^.is_bool(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsDate: Boolean;
begin
  Result := PCefv8Value(FData)^.is_date(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsDouble: Boolean;
begin
  Result := PCefv8Value(FData)^.is_double(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsFunction: Boolean;
begin
  Result := PCefv8Value(FData)^.is_function(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsInt: Boolean;
begin
  Result := PCefv8Value(FData)^.is_int(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsUInt: Boolean;
begin
  Result := PCefv8Value(FData)^.is_uint(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsNull: Boolean;
begin
  Result := PCefv8Value(FData)^.is_null(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsObject: Boolean;
begin
  Result := PCefv8Value(FData)^.is_object(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsSame(const that: ICefv8Value): Boolean;
begin
  Result := PCefv8Value(FData)^.is_same(PCefv8Value(FData),
    TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefv8ValueRef.IsString: Boolean;
begin
  Result := PCefv8Value(FData)^.is_string(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsUndefined: Boolean;
begin
  Result := PCefv8Value(FData)^.is_undefined(PCefv8Value(FData)) <> 0;
end;

function TCefv8ValueRef.SetValueByAccessor(const key: ustring;
  settings: TCefV8AccessControls; attribute: TCefV8PropertyAttributes): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefv8Value(FData)^.set_value_byaccessor(PCefv8Value(FData), @k,
    PByte(@settings)^, PByte(@attribute)^) <> 0;
end;

function TCefv8ValueRef.SetValueByIndex(index: Integer;
  const value: ICefv8Value): Boolean;
begin
  Result := PCefv8Value(FData)^.set_value_byindex(PCefv8Value(FData), index,
    TCef3Helper.CefGetData(value)) <> 0;
end;

function TCefv8ValueRef.SetValueByKey(const key: ustring;
  const value: ICefv8Value; attribute: TCefV8PropertyAttributes): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefv8Value(FData)^.set_value_bykey(PCefv8Value(FData), @k,
    TCef3Helper.CefGetData(value), PByte(@attribute)^) <> 0;
end;

class function TCefv8ValueRef.UnWrap(data: Pointer): ICefv8Value;
begin
  if data <> nil then
    Result := create(data) as ICefv8Value
  else
    Result := nil;
end;

{ TCefv8HandlerRef }

function TCefv8HandlerRef.execute(const name: ustring; const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefv8Value;
  var exception: ustring): Boolean;
var
  args: array of PCefv8Value;
  i: Integer;
  ret: PCefv8Value;
  exc: TCefString;
  n: TCefString;
begin
  SetLength(args, length(arguments));
  for i := 0 to length(arguments) - 1 do
    args[i] := TCef3Helper.CefGetData(arguments[i]);
  ret := nil;
  FillChar(exc, SizeOf(exc), 0);
  n := TCef3Helper.CefString(name);
  Result := PCefv8Handler(FData)^.execute(PCefv8Handler(FData), @n,
    TCef3Helper.CefGetData(obj), length(arguments), @args, ret, exc) <> 0;
  retval := TCefv8ValueRef.UnWrap(ret);
  exception := TCef3Helper.CefStringClearAndGet(exc);
end;

class function TCefv8HandlerRef.UnWrap(data: Pointer): ICefv8Handler;
begin
  if data <> nil then
    Result := create(data) as ICefv8Handler
  else
    Result := nil;
end;

{ TCefv8HandlerOwn }

constructor TCefv8HandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefv8Handler));
  with PCefv8Handler(FData)^ do
    execute := cef_v8_handler_execute;
end;

function TCefv8HandlerOwn.execute(const name: ustring; const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefv8Value;
  var exception: ustring): Boolean;
begin
  Result := False;
end;

{ TCefTaskOwn }

constructor TCefTaskOwn.create;
begin
  inherited CreateData(SizeOf(TCefTask));
  with PCefTask(FData)^ do
    execute := cef_task_execute;
end;

procedure TCefTaskOwn.execute;
begin

end;

{ TCefStringMapOwn }

procedure TCefStringMapOwn.Append(const key, value: ustring);
var
  k, v: TCefString;
begin
  k := TCef3Helper.CefString(key);
  v := TCef3Helper.CefString(value);
  cef_string_map_append(FStringMap, @k, @v);
end;

procedure TCefStringMapOwn.clear;
begin
  cef_string_map_clear(FStringMap);
end;

constructor TCefStringMapOwn.create;
begin
  FStringMap := cef_string_map_alloc;
end;

destructor TCefStringMapOwn.Destroy;
begin
  cef_string_map_free(FStringMap);
end;

function TCefStringMapOwn.find(const key: ustring): ustring;
var
  str, k: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  k := TCef3Helper.CefString(key);
  cef_string_map_find(FStringMap, @k, str);
  Result := TCef3Helper.CefString(@str);
end;

function TCefStringMapOwn.GetHandle: TCefStringMap;
begin
  Result := FStringMap;
end;

function TCefStringMapOwn.GetKey(index: Integer): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_map_key(FStringMap, index, str);
  Result := TCef3Helper.CefString(@str);
end;

function TCefStringMapOwn.GetSize: Integer;
begin
  Result := cef_string_map_size(FStringMap);
end;

function TCefStringMapOwn.GetValue(index: Integer): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_map_value(FStringMap, index, str);
  Result := TCef3Helper.CefString(@str);
end;

{ TCefStringMultimapOwn }

procedure TCefStringMultimapOwn.Append(const key, value: ustring);
var
  k, v: TCefString;
begin
  k := TCef3Helper.CefString(key);
  v := TCef3Helper.CefString(value);
  cef_string_multimap_append(FStringMap, @k, @v);
end;

procedure TCefStringMultimapOwn.clear;
begin
  cef_string_multimap_clear(FStringMap);
end;

constructor TCefStringMultimapOwn.create;
begin
  FStringMap := cef_string_multimap_alloc;
end;

destructor TCefStringMultimapOwn.Destroy;
begin
  cef_string_multimap_free(FStringMap);
  inherited;
end;

function TCefStringMultimapOwn.FindCount(const key: ustring): Integer;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := cef_string_multimap_find_count(FStringMap, @k);
end;

function TCefStringMultimapOwn.GetEnumerate(const key: ustring;
  ValueIndex: Integer): ustring;
var
  k, v: TCefString;
begin
  k := TCef3Helper.CefString(key);
  FillChar(v, SizeOf(v), 0);
  cef_string_multimap_enumerate(FStringMap, @k, ValueIndex, v);
  Result := TCef3Helper.CefString(@v);
end;

function TCefStringMultimapOwn.GetHandle: TCefStringMultimap;
begin
  Result := FStringMap;
end;

function TCefStringMultimapOwn.GetKey(index: Integer): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_multimap_key(FStringMap, index, str);
  Result := TCef3Helper.CefString(@str);
end;

function TCefStringMultimapOwn.GetSize: Integer;
begin
  Result := cef_string_multimap_size(FStringMap);
end;

function TCefStringMultimapOwn.GetValue(index: Integer): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_multimap_value(FStringMap, index, str);
  Result := TCef3Helper.CefString(@str);
end;

{ TCefDownloadHandlerOwn }

constructor TCefDownloadHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefDownloadHandler));
  with PCefDownloadHandler(FData)^ do
  begin
    on_before_download := cef_download_handler_on_before_download;
    on_download_updated := cef_download_handler_on_download_updated;
  end;
end;

procedure TCefDownloadHandlerOwn.OnBeforeDownload(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
begin

end;

procedure TCefDownloadHandlerOwn.OnDownloadUpdated(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin

end;

{ TCefXmlReaderRef }

function TCefXmlReaderRef.close: Boolean;
begin
  Result := PCefXmlReader(FData).close(FData) <> 0;
end;

class function TCefXmlReaderRef.New(const stream: ICefStreamReader;
  encodingType: TCefXmlEncodingType; const URI: ustring): ICefXmlReader;
var
  u: TCefString;
begin
  u := TCef3Helper.CefString(URI);
  Result := UnWrap(cef_xml_reader_create(TCef3Helper.CefGetData(stream),
    encodingType, @u));
end;

function TCefXmlReaderRef.GetAttributeByIndex(index: Integer): ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_attribute_byindex(FData, index));
end;

function TCefXmlReaderRef.GetAttributeByLName(const localName,
  namespaceURI: ustring): ustring;
var
  l, n: TCefString;
begin
  l := TCef3Helper.CefString(localName);
  n := TCef3Helper.CefString(namespaceURI);
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_attribute_bylname(FData, @l, @n));
end;

function TCefXmlReaderRef.GetAttributeByQName(const qualifiedName
  : ustring): ustring;
var
  q: TCefString;
begin
  q := TCef3Helper.CefString(qualifiedName);
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_attribute_byqname(FData, @q));
end;

function TCefXmlReaderRef.GetAttributeCount: NativeUInt;
begin
  Result := PCefXmlReader(FData).get_attribute_count(FData);
end;

function TCefXmlReaderRef.GetBaseUri: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_base_uri(FData));
end;

function TCefXmlReaderRef.GetDepth: Integer;
begin
  Result := PCefXmlReader(FData).get_depth(FData);
end;

function TCefXmlReaderRef.GetError: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_error(FData));
end;

function TCefXmlReaderRef.GetInnerXml: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_inner_xml(FData));
end;

function TCefXmlReaderRef.GetLineNumber: Integer;
begin
  Result := PCefXmlReader(FData).get_line_number(FData);
end;

function TCefXmlReaderRef.GetLocalName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_local_name(FData));
end;

function TCefXmlReaderRef.GetNamespaceUri: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_namespace_uri(FData));
end;

function TCefXmlReaderRef.GetOuterXml: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_outer_xml(FData));
end;

function TCefXmlReaderRef.GetPrefix: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_prefix(FData));
end;

function TCefXmlReaderRef.GetQualifiedName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_qualified_name(FData));
end;

function TCefXmlReaderRef.GetType: TCefXmlNodeType;
begin
  Result := PCefXmlReader(FData).get_type(FData);
end;

function TCefXmlReaderRef.GetValue: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_value(FData));
end;

function TCefXmlReaderRef.GetXmlLang: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefXmlReader(FData)
    .get_xml_lang(FData));
end;

function TCefXmlReaderRef.HasAttributes: Boolean;
begin
  Result := PCefXmlReader(FData).has_attributes(FData) <> 0;
end;

function TCefXmlReaderRef.HasError: Boolean;
begin
  Result := PCefXmlReader(FData).has_error(FData) <> 0;
end;

function TCefXmlReaderRef.HasValue: Boolean;
begin
  Result := PCefXmlReader(FData).has_value(FData) <> 0;
end;

function TCefXmlReaderRef.IsEmptyElement: Boolean;
begin
  Result := PCefXmlReader(FData).is_empty_element(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByIndex(index: Integer): Boolean;
begin
  Result := PCefXmlReader(FData).move_to_attribute_byindex(FData, index) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByLName(const localName,
  namespaceURI: ustring): Boolean;
var
  l, n: TCefString;
begin
  l := TCef3Helper.CefString(localName);
  n := TCef3Helper.CefString(namespaceURI);
  Result := PCefXmlReader(FData).move_to_attribute_bylname(FData, @l, @n) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByQName(const qualifiedName
  : ustring): Boolean;
var
  q: TCefString;
begin
  q := TCef3Helper.CefString(qualifiedName);
  Result := PCefXmlReader(FData).move_to_attribute_byqname(FData, @q) <> 0;
end;

function TCefXmlReaderRef.MoveToCarryingElement: Boolean;
begin
  Result := PCefXmlReader(FData).move_to_carrying_element(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToFirstAttribute: Boolean;
begin
  Result := PCefXmlReader(FData).move_to_first_attribute(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToNextAttribute: Boolean;
begin
  Result := PCefXmlReader(FData).move_to_next_attribute(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToNextNode: Boolean;
begin
  Result := PCefXmlReader(FData).move_to_next_node(FData) <> 0;
end;

class function TCefXmlReaderRef.UnWrap(data: Pointer): ICefXmlReader;
begin
  if data <> nil then
    Result := create(data) as ICefXmlReader
  else
    Result := nil;
end;

{ TCefZipReaderRef }

function TCefZipReaderRef.close: Boolean;
begin
  Result := PCefZipReader(FData).close(FData) <> 0;
end;

function TCefZipReaderRef.CloseFile: Boolean;
begin
  Result := PCefZipReader(FData).close_file(FData) <> 0;
end;

class function TCefZipReaderRef.New(const stream: ICefStreamReader)
  : ICefZipReader;
begin
  Result := UnWrap(cef_zip_reader_create(TCef3Helper.CefGetData(stream)));
end;

function TCefZipReaderRef.eof: Boolean;
begin
  Result := PCefZipReader(FData).eof(FData) <> 0;
end;

function TCefZipReaderRef.GetFileLastModified: TCefTime;
begin
  Result := PCefZipReader(FData).get_file_last_modified(FData);
end;

function TCefZipReaderRef.GetFileName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefZipReader(FData)
    .get_file_name(FData));
end;

function TCefZipReaderRef.GetFileSize: Int64;
begin
  Result := PCefZipReader(FData).get_file_size(FData);
end;

function TCefZipReaderRef.MoveToFile(const fileName: ustring;
  caseSensitive: Boolean): Boolean;
var
  f: TCefString;
begin
  f := TCef3Helper.CefString(fileName);
  Result := PCefZipReader(FData).move_to_file(FData, @f,
    ord(caseSensitive)) <> 0;
end;

function TCefZipReaderRef.MoveToFirstFile: Boolean;
begin
  Result := PCefZipReader(FData).move_to_first_file(FData) <> 0;
end;

function TCefZipReaderRef.MoveToNextFile: Boolean;
begin
  Result := PCefZipReader(FData).move_to_next_file(FData) <> 0;
end;

function TCefZipReaderRef.OpenFile(const password: ustring): Boolean;
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(password);
  Result := PCefZipReader(FData).open_file(FData, @p) <> 0;
end;

function TCefZipReaderRef.ReadFile(buffer: Pointer;
  bufferSize: NativeUInt): Integer;
begin
  Result := PCefZipReader(FData).read_file(FData, buffer, bufferSize);
end;

function TCefZipReaderRef.tell: Int64;
begin
  Result := PCefZipReader(FData).tell(FData);
end;

class function TCefZipReaderRef.UnWrap(data: Pointer): ICefZipReader;
begin
  if data <> nil then
    Result := create(data) as ICefZipReader
  else
    Result := nil;
end;

{ TCefFastTask }

constructor TCefFastTask.create(const method: TCefFastTaskProc);
begin
  inherited create;
  FMethod := method;
end;

procedure TCefFastTask.execute;
begin
  FMethod();
end;

class procedure TCefFastTask.New(threadId: TCefThreadId;
  const method: TCefFastTaskProc);
begin
  TCef3Helper.CefPostTask(threadId, create(method));
end;

class procedure TCefFastTask.NewDelayed(threadId: TCefThreadId; Delay: Int64;
  const method: TCefFastTaskProc);
begin
  TCef3Helper.CefPostDelayedTask(threadId, create(method), Delay);
end;

{ TCefv8ContextRef }

class function TCefv8ContextRef.current: ICefv8Context;
begin
  Result := UnWrap(cef_v8context_get_current_context)
end;

function TCefv8ContextRef.enter: Boolean;
begin
  Result := PCefv8Context(FData)^.enter(PCefv8Context(FData)) <> 0;
end;

class function TCefv8ContextRef.Entered: ICefv8Context;
begin
  Result := UnWrap(cef_v8context_get_entered_context)
end;

function TCefv8ContextRef.exit: Boolean;
begin
  Result := PCefv8Context(FData)^.exit(PCefv8Context(FData)) <> 0;
end;

function TCefv8ContextRef.GetBrowser: ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefv8Context(FData)
    ^.get_browser(PCefv8Context(FData)));
end;

function TCefv8ContextRef.GetFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefv8Context(FData)
    ^.get_frame(PCefv8Context(FData)))
end;

function TCefv8ContextRef.GetGlobal: ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefv8Context(FData)
    ^.get_global(PCefv8Context(FData)));
end;

function TCefv8ContextRef.GetTaskRunner: ICefTaskRunner;
begin
  Result := TCefTaskRunnerRef.UnWrap(PCefv8Context(FData)
    ^.get_task_runner(FData));
end;

function TCefv8ContextRef.IsSame(const that: ICefv8Context): Boolean;
begin
  Result := PCefv8Context(FData)^.is_same(PCefv8Context(FData),
    TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefv8ContextRef.IsValid: Boolean;
begin
  Result := PCefv8Context(FData)^.is_valid(FData) <> 0;
end;

function TCefv8ContextRef.eval(const code: ustring; var retval: ICefv8Value;
  var exception: ICefV8Exception): Boolean;
var
  c: TCefString;
  r: PCefv8Value;
  e: PCefV8Exception;
begin
  c := TCef3Helper.CefString(code);
  r := nil;
  e := nil;
  Result := PCefv8Context(FData)^.eval(PCefv8Context(FData), @c, r, e) <> 0;
  retval := TCefv8ValueRef.UnWrap(r);
  exception := TCefV8ExceptionRef.UnWrap(e);
end;

class function TCefv8ContextRef.UnWrap(data: Pointer): ICefv8Context;
begin
  if data <> nil then
    Result := create(data) as ICefv8Context
  else
    Result := nil;
end;

{ TCefDomVisitorOwn }

constructor TCefDomVisitorOwn.create;
begin
  inherited CreateData(SizeOf(TCefDomVisitor));
  with PCefDomVisitor(FData)^ do
    visit := cef_dom_visitor_visite;
end;

procedure TCefDomVisitorOwn.visit(const document: ICefDomDocument);
begin

end;

{ TCefFastDomVisitor }

constructor TCefFastDomVisitor.create(const proc: TCefDomVisitorProc);
begin
  inherited create;
  FProc := proc;
end;

procedure TCefFastDomVisitor.visit(const document: ICefDomDocument);
begin
  FProc(document);
end;

{ TCefDomDocumentRef }

function TCefDomDocumentRef.GetBaseUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomDocument(FData)
    ^.get_base_url(PCefDomDocument(FData)))
end;

function TCefDomDocumentRef.GetBody: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)
    ^.get_body(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetCompleteUrl(const partialURL: ustring): ustring;
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(partialURL);
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomDocument(FData)
    ^.get_complete_url(PCefDomDocument(FData), @p));
end;

function TCefDomDocumentRef.GetDocument: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)
    ^.get_document(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetElementById(const id: ustring): ICefDomNode;
var
  i: TCefString;
begin
  i := TCef3Helper.CefString(id);
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)
    ^.get_element_by_id(PCefDomDocument(FData), @i));
end;

function TCefDomDocumentRef.GetFocusedNode: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)
    ^.get_focused_node(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetHead: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)
    ^.get_head(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionAsMarkup: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomDocument(FData)
    ^.get_selection_as_markup(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionAsText: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomDocument(FData)
    ^.get_selection_as_text(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionEndOffset: Integer;
begin
  Result := PCefDomDocument(FData)^.get_selection_end_offset
    (PCefDomDocument(FData));
end;

function TCefDomDocumentRef.GetSelectionStartOffset: Integer;
begin
  Result := PCefDomDocument(FData)^.get_selection_start_offset
    (PCefDomDocument(FData));
end;

function TCefDomDocumentRef.GetTitle: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomDocument(FData)
    ^.get_title(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetType: TCefDomDocumentType;
begin
  Result := PCefDomDocument(FData)^.get_type(PCefDomDocument(FData));
end;

function TCefDomDocumentRef.HasSelection: Boolean;
begin
  Result := PCefDomDocument(FData)^.has_selection(PCefDomDocument(FData)) <> 0;
end;

class function TCefDomDocumentRef.UnWrap(data: Pointer): ICefDomDocument;
begin
  if data <> nil then
    Result := create(data) as ICefDomDocument
  else
    Result := nil;
end;

{ TCefDomNodeRef }

function TCefDomNodeRef.GetAsMarkup: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomNode(FData)
    ^.get_as_markup(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetDocument: ICefDomDocument;
begin
  Result := TCefDomDocumentRef.UnWrap(PCefDomNode(FData)
    ^.get_document(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetElementAttribute(const attrName: ustring): ustring;
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(attrName);
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomNode(FData)
    ^.get_element_attribute(PCefDomNode(FData), @p));
end;

procedure TCefDomNodeRef.GetElementAttributes(const attrMap: ICefStringMap);
begin
  PCefDomNode(FData)^.get_element_attributes(PCefDomNode(FData),
    attrMap.Handle);
end;

function TCefDomNodeRef.GetElementInnerText: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomNode(FData)
    ^.get_element_inner_text(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetElementTagName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomNode(FData)
    ^.get_element_tag_name(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetFirstChild: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)
    ^.get_first_child(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetFormControlElementType: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomNode(FData)
    ^.get_form_control_element_type(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetLastChild: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)
    ^.get_last_child(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomNode(FData)
    ^.get_name(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetNextSibling: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)
    ^.get_next_sibling(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetParent: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)
    ^.get_parent(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetPreviousSibling: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_previous_sibling
    (PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetType: TCefDomNodeType;
begin
  Result := PCefDomNode(FData)^.get_type(PCefDomNode(FData));
end;

function TCefDomNodeRef.GetValue: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDomNode(FData)
    ^.get_value(PCefDomNode(FData)));
end;

function TCefDomNodeRef.HasChildren: Boolean;
begin
  Result := PCefDomNode(FData)^.has_children(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.HasElementAttribute(const attrName: ustring): Boolean;
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(attrName);
  Result := PCefDomNode(FData)^.has_element_attribute
    (PCefDomNode(FData), @p) <> 0;
end;

function TCefDomNodeRef.HasElementAttributes: Boolean;
begin
  Result := PCefDomNode(FData)^.has_element_attributes(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsEditable: Boolean;
begin
  Result := PCefDomNode(FData)^.is_editable(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsElement: Boolean;
begin
  Result := PCefDomNode(FData)^.is_element(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsFormControlElement: Boolean;
begin
  Result := PCefDomNode(FData)^.is_form_control_element
    (PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsSame(const that: ICefDomNode): Boolean;
begin
  Result := PCefDomNode(FData)^.is_same(PCefDomNode(FData),
    TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefDomNodeRef.IsText: Boolean;
begin
  Result := PCefDomNode(FData)^.is_text(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.SetElementAttribute(const attrName,
  value: ustring): Boolean;
var
  p1, p2: TCefString;
begin
  p1 := TCef3Helper.CefString(attrName);
  p2 := TCef3Helper.CefString(value);
  Result := PCefDomNode(FData)^.set_element_attribute(PCefDomNode(FData), @p1,
    @p2) <> 0;
end;

function TCefDomNodeRef.SetValue(const value: ustring): Boolean;
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(value);
  Result := PCefDomNode(FData)^.set_value(PCefDomNode(FData), @p) <> 0;
end;

class function TCefDomNodeRef.UnWrap(data: Pointer): ICefDomNode;
begin
  if data <> nil then
    Result := create(data) as ICefDomNode
  else
    Result := nil;
end;

{ TCefResponseRef }

class function TCefResponseRef.New: ICefResponse;
begin
  Result := UnWrap(cef_response_create);
end;

function TCefResponseRef.GetHeader(const name: ustring): ustring;
var
  n: TCefString;
begin
  n := TCef3Helper.CefString(name);
  Result := TCef3Helper.CefStringFreeAndGet(PCefResponse(FData)
    ^.get_header(PCefResponse(FData), @n));
end;

procedure TCefResponseRef.GetHeaderMap(const headerMap: ICefStringMultimap);
begin
  PCefResponse(FData)^.get_header_map(PCefResponse(FData), headerMap.Handle);
end;

function TCefResponseRef.GetMimeType: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefResponse(FData)
    ^.get_mime_type(PCefResponse(FData)));
end;

function TCefResponseRef.GetStatus: Integer;
begin
  Result := PCefResponse(FData)^.get_status(PCefResponse(FData));
end;

function TCefResponseRef.GetStatusText: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefResponse(FData)
    ^.get_status_text(PCefResponse(FData)));
end;

function TCefResponseRef.IsReadOnly: Boolean;
begin
  Result := PCefResponse(FData)^.is_read_only(PCefResponse(FData)) <> 0;
end;

procedure TCefResponseRef.SetHeaderMap(const headerMap: ICefStringMultimap);
begin
  PCefResponse(FData)^.set_header_map(PCefResponse(FData), headerMap.Handle);
end;

procedure TCefResponseRef.SetMimeType(const mimeType: ustring);
var
  txt: TCefString;
begin
  txt := TCef3Helper.CefString(mimeType);
  PCefResponse(FData)^.set_mime_type(PCefResponse(FData), @txt);
end;

procedure TCefResponseRef.SetStatus(status: Integer);
begin
  PCefResponse(FData)^.set_status(PCefResponse(FData), status);
end;

procedure TCefResponseRef.SetStatusText(const statusText: ustring);
var
  txt: TCefString;
begin
  txt := TCef3Helper.CefString(statusText);
  PCefResponse(FData)^.set_status_text(PCefResponse(FData), @txt);
end;

class function TCefResponseRef.UnWrap(data: Pointer): ICefResponse;
begin
  if data <> nil then
    Result := create(data) as ICefResponse
  else
    Result := nil;
end;

{ TCefV8AccessorOwn }

constructor TCefV8AccessorOwn.create;
begin
  inherited CreateData(SizeOf(TCefV8Accessor));
  PCefV8Accessor(FData)^.get := cef_v8_accessor_get;
  PCefV8Accessor(FData)^.put := cef_v8_accessor_put;
end;

function TCefV8AccessorOwn.get(const name: ustring; const obj: ICefv8Value;
  out value: ICefv8Value; const exception: ustring): Boolean;
begin
  Result := False;
end;

function TCefV8AccessorOwn.put(const name: ustring;
  const obj, value: ICefv8Value; const exception: ustring): Boolean;
begin
  Result := False;
end;

{ TCefFastV8Accessor }

constructor TCefFastV8Accessor.create(const getter: TCefV8AccessorGetterProc;
  const setter: TCefV8AccessorSetterProc);
begin
  FGetter := getter;
  FSetter := setter;
end;

function TCefFastV8Accessor.get(const name: ustring; const obj: ICefv8Value;
  out value: ICefv8Value; const exception: ustring): Boolean;
begin
  if Assigned(FGetter) then
    Result := FGetter(name, obj, value, exception)
  else
    Result := False;
end;

function TCefFastV8Accessor.put(const name: ustring;
  const obj, value: ICefv8Value; const exception: ustring): Boolean;
begin
  if Assigned(FSetter) then
    Result := FSetter(name, obj, value, exception)
  else
    Result := False;
end;

{ TCefCookieVisitorOwn }

constructor TCefCookieVisitorOwn.create;
begin
  inherited CreateData(SizeOf(TCefCookieVisitor));
  PCefCookieVisitor(FData)^.visit := cef_cookie_visitor_visit;
end;

function TCefCookieVisitorOwn.visit(const name, value, domain, path: ustring;
  secure, httponly, hasExpires: Boolean; const creation, lastAccess,
  expires: TDateTime; count, total: Integer; out deleteCookie: Boolean)
  : Boolean;
begin
  Result := True;
end;

{ TCefFastCookieVisitor }

constructor TCefFastCookieVisitor.create(const visitor: TCefCookieVisitorProc);
begin
  inherited create;
  FVisitor := visitor;
end;

function TCefFastCookieVisitor.visit(const name, value, domain, path: ustring;
  secure, httponly, hasExpires: Boolean; const creation, lastAccess,
  expires: TDateTime; count, total: Integer; out deleteCookie: Boolean)
  : Boolean;
begin
  Result := FVisitor(name, value, domain, path, secure, httponly, hasExpires,
    creation, lastAccess, expires, count, total, deleteCookie);
end;

{ TCefClientOwn }

constructor TCefClientOwn.create;
begin
  inherited CreateData(SizeOf(TCefClient));
  with PCefClient(FData)^ do
  begin
    get_context_menu_handler := cef_client_get_context_menu_handler;
    get_dialog_handler := cef_client_get_dialog_handler;
    get_display_handler := cef_client_get_display_handler;
    get_download_handler := cef_client_get_download_handler;
    get_drag_handler := cef_client_get_drag_handler;
    get_find_handler := cef_client_get_find_handler;
    get_focus_handler := cef_client_get_focus_handler;
    get_geolocation_handler := cef_client_get_geolocation_handler;
    get_jsdialog_handler := cef_client_get_jsdialog_handler;
    get_keyboard_handler := cef_client_get_keyboard_handler;
    get_life_span_handler := cef_client_get_life_span_handler;
    get_load_handler := cef_client_get_load_handler;
    get_render_handler := cef_client_get_get_render_handler;
    get_request_handler := cef_client_get_request_handler;
    on_process_message_received := cef_client_on_process_message_received;
  end;
end;

function TCefClientOwn.GetContextMenuHandler: ICefContextMenuHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetDialogHandler: ICefDialogHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetDisplayHandler: ICefDisplayHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetDownloadHandler: ICefDownloadHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetDragHandler: ICefDragHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetFindHandler: ICefFindHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetFocusHandler: ICefFocusHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetGeolocationHandler: ICefGeolocationHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetJsdialogHandler: ICefJsDialogHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetKeyboardHandler: ICefKeyboardHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetLifeSpanHandler: ICefLifeSpanHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetLoadHandler: ICefLoadHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetRenderHandler: ICefRenderHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetRequestHandler: ICefRequestHandler;
begin
  Result := nil;
end;

function TCefClientOwn.OnProcessMessageReceived(const browser: ICefBrowser;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;
begin
  Result := False;
end;

{ TCefGeolocationHandlerOwn }

constructor TCefGeolocationHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefGeolocationHandler));
  with PCefGeolocationHandler(FData)^ do
  begin
    on_request_geolocation_permission :=
      cef_geolocation_handler_on_request_geolocation_permission;
    on_cancel_geolocation_permission :=
      cef_geolocation_handler_on_cancel_geolocation_permission;
  end;
end;

function TCefGeolocationHandlerOwn.OnRequestGeolocationPermission
  (const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer;
  const callback: ICefGeolocationCallback): Boolean;
begin
  Result := False;
end;

procedure TCefGeolocationHandlerOwn.OnCancelGeolocationPermission
  (const browser: ICefBrowser; requestId: Integer);
begin

end;

{ TCefLifeSpanHandlerOwn }

constructor TCefLifeSpanHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefLifeSpanHandler));
  with PCefLifeSpanHandler(FData)^ do
  begin
    on_before_popup := cef_life_span_handler_on_before_popup;
    on_after_created := cef_life_span_handler_on_after_created;
    on_before_close := cef_life_span_handler_on_before_close;
    do_close := cef_life_span_handler_do_close;
  end;
end;

procedure TCefLifeSpanHandlerOwn.OnAfterCreated(const browser: ICefBrowser);
begin

end;

procedure TCefLifeSpanHandlerOwn.OnBeforeClose(const browser: ICefBrowser);
begin

end;

function TCefLifeSpanHandlerOwn.OnBeforePopup(const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var noJavascriptAccess: Boolean): Boolean;
begin
  Result := False;
end;

function TCefLifeSpanHandlerOwn.DoClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;
end;

function TCefLifeSpanHandlerOwn.RunModal(const browser: ICefBrowser): Boolean;
begin
  Result := False;
end;


{ TCefLoadHandlerOwn }

constructor TCefLoadHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefLoadHandler));
  with PCefLoadHandler(FData)^ do
  begin
    on_loading_state_change := cef_load_handler_on_loading_state_change;
    on_load_start := cef_load_handler_on_load_start;
    on_load_end := cef_load_handler_on_load_end;
    on_load_error := cef_load_handler_on_load_error;
  end;
end;

procedure TCefLoadHandlerOwn.OnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin

end;

procedure TCefLoadHandlerOwn.OnLoadError(const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
begin

end;

procedure TCefLoadHandlerOwn.OnLoadingStateChange(const browser: ICefBrowser;
  isLoading, canGoBack, canGoForward: Boolean);
begin

end;

procedure TCefLoadHandlerOwn.OnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin

end;

{ TCefRequestHandlerOwn }

constructor TCefRequestHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefRequestHandler));
  with PCefRequestHandler(FData)^ do
  begin
    on_before_browse := cef_request_handler_on_before_browse;
    on_open_urlfrom_tab := cef_request_handler_on_open_urlfrom_tab;
    //on_before_resource_load := cef_request_handler_on_before_resource_load;
    get_resource_handler := cef_request_handler_get_resource_handler;
    on_resource_redirect := cef_request_handler_on_resource_redirect;
    on_resource_response := cef_request_handler_on_resource_response;
    get_resource_response_filter := cef_request_handler_get_resource_response_filter;
    on_resource_load_complete := cef_request_handler_on_resource_load_complete;
    get_auth_credentials := cef_request_handler_get_auth_credentials;
    on_quota_request := cef_request_handler_on_quota_request;
    on_protocol_execution := cef_request_handler_on_protocol_execution;
    on_certificate_error := cef_request_handler_on_certificate_error;
    on_plugin_crashed := cef_request_handler_on_plugin_crashed;
    on_render_view_ready := cef_request_handler_on_render_view_ready;
    on_render_process_terminated := cef_request_handler_on_render_process_terminated;
  end;
end;

function TCefRequestHandlerOwn.GetAuthCredentials(const browser: ICefBrowser;
  const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer;
  const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.GetCookieManager(const browser: ICefBrowser;
  const mainUrl: ustring): ICefCookieManager;
begin
  Result := nil;
end;

function TCefRequestHandlerOwn.OnBeforeBrowse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  isRedirect: Boolean): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.OnBeforeResourceLoad(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const callback: ICefRequestCallback): TCefReturnValue;
begin
  Result := RV_CONTINUE;
end;

function TCefRequestHandlerOwn.OnCertificateError(const browser: ICefBrowser;
  certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo;
  const callback: ICefRequestCallback): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.OnOpenUrlFromTab(const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.GetResourceHandler(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler;
begin
  Result := nil;
end;

function TCefRequestHandlerOwn.GetResourceResponseFilter(
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse): ICefResponseFilter;
begin
  Result := nil;
end;

procedure TCefRequestHandlerOwn.OnPluginCrashed(const browser: ICefBrowser;
  const pluginPath: ustring);
begin

end;

procedure TCefRequestHandlerOwn.OnProtocolExecution(const browser: ICefBrowser;
  const url: ustring; out allowOsExecution: Boolean);
begin

end;

function TCefRequestHandlerOwn.OnQuotaRequest(const browser: ICefBrowser;
  const originUrl: ustring; newSize: Int64;
  const callback: ICefRequestCallback): Boolean;
begin
  Result := False;
end;

procedure TCefRequestHandlerOwn.OnRenderProcessTerminated
  (const browser: ICefBrowser; status: TCefTerminationStatus);
begin

end;

procedure TCefRequestHandlerOwn.OnRenderViewReady(const browser: ICefBrowser);
begin

end;

procedure TCefRequestHandlerOwn.OnResourceLoadComplete(
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse;
  status: TCefUrlRequestStatus; receivedContentLength: Int64);
begin

end;

procedure TCefRequestHandlerOwn.OnResourceRedirect(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest; var newUrl: ustring);
begin

end;

function TCefRequestHandlerOwn.OnResourceResponse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const response: ICefResponse): Boolean;
begin
  Result := False;
end;

{ TCefDisplayHandlerOwn }

constructor TCefDisplayHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefDisplayHandler));
  with PCefDisplayHandler(FData)^ do
  begin
    on_address_change := cef_display_handler_on_address_change;
    on_title_change := cef_display_handler_on_title_change;
    on_favicon_urlchange := cef_display_handler_on_favicon_urlchange;
    on_fullscreen_mode_change := cef_display_handler_on_fullscreen_mode_change;
    on_tooltip := cef_display_handler_on_tooltip;
    on_status_message := cef_display_handler_on_status_message;
    on_console_message := cef_display_handler_on_console_message;
  end;
end;

procedure TCefDisplayHandlerOwn.OnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin

end;

function TCefDisplayHandlerOwn.OnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: Integer): Boolean;
begin
  Result := False;
end;

procedure TCefDisplayHandlerOwn.OnFaviconUrlChange(const browser: ICefBrowser;
  iconUrls: TStrings);
begin

end;

procedure TCefDisplayHandlerOwn.OnFullScreenModeChange(
  const browser: ICefBrowser; fullscreen: Boolean);
begin

end;

procedure TCefDisplayHandlerOwn.OnStatusMessage(const browser: ICefBrowser;
  const value: ustring);
begin

end;

procedure TCefDisplayHandlerOwn.OnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin

end;

function TCefDisplayHandlerOwn.OnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
begin
  Result := False;
end;

{ TCefFocusHandlerOwn }

constructor TCefFocusHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefFocusHandler));
  with PCefFocusHandler(FData)^ do
  begin
    on_take_focus := cef_focus_handler_on_take_focus;
    on_set_focus := cef_focus_handler_on_set_focus;
    on_got_focus := cef_focus_handler_on_got_focus;
  end;
end;

function TCefFocusHandlerOwn.OnSetFocus(const browser: ICefBrowser;
  source: TCefFocusSource): Boolean;
begin
  Result := False;
end;

procedure TCefFocusHandlerOwn.OnGotFocus(const browser: ICefBrowser);
begin

end;

procedure TCefFocusHandlerOwn.OnTakeFocus(const browser: ICefBrowser;
  next: Boolean);
begin

end;

{ TCefKeyboardHandlerOwn }

constructor TCefKeyboardHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefKeyboardHandler));
  with PCefKeyboardHandler(FData)^ do
  begin
    on_pre_key_event := cef_keyboard_handler_on_pre_key_event;
    on_key_event := cef_keyboard_handler_on_key_event;
  end;
end;

function TCefKeyboardHandlerOwn.OnPreKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle;
  out isKeyboardShortcut: Boolean): Boolean;
begin
  Result := False;
end;

function TCefKeyboardHandlerOwn.OnKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean;
begin

  Result := False;
end;

{ TCefJsDialogHandlerOwn }

constructor TCefJsDialogHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefJsDialogHandler));
  with PCefJsDialogHandler(FData)^ do
  begin
    on_jsdialog := cef_jsdialog_handler_on_jsdialog;
    on_before_unload_dialog := cef_jsdialog_handler_on_before_unload_dialog;
    on_reset_dialog_state := cef_jsdialog_handler_on_reset_dialog_state;
    on_dialog_closed := cef_jsdialog_handler_on_dialog_closed;
  end;
end;

function TCefJsDialogHandlerOwn.OnJsdialog(const browser: ICefBrowser;
  const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
  const messageText, defaultPromptText: ustring; callback: ICefJsDialogCallback;
  out suppressMessage: Boolean): Boolean;
begin
  Result := False;
end;

function TCefJsDialogHandlerOwn.OnBeforeUnloadDialog(const browser: ICefBrowser;
  const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback): Boolean;
begin

  Result := False;
end;

procedure TCefJsDialogHandlerOwn.OnDialogClosed(const browser: ICefBrowser);
begin

end;

procedure TCefJsDialogHandlerOwn.OnResetDialogState(const browser: ICefBrowser);
begin

end;

{ TCefContextMenuHandlerOwn }

constructor TCefContextMenuHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefContextMenuHandler));
  with PCefContextMenuHandler(FData)^ do
  begin
    on_before_context_menu := cef_context_menu_handler_on_before_context_menu;
    run_context_menu := cef_context_menu_handler_run_context_menu;
    on_context_menu_command := cef_context_menu_handler_on_context_menu_command;
    on_context_menu_dismissed :=
      cef_context_menu_handler_on_context_menu_dismissed;
  end;
end;

procedure TCefContextMenuHandlerOwn.OnBeforeContextMenu(const browser
  : ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams;
  const model: ICefMenuModel);
begin

end;

function TCefContextMenuHandlerOwn.OnContextMenuCommand(const browser
  : ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams;
  commandId: Integer; eventFlags: TCefEventFlags): Boolean;
begin
  Result := False;
end;

procedure TCefContextMenuHandlerOwn.OnContextMenuDismissed
  (const browser: ICefBrowser; const frame: ICefFrame);
begin

end;

function TCefContextMenuHandlerOwn.RunContextMenu(const browser: ICefBrowser;
  const frame: ICefFrame; const params: ICefContextMenuParams;
  const model: ICefMenuModel;
  const callback: ICefRunContextMenuCallback): Boolean;
begin
  Result := False;
end;

{ TCefV8ExceptionRef }

function TCefV8ExceptionRef.GetEndColumn: Integer;
begin
  Result := PCefV8Exception(FData)^.get_end_column(FData);
end;

function TCefV8ExceptionRef.GetEndPosition: Integer;
begin
  Result := PCefV8Exception(FData)^.get_end_position(FData);
end;

function TCefV8ExceptionRef.GetLineNumber: Integer;
begin
  Result := PCefV8Exception(FData)^.get_line_number(FData);
end;

function TCefV8ExceptionRef.GetMessage: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefV8Exception(FData)
    ^.get_message(FData));
end;

function TCefV8ExceptionRef.GetScriptResourceName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefV8Exception(FData)
    ^.get_script_resource_name(FData));
end;

function TCefV8ExceptionRef.GetSourceLine: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefV8Exception(FData)
    ^.get_source_line(FData));
end;

function TCefV8ExceptionRef.GetStartColumn: Integer;
begin
  Result := PCefV8Exception(FData)^.get_start_column(FData);
end;

function TCefV8ExceptionRef.GetStartPosition: Integer;
begin
  Result := PCefV8Exception(FData)^.get_start_position(FData);
end;

class function TCefV8ExceptionRef.UnWrap(data: Pointer): ICefV8Exception;
begin
  if data <> nil then
    Result := create(data) as ICefV8Exception
  else
    Result := nil;
end;

{ TCefResourceBundleHandlerOwn }

constructor TCefResourceBundleHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefResourceBundleHandler));
  with PCefResourceBundleHandler(FData)^ do
  begin
    get_localized_string := cef_resource_bundle_handler_get_localized_string;
    get_data_resource := cef_resource_bundle_handler_get_data_resource;
    get_data_resource_for_scale := cef_resource_bundle_handler_get_data_resource_for_scale;
  end;
end;

{ TCefFastResourceBundle }

constructor TCefFastResourceBundle.Create(AGetDataResource: TGetDataResource;
  AGetLocalizedString: TGetLocalizedString; AGetDataResourceForScale: TGetDataResourceForScale);
begin
  inherited Create;
  FGetDataResource := AGetDataResource;
  FGetLocalizedString := AGetLocalizedString;
  FGetDataResourceForScale := AGetDataResourceForScale;
end;

function TCefFastResourceBundle.GetDataResource(resourceId: Integer;
  out data: Pointer; out dataSize: NativeUInt): Boolean;
begin
  if Assigned(FGetDataResource) then
    Result := FGetDataResource(resourceId, data, dataSize) else
    Result := False;
end;

function TCefFastResourceBundle.GetDataResourceForScale(resourceId: Integer;
  scaleFactor: TCefScaleFactor; out data: Pointer;
  dataSize: NativeUInt): Boolean;
begin
  if Assigned(FGetDataResourceForScale) then
    Result := FGetDataResourceForScale(resourceId, scaleFactor, data, dataSize) else
    Result := False;
end;

function TCefFastResourceBundle.GetLocalizedString(stringId: Integer;
  out stringVal: ustring): Boolean;
begin
  if Assigned(FGetLocalizedString) then
    Result := FGetLocalizedString(stringId, stringVal) else
    Result := False;
end;

{ TCefAppOwn }

constructor TCefAppOwn.create;
begin
  inherited CreateData(SizeOf(TCefApp));
  with PCefApp(FData)^ do
  begin
    on_before_command_line_processing :=
      cef_app_on_before_command_line_processing;
    on_register_custom_schemes := cef_app_on_register_custom_schemes;
    get_resource_bundle_handler := cef_app_get_resource_bundle_handler;
    get_browser_process_handler := cef_app_get_browser_process_handler;
    get_render_process_handler := cef_app_get_render_process_handler;
  end;
end;

{ TCefCookieManagerRef }

class function TCefCookieManagerRef.New(const path: ustring; persistSessionCookies: Boolean;
  const callback: ICefCompletionCallback): ICefCookieManager;
var
  pth: TCefString;
begin
  pth := TCef3Helper.CefString(path);
  Result := UnWrap(cef_cookie_manager_create_manager(@pth, Ord(persistSessionCookies), TCef3Helper.CefGetData(callback)));
end;

class function TCefCookieManagerRef.NewProc(const path: ustring;
  persistSessionCookies: Boolean;
  const callback: TCefCompletionCallbackProc): ICefCookieManager;
begin
  Result := New(path, persistSessionCookies, TCefFastCompletionCallback.Create(callback));
end;

function TCefCookieManagerRef.DeleteCookies(const url,
  cookieName: ustring; const callback: ICefDeleteCookiesCallback): Boolean;
var
  u, n: TCefString;
begin
  u := TCef3Helper.CefString(url);
  n := TCef3Helper.CefString(cookieName);
  Result := PCefCookieManager(FData).delete_cookies(
    PCefCookieManager(FData), @u, @n, TCef3Helper.CefGetData(callback)) <> 0;
end;

function TCefCookieManagerRef.DeleteCookiesProc(const url, cookieName: ustring;
  const callback: TCefDeleteCookiesCallbackProc): Boolean;
begin
  Result := DeleteCookies(url, cookieName, TCefFastDeleteCookiesCallback.Create(callback));
end;

function TCefCookieManagerRef.FlushStore(
  const handler: ICefCompletionCallback): Boolean;
begin
    Result := PCefCookieManager(FData).flush_store(PCefCookieManager(FData),
    TCef3Helper.CefGetData(handler)) <> 0;
end;

function TCefCookieManagerRef.FlushStoreProc(
  const proc: TCefCompletionCallbackProc): Boolean;
begin
  Result := FlushStore(TCefFastCompletionCallback.Create(proc));
end;

class function TCefCookieManagerRef.Global(const callback: ICefCompletionCallback): ICefCookieManager;
begin
  Result := UnWrap(cef_cookie_manager_get_global_manager(TCef3Helper.CefGetData(callback)));
end;

class function TCefCookieManagerRef.GlobalProc(
  const callback: TCefCompletionCallbackProc): ICefCookieManager;
begin
  Result := Global(TCefFastCompletionCallback.Create(callback));
end;

function TCefCookieManagerRef.SetCookie(const url, name, value, domain,
  path: ustring; secure, httponly, hasExpires: Boolean; const creation,
  lastAccess, expires: TDateTime; const callback: ICefSetCookieCallback): Boolean;
var
  str: TCefString;
  cook: TCefCookie;
begin
  str := TCef3Helper.CefString(url);
  cook.name := TCef3Helper.CefString(name);
  cook.value := TCef3Helper.CefString(value);
  cook.domain := TCef3Helper.CefString(domain);
  cook.path := TCef3Helper.CefString(path);
  cook.secure := Ord(secure);
  cook.httponly := Ord(httponly);
  cook.creation := TCef3Helper.DateTimeToCefTime(creation);
  cook.last_access := TCef3Helper.DateTimeToCefTime(lastAccess);
  cook.has_expires := Ord(hasExpires);
  if hasExpires then
    cook.expires := TCef3Helper.DateTimeToCefTime(expires) else
    FillChar(cook.expires, SizeOf(TCefTime), 0);
  Result := PCefCookieManager(FData).set_cookie(
    PCefCookieManager(FData), @str, @cook, TCef3Helper.CefGetData(callback)) <> 0;
end;

function TCefCookieManagerRef.SetCookieProc(const url, name, value, domain,
  path: ustring; secure, httponly, hasExpires: Boolean; const creation,
  lastAccess, expires: TDateTime;
  const callback: TCefSetCookieCallbackProc): Boolean;
begin
  Result := SetCookie(url, name, value, domain, path, secure,
    httponly, hasExpires, creation, lastAccess, expires,
    TCefFastSetCookieCallback.Create(callback));
end;

function TCefCookieManagerRef.SetStoragePath(const path: ustring;
  persistSessionCookies: Boolean; const callback: ICefCompletionCallback): Boolean;
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(path);
  Result := PCefCookieManager(FData)^.set_storage_path(
    PCefCookieManager(FData), @p, Ord(persistSessionCookies), TCef3Helper.CefGetData(callback)) <> 0;
end;

function TCefCookieManagerRef.SetStoragePathProc(const path: ustring;
  persistSessionCookies: Boolean;
  const callback: TCefCompletionCallbackProc): Boolean;
begin
  Result := SetStoragePath(path, persistSessionCookies, TCefFastCompletionCallback.Create(callback));
end;

procedure TCefCookieManagerRef.SetSupportedSchemes(schemes: TStrings; const callback: ICefCompletionCallback);
var
  list: TCefStringList;
  i: Integer;
  item: TCefString;
begin
  list := cef_string_list_alloc();
  try
    if (schemes <> nil) then
      for i := 0 to schemes.Count - 1 do
      begin
        item := TCef3Helper.CefString(schemes[i]);
        cef_string_list_append(list, @item);
      end;
    PCefCookieManager(FData).set_supported_schemes(
      PCefCookieManager(FData), list, TCef3Helper.CefGetData(callback));

  finally
    cef_string_list_free(list);
  end;
end;

procedure TCefCookieManagerRef.SetSupportedSchemesProc(schemes: TStrings;
  const callback: TCefCompletionCallbackProc);
begin
  SetSupportedSchemes(schemes, TCefFastCompletionCallback.Create(callback));
end;

class function TCefCookieManagerRef.UnWrap(data: Pointer): ICefCookieManager;
begin
  if data <> nil then
    Result := create(data) as ICefCookieManager
  else
    Result := nil;
end;

function TCefCookieManagerRef.VisitAllCookies(const visitor
  : ICefCookieVisitor): Boolean;
begin
  Result := PCefCookieManager(FData).visit_all_cookies(PCefCookieManager(FData),
    TCef3Helper.CefGetData(visitor)) <> 0;
end;

function TCefCookieManagerRef.VisitAllCookiesProc(const visitor
  : TCefCookieVisitorProc): Boolean;
begin
  Result := VisitAllCookies(TCefFastCookieVisitor.create(visitor)
    as ICefCookieVisitor);
end;

function TCefCookieManagerRef.VisitUrlCookies(const url: ustring;
  includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean;
var
  str: TCefString;
begin
  str := TCef3Helper.CefString(url);
  Result := PCefCookieManager(FData).visit_url_cookies(PCefCookieManager(FData),
    @str, ord(includeHttpOnly), TCef3Helper.CefGetData(visitor)) <> 0;
end;

function TCefCookieManagerRef.VisitUrlCookiesProc(const url: ustring;
  includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean;
begin
  Result := VisitUrlCookies(url, includeHttpOnly,
    TCefFastCookieVisitor.create(visitor) as ICefCookieVisitor);
end;

{ TCefWebPluginInfoRef }

function TCefWebPluginInfoRef.GetDescription: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefWebPluginInfo(FData)
    ^.get_description(PCefWebPluginInfo(FData)));
end;

function TCefWebPluginInfoRef.GetName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefWebPluginInfo(FData)
    ^.get_name(PCefWebPluginInfo(FData)));
end;

function TCefWebPluginInfoRef.GetPath: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefWebPluginInfo(FData)
    ^.get_path(PCefWebPluginInfo(FData)));
end;

function TCefWebPluginInfoRef.GetVersion: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefWebPluginInfo(FData)
    ^.get_version(PCefWebPluginInfo(FData)));
end;

class function TCefWebPluginInfoRef.UnWrap(data: Pointer): ICefWebPluginInfo;
begin
  if data <> nil then
    Result := create(data) as ICefWebPluginInfo
  else
    Result := nil;
end;

{ TCefBrowserHostRef }

procedure TCefBrowserHostRef.CloseDevTools;
begin
  PCefBrowserHost(FData).close_dev_tools(FData);
end;

procedure TCefBrowserHostRef.DragSourceEndedAt(x, y: Integer;
  op: TCefDragOperation);
begin
  PCefBrowserHost(FData).drag_source_ended_at(FData, x, y, op);
end;

procedure TCefBrowserHostRef.DragSourceSystemDragEnded;
begin
  PCefBrowserHost(FData).drag_source_system_drag_ended(FData);
end;

procedure TCefBrowserHostRef.DragTargetDragEnter(const dragData: ICefDragData;
  const event: PCefMouseEvent; allowedOps: TCefDragOperations);
begin
  PCefBrowserHost(FData).drag_target_drag_enter(FData,
    TCef3Helper.CefGetData(dragData), event, allowedOps);
end;

procedure TCefBrowserHostRef.DragTargetDragLeave;
begin
  PCefBrowserHost(FData).drag_target_drag_leave(FData);
end;

procedure TCefBrowserHostRef.DragTargetDragOver(const event: PCefMouseEvent;
  allowedOps: TCefDragOperations);
begin
  PCefBrowserHost(FData).drag_target_drag_over(FData, event, allowedOps);
end;

procedure TCefBrowserHostRef.DragTargetDrop(event: PCefMouseEvent);
begin
  PCefBrowserHost(FData).drag_target_drop(FData, event);
end;

procedure TCefBrowserHostRef.find(identifier: Integer;
  const searchText: ustring; forward, matchCase, findNext: Boolean);
var
  s: TCefString;
begin
  s := TCef3Helper.CefString(searchText);
  PCefBrowserHost(FData).find(FData, identifier, @s, ord(forward),
    ord(matchCase), ord(findNext));
end;

function TCefBrowserHostRef.GetBrowser: ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefBrowserHost(FData)
    .get_browser(PCefBrowserHost(FData)));
end;

procedure TCefBrowserHostRef.print;
begin
  PCefBrowserHost(FData).print(FData);
end;

procedure TCefBrowserHostRef.PrintToPdf(const path: ustring;
  settings: PCefPdfPrintSettings; const callback: ICefPdfPrintCallback);
var
  str: TCefString;
begin
  str := TCef3Helper.CefString(path);
  PCefBrowserHost(FData).print_to_pdf(FData, @str, settings, TCef3Helper.CefGetData(callback));
end;

procedure TCefBrowserHostRef.PrintToPdfProc(const path: ustring;
  settings: PCefPdfPrintSettings; const callback: TOnPdfPrintFinishedProc);
begin
  PrintToPdf(path, settings, TCefFastPdfPrintCallback.Create(callback));
end;

procedure TCefBrowserHostRef.ReplaceMisspelling(const word: ustring);
var
  str: TCefString;
begin
  str := TCef3Helper.CefString(word);
  PCefBrowserHost(FData).replace_misspelling(FData, @str);
end;

procedure TCefBrowserHostRef.RunFileDialog(mode: TCefFileDialogMode;
  const title, defaultFilePath: ustring; acceptFilters: TStrings;
  selectedAcceptFilter: Integer; const callback: ICefRunFileDialogCallback);
var
  t, f: TCefString;
  list: TCefStringList;
  item: TCefString;
  i: Integer;
begin
  t := TCef3Helper.CefString(title);
  f := TCef3Helper.CefString(defaultFilePath);
  list := cef_string_list_alloc();
  try
    for i := 0 to acceptFilters.Count - 1 do
    begin
      item := TCef3Helper.CefString(acceptFilters[i]);
      cef_string_list_append(list, @item);
    end;
    PCefBrowserHost(FData).run_file_dialog(PCefBrowserHost(FData), mode, @t, @f,
      list, selectedAcceptFilter, TCef3Helper.CefGetData(callback));
  finally
    cef_string_list_free(list);
  end;
end;

procedure TCefBrowserHostRef.RunFileDialogProc(mode: TCefFileDialogMode;
  const title, defaultFilePath: ustring; acceptFilters: TStrings;
  selectedAcceptFilter: Integer; const callback: TCefRunFileDialogCallbackProc);
begin
  RunFileDialog(mode, title, defaultFilePath, acceptFilters, selectedAcceptFilter,
    TCefFastRunFileDialogCallback.Create(callback));
end;

procedure TCefBrowserHostRef.AddWordToDictionary(const word: ustring);
var
  str: TCefString;
begin
  str := TCef3Helper.CefString(word);
  PCefBrowserHost(FData).add_word_to_dictionary(FData, @str);
end;

procedure TCefBrowserHostRef.CloseBrowser(forceClose: Boolean);
begin
  PCefBrowserHost(FData).close_browser(PCefBrowserHost(FData), ord(forceClose));
end;

procedure TCefBrowserHostRef.SendCaptureLostEvent;
begin
  PCefBrowserHost(FData).send_capture_lost_event(FData);
end;

procedure TCefBrowserHostRef.SendFocusEvent(setFocus: Boolean);
begin
  PCefBrowserHost(FData).send_focus_event(FData, ord(setFocus));
end;

procedure TCefBrowserHostRef.SendKeyEvent(const event: PCefKeyEvent);
begin
  PCefBrowserHost(FData).send_key_event(FData, event);
end;

procedure TCefBrowserHostRef.SendMouseClickEvent(const event: PCefMouseEvent;
  kind: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
begin
  PCefBrowserHost(FData).send_mouse_click_event(FData, event, kind,
    ord(mouseUp), clickCount);
end;

procedure TCefBrowserHostRef.SendMouseMoveEvent(const event: PCefMouseEvent;
  mouseLeave: Boolean);
begin
  PCefBrowserHost(FData).send_mouse_move_event(FData, event, ord(mouseLeave));
end;

procedure TCefBrowserHostRef.SendMouseWheelEvent(const event: PCefMouseEvent;
  deltaX, deltaY: Integer);
begin
  PCefBrowserHost(FData).send_mouse_wheel_event(FData, event, deltaX, deltaY);
end;

procedure TCefBrowserHostRef.setFocus(focus: Boolean);
begin
  PCefBrowserHost(FData).set_focus(PCefBrowserHost(FData), ord(focus));
end;

procedure TCefBrowserHostRef.SetMouseCursorChangeDisabled(disabled: Boolean);
begin
  PCefBrowserHost(FData).set_mouse_cursor_change_disabled
    (PCefBrowserHost(FData), ord(disabled));
end;

procedure TCefBrowserHostRef.SetWindowlessFrameRate(frameRate: Integer);
begin
  PCefBrowserHost(FData).set_windowless_frame_rate(PCefBrowserHost(FData), frameRate);
end;

procedure TCefBrowserHostRef.SetWindowVisibility(visible: Boolean);
begin
  PCefBrowserHost(FData).set_window_visibility(PCefBrowserHost(FData), Ord(visible));
end;

function TCefBrowserHostRef.GetWindowHandle: TCefWindowHandle;
begin
  Result := PCefBrowserHost(FData).get_window_handle(PCefBrowserHost(FData))
end;

function TCefBrowserHostRef.GetWindowlessFrameRate: Integer;
begin
  Result := PCefBrowserHost(FData).get_windowless_frame_rate(PCefBrowserHost(FData));
end;

function TCefBrowserHostRef.GetOpenerWindowHandle: TCefWindowHandle;
begin
  Result := PCefBrowserHost(FData).get_opener_window_handle
    (PCefBrowserHost(FData));
end;

function TCefBrowserHostRef.GetRequestContext: ICefRequestContext;
begin
  Result := TCefRequestContextRef.UnWrap(PCefBrowserHost(FData)
    .get_request_context(FData));
end;

procedure TCefBrowserHostRef.GetNavigationEntries(const visitor
  : ICefNavigationEntryVisitor; currentOnly: Boolean);
begin
  PCefBrowserHost(FData).get_navigation_entries(FData,
    TCef3Helper.CefGetData(visitor), ord(currentOnly));
end;

procedure TCefBrowserHostRef.GetNavigationEntriesProc
  (const proc: TCefNavigationEntryVisitorProc; currentOnly: Boolean);
begin
  GetNavigationEntries(TCefFastNavigationEntryVisitor.create(proc),
    currentOnly);
end;

function TCefBrowserHostRef.GetNsTextInputContext: TCefTextInputContext;
begin
  Result := PCefBrowserHost(FData).get_nstext_input_context
    (PCefBrowserHost(FData));
end;

function TCefBrowserHostRef.GetZoomLevel: Double;
begin
  Result := PCefBrowserHost(FData).get_zoom_level(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.HandleKeyEventAfterTextInputClient
  (keyEvent: TCefEventHandle);
begin
  PCefBrowserHost(FData).handle_key_event_after_text_input_client
    (PCefBrowserHost(FData), keyEvent);
end;

procedure TCefBrowserHostRef.HandleKeyEventBeforeTextInputClient
  (keyEvent: TCefEventHandle);
begin
  PCefBrowserHost(FData).handle_key_event_before_text_input_client
    (PCefBrowserHost(FData), keyEvent);
end;

procedure TCefBrowserHostRef.invalidate(kind: TCefPaintElementType);
begin
  PCefBrowserHost(FData).invalidate(FData, kind);
end;

function TCefBrowserHostRef.IsMouseCursorChangeDisabled: Boolean;
begin
  Result := PCefBrowserHost(FData).is_mouse_cursor_change_disabled(FData) <> 0
end;

function TCefBrowserHostRef.IsWindowRenderingDisabled: Boolean;
begin
  Result := PCefBrowserHost(FData).is_window_rendering_disabled(FData) <> 0
end;

procedure TCefBrowserHostRef.NotifyMoveOrResizeStarted;
begin
  PCefBrowserHost(FData).notify_move_or_resize_started(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.NotifyScreenInfoChanged;
begin
  PCefBrowserHost(FData).notify_screen_info_changed(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.SetZoomLevel(zoomLevel: Double);
begin
  PCefBrowserHost(FData).set_zoom_level(PCefBrowserHost(FData), zoomLevel);
end;

procedure TCefBrowserHostRef.ShowDevTools(const windowInfo: PCefWindowInfo;
  const client: ICefClient; const settings: PCefBrowserSettings;
  inspectElementAt: PCefPoint);
begin
  PCefBrowserHost(FData).show_dev_tools(FData, windowInfo,
    TCef3Helper.CefGetData(client), settings, inspectElementAt);
end;

procedure TCefBrowserHostRef.StartDownload(const url: ustring);
var
  u: TCefString;
begin
  u := TCef3Helper.CefString(url);
  PCefBrowserHost(FData).start_download(PCefBrowserHost(FData), @u);
end;

procedure TCefBrowserHostRef.StopFinding(clearSelection: Boolean);
begin
  PCefBrowserHost(FData).stop_finding(FData, ord(clearSelection));
end;

class function TCefBrowserHostRef.UnWrap(data: Pointer): ICefBrowserHost;
begin
  if data <> nil then
    Result := create(data) as ICefBrowserHost
  else
    Result := nil;
end;

procedure TCefBrowserHostRef.WasHidden(hidden: Boolean);
begin
  PCefBrowserHost(FData).was_hidden(FData, ord(hidden));
end;

procedure TCefBrowserHostRef.WasResized;
begin
  PCefBrowserHost(FData).was_resized(FData);
end;

{ TCefProcessMessageRef }

function TCefProcessMessageRef.copy: ICefProcessMessage;
begin
  Result := UnWrap(PCefProcessMessage(FData)^.copy(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.GetArgumentList: ICefListValue;
begin
  Result := TCefListValueRef.UnWrap(PCefProcessMessage(FData)
    ^.get_argument_list(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.GetName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefProcessMessage(FData)
    ^.get_name(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.IsReadOnly: Boolean;
begin
  Result := PCefProcessMessage(FData)
    ^.is_read_only(PCefProcessMessage(FData)) <> 0;
end;

function TCefProcessMessageRef.IsValid: Boolean;
begin
  Result := PCefProcessMessage(FData)^.is_valid(PCefProcessMessage(FData)) <> 0;
end;

class function TCefProcessMessageRef.New(const name: ustring)
  : ICefProcessMessage;
var
  n: TCefString;
begin
  n := TCef3Helper.CefString(name);
  Result := UnWrap(cef_process_message_create(@n));
end;

class function TCefProcessMessageRef.UnWrap(data: Pointer): ICefProcessMessage;
begin
  if data <> nil then
    Result := create(data) as ICefProcessMessage
  else
    Result := nil;
end;

{ TCefStringVisitorOwn }

constructor TCefStringVisitorOwn.create;
begin
  inherited CreateData(SizeOf(TCefStringVisitor));
  with PCefStringVisitor(FData)^ do
    visit := cef_string_visitor_visit;
end;

procedure TCefStringVisitorOwn.visit(const str: ustring);
begin

end;

{ TCefFastStringVisitor }

constructor TCefFastStringVisitor.create(const callback: TCefStringVisitorProc);
begin
  inherited create;
  FVisit := callback;
end;

procedure TCefFastStringVisitor.visit(const str: ustring);
begin
  FVisit(str);
end;

{ TCefDownLoadItemRef }

function TCefDownLoadItemRef.GetContentDisposition: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDownloadItem(FData)
    ^.get_content_disposition(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetCurrentSpeed: Int64;
begin
  Result := PCefDownloadItem(FData)^.get_current_speed(PCefDownloadItem(FData));
end;

function TCefDownLoadItemRef.GetEndTime: TDateTime;
begin
  Result := TCef3Helper.CefTimeToDateTime(PCefDownloadItem(FData)
    ^.get_end_time(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetFullPath: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDownloadItem(FData)
    ^.get_full_path(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetId: Cardinal;
begin
  Result := PCefDownloadItem(FData)^.get_id(PCefDownloadItem(FData));
end;

function TCefDownLoadItemRef.GetMimeType: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDownloadItem(FData)
    ^.get_mime_type(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetOriginalUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDownloadItem(FData)^.get_original_url(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetPercentComplete: Integer;
begin
  Result := PCefDownloadItem(FData)^.get_percent_complete
    (PCefDownloadItem(FData));
end;

function TCefDownLoadItemRef.GetReceivedBytes: Int64;
begin
  Result := PCefDownloadItem(FData)^.get_received_bytes
    (PCefDownloadItem(FData));
end;

function TCefDownLoadItemRef.GetStartTime: TDateTime;
begin
  Result := TCef3Helper.CefTimeToDateTime(PCefDownloadItem(FData)
    ^.get_start_time(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetSuggestedFileName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDownloadItem(FData)
    ^.get_suggested_file_name(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetTotalBytes: Int64;
begin
  Result := PCefDownloadItem(FData)^.get_total_bytes(PCefDownloadItem(FData));
end;

function TCefDownLoadItemRef.GetUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDownloadItem(FData)
    ^.get_url(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.IsCanceled: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_canceled(PCefDownloadItem(FData)) <> 0;
end;

function TCefDownLoadItemRef.IsComplete: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_complete(PCefDownloadItem(FData)) <> 0;
end;

function TCefDownLoadItemRef.IsInProgress: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_in_progress
    (PCefDownloadItem(FData)) <> 0;
end;

function TCefDownLoadItemRef.IsValid: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_valid(PCefDownloadItem(FData)) <> 0;
end;

class function TCefDownLoadItemRef.UnWrap(data: Pointer): ICefDownloadItem;
begin
  if data <> nil then
    Result := create(data) as ICefDownloadItem
  else
    Result := nil;
end;

{ TCefBeforeDownloadCallbackRef }

procedure TCefBeforeDownloadCallbackRef.cont(const downloadPath: ustring;
  showDialog: Boolean);
var
  dp: TCefString;
begin
  dp := TCef3Helper.CefString(downloadPath);
  PCefBeforeDownloadCallback(FData).cont(PCefBeforeDownloadCallback(FData), @dp,
    ord(showDialog));
end;

class function TCefBeforeDownloadCallbackRef.UnWrap(data: Pointer)
  : ICefBeforeDownloadCallback;
begin
  if data <> nil then
    Result := create(data) as ICefBeforeDownloadCallback
  else
    Result := nil;
end;

{ TCefDownloadItemCallbackRef }

procedure TCefDownloadItemCallbackRef.cancel;
begin
  PCefDownloadItemCallback(FData).cancel(PCefDownloadItemCallback(FData));
end;

procedure TCefDownloadItemCallbackRef.Pause;
begin
  PCefDownloadItemCallback(FData).pause(PCefDownloadItemCallback(FData));
end;

procedure TCefDownloadItemCallbackRef.Resume;
begin
  PCefDownloadItemCallback(FData).resume(PCefDownloadItemCallback(FData));
end;

class function TCefDownloadItemCallbackRef.UnWrap(data: Pointer)
  : ICefDownloadItemCallback;
begin
  if data <> nil then
    Result := create(data) as ICefDownloadItemCallback
  else
    Result := nil;
end;

{ TCefAuthCallbackRef }

procedure TCefAuthCallbackRef.cancel;
begin
  PCefAuthCallback(FData).cancel(PCefAuthCallback(FData));
end;

procedure TCefAuthCallbackRef.cont(const username, password: ustring);
var
  u, p: TCefString;
begin
  u := TCef3Helper.CefString(username);
  p := TCef3Helper.CefString(password);
  PCefAuthCallback(FData).cont(PCefAuthCallback(FData), @u, @p);
end;

class function TCefAuthCallbackRef.UnWrap(data: Pointer): ICefAuthCallback;
begin
  if data <> nil then
    Result := create(data) as ICefAuthCallback
  else
    Result := nil;
end;

{ TCefJsDialogCallbackRef }

procedure TCefJsDialogCallbackRef.cont(success: Boolean;
  const userInput: ustring);
var
  ui: TCefString;
begin
  ui := TCef3Helper.CefString(userInput);
  PCefJsDialogCallback(FData).cont(PCefJsDialogCallback(FData),
    ord(success), @ui);
end;

class function TCefJsDialogCallbackRef.UnWrap(data: Pointer)
  : ICefJsDialogCallback;
begin
  if data <> nil then
    Result := create(data) as ICefJsDialogCallback
  else
    Result := nil;
end;

{ TCefCommandLineRef }

procedure TCefCommandLineRef.AppendArgument(const argument: ustring);
var
  a: TCefString;
begin
  a := TCef3Helper.CefString(argument);
  PCefCommandLine(FData).append_argument(PCefCommandLine(FData), @a);
end;

procedure TCefCommandLineRef.AppendSwitch(const name: ustring);
var
  n: TCefString;
begin
  n := TCef3Helper.CefString(name);
  PCefCommandLine(FData).append_switch(PCefCommandLine(FData), @n);
end;

procedure TCefCommandLineRef.AppendSwitchWithValue(const name, value: ustring);
var
  n, v: TCefString;
begin
  n := TCef3Helper.CefString(name);
  v := TCef3Helper.CefString(value);
  PCefCommandLine(FData).append_switch_with_value
    (PCefCommandLine(FData), @n, @v);
end;

function TCefCommandLineRef.copy: ICefCommandLine;
begin
  Result := UnWrap(PCefCommandLine(FData).copy(PCefCommandLine(FData)));
end;

procedure TCefCommandLineRef.GetArguments(arguments: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefCommandLine(FData).get_arguments(PCefCommandLine(FData), list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      arguments.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

procedure TCefCommandLineRef.GetArgv(args: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefCommandLine(FData).get_argv(FData, list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      args.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefCommandLineRef.GetCommandLineString: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefCommandLine(FData)
    .get_command_line_string(PCefCommandLine(FData)));
end;

function TCefCommandLineRef.GetProgram: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefCommandLine(FData)
    .get_program(PCefCommandLine(FData)));
end;

procedure TCefCommandLineRef.GetSwitches(switches: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefCommandLine(FData).get_switches(PCefCommandLine(FData), list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      switches.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefCommandLineRef.GetSwitchValue(const name: ustring): ustring;
var
  n: TCefString;
begin
  n := TCef3Helper.CefString(name);
  Result := TCef3Helper.CefStringFreeAndGet(PCefCommandLine(FData)
    .get_switch_value(PCefCommandLine(FData), @n));
end;

class function TCefCommandLineRef.Global: ICefCommandLine;
begin
  Result := UnWrap(cef_command_line_get_global);
end;

function TCefCommandLineRef.HasArguments: Boolean;
begin
  Result := PCefCommandLine(FData).has_arguments(PCefCommandLine(FData)) <> 0;
end;

function TCefCommandLineRef.HasSwitch(const name: ustring): Boolean;
var
  n: TCefString;
begin
  n := TCef3Helper.CefString(name);
  Result := PCefCommandLine(FData).has_switch(PCefCommandLine(FData), @n) <> 0;
end;

function TCefCommandLineRef.HasSwitches: Boolean;
begin
  Result := PCefCommandLine(FData).has_switches(PCefCommandLine(FData)) <> 0;
end;

procedure TCefCommandLineRef.InitFromArgv(argc: Integer;
  const argv: PPAnsiChar);
begin
  PCefCommandLine(FData).init_from_argv(PCefCommandLine(FData), argc, argv);
end;

procedure TCefCommandLineRef.InitFromString(const commandLine: ustring);
var
  cl: TCefString;
begin
  cl := TCef3Helper.CefString(commandLine);
  PCefCommandLine(FData).init_from_string(PCefCommandLine(FData), @cl);
end;

function TCefCommandLineRef.IsReadOnly: Boolean;
begin
  Result := PCefCommandLine(FData).is_read_only(PCefCommandLine(FData)) <> 0;
end;

function TCefCommandLineRef.IsValid: Boolean;
begin
  Result := PCefCommandLine(FData).is_valid(PCefCommandLine(FData)) <> 0;
end;

class function TCefCommandLineRef.New: ICefCommandLine;
begin
  Result := UnWrap(cef_command_line_create);
end;

procedure TCefCommandLineRef.PrependWrapper(const wrapper: ustring);
var
  w: TCefString;
begin
  w := TCef3Helper.CefString(wrapper);
  PCefCommandLine(FData).prepend_wrapper(PCefCommandLine(FData), @w);
end;

procedure TCefCommandLineRef.reset;
begin
  PCefCommandLine(FData).reset(PCefCommandLine(FData));
end;

procedure TCefCommandLineRef.SetProgram(const prog: ustring);
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(prog);
  PCefCommandLine(FData).set_program(PCefCommandLine(FData), @p);
end;

class function TCefCommandLineRef.UnWrap(data: Pointer): ICefCommandLine;
begin
  if data <> nil then
    Result := create(data) as ICefCommandLine
  else
    Result := nil;
end;

{ TCefSchemeRegistrarRef }

function TCefSchemeRegistrarRef.AddCustomScheme(const schemeName: ustring;
  IsStandard, IsLocal, IsDisplayIsolated: Boolean): Boolean;
var
  sn: TCefString;
begin
  sn := TCef3Helper.CefString(schemeName);
  Result := PCefSchemeRegistrar(FData).add_custom_scheme
    (PCefSchemeRegistrar(FData), @sn, ord(IsStandard), ord(IsLocal),
    ord(IsDisplayIsolated)) <> 0;
end;

class function TCefSchemeRegistrarRef.UnWrap(data: Pointer)
  : ICefSchemeRegistrar;
begin
  if data <> nil then
    Result := create(data) as ICefSchemeRegistrar
  else
    Result := nil;
end;

{ TCefGeolocationCallbackRef }

procedure TCefGeolocationCallbackRef.cont(allow: Boolean);
begin
  PCefGeolocationCallback(FData).cont(PCefGeolocationCallback(FData),
    ord(allow));
end;

class function TCefGeolocationCallbackRef.UnWrap(data: Pointer)
  : ICefGeolocationCallback;
begin
  if data <> nil then
    Result := create(data) as ICefGeolocationCallback
  else
    Result := nil;
end;

{ TCefContextMenuParamsRef }

function TCefContextMenuParamsRef.GetDictionarySuggestions(const suggestions
  : TStringList): Boolean;
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    Result := PCefContextMenuParams(FData).get_dictionary_suggestions
      (PCefContextMenuParams(FData), list) <> 0;
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      suggestions.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefContextMenuParamsRef.GetEditStateFlags
  : TCefContextMenuEditStateFlags;
begin
  Byte(Result) := PCefContextMenuParams(FData).get_edit_state_flags
    (PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetFrameCharset: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefContextMenuParams(FData)
    .get_frame_charset(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetFrameUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefContextMenuParams(FData)
    .get_frame_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetLinkUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefContextMenuParams(FData)
    .get_link_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetMediaStateFlags
  : TCefContextMenuMediaStateFlags;
begin
  word(Result) := PCefContextMenuParams(FData).get_media_state_flags
    (PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetMediaType: TCefContextMenuMediaType;
begin
  Result := PCefContextMenuParams(FData)
    .get_media_type(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetMisspelledWord: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefContextMenuParams(FData)
    .get_misspelled_word(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetPageUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefContextMenuParams(FData)
    .get_page_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetSelectionText: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefContextMenuParams(FData)
    .get_selection_text(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetSourceUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefContextMenuParams(FData)
    .get_source_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetTypeFlags: TCefContextMenuTypeFlags;
begin
  Byte(Result) := PCefContextMenuParams(FData)
    .get_type_flags(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetUnfilteredLinkUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefContextMenuParams(FData)
    .get_unfiltered_link_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetXCoord: Integer;
begin
  Result := PCefContextMenuParams(FData)
    .get_xcoord(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetYCoord: Integer;
begin
  Result := PCefContextMenuParams(FData)
    .get_ycoord(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.IsCustomMenu: Boolean;
begin
  Result := PCefContextMenuParams(FData).is_custom_menu(PCefContextMenuParams(FData)) <> 0;
end;

function TCefContextMenuParamsRef.IsEditable: Boolean;
begin
  Result := PCefContextMenuParams(FData)
    .is_editable(PCefContextMenuParams(FData)) <> 0;
end;

function TCefContextMenuParamsRef.IsPepperMenu: Boolean;
begin
  Result := PCefContextMenuParams(FData).is_pepper_menu(PCefContextMenuParams(FData)) <> 0;
end;

function TCefContextMenuParamsRef.IsSpellCheckEnabled: Boolean;
begin
  Result := PCefContextMenuParams(FData).is_spell_check_enabled
    (PCefContextMenuParams(FData)) <> 0;
end;

function TCefContextMenuParamsRef.HasImageContents: Boolean;
begin
  Result := PCefContextMenuParams(FData).has_image_contents
    (PCefContextMenuParams(FData)) <> 0;
end;

class function TCefContextMenuParamsRef.UnWrap(data: Pointer)
  : ICefContextMenuParams;
begin
  if data <> nil then
    Result := create(data) as ICefContextMenuParams
  else
    Result := nil;
end;

{ TCefMenuModelRef }

function TCefMenuModelRef.AddCheckItem(commandId: Integer;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TCef3Helper.CefString(text);
  Result := PCefMenuModel(FData).add_check_item(PCefMenuModel(FData),
    commandId, @t) <> 0;
end;

function TCefMenuModelRef.AddItem(commandId: Integer;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TCef3Helper.CefString(text);
  Result := PCefMenuModel(FData).add_item(PCefMenuModel(FData),
    commandId, @t) <> 0;
end;

function TCefMenuModelRef.AddRadioItem(commandId: Integer; const text: ustring;
  groupId: Integer): Boolean;
var
  t: TCefString;
begin
  t := TCef3Helper.CefString(text);
  Result := PCefMenuModel(FData).add_radio_item(PCefMenuModel(FData), commandId,
    @t, groupId) <> 0;
end;

function TCefMenuModelRef.AddSeparator: Boolean;
begin
  Result := PCefMenuModel(FData).add_separator(PCefMenuModel(FData)) <> 0;
end;

function TCefMenuModelRef.AddSubMenu(commandId: Integer; const text: ustring)
  : ICefMenuModel;
var
  t: TCefString;
begin
  t := TCef3Helper.CefString(text);
  Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)
    .add_sub_menu(PCefMenuModel(FData), commandId, @t));
end;

function TCefMenuModelRef.clear: Boolean;
begin
  Result := PCefMenuModel(FData).clear(PCefMenuModel(FData)) <> 0;
end;

function TCefMenuModelRef.GetAccelerator(commandId: Integer;
  out keyCode: Integer; out shiftPressed, ctrlPressed,
  altPressed: Boolean): Boolean;
var
  sp, cp, ap: Integer;
begin
  Result := PCefMenuModel(FData).get_accelerator(PCefMenuModel(FData),
    commandId, @keyCode, @sp, @cp, @ap) <> 0;
  shiftPressed := sp <> 0;
  ctrlPressed := cp <> 0;
  altPressed := ap <> 0;
end;

function TCefMenuModelRef.GetAcceleratorAt(index: Integer; out keyCode: Integer;
  out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
var
  sp, cp, ap: Integer;
begin
  Result := PCefMenuModel(FData).get_accelerator_at(PCefMenuModel(FData), index,
    @keyCode, @sp, @cp, @ap) <> 0;
  shiftPressed := sp <> 0;
  ctrlPressed := cp <> 0;
  altPressed := ap <> 0;
end;

function TCefMenuModelRef.GetCommandIdAt(index: Integer): Integer;
begin
  Result := PCefMenuModel(FData).get_command_id_at(PCefMenuModel(FData), index);
end;

function TCefMenuModelRef.GetCount: Integer;
begin
  Result := PCefMenuModel(FData).get_count(PCefMenuModel(FData));
end;

function TCefMenuModelRef.GetGroupId(commandId: Integer): Integer;
begin
  Result := PCefMenuModel(FData).get_group_id(PCefMenuModel(FData), commandId);
end;

function TCefMenuModelRef.GetGroupIdAt(index: Integer): Integer;
begin
  Result := PCefMenuModel(FData).get_group_id(PCefMenuModel(FData), index);
end;

function TCefMenuModelRef.GetIndexOf(commandId: Integer): Integer;
begin
  Result := PCefMenuModel(FData).get_index_of(PCefMenuModel(FData), commandId);
end;

function TCefMenuModelRef.GetLabel(commandId: Integer): ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefMenuModel(FData)
    .get_label(PCefMenuModel(FData), commandId));
end;

function TCefMenuModelRef.GetLabelAt(index: Integer): ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefMenuModel(FData)
    .get_label_at(PCefMenuModel(FData), index));
end;

function TCefMenuModelRef.GetSubMenu(commandId: Integer): ICefMenuModel;
begin
  Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)
    .get_sub_menu(PCefMenuModel(FData), commandId));
end;

function TCefMenuModelRef.GetSubMenuAt(index: Integer): ICefMenuModel;
begin
  Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)
    .get_sub_menu_at(PCefMenuModel(FData), index));
end;

function TCefMenuModelRef.GetType(commandId: Integer): TCefMenuItemType;
begin
  Result := PCefMenuModel(FData).get_type(PCefMenuModel(FData), commandId);
end;

function TCefMenuModelRef.GetTypeAt(index: Integer): TCefMenuItemType;
begin
  Result := PCefMenuModel(FData).get_type_at(PCefMenuModel(FData), index);
end;

function TCefMenuModelRef.HasAccelerator(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).has_accelerator(PCefMenuModel(FData),
    commandId) <> 0;
end;

function TCefMenuModelRef.HasAcceleratorAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).has_accelerator_at(PCefMenuModel(FData),
    index) <> 0;
end;

function TCefMenuModelRef.InsertCheckItemAt(index, commandId: Integer;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TCef3Helper.CefString(text);
  Result := PCefMenuModel(FData).insert_check_item_at(PCefMenuModel(FData),
    index, commandId, @t) <> 0;
end;

function TCefMenuModelRef.InsertItemAt(index, commandId: Integer;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TCef3Helper.CefString(text);
  Result := PCefMenuModel(FData).insert_item_at(PCefMenuModel(FData), index,
    commandId, @t) <> 0;
end;

function TCefMenuModelRef.InsertRadioItemAt(index, commandId: Integer;
  const text: ustring; groupId: Integer): Boolean;
var
  t: TCefString;
begin
  t := TCef3Helper.CefString(text);
  Result := PCefMenuModel(FData).insert_radio_item_at(PCefMenuModel(FData),
    index, commandId, @t, groupId) <> 0;
end;

function TCefMenuModelRef.InsertSeparatorAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).insert_separator_at(PCefMenuModel(FData),
    index) <> 0;
end;

function TCefMenuModelRef.InsertSubMenuAt(index, commandId: Integer;
  const text: ustring): ICefMenuModel;
var
  t: TCefString;
begin
  t := TCef3Helper.CefString(text);
  Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)
    .insert_sub_menu_at(PCefMenuModel(FData), index, commandId, @t));
end;

function TCefMenuModelRef.IsChecked(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).is_checked(PCefMenuModel(FData),
    commandId) <> 0;
end;

function TCefMenuModelRef.IsCheckedAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).is_checked_at(PCefMenuModel(FData),
    index) <> 0;
end;

function TCefMenuModelRef.IsEnabled(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).is_enabled(PCefMenuModel(FData),
    commandId) <> 0;
end;

function TCefMenuModelRef.IsEnabledAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).is_enabled_at(PCefMenuModel(FData),
    index) <> 0;
end;

function TCefMenuModelRef.IsVisible(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).is_visible(PCefMenuModel(FData),
    commandId) <> 0;
end;

function TCefMenuModelRef.isVisibleAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).is_visible_at(PCefMenuModel(FData),
    index) <> 0;
end;

function TCefMenuModelRef.remove(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).remove(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.RemoveAccelerator(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).remove_accelerator(PCefMenuModel(FData),
    commandId) <> 0;
end;

function TCefMenuModelRef.RemoveAcceleratorAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).remove_accelerator_at(PCefMenuModel(FData),
    index) <> 0;
end;

function TCefMenuModelRef.RemoveAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).remove_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.SetAccelerator(commandId, keyCode: Integer;
  shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData).set_accelerator(PCefMenuModel(FData),
    commandId, keyCode, ord(shiftPressed), ord(ctrlPressed),
    ord(altPressed)) <> 0;
end;

function TCefMenuModelRef.SetAcceleratorAt(index, keyCode: Integer;
  shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData).set_accelerator_at(PCefMenuModel(FData), index,
    keyCode, ord(shiftPressed), ord(ctrlPressed), ord(altPressed)) <> 0;
end;

function TCefMenuModelRef.setChecked(commandId: Integer;
  checked: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData).set_checked(PCefMenuModel(FData), commandId,
    ord(checked)) <> 0;
end;

function TCefMenuModelRef.setCheckedAt(index: Integer;
  checked: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData).set_checked_at(PCefMenuModel(FData), index,
    ord(checked)) <> 0;
end;

function TCefMenuModelRef.SetCommandIdAt(index, commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).set_command_id_at(PCefMenuModel(FData), index,
    commandId) <> 0;
end;

function TCefMenuModelRef.SetEnabled(commandId: Integer;
  enabled: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData).set_enabled(PCefMenuModel(FData), commandId,
    ord(enabled)) <> 0;
end;

function TCefMenuModelRef.SetEnabledAt(index: Integer;
  enabled: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData).set_enabled_at(PCefMenuModel(FData), index,
    ord(enabled)) <> 0;
end;

function TCefMenuModelRef.SetGroupId(commandId, groupId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).set_group_id(PCefMenuModel(FData), commandId,
    groupId) <> 0;
end;

function TCefMenuModelRef.SetGroupIdAt(index, groupId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData).set_group_id_at(PCefMenuModel(FData), index,
    groupId) <> 0;
end;

function TCefMenuModelRef.SetLabel(commandId: Integer;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TCef3Helper.CefString(text);
  Result := PCefMenuModel(FData).set_label(PCefMenuModel(FData),
    commandId, @t) <> 0;
end;

function TCefMenuModelRef.SetLabelAt(index: Integer;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TCef3Helper.CefString(text);
  Result := PCefMenuModel(FData).set_label_at(PCefMenuModel(FData),
    index, @t) <> 0;
end;

function TCefMenuModelRef.SetVisible(commandId: Integer;
  visible: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData).set_visible(PCefMenuModel(FData), commandId,
    ord(visible)) <> 0;
end;

function TCefMenuModelRef.SetVisibleAt(index: Integer;
  visible: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData).set_visible_at(PCefMenuModel(FData), index,
    ord(visible)) <> 0;
end;

class function TCefMenuModelRef.UnWrap(data: Pointer): ICefMenuModel;
begin
  if data <> nil then
    Result := create(data) as ICefMenuModel
  else
    Result := nil;
end;

{ TCefListValueRef }

function TCefListValueRef.clear: Boolean;
begin
  Result := PCefListValue(FData).clear(PCefListValue(FData)) <> 0;
end;

function TCefListValueRef.copy: ICefListValue;
begin
  Result := UnWrap(PCefListValue(FData).copy(PCefListValue(FData)));
end;

class function TCefListValueRef.New: ICefListValue;
begin
  Result := UnWrap(cef_list_value_create);
end;

function TCefListValueRef.GetBinary(index: Integer): ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefListValue(FData)
    .get_binary(PCefListValue(FData), index));
end;

function TCefListValueRef.GetBool(index: Integer): Boolean;
begin
  Result := PCefListValue(FData).get_bool(PCefListValue(FData), index) <> 0;
end;

function TCefListValueRef.GetDictionary(index: Integer): ICefDictionaryValue;
begin
  Result := TCefDictionaryValueRef.UnWrap(PCefListValue(FData)
    .get_dictionary(PCefListValue(FData), index));
end;

function TCefListValueRef.GetDouble(index: Integer): Double;
begin
  Result := PCefListValue(FData).get_double(PCefListValue(FData), index);
end;

function TCefListValueRef.GetInt(index: Integer): Integer;
begin
  Result := PCefListValue(FData).get_int(PCefListValue(FData), index);
end;

function TCefListValueRef.GetList(index: Integer): ICefListValue;
begin
  Result := UnWrap(PCefListValue(FData).get_list(PCefListValue(FData), index));
end;

function TCefListValueRef.GetSize: NativeUInt;
begin
  Result := PCefListValue(FData).get_size(PCefListValue(FData));
end;

function TCefListValueRef.GetString(index: Integer): ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefListValue(FData)
    .get_string(PCefListValue(FData), index));
end;

function TCefListValueRef.GetType(index: Integer): TCefValueType;
begin
  Result := PCefListValue(FData).get_type(PCefListValue(FData), index);
end;

function TCefListValueRef.GetValue(index: Integer): ICefValue;
begin
  Result := TCefValueRef.UnWrap(PCefListValue(FData).get_value(PCefListValue(FData), index));
end;

function TCefListValueRef.IsEqual(const that: ICefListValue): Boolean;
begin
  Result := PCefListValue(FData).is_equal(PCefListValue(FData), TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefListValueRef.IsOwned: Boolean;
begin
  Result := PCefListValue(FData).is_owned(PCefListValue(FData)) <> 0;
end;

function TCefListValueRef.IsReadOnly: Boolean;
begin
  Result := PCefListValue(FData).is_read_only(PCefListValue(FData)) <> 0;
end;

function TCefListValueRef.IsSame(const that: ICefListValue): Boolean;
begin
  Result := PCefListValue(FData).is_same(PCefListValue(FData), TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefListValueRef.IsValid: Boolean;
begin
  Result := PCefListValue(FData).is_valid(PCefListValue(FData)) <> 0;
end;

function TCefListValueRef.remove(index: Integer): Boolean;
begin
  Result := PCefListValue(FData).remove(PCefListValue(FData), index) <> 0;
end;

function TCefListValueRef.SetBinary(index: Integer;
  const value: ICefBinaryValue): Boolean;
begin
  Result := PCefListValue(FData).set_binary(PCefListValue(FData), index,
    TCef3Helper.CefGetData(value)) <> 0;
end;

function TCefListValueRef.SetBool(index: Integer; value: Boolean): Boolean;
begin
  Result := PCefListValue(FData).set_bool(PCefListValue(FData), index,
    ord(value)) <> 0;
end;

function TCefListValueRef.SetDictionary(index: Integer;
  const value: ICefDictionaryValue): Boolean;
begin
  Result := PCefListValue(FData).set_dictionary(PCefListValue(FData), index,
    TCef3Helper.CefGetData(value)) <> 0;
end;

function TCefListValueRef.SetDouble(index: Integer; value: Double): Boolean;
begin
  Result := PCefListValue(FData).set_double(PCefListValue(FData), index,
    value) <> 0;
end;

function TCefListValueRef.SetInt(index, value: Integer): Boolean;
begin
  Result := PCefListValue(FData).set_int(PCefListValue(FData), index,
    value) <> 0;
end;

function TCefListValueRef.SetList(index: Integer;
  const value: ICefListValue): Boolean;
begin
  Result := PCefListValue(FData).set_list(PCefListValue(FData), index,
    TCef3Helper.CefGetData(value)) <> 0;
end;

function TCefListValueRef.SetNull(index: Integer): Boolean;
begin
  Result := PCefListValue(FData).set_null(PCefListValue(FData), index) <> 0;
end;

function TCefListValueRef.SetSize(size: NativeUInt): Boolean;
begin
  Result := PCefListValue(FData).set_size(PCefListValue(FData), size) <> 0;
end;

function TCefListValueRef.SetString(index: Integer;
  const value: ustring): Boolean;
var
  v: TCefString;
begin
  v := TCef3Helper.CefString(value);
  Result := PCefListValue(FData).set_string(PCefListValue(FData),
    index, @v) <> 0;
end;

function TCefListValueRef.SetValue(index: Integer;
  const value: ICefValue): Boolean;
begin
  Result := PCefListValue(FData).set_value(PCefListValue(FData), index, TCef3Helper.CefGetData(value)) <> 0;
end;

class function TCefListValueRef.UnWrap(data: Pointer): ICefListValue;
begin
  if data <> nil then
    Result := create(data) as ICefListValue
  else
    Result := nil;
end;

{ TCefBinaryValueRef }

function TCefBinaryValueRef.copy: ICefBinaryValue;
begin
  Result := UnWrap(PCefBinaryValue(FData).copy(PCefBinaryValue(FData)));
end;

function TCefBinaryValueRef.GetData(buffer: Pointer;
  bufferSize, dataOffset: NativeUInt): NativeUInt;
begin
  Result := PCefBinaryValue(FData).get_data(PCefBinaryValue(FData), buffer,
    bufferSize, dataOffset);
end;

function TCefBinaryValueRef.GetSize: NativeUInt;
begin
  Result := PCefBinaryValue(FData).get_size(PCefBinaryValue(FData));
end;

function TCefBinaryValueRef.IsEqual(const that: ICefBinaryValue): Boolean;
begin
  Result := PCefBinaryValue(FData).is_equal(PCefBinaryValue(FData), TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefBinaryValueRef.IsOwned: Boolean;
begin
  Result := PCefBinaryValue(FData).is_owned(PCefBinaryValue(FData)) <> 0;
end;

function TCefBinaryValueRef.IsSame(const that: ICefBinaryValue): Boolean;
begin
  Result := PCefBinaryValue(FData).is_same(PCefBinaryValue(FData), TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefBinaryValueRef.IsValid: Boolean;
begin
  Result := PCefBinaryValue(FData).is_valid(PCefBinaryValue(FData)) <> 0;
end;

class function TCefBinaryValueRef.New(const data: Pointer; dataSize: NativeUInt)
  : ICefBinaryValue;
begin
  Result := UnWrap(cef_binary_value_create(data, dataSize));
end;

class function TCefBinaryValueRef.UnWrap(data: Pointer): ICefBinaryValue;
begin
  if data <> nil then
    Result := create(data) as ICefBinaryValue
  else
    Result := nil;
end;

{ TCefDictionaryValueRef }

function TCefDictionaryValueRef.clear: Boolean;
begin
  Result := PCefDictionaryValue(FData).clear(PCefDictionaryValue(FData)) <> 0;
end;

function TCefDictionaryValueRef.copy(excludeEmptyChildren: Boolean)
  : ICefDictionaryValue;
begin
  Result := UnWrap(PCefDictionaryValue(FData).copy(PCefDictionaryValue(FData),
    ord(excludeEmptyChildren)));
end;

function TCefDictionaryValueRef.GetBinary(const key: ustring): ICefBinaryValue;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := TCefBinaryValueRef.UnWrap(PCefDictionaryValue(FData)
    .get_binary(PCefDictionaryValue(FData), @k));
end;

function TCefDictionaryValueRef.GetBool(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData)
    .get_bool(PCefDictionaryValue(FData), @k) <> 0;
end;

function TCefDictionaryValueRef.GetDictionary(const key: ustring)
  : ICefDictionaryValue;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := UnWrap(PCefDictionaryValue(FData)
    .get_dictionary(PCefDictionaryValue(FData), @k));
end;

function TCefDictionaryValueRef.GetDouble(const key: ustring): Double;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData)
    .get_double(PCefDictionaryValue(FData), @k);
end;

function TCefDictionaryValueRef.GetInt(const key: ustring): Integer;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData).get_int(PCefDictionaryValue(FData), @k);
end;

function TCefDictionaryValueRef.GetKeys(const keys: TStrings): Boolean;
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    Result := PCefDictionaryValue(FData).get_keys(PCefDictionaryValue(FData),
      list) <> 0;
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      keys.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefDictionaryValueRef.GetList(const key: ustring): ICefListValue;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := TCefListValueRef.UnWrap(PCefDictionaryValue(FData)
    .get_list(PCefDictionaryValue(FData), @k));
end;

function TCefDictionaryValueRef.GetSize: NativeUInt;
begin
  Result := PCefDictionaryValue(FData).get_size(PCefDictionaryValue(FData));
end;

function TCefDictionaryValueRef.GetString(const key: ustring): ustring;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := TCef3Helper.CefStringFreeAndGet(PCefDictionaryValue(FData)
    .get_string(PCefDictionaryValue(FData), @k));
end;

function TCefDictionaryValueRef.GetType(const key: ustring): TCefValueType;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData).get_type(PCefDictionaryValue(FData), @k);
end;

function TCefDictionaryValueRef.GetValue(const key: ustring): ICefValue;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := TCefValueRef.UnWrap(PCefDictionaryValue(FData).get_value(PCefDictionaryValue(FData), @k));
end;

function TCefDictionaryValueRef.HasKey(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData)
    .has_key(PCefDictionaryValue(FData), @k) <> 0;
end;

function TCefDictionaryValueRef.IsEqual(
  const that: ICefDictionaryValue): Boolean;
begin
  Result := PCefDictionaryValue(FData).is_equal(PCefDictionaryValue(FData), TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefDictionaryValueRef.IsOwned: Boolean;
begin
  Result := PCefDictionaryValue(FData)
    .is_owned(PCefDictionaryValue(FData)) <> 0;
end;

function TCefDictionaryValueRef.IsReadOnly: Boolean;
begin
  Result := PCefDictionaryValue(FData)
    .is_read_only(PCefDictionaryValue(FData)) <> 0;
end;

function TCefDictionaryValueRef.IsSame(
  const that: ICefDictionaryValue): Boolean;
begin
  Result := PCefDictionaryValue(FData).is_same(PCefDictionaryValue(FData), TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefDictionaryValueRef.IsValid: Boolean;
begin
  Result := PCefDictionaryValue(FData)
    .is_valid(PCefDictionaryValue(FData)) <> 0;
end;

class function TCefDictionaryValueRef.New: ICefDictionaryValue;
begin
  Result := UnWrap(cef_dictionary_value_create);
end;

function TCefDictionaryValueRef.remove(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData)
    .remove(PCefDictionaryValue(FData), @k) <> 0;
end;

function TCefDictionaryValueRef.SetBinary(const key: ustring;
  const value: ICefBinaryValue): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData).set_binary(PCefDictionaryValue(FData),
    @k, TCef3Helper.CefGetData(value)) <> 0;
end;

function TCefDictionaryValueRef.SetBool(const key: ustring;
  value: Boolean): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData).set_bool(PCefDictionaryValue(FData), @k,
    ord(value)) <> 0;
end;

function TCefDictionaryValueRef.SetDictionary(const key: ustring;
  const value: ICefDictionaryValue): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData)
    .set_dictionary(PCefDictionaryValue(FData), @k,
    TCef3Helper.CefGetData(value)) <> 0;
end;

function TCefDictionaryValueRef.SetDouble(const key: ustring;
  value: Double): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData).set_double(PCefDictionaryValue(FData),
    @k, value) <> 0;
end;

function TCefDictionaryValueRef.SetInt(const key: ustring;
  value: Integer): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData).set_int(PCefDictionaryValue(FData), @k,
    value) <> 0;
end;

function TCefDictionaryValueRef.SetList(const key: ustring;
  const value: ICefListValue): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData).set_list(PCefDictionaryValue(FData), @k,
    TCef3Helper.CefGetData(value)) <> 0;
end;

function TCefDictionaryValueRef.SetNull(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData)
    .set_null(PCefDictionaryValue(FData), @k) <> 0;
end;

function TCefDictionaryValueRef.SetString(const key, value: ustring): Boolean;
var
  k, v: TCefString;
begin
  k := TCef3Helper.CefString(key);
  v := TCef3Helper.CefString(value);
  Result := PCefDictionaryValue(FData).set_string(PCefDictionaryValue(FData),
    @k, @v) <> 0;
end;

function TCefDictionaryValueRef.SetValue(const key: ustring;
  const value: ICefValue): Boolean;
var
  k: TCefString;
begin
  k := TCef3Helper.CefString(key);
  Result := PCefDictionaryValue(FData).set_value(PCefDictionaryValue(FData), @k, TCef3Helper.CefGetData(value)) <> 0;
end;

class function TCefDictionaryValueRef.UnWrap(data: Pointer)
  : ICefDictionaryValue;
begin
  if data <> nil then
    Result := create(data) as ICefDictionaryValue
  else
    Result := nil;
end;

{ TCefBrowserProcessHandlerOwn }

constructor TCefBrowserProcessHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefBrowserProcessHandler));
  with PCefBrowserProcessHandler(FData)^ do
  begin
    on_context_initialized :=
      cef_browser_process_handler_on_context_initialized;
    on_before_child_process_launch :=
      cef_browser_process_handler_on_before_child_process_launch;
    on_render_process_thread_created :=
      cef_browser_process_handler_on_render_process_thread_created;
    get_print_handler := nil; // linux
  end;
end;

procedure TCefBrowserProcessHandlerOwn.OnBeforeChildProcessLaunch
  (const commandLine: ICefCommandLine);
begin

end;

procedure TCefBrowserProcessHandlerOwn.OnContextInitialized;
begin

end;

procedure TCefBrowserProcessHandlerOwn.OnRenderProcessThreadCreated
  (const extraInfo: ICefListValue);
begin

end;

{ TCefRenderProcessHandlerOwn }

constructor TCefRenderProcessHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefRenderProcessHandler));
  with PCefRenderProcessHandler(FData)^ do
  begin
    on_render_thread_created :=
      cef_render_process_handler_on_render_thread_created;
    on_web_kit_initialized := cef_render_process_handler_on_web_kit_initialized;
    on_browser_created := cef_render_process_handler_on_browser_created;
    on_browser_destroyed := cef_render_process_handler_on_browser_destroyed;
    get_load_handler := cef_render_process_handler_get_load_handler;
    on_before_navigation := cef_render_process_handler_on_before_navigation;
    on_context_created := cef_render_process_handler_on_context_created;
    on_context_released := cef_render_process_handler_on_context_released;
    on_uncaught_exception := cef_render_process_handler_on_uncaught_exception;
    on_focused_node_changed :=
      cef_render_process_handler_on_focused_node_changed;
    on_process_message_received :=
      cef_render_process_handler_on_process_message_received;
  end;
end;

function TCefRenderProcessHandlerOwn.GetLoadHandler: PCefLoadHandler;
begin
  Result := nil;
end;

function TCefRenderProcessHandlerOwn.OnBeforeNavigation(const browser
  : ICefBrowser; const frame: ICefFrame; const request: ICefRequest;
  navigationType: TCefNavigationType; isRedirect: Boolean): Boolean;
begin
  Result := False;
end;

procedure TCefRenderProcessHandlerOwn.OnBrowserCreated(const browser
  : ICefBrowser);
begin

end;

procedure TCefRenderProcessHandlerOwn.OnBrowserDestroyed(const browser
  : ICefBrowser);
begin

end;

procedure TCefRenderProcessHandlerOwn.OnContextCreated(const browser
  : ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
begin

end;

procedure TCefRenderProcessHandlerOwn.OnContextReleased(const browser
  : ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
begin

end;

procedure TCefRenderProcessHandlerOwn.OnFocusedNodeChanged
  (const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
begin

end;

function TCefRenderProcessHandlerOwn.OnProcessMessageReceived
  (const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage): Boolean;
begin
  Result := False;
end;

procedure TCefRenderProcessHandlerOwn.OnRenderThreadCreated(const extraInfo
  : ICefListValue);
begin

end;

procedure TCefRenderProcessHandlerOwn.OnUncaughtException
  (const browser: ICefBrowser; const frame: ICefFrame;
  const context: ICefv8Context; const exception: ICefV8Exception;
  const stackTrace: ICefV8StackTrace);
begin

end;

procedure TCefRenderProcessHandlerOwn.OnWebKitInitialized;
begin

end;

{ TCefResourceHandlerOwn }

procedure TCefResourceHandlerOwn.cancel;
begin

end;

function TCefResourceHandlerOwn.CanGetCookie(const cookie: PCefCookie): Boolean;
begin
  Result := False;
end;

function TCefResourceHandlerOwn.CanSetCookie(const cookie: PCefCookie): Boolean;
begin
  Result := False;
end;

constructor TCefResourceHandlerOwn.create(const browser: ICefBrowser;
  const frame: ICefFrame; const schemeName: ustring;
  const request: ICefRequest);
begin
  inherited CreateData(SizeOf(TCefResourceHandler));
  with PCefResourceHandler(FData)^ do
  begin
    process_request := cef_resource_handler_process_request;
    get_response_headers := cef_resource_handler_get_response_headers;
    read_response := cef_resource_handler_read_response;
    can_get_cookie := cef_resource_handler_can_get_cookie;
    can_set_cookie := cef_resource_handler_can_set_cookie;
    cancel := cef_resource_handler_cancel;
  end;
end;

procedure TCefResourceHandlerOwn.GetResponseHeaders(const response
  : ICefResponse; out responseLength: Int64; out redirectUrl: ustring);
begin

end;

function TCefResourceHandlerOwn.ProcessRequest(const request: ICefRequest;
  const callback: ICefCallback): Boolean;
begin
  Result := False;
end;

function TCefResourceHandlerOwn.ReadResponse(const dataOut: Pointer;
  bytesToRead: Integer; var bytesRead: Integer;
  const callback: ICefCallback): Boolean;
begin
  Result := False;
end;

{ TCefSchemeHandlerFactoryOwn }

constructor TCefSchemeHandlerFactoryOwn.create(const AClass
  : TCefResourceHandlerClass);
begin
  inherited CreateData(SizeOf(TCefSchemeHandlerFactory));
  FClass := AClass;
  with PCefSchemeHandlerFactory(FData)^ do
    create := cef_scheme_handler_factory_create;
end;

function TCefSchemeHandlerFactoryOwn.New(const browser: ICefBrowser;
  const frame: ICefFrame; const schemeName: ustring; const request: ICefRequest)
  : ICefResourceHandler;
begin
  Result := FClass.create(browser, frame, schemeName, request);
end;

{ TCefCallbackRef }

procedure TCefCallbackRef.cancel;
begin
  PCefCallback(FData)^.cancel(PCefCallback(FData));
end;

procedure TCefCallbackRef.cont;
begin
  PCefCallback(FData)^.cont(PCefCallback(FData));
end;

class function TCefCallbackRef.UnWrap(data: Pointer): ICefCallback;
begin
  if data <> nil then
    Result := create(data) as ICefCallback
  else
    Result := nil;
end;

{ TCefUrlrequestClientOwn }

constructor TCefUrlrequestClientOwn.create;
begin
  inherited CreateData(SizeOf(TCefUrlRequestClient));
  with PCefUrlRequestClient(FData)^ do
  begin
    on_request_complete := cef_url_request_client_on_request_complete;
    on_upload_progress := cef_url_request_client_on_upload_progress;
    on_download_progress := cef_url_request_client_on_download_progress;
    on_download_data := cef_url_request_client_on_download_data;
    get_auth_credentials := cef_url_request_client_get_auth_credentials;
  end;
end;

procedure TCefUrlrequestClientOwn.OnDownloadData(const request: ICefUrlRequest;
  data: Pointer; dataLength: NativeUInt);
begin

end;

procedure TCefUrlrequestClientOwn.OnDownloadProgress(const request
  : ICefUrlRequest; current, total: UInt64);
begin

end;

function TCefUrlrequestClientOwn.OnGetAuthCredentials(isProxy: Boolean;
  const host: ustring; port: Integer; const realm, scheme: ustring;
  const callback: ICefAuthCallback): Boolean;
begin
  Result := False;
end;

procedure TCefUrlrequestClientOwn.OnRequestComplete(const request
  : ICefUrlRequest);
begin

end;

procedure TCefUrlrequestClientOwn.OnUploadProgress(const request
  : ICefUrlRequest; current, total: UInt64);
begin

end;

{ TCefUrlRequestRef }

procedure TCefUrlRequestRef.cancel;
begin
  PCefUrlRequest(FData).cancel(PCefUrlRequest(FData));
end;

class function TCefUrlRequestRef.New(const request: ICefRequest; const client: ICefUrlRequestClient;
  const requestContext: ICefRequestContext): ICefUrlRequest;
begin
  Result := UnWrap(cef_urlrequest_create(TCef3Helper.CefGetData(request), TCef3Helper.CefGetData(client), TCef3Helper.CefGetData(requestContext)));
end;

function TCefUrlRequestRef.GetRequest: ICefRequest;
begin
  Result := TCefRequestRef.UnWrap(PCefUrlRequest(FData)
    .get_request(PCefUrlRequest(FData)));
end;

function TCefUrlRequestRef.GetRequestError: Integer;
begin
  Result := PCefUrlRequest(FData).get_request_error(PCefUrlRequest(FData));
end;

function TCefUrlRequestRef.GetRequestStatus: TCefUrlRequestStatus;
begin
  Result := PCefUrlRequest(FData).get_request_status(PCefUrlRequest(FData));
end;

function TCefUrlRequestRef.GetResponse: ICefResponse;
begin
  Result := TCefResponseRef.UnWrap(PCefUrlRequest(FData)
    .get_response(PCefUrlRequest(FData)));
end;

class function TCefUrlRequestRef.UnWrap(data: Pointer): ICefUrlRequest;
begin
  if data <> nil then
    Result := create(data) as ICefUrlRequest
  else
    Result := nil;
end;

{ TCefWebPluginInfoVisitorOwn }

constructor TCefWebPluginInfoVisitorOwn.create;
begin
  inherited CreateData(SizeOf(TCefWebPluginInfoVisitor));
  PCefWebPluginInfoVisitor(FData).visit := cef_web_plugin_info_visitor_visit;
end;

function TCefWebPluginInfoVisitorOwn.visit(const info: ICefWebPluginInfo;
  count, total: Integer): Boolean;
begin
  Result := False;
end;

{ TCefFastWebPluginInfoVisitor }

constructor TCefFastWebPluginInfoVisitor.create
  (const proc: TCefWebPluginInfoVisitorProc);
begin
  inherited create;
  FProc := proc;
end;

function TCefFastWebPluginInfoVisitor.visit(const info: ICefWebPluginInfo;
  count, total: Integer): Boolean;
begin
  Result := FProc(info, count, total);
end;

{ TCefRequestCallbackRef }

procedure TCefRequestCallbackRef.Cancel;
begin
  PCefRequestCallback(FData).cancel(FData);
end;

procedure TCefRequestCallbackRef.Cont(allow: Boolean);
begin
  PCefRequestCallback(FData).cont(FData, Ord(allow));
end;

class function TCefRequestCallbackRef.UnWrap(data: Pointer): ICefRequestCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefRequestCallback else
    Result := nil;
end;

{ TCefV8StackFrameRef }

function TCefV8StackFrameRef.GetColumn: Integer;
begin
  Result := PCefV8StackFrame(FData).get_column(FData);
end;

function TCefV8StackFrameRef.GetFunctionName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefV8StackFrame(FData)
    .get_function_name(FData));
end;

function TCefV8StackFrameRef.GetLineNumber: Integer;
begin
  Result := PCefV8StackFrame(FData).get_line_number(FData);
end;

function TCefV8StackFrameRef.GetScriptName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefV8StackFrame(FData)
    .get_script_name(FData));
end;

function TCefV8StackFrameRef.GetScriptNameOrSourceUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefV8StackFrame(FData)
    .get_script_name_or_source_url(FData));
end;

function TCefV8StackFrameRef.IsConstructor: Boolean;
begin
  Result := PCefV8StackFrame(FData).is_constructor(FData) <> 0;
end;

function TCefV8StackFrameRef.IsEval: Boolean;
begin
  Result := PCefV8StackFrame(FData).is_eval(FData) <> 0;
end;

function TCefV8StackFrameRef.IsValid: Boolean;
begin
  Result := PCefV8StackFrame(FData).is_valid(FData) <> 0;
end;

class function TCefV8StackFrameRef.UnWrap(data: Pointer): ICefV8StackFrame;
begin
  if data <> nil then
    Result := create(data) as ICefV8StackFrame
  else
    Result := nil;
end;

{ TCefV8StackTraceRef }

class function TCefV8StackTraceRef.current(frameLimit: Integer)
  : ICefV8StackTrace;
begin
  Result := UnWrap(cef_v8stack_trace_get_current(frameLimit));
end;

function TCefV8StackTraceRef.GetFrame(index: Integer): ICefV8StackFrame;
begin
  Result := TCefV8StackFrameRef.UnWrap(PCefV8StackTrace(FData)
    .get_frame(FData, index));
end;

function TCefV8StackTraceRef.GetFrameCount: Integer;
begin
  Result := PCefV8StackTrace(FData).get_frame_count(FData);
end;

function TCefV8StackTraceRef.IsValid: Boolean;
begin
  Result := PCefV8StackTrace(FData).is_valid(FData) <> 0;
end;

class function TCefV8StackTraceRef.UnWrap(data: Pointer): ICefV8StackTrace;
begin
  if data <> nil then
    Result := create(data) as ICefV8StackTrace
  else
    Result := nil;
end;

{ TCefWebPluginUnstableCallbackOwn }

constructor TCefWebPluginUnstableCallbackOwn.create;
begin
  inherited CreateData(SizeOf(TCefWebPluginUnstableCallback));
  PCefWebPluginUnstableCallback(FData).is_unstable :=
    cef_web_plugin_unstable_callback_is_unstable;
end;

procedure TCefWebPluginUnstableCallbackOwn.IsUnstable(const path: ustring;
  unstable: Boolean);
begin

end;

{ TCefFastWebPluginUnstableCallback }

constructor TCefFastWebPluginUnstableCallback.create(const callback
  : TCefWebPluginIsUnstableProc);
begin
  FCallback := callback;
end;

procedure TCefFastWebPluginUnstableCallback.IsUnstable(const path: ustring;
  unstable: Boolean);
begin
  FCallback(path, unstable);
end;

{ TCefRunFileDialogCallbackOwn }

constructor TCefRunFileDialogCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRunFileDialogCallback));
  with PCefRunFileDialogCallback(FData)^ do
    on_file_dialog_dismissed := cef_run_file_dialog_callback_on_file_dialog_dismissed;
end;

procedure TCefRunFileDialogCallbackOwn.OnFileDialogDismissed(
  selectedAcceptFilter: Integer; filePaths: TStrings);
begin

end;

{ TCefFastRunFileDialogCallback }

procedure TCefFastRunFileDialogCallback.OnFileDialogDismissed(
  selectedAcceptFilter: Integer; filePaths: TStrings);
begin
  FCallback(selectedAcceptFilter, filePaths);
end;

constructor TCefFastRunFileDialogCallback.create
  (callback: TCefRunFileDialogCallbackProc);
begin
  inherited create;
  FCallback := callback;
end;

{ TCefTaskRef }

procedure TCefTaskRef.execute;
begin
  PCefTask(FData).execute(FData);
end;

class function TCefTaskRef.UnWrap(data: Pointer): ICefTask;
begin
  if data <> nil then
    Result := create(data) as ICefTask
  else
    Result := nil;
end;

{ TCefTaskRunnerRef }

function TCefTaskRunnerRef.BelongsToCurrentThread: Boolean;
begin
  Result := PCefTaskRunner(FData).belongs_to_current_thread(FData) <> 0;
end;

function TCefTaskRunnerRef.BelongsToThread(threadId: TCefThreadId): Boolean;
begin
  Result := PCefTaskRunner(FData).belongs_to_thread(FData, threadId) <> 0;
end;

class function TCefTaskRunnerRef.GetForCurrentThread: ICefTaskRunner;
begin
  Result := UnWrap(cef_task_runner_get_for_current_thread());
end;

class function TCefTaskRunnerRef.GetForThread(threadId: TCefThreadId)
  : ICefTaskRunner;
begin
  Result := UnWrap(cef_task_runner_get_for_thread(threadId));
end;

function TCefTaskRunnerRef.IsSame(const that: ICefTaskRunner): Boolean;
begin
  Result := PCefTaskRunner(FData).is_same(FData,
    TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefTaskRunnerRef.PostDelayedTask(const task: ICefTask;
  delayMs: Int64): Boolean;
begin
  Result := PCefTaskRunner(FData).post_delayed_task(FData,
    TCef3Helper.CefGetData(task), delayMs) <> 0;
end;

function TCefTaskRunnerRef.PostTask(const task: ICefTask): Boolean;
begin
  Result := PCefTaskRunner(FData).post_task(FData,
    TCef3Helper.CefGetData(task)) <> 0;
end;

class function TCefTaskRunnerRef.UnWrap(data: Pointer): ICefTaskRunner;
begin
  if data <> nil then
    Result := create(data) as ICefTaskRunner
  else
    Result := nil;
end;

{ TCefEndTracingCallbackOwn }

constructor TCefEndTracingCallbackOwn.create;
begin
  inherited CreateData(SizeOf(TCefEndTracingCallback));
  with PCefEndTracingCallback(FData)^ do
    on_end_tracing_complete := cef_end_tracing_callback_on_end_tracing_complete;
end;

procedure TCefEndTracingCallbackOwn.OnEndTracingComplete(const tracingFile
  : ustring);
begin

end;

{ TCefGetGeolocationCallbackOwn }

constructor TCefGetGeolocationCallbackOwn.create;
begin
  inherited CreateData(SizeOf(TCefGetGeolocationCallback));
  with PCefGetGeolocationCallback(FData)^ do
    on_location_update := cef_get_geolocation_callback_on_location_update;
end;

procedure TCefGetGeolocationCallbackOwn.OnLocationUpdate(const position
  : PCefGeoposition);
begin

end;

{ TCefFastGetGeolocationCallback }

constructor TCefFastGetGeolocationCallback.create(const callback
  : TOnLocationUpdate);
begin
  inherited create;
  FCallback := callback;
end;

procedure TCefFastGetGeolocationCallback.OnLocationUpdate(const position
  : PCefGeoposition);
begin
  FCallback(position);
end;

{ TCefFileDialogCallbackRef }

procedure TCefFileDialogCallbackRef.cancel;
begin
  PCefFileDialogCallback(FData).cancel(FData);
end;

procedure TCefFileDialogCallbackRef.Cont(selectedAcceptFilter: Integer; filePaths: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    for i := 0 to filePaths.Count - 1 do
    begin
      str := TCef3Helper.CefString(filePaths[i]);
      cef_string_list_append(list, @str);
    end;
    PCefFileDialogCallback(FData).cont(FData, selectedAcceptFilter, list);
  finally
    cef_string_list_free(list);
  end;
end;

class function TCefFileDialogCallbackRef.UnWrap(data: Pointer)
  : ICefFileDialogCallback;
begin
  if data <> nil then
    Result := create(data) as ICefFileDialogCallback
  else
    Result := nil;
end;

{ TCefDialogHandlerOwn }

constructor TCefDialogHandlerOwn.create;
begin
  CreateData(SizeOf(TCefDialogHandler));
  with PCefDialogHandler(FData)^ do
    on_file_dialog := cef_dialog_handler_on_file_dialog;
end;

function TCefDialogHandlerOwn.OnFileDialog(const browser: ICefBrowser;
  mode: TCefFileDialogMode; const title, defaultFilePath: ustring;
  acceptFilters: TStrings; selectedAcceptFilter: Integer;
  const callback: ICefFileDialogCallback): Boolean;
begin
  Result := False;
end;

{ TCefRenderHandlerOwn }

constructor TCefRenderHandlerOwn.create;
begin
  CreateData(SizeOf(TCefRenderHandler), False);
  with PCefRenderHandler(FData)^ do
  begin
    get_root_screen_rect := cef_render_handler_get_root_screen_rect;
    get_view_rect := cef_render_handler_get_view_rect;
    get_screen_point := cef_render_handler_get_screen_point;
    on_popup_show := cef_render_handler_on_popup_show;
    on_popup_size := cef_render_handler_on_popup_size;
    on_paint := cef_render_handler_on_paint;
    on_cursor_change := cef_render_handler_on_cursor_change;
    start_dragging := cef_render_handler_start_dragging;
    update_drag_cursor := cef_render_handler_update_drag_cursor;
    on_scroll_offset_changed := cef_render_handler_on_scroll_offset_changed;
  end;
end;

function TCefRenderHandlerOwn.GetRootScreenRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

function TCefRenderHandlerOwn.GetScreenInfo(const browser: ICefBrowser;
  screenInfo: PCefScreenInfo): Boolean;
begin
  Result := False;
end;

function TCefRenderHandlerOwn.GetScreenPoint(const browser: ICefBrowser;
  viewX, viewY: Integer; screenX, screenY: PInteger): Boolean;
begin
  Result := False;
end;

function TCefRenderHandlerOwn.GetViewRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

procedure TCefRenderHandlerOwn.OnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle; CursorType: TCefCursorType;
  const customCursorInfo: PCefCursorInfo);
begin

end;

procedure TCefRenderHandlerOwn.OnPaint(const browser: ICefBrowser;
  kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer;
  width, height: Integer);
begin

end;

procedure TCefRenderHandlerOwn.OnPopupShow(const browser: ICefBrowser;
  show: Boolean);
begin

end;

procedure TCefRenderHandlerOwn.OnPopupSize(const browser: ICefBrowser;
  const rect: PCefRect);
begin

end;

procedure TCefRenderHandlerOwn.OnScrollOffsetChanged(
  const browser: ICefBrowser; x, y: Double);
begin

end;

function TCefRenderHandlerOwn.OnStartDragging(const browser: ICefBrowser;
  const dragData: ICefDragData; allowedOps: TCefDragOperations;
  x, y: Integer): Boolean;
begin
  Result := False;
end;

procedure TCefRenderHandlerOwn.OnUpdateDragCursor(const browser: ICefBrowser;
  operation: TCefDragOperation);
begin

end;

{ TCefCompletionHandlerOwn }

constructor TCefCompletionCallbackOwn.create;
begin
  inherited CreateData(SizeOf(TCefCompletionCallback));
  with PCefCompletionCallback(FData)^ do
    on_complete := cef_completion_callback_on_complete;
end;

procedure TCefCompletionCallbackOwn.OnComplete;
begin

end;

{ TCefFastCompletionHandler }

constructor TCefFastCompletionCallback.create
  (const proc: TCefCompletionCallbackProc);
begin
  inherited create;
  FProc := proc;
end;

procedure TCefFastCompletionCallback.OnComplete;
begin
  FProc();
end;

{ TCefDragDataRef }

procedure TCefDragDataRef.AddFile(const path, displayName: ustring);
var
  p, d: TCefString;
begin
  p := TCef3Helper.CefString(path);
  d := TCef3Helper.CefString(displayName);
  PCefDragData(FData).add_file(FData, @p, @d);
end;

function TCefDragDataRef.clone: ICefDragData;
begin
  Result := UnWrap(PCefDragData(FData).clone(FData));
end;

function TCefDragDataRef.GetFileContents(const writer: ICefStreamWriter)
  : NativeUInt;
begin
  Result := PCefDragData(FData).get_file_contents(FData,
    TCef3Helper.CefGetData(writer))
end;

function TCefDragDataRef.GetFileName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDragData(FData)
    .get_file_name(FData));
end;

function TCefDragDataRef.GetFileNames(names: TStrings): Integer;
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    Result := PCefDragData(FData).get_file_names(FData, list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      names.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefDragDataRef.GetFragmentBaseUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDragData(FData)
    .get_fragment_base_url(FData));
end;

function TCefDragDataRef.GetFragmentHtml: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDragData(FData)
    .get_fragment_html(FData));
end;

function TCefDragDataRef.GetFragmentText: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDragData(FData)
    .get_fragment_text(FData));
end;

function TCefDragDataRef.GetLinkMetadata: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDragData(FData)
    .get_link_metadata(FData));
end;

function TCefDragDataRef.GetLinkTitle: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDragData(FData)
    .get_link_title(FData));
end;

function TCefDragDataRef.GetLinkUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefDragData(FData)
    .get_link_url(FData));
end;

function TCefDragDataRef.IsFile: Boolean;
begin
  Result := PCefDragData(FData).is_file(FData) <> 0;
end;

function TCefDragDataRef.IsFragment: Boolean;
begin
  Result := PCefDragData(FData).is_fragment(FData) <> 0;
end;

function TCefDragDataRef.IsLink: Boolean;
begin
  Result := PCefDragData(FData).is_link(FData) <> 0;
end;

function TCefDragDataRef.IsReadOnly: Boolean;
begin
  Result := PCefDragData(FData).is_read_only(FData) <> 0;
end;

class function TCefDragDataRef.New: ICefDragData;
begin
  Result := UnWrap(cef_drag_data_create());
end;

procedure TCefDragDataRef.ResetFileContents;
begin
  PCefDragData(FData).reset_file_contents(FData);
end;

procedure TCefDragDataRef.SetFragmentBaseUrl(const BaseUrl: ustring);
var
  s: TCefString;
begin
  s := TCef3Helper.CefString(BaseUrl);
  PCefDragData(FData).set_fragment_base_url(FData, @s);
end;

procedure TCefDragDataRef.SetFragmentHtml(const html: ustring);
var
  s: TCefString;
begin
  s := TCef3Helper.CefString(html);
  PCefDragData(FData).set_fragment_html(FData, @s);
end;

procedure TCefDragDataRef.SetFragmentText(const text: ustring);
var
  s: TCefString;
begin
  s := TCef3Helper.CefString(text);
  PCefDragData(FData).set_fragment_text(FData, @s);
end;

procedure TCefDragDataRef.SetLinkMetadata(const data: ustring);
var
  s: TCefString;
begin
  s := TCef3Helper.CefString(data);
  PCefDragData(FData).set_link_metadata(FData, @s);
end;

procedure TCefDragDataRef.SetLinkTitle(const title: ustring);
var
  s: TCefString;
begin
  s := TCef3Helper.CefString(title);
  PCefDragData(FData).set_link_title(FData, @s);
end;

procedure TCefDragDataRef.SetLinkUrl(const url: ustring);
var
  s: TCefString;
begin
  s := TCef3Helper.CefString(url);
  PCefDragData(FData).set_link_url(FData, @s);
end;

class function TCefDragDataRef.UnWrap(data: Pointer): ICefDragData;
begin
  if data <> nil then
    Result := create(data) as ICefDragData
  else
    Result := nil;
end;

{ TCefDragHandlerOwn }

constructor TCefDragHandlerOwn.Create;
begin
  CreateData(SizeOf(TCefDragHandler), False);
  with PCefDragHandler(FData)^ do
  begin
    on_drag_enter := cef_drag_handler_on_drag_enter;
{$ifdef Win32}
    on_draggable_regions_changed := cef_drag_handler_on_draggable_regions_changed;
{$endif}
  end;
end;

function TCefDragHandlerOwn.OnDragEnter(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
begin
  Result := False;
end;

procedure TCefDragHandlerOwn.OnDraggableRegionsChanged(
  const browser: ICefBrowser; regionsCount: NativeUInt;
  regions: PCefDraggableRegionArray);
begin

end;

{ TCefRequestContextRef }

function TCefRequestContextRef.CanSetPreference(const name: ustring): Boolean;
var
  n: TCefString;
begin
  n := TCef3Helper.CefString(name);
  Result := PCefRequestContext(FData).can_set_preference(FData, @n) <> 0;
end;

procedure TCefRequestContextRef.ClearCertificateExceptions(
  const callback: ICefCompletionCallback);
begin
  PCefRequestContext(FData).clear_certificate_exceptions(FData, TCef3Helper.CefGetData(callback));
end;

function TCefRequestContextRef.ClearSchemeHandlerFactories: Boolean;
begin
  Result := PCefRequestContext(FData).clear_scheme_handler_factories(FData) <> 0;
end;

procedure TCefRequestContextRef.CloseAllConnections(
  const callback: ICefCompletionCallback);
begin
  PCefRequestContext(FData).close_all_connections(FData, TCef3Helper.CefGetData(callback));
end;

function TCefRequestContextRef.GetAllPreferences(
  includeDefaults: Boolean): ICefDictionaryValue;
begin
  Result := TCefDictionaryValueRef.UnWrap(PCefRequestContext(FData).get_all_preferences(FData, Ord(includeDefaults)));
end;

function TCefRequestContextRef.GetCachePath: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefRequestContext(FData).get_cache_path(FData));
end;

function TCefRequestContextRef.GetDefaultCookieManager(
  const callback: ICefCompletionCallback): ICefCookieManager;
begin
  Result := TCefCookieManagerRef.UnWrap(
    PCefRequestContext(FData).get_default_cookie_manager(
      FData, TCef3Helper.CefGetData(callback)));
end;

function TCefRequestContextRef.GetDefaultCookieManagerProc(
  const callback: TCefCompletionCallbackProc): ICefCookieManager;
begin
  Result := GetDefaultCookieManager(TCefFastCompletionCallback.Create(callback));
end;

function TCefRequestContextRef.GetHandler: ICefRequestContextHandler;
begin
  Result := TCefRequestContextHandlerRef.UnWrap(PCefRequestContext(FData).get_handler(FData));
end;

function TCefRequestContextRef.GetPreference(const name: ustring): ICefValue;
var
  n: TCefString;
begin
  n := TCef3Helper.CefString(name);
  Result :=  TCefValueRef.UnWrap(PCefRequestContext(FData).get_preference(FData, @n));
end;

class function TCefRequestContextRef.Global: ICefRequestContext;
begin
  Result := UnWrap(cef_request_context_get_global_context());
end;

function TCefRequestContextRef.HasPreference(const name: ustring): Boolean;
var
  n: TCefString;
begin
  n := TCef3Helper.CefString(name);
  Result := PCefRequestContext(FData).has_preference(FData, @n) <> 0;
end;

function TCefRequestContextRef.IsGlobal: Boolean;
begin
  Result := PCefRequestContext(FData).is_global(FData) <> 0;
end;

function TCefRequestContextRef.IsSame(const other: ICefRequestContext): Boolean;
begin
  Result := PCefRequestContext(FData).is_same(FData,
    TCef3Helper.CefGetData(other)) <> 0;
end;

function TCefRequestContextRef.IsSharingWith(
  const other: ICefRequestContext): Boolean;
begin
  Result:= PCefRequestContext(FData).is_sharing_with(FData, TCef3Helper.CefGetData(other)) <> 0;
end;

class function TCefRequestContextRef.New(
  const settings: PCefRequestContextSettings;
  const handler: ICefRequestContextHandler): ICefRequestContext;
begin
  Result := UnWrap(cef_request_context_create_context(settings, TCef3Helper.CefGetData(handler)));
end;

procedure TCefRequestContextRef.PurgePluginListCache(reloadPages: Boolean);
begin
  PCefRequestContext(FData).purge_plugin_list_cache(FData, Ord(reloadPages));
end;

function TCefRequestContextRef.RegisterSchemeHandlerFactory(const schemeName,
  domainName: ustring; const factory: ICefSchemeHandlerFactory): Boolean;
var
  s, d: TCefString;
begin
  s := TCef3Helper.CefString(schemeName);
  d := TCef3Helper.CefString(domainName);
  Result := PCefRequestContext(FData).register_scheme_handler_factory(FData, @s, @d, TCef3Helper.CefGetData(factory)) <> 0;
end;

procedure TCefRequestContextRef.ResolveHost(const origin: ustring;
  const callback: ICefResolveCallback);
var
  o: TCefString;
begin
  o := TCef3Helper.CefString(origin);
  PCefRequestContext(FData).resolve_host(FData, @o, TCef3Helper.CefGetData(callback));
end;

function TCefRequestContextRef.ResolveHostCached(const origin: ustring;
  resolvedIps: TStrings): TCefErrorCode;
var
  ips: TCefStringList;
  o, str: TCefString;
  i: Integer;
begin
  ips := cef_string_list_alloc;
  try
    o := TCef3Helper.CefString(origin);
    Result := PCefRequestContext(FData).resolve_host_cached(FData, @o, ips);
    if Assigned(ips) then
      for i := 0 to cef_string_list_size(ips) - 1 do
      begin
        FillChar(str, SizeOf(str), 0);
        cef_string_list_value(ips, i, @str);
        resolvedIps.Add(TCef3Helper.CefStringClearAndGet(str));
      end;
  finally
    cef_string_list_free(ips);
  end;
end;

function TCefRequestContextRef.SetPreference(const name: ustring;
  const value: ICefValue; out error: ustring): Boolean;
var
  n, e: TCefString;
begin
  n := TCef3Helper.CefString(name);
  FillChar(e, SizeOf(e), 0);
  Result := PCefRequestContext(FData).set_preference(FData, @n, TCef3Helper.CefGetData(value), @e) <> 0;
  error := TCef3Helper.CefString(@e);
end;

class function TCefRequestContextRef.Shared(const other: ICefRequestContext;
  const handler: ICefRequestContextHandler): ICefRequestContext;
begin
  Result := UnWrap(create_context_shared(TCef3Helper.CefGetData(other), TCef3Helper.CefGetData(handler)));
end;

class function TCefRequestContextRef.UnWrap(data: Pointer): ICefRequestContext;
begin
  if data <> nil then
    Result := create(data) as ICefRequestContext
  else
    Result := nil;
end;

{ TCefRequestContextHandlerRef }

function TCefRequestContextHandlerRef.GetCookieManager: ICefCookieManager;
begin
  Result := TCefCookieManagerRef.UnWrap(PCefRequestContextHandler(FData)
    .get_cookie_manager(FData));
end;

function TCefRequestContextHandlerRef.OnBeforePluginLoad(const mimeType,
  pluginUrl, topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo;
  pluginPolicy: PCefPluginPolicy): Boolean;
var
  mt, pu, ou: TCefString;
begin
  mt := TCef3Helper.CefString(mimeType);
  pu:= TCef3Helper.CefString(pluginUrl);
  ou := TCef3Helper.CefString(topOriginUrl);
  Result := PCefRequestContextHandler(FData).on_before_plugin_load(
    FData, @mt, @pu, @ou, TCef3Helper.CefGetData(pluginInfo), pluginPolicy) <> 0;
end;

class function TCefRequestContextHandlerRef.UnWrap(data: Pointer)
  : ICefRequestContextHandler;
begin
  if data <> nil then
    Result := create(data) as ICefRequestContextHandler
  else
    Result := nil;
end;

{ TCefRequestContextHandlerOwn }

constructor TCefRequestContextHandlerOwn.create;
begin
  CreateData(SizeOf(TCefRequestContextHandler), False);
  with PCefRequestContextHandler(FData)^ do
  begin
    get_cookie_manager := cef_request_context_handler_get_cookie_manager;
    on_before_plugin_load := cef_request_context_handler_on_before_plugin_load;
  end;
end;

function TCefRequestContextHandlerOwn.GetCookieManager: ICefCookieManager;
begin
  Result := nil;
end;

function TCefRequestContextHandlerOwn.OnBeforePluginLoad(const mimeType,
  pluginUrl, topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo;
  pluginPolicy: PCefPluginPolicy): Boolean;
begin
  Result := False;
end;

{ TCefFastRequestContextHandler }

constructor TCefFastRequestContextHandler.create
  (const proc: TCefRequestContextHandlerProc);
begin
  FProc := proc;
  inherited create;
end;

function TCefFastRequestContextHandler.GetCookieManager: ICefCookieManager;
begin
  Result := FProc();
end;

{ TCefPrintSettingsRef }

function TCefPrintSettingsRef.copy: ICefPrintSettings;
begin
  Result := UnWrap(PCefPrintSettings(FData).copy(FData))
end;

function TCefPrintSettingsRef.GetColorModel: TCefColorModel;
begin
  Result := PCefPrintSettings(FData).get_color_model(FData);
end;

function TCefPrintSettingsRef.GetCopies: Integer;
begin
  Result := PCefPrintSettings(FData).get_copies(FData);
end;

function TCefPrintSettingsRef.GetDeviceName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefPrintSettings(FData)
    .get_device_name(FData));
end;

function TCefPrintSettingsRef.GetDpi: Integer;
begin
  Result := PCefPrintSettings(FData).get_dpi(FData);
end;

function TCefPrintSettingsRef.GetDuplexMode: TCefDuplexMode;
begin
  Result := PCefPrintSettings(FData).get_duplex_mode(FData);
end;

procedure TCefPrintSettingsRef.GetPageRanges(out ranges: TCefPageRangeArray);
var
  len: NativeUInt;
begin
  len := GetPageRangesCount;
  SetLength(ranges, len);
  if len > 0 then
    PCefPrintSettings(FData).get_page_ranges(FData, @len, @ranges[0]);
end;

function TCefPrintSettingsRef.GetPageRangesCount: NativeUInt;
begin
  Result := PCefPrintSettings(FData).get_page_ranges_count(FData);
end;

function TCefPrintSettingsRef.IsLandscape: Boolean;
begin
  Result := PCefPrintSettings(FData).is_landscape(FData) <> 0;
end;

function TCefPrintSettingsRef.IsReadOnly: Boolean;
begin
  Result := PCefPrintSettings(FData).is_read_only(FData) <> 0;
end;

function TCefPrintSettingsRef.IsSelectionOnly: Boolean;
begin
  Result := PCefPrintSettings(FData).is_selection_only(FData) <> 0;
end;

function TCefPrintSettingsRef.IsValid: Boolean;
begin
  Result := PCefPrintSettings(FData).is_valid(FData) <> 0;
end;

class function TCefPrintSettingsRef.New: ICefPrintSettings;
begin
  Result := UnWrap(cef_print_settings_create);
end;

procedure TCefPrintSettingsRef.SetCollate(collate: Boolean);
begin
  PCefPrintSettings(FData).set_collate(FData, ord(collate));
end;

procedure TCefPrintSettingsRef.SetColorModel(model: TCefColorModel);
begin
  PCefPrintSettings(FData).set_color_model(FData, model);
end;

procedure TCefPrintSettingsRef.SetCopies(copies: Integer);
begin
  PCefPrintSettings(FData).set_copies(FData, copies);
end;

procedure TCefPrintSettingsRef.SetDeviceName(const name: ustring);
var
  s: TCefString;
begin
  s := TCef3Helper.CefString(name);
  PCefPrintSettings(FData).set_device_name(FData, @s);
end;

procedure TCefPrintSettingsRef.SetDpi(dpi: Integer);
begin
  PCefPrintSettings(FData).set_dpi(FData, dpi);
end;

procedure TCefPrintSettingsRef.SetDuplexMode(mode: TCefDuplexMode);
begin
  PCefPrintSettings(FData).set_duplex_mode(FData, mode);
end;

procedure TCefPrintSettingsRef.SetOrientation(landscape: Boolean);
begin
  PCefPrintSettings(FData).set_orientation(FData, ord(landscape));
end;

procedure TCefPrintSettingsRef.SetPageRanges(const ranges: TCefPageRangeArray);
var
  len: NativeUInt;
begin
  len := length(ranges);
  if len > 0 then
    PCefPrintSettings(FData).set_page_ranges(FData, len, @ranges[0])
  else
    PCefPrintSettings(FData).set_page_ranges(FData, 0, nil);
end;

procedure TCefPrintSettingsRef.SetPrinterPrintableArea
  (const physicalSizeDeviceUnits: PCefSize;
  const printableAreaDeviceUnits: PCefRect; landscapeNeedsFlip: Boolean);
begin
  PCefPrintSettings(FData).set_printer_printable_area(FData,
    physicalSizeDeviceUnits, printableAreaDeviceUnits, ord(landscapeNeedsFlip));
end;

procedure TCefPrintSettingsRef.SetSelectionOnly(selectionOnly: Boolean);
begin
  PCefPrintSettings(FData).set_selection_only(FData, ord(selectionOnly));
end;

class function TCefPrintSettingsRef.UnWrap(data: Pointer): ICefPrintSettings;
begin
  if data <> nil then
    Result := create(data) as ICefPrintSettings
  else
    Result := nil;
end;

function TCefPrintSettingsRef.WillCollate: Boolean;
begin
  Result := PCefPrintSettings(FData).will_collate(FData) <> 0;
end;

{ TCefStreamWriterRef }

class function TCefStreamWriterRef.CreateForFile(const fileName: ustring)
  : ICefStreamWriter;
var
  s: TCefString;
begin
  s := TCef3Helper.CefString(fileName);
  Result := UnWrap(cef_stream_writer_create_for_file(@s));
end;

class function TCefStreamWriterRef.CreateForHandler(const handler
  : ICefWriteHandler): ICefStreamWriter;
begin
  Result := UnWrap(cef_stream_writer_create_for_handler
    (TCef3Helper.CefGetData(handler)));
end;

function TCefStreamWriterRef.flush: Integer;
begin
  Result := PCefStreamWriter(FData).flush(FData);
end;

function TCefStreamWriterRef.MayBlock: Boolean;
begin
  Result := PCefStreamWriter(FData).may_block(FData) <> 0;
end;

function TCefStreamWriterRef.seek(offset: Int64; whence: Integer): Integer;
begin
  Result := PCefStreamWriter(FData).seek(FData, offset, whence);
end;

function TCefStreamWriterRef.tell: Int64;
begin
  Result := PCefStreamWriter(FData).tell(FData);
end;

class function TCefStreamWriterRef.UnWrap(data: Pointer): ICefStreamWriter;
begin
  if data <> nil then
    Result := create(data) as ICefStreamWriter
  else
    Result := nil;
end;

function TCefStreamWriterRef.write(const ptr: Pointer; size, n: NativeUInt)
  : NativeUInt;
begin
  Result := PCefStreamWriter(FData).write(FData, ptr, size, n);
end;

{ TCefWriteHandlerOwn }

constructor TCefWriteHandlerOwn.create;
begin
  inherited CreateData(SizeOf(TCefWriteHandler));
  with PCefWriteHandler(FData)^ do
  begin
    write := cef_write_handler_write;
    seek := cef_write_handler_seek;
    tell := cef_write_handler_tell;
    flush := cef_write_handler_flush;
    may_block := cef_write_handler_may_block;
  end;
end;

function TCefWriteHandlerOwn.flush: Integer;
begin
  Result := 0;
end;

function TCefWriteHandlerOwn.MayBlock: Boolean;
begin
  Result := False;
end;

function TCefWriteHandlerOwn.seek(offset: Int64; whence: Integer): Integer;
begin
  Result := 0;
end;

function TCefWriteHandlerOwn.tell: Int64;
begin
  Result := 0;
end;

function TCefWriteHandlerOwn.write(const ptr: Pointer; size, n: NativeUInt)
  : NativeUInt;
begin
  Result := 0;
end;

{ TCefNavigationEntryRef }

function TCefNavigationEntryRef.IsValid: Boolean;
begin
  Result := PCefNavigationEntry(FData).is_valid(FData) <> 0;
end;

function TCefNavigationEntryRef.GetUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefNavigationEntry(FData)
    .get_url(FData));
end;

function TCefNavigationEntryRef.GetDisplayUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefNavigationEntry(FData)
    .get_display_url(FData));
end;

function TCefNavigationEntryRef.GetOriginalUrl: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefNavigationEntry(FData)
    .get_original_url(FData));
end;

function TCefNavigationEntryRef.GetTitle: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefNavigationEntry(FData)
    .get_title(FData));
end;

function TCefNavigationEntryRef.GetTransitionType: TCefTransitionType;
begin
  Result := PCefNavigationEntry(FData).get_transition_type(FData);
end;

function TCefNavigationEntryRef.HasPostData: Boolean;
begin
  Result := PCefNavigationEntry(FData).has_post_data(FData) <> 0;
end;

function TCefNavigationEntryRef.GetCompletionTime: TDateTime;
begin
  Result := TCef3Helper.CefTimeToDateTime(PCefNavigationEntry(FData)
    .get_completion_time(FData));
end;

function TCefNavigationEntryRef.GetHttpStatusCode: Integer;
begin
  Result := PCefNavigationEntry(FData).get_http_status_code(FData);
end;

class function TCefNavigationEntryRef.UnWrap(data: Pointer)
  : ICefNavigationEntry;
begin
  if data <> nil then
    Result := create(data) as ICefNavigationEntry
  else
    Result := nil;
end;

{ TCefNavigationEntryVisitorOwn }

constructor TCefNavigationEntryVisitorOwn.create;
begin
  CreateData(SizeOf(TCefNavigationEntryVisitor), False);
  with PCefNavigationEntryVisitor(FData)^ do
    visit := cef_navigation_entry_visitor_visit;
end;

function TCefNavigationEntryVisitorOwn.visit(const entry: ICefNavigationEntry;
  current: Boolean; index, total: Integer): Boolean;
begin
  Result := False;
end;

{ TCefFastNavigationEntryVisitor }

constructor TCefFastNavigationEntryVisitor.create
  (const proc: TCefNavigationEntryVisitorProc);
begin
  FVisitor := proc;
  inherited create;
end;

function TCefFastNavigationEntryVisitor.visit(const entry: ICefNavigationEntry;
  current: Boolean; index, total: Integer): Boolean;
begin
  Result := FVisitor(entry, current, index, total);
end;

{ TCefFindHandlerOwn }

constructor TCefFindHandlerOwn.Create;
begin
  CreateData(SizeOf(TCefFindHandler), False);
  with PCefFindHandler(FData)^ do
    on_find_result := cef_find_handler_on_find_result;
end;

{ TCefSetCookieCallbackOwn }

constructor TCefSetCookieCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefSetCookieCallback));
  with PCefSetCookieCallback(FData)^ do
    on_complete := cef_set_cookie_callback_on_complete;
end;

{ TCefFastSetCookieCallback }

constructor TCefFastSetCookieCallback.Create(
  const callback: TCefSetCookieCallbackProc);
begin
  inherited Create;
  FCallback := callback;
end;

procedure TCefFastSetCookieCallback.OnComplete(success: Boolean);
begin
  FCallback(success);
end;

{ TCefDeleteCookiesCallbackOwn }

constructor TCefDeleteCookiesCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDeleteCookiesCallback));
  with PCefDeleteCookiesCallback(FData)^ do
    on_complete := cef_delete_cookie_callback_on_complete;
end;

{ TCefFastDeleteCookiesCallback }

constructor TCefFastDeleteCookiesCallback.Create(
  const callback: TCefDeleteCookiesCallbackProc);
begin
  inherited Create;
  FCallback := callback;
end;

procedure TCefFastDeleteCookiesCallback.OnComplete(numDeleted: Integer);
begin
  FCallback(numDeleted);
end;

{ TCefValueRef }

function TCefValueRef.Copy: ICefValue;
begin
  Result := UnWrap(PCefValue(FData).copy(FData));
end;

function TCefValueRef.GetBinary: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefValue(FData).get_binary(FData));
end;

function TCefValueRef.GetBool: Boolean;
begin
  Result := PCefValue(FData).get_bool(FData) <> 0;
end;

function TCefValueRef.GetDictionary: ICefDictionaryValue;
begin
  Result := TCefDictionaryValueRef.UnWrap(PCefValue(FData).get_dictionary(FData));
end;

function TCefValueRef.GetDouble: Double;
begin
  Result := PCefValue(FData).get_double(FData);
end;

function TCefValueRef.GetInt: Integer;
begin
  Result := PCefValue(FData).get_int(FData);
end;

function TCefValueRef.GetList: ICefListValue;
begin
  Result := TCefListValueRef.UnWrap(PCefValue(FData).get_list(FData));
end;

function TCefValueRef.GetString: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefValue(FData).get_string(FData));
end;

function TCefValueRef.GetType: TCefValueType;
begin
  Result := PCefValue(FData).get_type(FData);
end;

function TCefValueRef.IsEqual(const that: ICefValue): Boolean;
begin
  Result := PCefValue(FData).is_equal(FData, TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefValueRef.IsOwned: Boolean;
begin
  Result := PCefValue(FData).is_owned(FData) <> 0;
end;

function TCefValueRef.IsReadOnly: Boolean;
begin
  Result := PCefValue(FData).is_read_only(FData) <> 0;
end;

function TCefValueRef.IsSame(const that: ICefValue): Boolean;
begin
  Result := PCefValue(FData).is_same(FData, TCef3Helper.CefGetData(that)) <> 0;
end;

function TCefValueRef.IsValid: Boolean;
begin
  Result := PCefValue(FData).is_valid(FData) <> 0;
end;

class function TCefValueRef.New: ICefValue;
begin
  Result := UnWrap(cef_value_create());
end;

function TCefValueRef.SetBinary(const value: ICefBinaryValue): Boolean;
begin
  Result := PCefValue(FData).set_binary(FData, TCef3Helper.CefGetData(value)) <> 0;
end;

function TCefValueRef.SetBool(value: Integer): Boolean;
begin
  Result := PCefValue(FData).set_bool(FData, value) <> 0;
end;

function TCefValueRef.SetDictionary(const value: ICefDictionaryValue): Boolean;
begin
  Result := PCefValue(FData).set_dictionary(FData, TCef3Helper.CefGetData(value)) <> 0;
end;

function TCefValueRef.SetDouble(value: Double): Boolean;
begin
  Result := PCefValue(FData).set_double(FData, value) <> 0;
end;

function TCefValueRef.SetInt(value: Integer): Boolean;
begin
  Result := PCefValue(FData).set_int(FData, value) <> 0;
end;

function TCefValueRef.SetList(const value: ICefListValue): Boolean;
begin
  Result := PCefValue(FData).set_list(FData, TCef3Helper.CefGetData(value)) <> 0;
end;

function TCefValueRef.SetNull: Boolean;
begin
  Result := PCefValue(FData).set_null(FData) <> 0;
end;

function TCefValueRef.SetString(const value: ustring): Boolean;
var
 s: TCefString;
begin
  s := TCef3Helper.CefString(value);
  Result := PCefValue(FData).set_string(FData, @s) <> 0;
end;

class function TCefValueRef.UnWrap(data: Pointer): ICefValue;
begin
  if data <> nil then
    Result := Create(data) as ICefValue else
    Result := nil;
end;

{ TCefSslCertPrincipalRef }

function TCefSslCertPrincipalRef.GetCommonName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefSslCertPrincipal(FData).get_common_name(FData));
end;

function TCefSslCertPrincipalRef.GetCountryName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefSslCertPrincipal(FData).get_country_name(FData));
end;

function TCefSslCertPrincipalRef.GetDisplayName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefSslCertPrincipal(FData).get_display_name(FData));
end;

procedure TCefSslCertPrincipalRef.GetDomainComponents(components: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefSslCertPrincipal(FData).get_domain_components(FData, list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      components.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefSslCertPrincipalRef.GetLocalityName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefSslCertPrincipal(FData).get_locality_name(FData));
end;

procedure TCefSslCertPrincipalRef.GetOrganizationNames(names: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefSslCertPrincipal(FData).get_organization_names(FData, list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      names.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

procedure TCefSslCertPrincipalRef.GetOrganizationUnitNames(names: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefSslCertPrincipal(FData).get_organization_unit_names(FData, list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      names.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefSslCertPrincipalRef.GetStateOrProvinceName: ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefSslCertPrincipal(FData).get_state_or_province_name(FData));
end;

procedure TCefSslCertPrincipalRef.GetStreetAddresses(addresses: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefSslCertPrincipal(FData).get_street_addresses(FData, list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      addresses.Add(TCef3Helper.CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

class function TCefSslCertPrincipalRef.UnWrap(
  data: Pointer): ICefSslCertPrincipal;
begin
  if data <> nil then
    Result := Create(data) as ICefSslCertPrincipal else
    Result := nil;
end;

{ TCefSslInfoRef }

function TCefSslInfoRef.GetCertStatus: TCefCertStatus;
begin
  Result := PCefSslInfo(FData).get_cert_status(FData);
end;

function TCefSslInfoRef.GetDerEncoded: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefSslInfo(FData).get_derencoded(FData));
end;

function TCefSslInfoRef.GetDEREncodedIssuerChain(
  chainCount: NativeUInt): IInterfaceList;
var
  arr: PPCefBinaryValue;
  i: Integer;
begin
  Result := TInterfaceList.Create;
  GetMem(arr, chainCount * SizeOf(Pointer));
  try
    PCefSslInfo(FData).get_derencoded_issuer_chain(FData, chainCount, arr);
    for i := 0 to chainCount - 1 do
       Result.Add(TCefBinaryValueRef.UnWrap(PPointerArray(arr)[i]));
  finally
    FreeMem(arr);
  end;
end;

function TCefSslInfoRef.GetIssuer: ICefSslCertPrincipal;
begin
  Result := TCefSslCertPrincipalRef.UnWrap(PCefSslInfo(FData).get_issuer(FData));
end;

function TCefSslInfoRef.GetIssuerChainSize: NativeUInt;
begin
  Result := PCefSslInfo(FData).get_issuer_chain_size(FData);
end;

function TCefSslInfoRef.GetPEMEencodedIssuerChain(
  chainCount: NativeUInt): IInterfaceList;
var
  arr: PPCefBinaryValue;
  i: Integer;
begin
  Result := TInterfaceList.Create;
  GetMem(arr, chainCount * SizeOf(Pointer));
  try
    PCefSslInfo(FData).get_pemencoded_issuer_chain(FData, chainCount, arr);
    for i := 0 to chainCount - 1 do
       Result.Add(TCefBinaryValueRef.UnWrap(PPointerArray(arr)[i]));
  finally
    FreeMem(arr);
  end;
end;

function TCefSslInfoRef.GetPemEncoded: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefSslInfo(FData).get_pemencoded(FData));
end;

function TCefSslInfoRef.GetSerialNumber: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefSslInfo(FData).get_serial_number(FData));
end;

function TCefSslInfoRef.GetSubject: ICefSslCertPrincipal;
begin
  Result := TCefSslCertPrincipalRef.UnWrap(PCefSslInfo(FData).get_subject(FData));
end;

function TCefSslInfoRef.GetValidExpiry: TCefTime;
begin
  Result := PCefSslInfo(FData).get_valid_expiry(FData);
end;

function TCefSslInfoRef.GetValidStart: TCefTime;
begin
  Result := PCefSslInfo(FData).get_valid_start(FData);
end;

function TCefSslInfoRef.IsCertStatusError: Boolean;
begin
  Result := PCefSslInfo(FData).is_cert_status_error(FData) <> 0;
end;

function TCefSslInfoRef.IsCertStatusMinorError: Boolean;
begin
  Result := PCefSslInfo(FData).is_cert_status_minor_error(FData) <> 0;
end;

class function TCefSslInfoRef.UnWrap(data: Pointer): ICefSslInfo;
begin
  if data <> nil then
    Result := Create(data) as ICefSslInfo else
    Result := nil;
end;

{ TCefPdfPrintCallbackOwn }

constructor TCefPdfPrintCallbackOwn.Create;
begin
  CreateData(SizeOf(TCefPdfPrintCallback), False);
  with PCefPdfPrintCallback(FData)^ do
    on_pdf_print_finished := cef_pdf_print_callback_on_pdf_print_finished;
end;

{ TCefFastPdfPrintCallback }

constructor TCefFastPdfPrintCallback.Create(
  const proc: TOnPdfPrintFinishedProc);
begin
  FProc := proc;
  inherited Create;
end;

procedure TCefFastPdfPrintCallback.OnPdfPrintFinished(const path: ustring;
  ok: Boolean);
begin
  FProc(path, ok);
end;

{ TCefRunContextMenuCallbackRef }

procedure TCefRunContextMenuCallbackRef.Cancel;
begin
  PCefRunContextMenuCallback(FData).cancel(FData);
end;

procedure TCefRunContextMenuCallbackRef.Cont(commandId: Integer;
  eventFlags: TCefEventFlags);
begin
  PCefRunContextMenuCallback(FData).cont(FData, commandId, eventFlags);
end;

class function TCefRunContextMenuCallbackRef.UnWrap(
  data: Pointer): ICefRunContextMenuCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefRunContextMenuCallback else
    Result := nil;
end;

{ TCefResourceBundleRef }

function TCefResourceBundleRef.GetDataResource(resourceId: Integer;
  out data: Pointer; out dataSize: NativeUInt): Boolean;
begin
  Result := PCefResourceBundle(FData).get_data_resource(FData, resourceId,
    data, dataSize) <> 0;
end;

function TCefResourceBundleRef.GetDataResourceForScale(resourceId: Integer;
  scaleFactor: TCefScaleFactor; out data: Pointer;
  out dataSize: NativeUInt): Boolean;
begin
  Result := PCefResourceBundle(FData).get_data_resource_for_scale(FData,
    resourceId, scaleFactor, data, dataSize) <> 0;
end;

function TCefResourceBundleRef.GetLocalizedString(stringId: Integer): ustring;
begin
  Result := TCef3Helper.CefStringFreeAndGet(PCefResourceBundle(FData).get_localized_string(FData, stringId));
end;

class function TCefResourceBundleRef.Global: ICefResourceBundle;
begin
  Result := UnWrap(cef_resource_bundle_get_global());
end;

class function TCefResourceBundleRef.UnWrap(data: Pointer): ICefResourceBundle;
begin
  if data <> nil then
    Result := Create(data) as ICefResourceBundle else
    Result := nil;
end;

{ TCefRTTIExtension }

{$IFDEF DELPHI14_UP}

constructor TCefRTTIExtension.create(const value: TValue
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  ; SyncMainThread: Boolean
{$ENDIF}
  );
begin
  inherited create;
  FCtx := TRttiContext.create;
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  FSyncMainThread := SyncMainThread;
{$ENDIF}
  FValue := value;
end;

destructor TCefRTTIExtension.Destroy;
begin
  FCtx.Free;
  inherited;
end;

function TCefRTTIExtension.GetValue(pi: PTypeInfo; const v: ICefv8Value;
  var ret: TValue): Boolean;
  function ProcessInt: Boolean;
  var
    sv: record case Byte of 0: (ub: Byte);
    1: (sb: ShortInt);
    2: (uw: word);
    3: (sw: SmallInt);
    4: (si: Integer);
    5: (ui: Cardinal);
  end;
pd:
PTypeData;
begin
  pd := GetTypeData(pi);
  if v.IsInt and (v.GetIntValue >= pd.MinValue) and
    (v.GetIntValue <= pd.MaxValue) then
  begin
    case pd.OrdType of
      otSByte:
        sv.sb := v.GetIntValue;
      otUByte:
        sv.ub := v.GetIntValue;
      otSWord:
        sv.sw := v.GetIntValue;
      otUWord:
        sv.uw := v.GetIntValue;
      otSLong:
        sv.si := v.GetIntValue;
      otULong:
        sv.ui := v.GetIntValue;
    end;
    TValue.Make(@sv, pi, ret);
  end
  // 2015.7.3@swish:修正了参数为布尔类型时无法正确传递的问题
  else if v.IsBool then
    ret := v.GetBoolValue
    // End changes
  else
    exit(False);
  Result := True;
end;

function ProcessInt64: Boolean;
var
  i: Int64;
begin
  i := StrToInt64(v.GetStringValue); // hack
  TValue.Make(@i, pi, ret);
  Result := True;
end;

function ProcessUString: Boolean;
var
  vus: string;
begin
  if v.IsString then
  begin
    vus := v.GetStringValue;
    TValue.Make(@vus, pi, ret);
  end
  else
    exit(False);
  Result := True;
end;

function ProcessLString: Boolean;
var
  vas: AnsiString;
begin
  if v.IsString then
  begin
    vas := AnsiString(v.GetStringValue);
    TValue.Make(@vas, pi, ret);
  end
  else
    exit(False);
  Result := True;
end;

function ProcessWString: Boolean;
var
  vws: WideString;
begin
  if v.IsString then
  begin
    vws := v.GetStringValue;
    TValue.Make(@vws, pi, ret);
  end
  else
    exit(False);
  Result := True;
end;

function ProcessFloat: Boolean;
var
  sv: record case Byte of 0: (fs: Single);
  1: (fd: Double);
  2: (fe: Extended);
  3: (fc: Comp);
  4: (fcu: Currency);
end;
begin
  if v.IsDouble or v.IsInt then
  begin
    case GetTypeData(pi).FloatType of
      ftSingle:
        sv.fs := v.GetDoubleValue;
      ftDouble:
        sv.fd := v.GetDoubleValue;
      ftExtended:
        sv.fe := v.GetDoubleValue;
      ftComp:
        sv.fc := v.GetDoubleValue;
      ftCurr:
        sv.fcu := v.GetDoubleValue;
    end;
    TValue.Make(@sv, pi, ret);
  end
  else if v.IsDate then
  begin
    sv.fd := v.GetDateValue;
    TValue.Make(@sv, pi, ret);
  end
  else
    exit(False);
  Result := True;
end;

function ProcessSet: Boolean;
var
  sv: record case Byte of 0: (ub: Byte);
  1: (sb: ShortInt);
  2: (uw: word);
  3: (sw: SmallInt);
  4: (si: Integer);
  5: (ui: Cardinal);
end;
begin
  if v.IsInt then
  begin
    case GetTypeData(pi).OrdType of
      otSByte:
        sv.sb := v.GetIntValue;
      otUByte:
        sv.ub := v.GetIntValue;
      otSWord:
        sv.sw := v.GetIntValue;
      otUWord:
        sv.uw := v.GetIntValue;
      otSLong:
        sv.si := v.GetIntValue;
      otULong:
        sv.ui := v.GetIntValue;
    end;
    TValue.Make(@sv, pi, ret);
  end
  else
    exit(False);
  Result := True;
end;

function ProcessVariant: Boolean;
var
  vr: Variant;
  i: Integer;
  vl: TValue;
begin
  VarClear(vr);
  if v.IsString then
    vr := v.GetStringValue
  else if v.IsBool then
    vr := v.GetBoolValue
  else if v.IsInt then
    vr := v.GetIntValue
  else if v.IsDouble then
    vr := v.GetDoubleValue
  else if v.IsUndefined then
    TVarData(vr).VType := varEmpty
  else if v.IsNull then
    TVarData(vr).VType := varNull
  else if v.IsArray then
  begin
    vr := VarArrayCreate([0, v.GetArrayLength], varVariant);
    for i := 0 to v.GetArrayLength - 1 do
    begin
      if not GetValue(pi, v.GetValueByIndex(i), vl) then
        exit(False);
      VarArrayPut(vr, vl.AsVariant, i);
    end;
  end
  else
    exit(False);
  TValue.Make(@vr, pi, ret);
  Result := True;
end;

function ProcessObject: Boolean;
var
  ud: ICefv8Value;
  i: Pointer;
  td: PTypeData;
  rt: TRttiType;
begin
  if v.IsObject then
  begin
    ud := v.GetUserData;
    if (ud = nil) then
      exit(False);
{$IFDEF CPUX64}
    rt := StrToPtr(ud.GetValueByIndex(0).GetStringValue);
{$ELSE}
    rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
{$ENDIF}
    td := GetTypeData(rt.Handle);

    if (rt.TypeKind = tkClass) and td.ClassType.InheritsFrom
      (GetTypeData(pi).ClassType) then
    begin
{$IFDEF CPUX64}
      i := StrToPtr(ud.GetValueByIndex(1).GetStringValue);
{$ELSE}
      i := Pointer(ud.GetValueByIndex(1).GetIntValue);
{$ENDIF}
      TValue.Make(@i, pi, ret);
    end
    else
      exit(False);
  end
  else
    exit(False);
  Result := True;
end;

function ProcessClass: Boolean;
var
  ud: ICefv8Value;
  i: Pointer;
  rt: TRttiType;
begin
  if v.IsObject then
  begin
    ud := v.GetUserData;
    if (ud = nil) then
      exit(False);
{$IFDEF CPUX64}
    rt := StrToPtr(ud.GetValueByIndex(0).GetStringValue);
{$ELSE}
    rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
{$ENDIF}
    if (rt.TypeKind = tkClassRef) then
    begin
{$IFDEF CPUX64}
      i := StrToPtr(ud.GetValueByIndex(1).GetStringValue);
{$ELSE}
      i := Pointer(ud.GetValueByIndex(1).GetIntValue);
{$ENDIF}
      TValue.Make(@i, pi, ret);
    end
    else
      exit(False);
  end
  else
    exit(False);
  Result := True;
end;

function ProcessRecord: Boolean;
var
  r: TRttiField;
  f: TValue;
  rec: Pointer;
begin
  if v.IsObject then
  begin
    TValue.Make(nil, pi, ret);
{$IFDEF DELPHI15_UP}
    rec := TValueData(ret).FValueData.GetReferenceToRawData;
{$ELSE}
    rec := IValueData(TValueData(ret).FHeapData).GetReferenceToRawData;
{$ENDIF}
    for r in FCtx.GetType(pi).GetFields do
    begin
      if not GetValue(r.FieldType.Handle, v.GetValueByKey(r.name), f) then
        exit(False);
      r.SetValue(rec, f);
    end;
    Result := True;
  end
  else
    Result := False;
end;

function ProcessInterface: Boolean;
begin
  if pi = TypeInfo(ICefv8Value) then
  begin
    TValue.Make(@v, pi, ret);
    Result := True;
  end
  else
    Result := False; // todo
end;
begin
  case pi.kind of
    tkInteger, tkEnumeration:
      Result := ProcessInt;
    tkInt64:
      Result := ProcessInt64;
    tkUString:
      Result := ProcessUString;
    tkLString:
      Result := ProcessLString;
    tkWString:
      Result := ProcessWString;
    tkFloat:
      Result := ProcessFloat;
    tkSet:
      Result := ProcessSet;
    tkVariant:
      Result := ProcessVariant;
    tkClass:
      Result := ProcessObject;
    tkClassRef:
      Result := ProcessClass;
    tkRecord:
      Result := ProcessRecord;
    tkInterface:
      Result := ProcessInterface;
  else
    Result := False;
  end;
end;

function TCefRTTIExtension.SetValue(const v: TValue;
  var ret: ICefv8Value): Boolean;

  function ProcessRecord: Boolean;
  var
    rf: TRttiField;
    vl: TValue;
    ud, v8: ICefv8Value;
    rec: Pointer;
    rt: TRttiType;
  begin
    ud := TCefv8ValueRef.NewArray(1);
    rt := FCtx.GetType(v.TypeInfo);
{$IFDEF CPUX64}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewString(PtrToStr(rt)));
{$ELSE}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
{$ENDIF}
    ret := TCefv8ValueRef.NewObject(nil);
    ret.SetUserData(ud);

{$IFDEF DELPHI15_UP}
    rec := TValueData(v).FValueData.GetReferenceToRawData;
{$ELSE}
    rec := IValueData(TValueData(v).FHeapData).GetReferenceToRawData;
{$ENDIF}
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    if FSyncMainThread then
    begin
      v8 := ret;
      TThread.Synchronize(nil,
        procedure
        var
          rf: TRttiField;
          o: ICefv8Value;
        begin
          for rf in rt.GetFields do
          begin
            vl := rf.GetValue(rec);
            SetValue(vl, o);
            v8.SetValueByKey(rf.name, o, []);
          end;
        end)
    end
    else
{$ENDIF}
      for rf in FCtx.GetType(v.TypeInfo).GetFields do
      begin
        vl := rf.GetValue(rec);
        if not SetValue(vl, v8) then
          exit(False);
        ret.SetValueByKey(rf.name, v8, []);
      end;
    Result := True;
  end;

  function ProcessObject: Boolean;
  var
    m: TRttiMethod;
    p: TRttiProperty;
    fl: TRttiField;
    f: ICefv8Value;
    _r, _g, _s, ud: ICefv8Value;
    _a: TCefv8ValueArray;
    rt: TRttiType;
  begin
    rt := FCtx.GetType(v.TypeInfo);

    ud := TCefv8ValueRef.NewArray(2);
{$IFDEF CPUX64}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewString(PtrToStr(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.NewString(PtrToStr(v.AsObject)));
{$ELSE}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.NewInt(Integer(v.AsObject)));
{$ENDIF}
    ret := TCefv8ValueRef.NewObject(nil); // todo
    ret.SetUserData(ud);

    for m in rt.GetMethods do
      if m.Visibility > mvProtected then
      begin
        f := TCefv8ValueRef.NewFunction(m.name, self);
        ret.SetValueByKey(m.name, f, []);
      end;

    for p in rt.GetProperties do
      if (p.Visibility > mvProtected) then
      begin
        if _g = nil then
          _g := ret.GetValueByKey('__defineGetter__');
        if _s = nil then
          _s := ret.GetValueByKey('__defineSetter__');
        SetLength(_a, 2);
        _a[0] := TCefv8ValueRef.NewString(p.name);
        if p.IsReadable then
        begin
          _a[1] := TCefv8ValueRef.NewFunction('$pg' + p.name, self);
          _r := _g.ExecuteFunction(ret, _a);
        end;
        if p.IsWritable then
        begin
          _a[1] := TCefv8ValueRef.NewFunction('$ps' + p.name, self);
          _r := _s.ExecuteFunction(ret, _a);
        end;
      end;

    for fl in rt.GetFields do
      if (fl.Visibility > mvProtected) then
      begin
        if _g = nil then
          _g := ret.GetValueByKey('__defineGetter__');
        if _s = nil then
          _s := ret.GetValueByKey('__defineSetter__');

        SetLength(_a, 2);
        _a[0] := TCefv8ValueRef.NewString(fl.name);
        _a[1] := TCefv8ValueRef.NewFunction('$vg' + fl.name, self);
        _r := _g.ExecuteFunction(ret, _a);
        _a[1] := TCefv8ValueRef.NewFunction('$vs' + fl.name, self);
        _r := _s.ExecuteFunction(ret, _a);
      end;

    Result := True;
  end;

  function ProcessClass: Boolean;
  var
    m: TRttiMethod;
    f, ud: ICefv8Value;
    c: TClass;
    rt: TRttiType;
  begin
    c := v.AsClass;
    rt := FCtx.GetType(c);

    ud := TCefv8ValueRef.NewArray(2);
{$IFDEF CPUX64}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewString(PtrToStr(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.NewString(PtrToStr(c)));
{$ELSE}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.NewInt(Integer(c)));
{$ENDIF}
    ret := TCefv8ValueRef.NewObject(nil); // todo
    ret.SetUserData(ud);

    if c <> nil then
    begin
      for m in rt.GetMethods do
        if (m.Visibility > mvProtected) and
          (m.MethodKind in [mkClassProcedure, mkClassFunction]) then
        begin
          f := TCefv8ValueRef.NewFunction(m.name, self);
          ret.SetValueByKey(m.name, f, []);
        end;
    end;

    Result := True;
  end;

  function ProcessVariant: Boolean;
  var
    vr: Variant;
  begin
    vr := v.AsVariant;
    case TVarData(vr).VType of
      varSmallint, varInteger, varShortInt:
        ret := TCefv8ValueRef.NewInt(vr);
      varByte, varWord, varLongWord:
        ret := TCefv8ValueRef.NewUInt(vr);
      varUString, varOleStr, varString:
        ret := TCefv8ValueRef.NewString(vr);
      varSingle, varDouble, varCurrency, varUInt64, varInt64:
        ret := TCefv8ValueRef.NewDouble(vr);
      varBoolean:
        ret := TCefv8ValueRef.NewBool(vr);
      varNull:
        ret := TCefv8ValueRef.NewNull;
      varEmpty:
        ret := TCefv8ValueRef.NewUndefined;
    else
      ret := nil;
      exit(False)
    end;
    Result := True;
  end;

  function ProcessInterface: Boolean;
  var
    m: TRttiMethod;
    f: ICefv8Value;
    ud: ICefv8Value;
    rt: TRttiType;
  begin

    if TypeInfo(ICefv8Value) = v.TypeInfo then
    begin
      ret := ICefv8Value(v.AsInterface);
      Result := True;
    end
    else
    begin
      rt := FCtx.GetType(v.TypeInfo);

      ud := TCefv8ValueRef.NewArray(2);
{$IFDEF CPUX64}
      ud.SetValueByIndex(0, TCefv8ValueRef.NewString(PtrToStr(rt)));
      ud.SetValueByIndex(1,
        TCefv8ValueRef.NewString(PtrToStr(Pointer(v.AsInterface))));
{$ELSE}
      ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
      ud.SetValueByIndex(1, TCefv8ValueRef.NewInt(Integer(v.AsInterface)));
{$ENDIF}
      ret := TCefv8ValueRef.NewObject(nil);
      ret.SetUserData(ud);

      for m in rt.GetMethods do
        if m.Visibility > mvProtected then
        begin
          f := TCefv8ValueRef.NewFunction(m.name, self);
          ret.SetValueByKey(m.name, f, []);
        end;

      Result := True;
    end;
  end;

  function ProcessFloat: Boolean;
  begin
    if v.TypeInfo = TypeInfo(TDateTime) then
      ret := TCefv8ValueRef.NewDate(TValueData(v).FAsDouble)
    else
      ret := TCefv8ValueRef.NewDouble(v.AsExtended);
    Result := True;
  end;

begin
  case v.TypeInfo.kind of
    tkUString, tkLString, tkWString, tkChar, tkWChar:
      ret := TCefv8ValueRef.NewString(v.AsString);
    tkInteger:
      ret := TCefv8ValueRef.NewInt(v.AsInteger);
    tkEnumeration:
      if v.TypeInfo = TypeInfo(Boolean) then
        ret := TCefv8ValueRef.NewBool(v.AsBoolean)
      else
        ret := TCefv8ValueRef.NewInt(TValueData(v).FAsSLong);
    tkFloat:
      if not ProcessFloat then
        exit(False);
    tkInt64:
      ret := TCefv8ValueRef.NewDouble(v.AsInt64);
    tkClass:
      if not ProcessObject then
        exit(False);
    tkClassRef:
      if not ProcessClass then
        exit(False);
    tkRecord:
      if not ProcessRecord then
        exit(False);
    tkVariant:
      if not ProcessVariant then
        exit(False);
    tkInterface:
      if not ProcessInterface then
        exit(False);
  else
    exit(False)
  end;
  Result := True;
end;

{$IFDEF CPUX64}

class function TCefRTTIExtension.StrToPtr(const str: ustring): Pointer;
begin
  HexToBin(PWideChar(str), @Result, SizeOf(Result));
end;

class function TCefRTTIExtension.PtrToStr(p: Pointer): ustring;
begin
  SetLength(Result, SizeOf(p) * 2);
  BinToHex(@p, PWideChar(Result), SizeOf(p));
end;
{$ENDIF}

function TCefRTTIExtension.execute(const name: ustring; const obj: ICefv8Value;
const arguments: TCefv8ValueArray; var retval: ICefv8Value;
var exception: ustring): Boolean;
var
  p: PChar;
  ud: ICefv8Value;
  rt: TRttiType;
  val: TObject;
  cls: TClass;
  m: TRttiMethod;
  pr: TRttiProperty;
  vl: TRttiField;
  args: array of TValue;
  prm: TArray<TRttiParameter>;
  i: Integer;
  ret: TValue;
begin
  Result := True;
  p := PChar(name);
  m := nil;
  if obj <> nil then
  begin
    ud := obj.GetUserData;
    if ud <> nil then
    begin
{$IFDEF CPUX64}
      rt := StrToPtr(ud.GetValueByIndex(0).GetStringValue);
{$ELSE}
      rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
{$ENDIF}
      case rt.TypeKind of
        tkClass:
          begin
{$IFDEF CPUX64}
            val := StrToPtr(ud.GetValueByIndex(1).GetStringValue);
{$ELSE}
            val := TObject(ud.GetValueByIndex(1).GetIntValue);
{$ENDIF}
            cls := GetTypeData(rt.Handle).ClassType;

            if p^ = '$' then
            begin
              Inc(p);
              case p^ of
                'p':
                  begin
                    Inc(p);
                    case p^ of
                      'g':
                        begin
                          Inc(p);
                          pr := rt.GetProperty(p);
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                          if FSyncMainThread then
                          begin
                            TThread.Synchronize(nil,
                              procedure
                              begin
                                ret := pr.GetValue(val);
                              end);
                            exit(SetValue(ret, retval));
                          end
                          else
{$ENDIF}
                            exit(SetValue(pr.GetValue(val), retval));
                        end;
                      's':
                        begin
                          Inc(p);
                          pr := rt.GetProperty(p);
                          if GetValue(pr.PropertyType.Handle, arguments[0], ret)
                          then
                          begin
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                            if FSyncMainThread then
                              TThread.Synchronize(nil,
                                procedure
                                begin
                                  pr.SetValue(val, ret)
                                end)
                            else
{$ENDIF}
                              pr.SetValue(val, ret);
                            exit(True);
                          end
                          else
                            exit(False);
                        end;
                    end;
                  end;
                'v':
                  begin
                    Inc(p);
                    case p^ of
                      'g':
                        begin
                          Inc(p);
                          vl := rt.GetField(p);
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                          if FSyncMainThread then
                          begin
                            TThread.Synchronize(nil,
                              procedure
                              begin
                                ret := vl.GetValue(val);
                              end);
                            exit(SetValue(ret, retval));
                          end
                          else
{$ENDIF}
                            exit(SetValue(vl.GetValue(val), retval));
                        end;
                      's':
                        begin
                          Inc(p);
                          vl := rt.GetField(p);
                          if GetValue(vl.FieldType.Handle, arguments[0], ret)
                          then
                          begin
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                            if FSyncMainThread then
                              TThread.Synchronize(nil,
                                procedure
                                begin
                                  vl.SetValue(val, ret)
                                end)
                            else
{$ENDIF}
                              vl.SetValue(val, ret);
                            exit(True);
                          end
                          else
                            exit(False);
                        end;
                    end;
                  end;
              end;
            end
            else
              m := rt.GetMethod(name);
          end;
        tkClassRef:
          begin
            val := nil;
{$IFDEF CPUX64}
            cls := StrToPtr(ud.GetValueByIndex(1).GetStringValue);
{$ELSE}
            cls := TClass(ud.GetValueByIndex(1).GetIntValue);
{$ENDIF}
            m := FCtx.GetType(cls).GetMethod(name);
          end;
      else
        m := nil;
        cls := nil;
        val := nil;
      end;

      prm := m.GetParameters;
      i := length(prm);
      if i = length(arguments) then
      begin
        SetLength(args, i);
        for i := 0 to i - 1 do
          if not GetValue(prm[i].ParamType.Handle, arguments[i], args[i]) then
            exit(False);

        case m.MethodKind of
          mkClassProcedure, mkClassFunction:
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
            if FSyncMainThread then
              TThread.Synchronize(nil,
                procedure
                begin
                  ret := m.Invoke(cls, args)
                end)
            else
{$ENDIF}
              ret := m.Invoke(cls, args);
          mkProcedure, mkFunction:
            if (val <> nil) then
            begin
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
              if FSyncMainThread then
                TThread.Synchronize(nil,
                  procedure
                  begin
                    ret := m.Invoke(val, args)
                  end)
              else
{$ENDIF}
                ret := m.Invoke(val, args);
            end
            else
              exit(False)
        else
          exit(False);
        end;

        if m.MethodKind in [mkClassFunction, mkFunction] then
          if not SetValue(ret, retval) then
            exit(False);
      end
      else
        exit(False);
    end
    else if p^ = '$' then
    begin
      Inc(p);
      case p^ of
        'g':
          SetValue(FValue, retval);
        's':
          GetValue(FValue.TypeInfo, arguments[0], FValue);
      else
        exit(False);
      end;
    end
    else
      exit(False);
  end
  else
    exit(False);
end;
{$ENDIF}

{ TCefResolveCallbackOwn }

constructor TCefResolveCallbackOwn.Create;
begin
  CreateData(SizeOf(TCefResolveCallback), False);
  with PCefResolveCallback(FData)^ do
    on_resolve_completed := cef_resolve_callback_on_resolve_completed;
end;

{ TCefResponseFilterOwn }

constructor TCefResponseFilterOwn.Create;
begin
  CreateData(SizeOf(TCefResponseFilter), False);
  with PCefResponseFilter(FData)^ do
  begin
    init_filter := cef_response_filter_init_filter;
    filter := cef_response_filter_filter;
  end;
end;

end.
