unit DcefB_Browser;

(*
  基于Dcef3编写的 多标签多进程浏览器 框架
  By BccSafe
  Blog: http://www.bccsafe.com/
  
  编程资质尚浅 若发现BUG或是设计缺陷 希望能联系我
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  
*)
interface

{$I Dcef3_cef.inc}

uses
  Windows, Classes, Controls, ComCtrls, Forms, ExtCtrls, Dialogs, StrUtils,
  SysUtils, Messages, Math,
{$IFDEF DELPHI14_UP}
  Generics.Collections,
{$ENDIF}
  Dcef3_cefgui, Dcef3_ceflib,
  DcefB_BasicEvents, DcefB_BorderLessPC, DcefB_Events, DcefB_Options,
  DcefB_BasicDialog, DcefB_Panel;

Const
  SBlankPageUrl = 'about:blank';
  SLoadingText = '正在加载';
  SNoTitleText = '无标题';
  SDialogTitleSuffix = ' 上的网页显示';
  SUnloadDialogTitle = '确认导航';
  SUnloadDialogText = '确定要离开此页吗？';
  SRunOnlyInSinglePro = '暂时只支持单进程模式';

type
  TCustomDcefBrowser = class;
  TBrowserPage = class;
  TPageContainer = class;

  TChromiumDevTools = class(TWinControl)
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure Resize; override;
  public
    procedure ShowDevTools(const Browser: ICefBrowser;
      inspectElementAt: PCefPoint = nil);
    procedure CloseDevTools(const Browser: ICefBrowser);
  published
    property Align;
    property Visible;
  end;

  TBasicBrowser = class(TWinControl)
  private
    FDcefBrowser: TCustomDcefBrowser;
    FParentBrowserPage: TBrowserPage;
    FBasicDcefBrowserEvents: TBasicDcefBrowserEvents;

    FBrowser: ICefBrowser;
    FClientHandler: ICefClient;

    FBrowserId: Integer;
    FDefaultUrl: ustring;
    FUserStyleSheetLocation: ustring;
    FDefaultEncoding: ustring;
    FPageID: Integer;

    FIsLoading: Boolean;
    FCanGoForward: Boolean;
    FCanGoBack: Boolean;

    FLastTitle: ustring;
    FLastAddress: ustring;

    procedure BrowserLoadingStateChange(const Browser: ICefBrowser;
      isLoading, canGoBack, canGoForward: Boolean);
    procedure BrowserStatusMessage(const Browser: ICefBrowser;
      const value: ustring);
    procedure BrowserAddressChange(const Browser: ICefBrowser;
      const frame: ICefFrame; const URL: ustring);
    procedure BrowserTitleChange(const Browser: ICefBrowser;
      const title: ustring);

    procedure BrowserBeforePopup(const Browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; out Result: Boolean);
    procedure BrowserAfterCreated(const Browser: ICefBrowser);
    procedure BrowserClose(const Browser: ICefBrowser; out Result: Boolean);
    procedure BrowserBeforeBrowse(const Browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean;
      out Result: Boolean);

    procedure BrowserPreKeyEvent(const Browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle;
      out isKeyboardShortcut: Boolean; out Result: Boolean);
    procedure BrowserKeyEvent(const Browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean);

    procedure BrowserLoadStart(const Browser: ICefBrowser;
      const frame: ICefFrame);
    procedure BrowserLoadEnd(const Browser: ICefBrowser; const frame: ICefFrame;
      httpStatusCode: Integer);
    procedure BrowserLoadError(const Browser: ICefBrowser;
      const frame: ICefFrame; errorCode: Integer;
      const errorText, failedUrl: ustring);

    procedure BrowserBeforeDownload(const Browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback);
    procedure BrowserDownloadUpdated(const Browser: ICefBrowser;
      const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback);

    procedure BrowserBeforeResourceLoad(const Browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; out Result: Boolean);
    procedure BrowserGetResourceHandler(const Browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      out Result: ICefResourceHandler);
    procedure BrowserResourceRedirect(const Browser: ICefBrowser;
      const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring);

    procedure BrowserGotFocus(const Browser: ICefBrowser);
    procedure BrowserSetFocus(const Browser: ICefBrowser;
      source: TCefFocusSource; out Result: Boolean);
    procedure BrowserTakeFocus(const Browser: ICefBrowser; next: Boolean);

    procedure BrowserBeforeContextMenu(const Browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      const model: ICefMenuModel);
    procedure BrowserContextMenuCommand(const Browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      commandId: Integer; eventFlags: TCefEventFlags; out Result: Boolean);
    procedure BrowserContextMenuDismissed(const Browser: ICefBrowser;
      const frame: ICefFrame);

    procedure BrowserJsdialog(const Browser: ICefBrowser;
      const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
      const messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean;
      out Result: Boolean);
    procedure BrowserBeforeUnloadDialog(const Browser: ICefBrowser;
      const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback; out Result: Boolean);
    procedure BrowserDialogClosed(const Browser: ICefBrowser);

    procedure BrowserPluginCrashed(const Browser: ICefBrowser;
      const pluginPath: ustring);
    procedure BrowserBeforePluginLoad(const Browser: ICefBrowser;
      const URL, policyUrl: ustring; const info: ICefWebPluginInfo;
      out Result: Boolean);

    procedure BrowserGetAuthCredentials(const Browser: ICefBrowser;
      const frame: ICefFrame; isProxy: Boolean; const host: ustring;
      port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback; out Result: Boolean);
    procedure BrowserConsoleMessage(const Browser: ICefBrowser;
      const Message, source: ustring; line: Integer; out Result: Boolean);
    procedure BrowserProtocolExecution(const Browser: ICefBrowser;
      const URL: ustring; out allowOsExecution: Boolean);

    procedure BrowserFileDialog(const Browser: ICefBrowser;
      mode: TCefFileDialogMode; const title, defaultFileName: ustring;
      acceptTypes: TStrings; const callback: ICefFileDialogCallback;
      out Result: Boolean);

    procedure BrowserRequestGeolocationPermission(const Browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer;
      const callback: ICefGeolocationCallback; out Result: Boolean);
    procedure BrowserCancelGeolocationPermission(const Browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer);

    procedure BrowserQuotaRequest(const Browser: ICefBrowser;
      const originUrl: ustring; newSize: Int64;
      const callback: ICefQuotaCallback; out Result: Boolean);
    procedure BrowserCertificateError(certError: TCefErrorCode;
      const requestUrl: ustring;
      const callback: ICefAllowCertificateErrorCallback; out Result: Boolean);

    procedure BrowserDragEnter(const Browser: ICefBrowser;
      const dragData: ICefDragData; mask: TCefDragOperations;
      out Result: Boolean);
    procedure BrowserStartDragging(const Browser: ICefBrowser;
      const dragData: ICefDragData; allowedOps: TCefDragOperations;
      x, y: Integer; out Result: Boolean);
    procedure BrowserUpdateDragCursor(const Browser: ICefBrowser;
      operation: TCefDragOperation);
    procedure BrowserCursorChange(const Browser: ICefBrowser;
      cursor: TCefCursorHandle; cursorType: TCefCursorType;
      const customCursorInfo: PCefCursorInfo);

    procedure LoadBrowserEvents;
    procedure GetSettings(var settings: TCefBrowserSettings);
    procedure CreateBrowser;

    function GetPageIndex: Integer;
    function GetActived: Boolean;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; ParentBrowserPage: TBrowserPage;
      DcefBrowser: TCustomDcefBrowser; DefaultEncoding: ustring; PID: Integer;
      CreateByPopup: Boolean; Const DefaultURL: ustring = SBlankPageUrl;
      FShow: Boolean = True); reintroduce;
    destructor Destroy; override;

    procedure UpdatePageInfo(FCheckActived: Boolean);

    property PageIndex: Integer read GetPageIndex;
    property PageID: Integer read FPageID;
    property Actived: Boolean read GetActived;
    property isLoading: Boolean read FIsLoading;
    property canGoBack: Boolean read FCanGoBack;
    property canGoForward: Boolean read FCanGoForward;
  end;

  TRenderProcessCallbackA = {$IFDEF DELPHI14_UP}reference to
{$ENDIF} procedure(ASender: TBrowserPage; AContext: ICefv8Context;
    AData: Pointer);

  TBrowserPage = class
  private
    FBasicBrowser: TBasicBrowser;
    FBrowserPanel: TDcefBPanel;
    FDevToolsPanel: TDcefBPanel;
    FDevTools: TChromiumDevTools;
    FSplitter: TSplitter;
    FTabsheet: TTabSheet;
    FSearchTextBar: TSearchTextBar;
    FPageID: Integer;
    FDefaultEncoding: ustring;
    FDebugWinHandle: HWND;

    FPageControl: TBorderLessPageControl;
    FDcefBrowser: Pointer;
    FParentItems: TPageContainer;

    FCreateByPopup: Boolean; // 1 dcef3弹出的新窗体   0 自己创建的窗体

    procedure BrowserPanelResize(Sender: TObject);
    procedure BringSearchBarToFront;
    procedure CreateTabSheet(FShow: Boolean; PID: Integer);
    function GetPageIndex: Integer;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    function GetIsLoading: Boolean;
    function GetBrowser: ICefBrowser;
    function GetUrl: string;
    procedure DebugPanelResize(Sender: TObject);
    procedure MoveBrowserWin(Method: Integer);
    function GetZoomLevel: string;
    function GetTitle: String;
    procedure SetTitle(const value: String);
    function GetTabVisible: Boolean;
    procedure SetTabVisible(const value: Boolean);
    function GetClientRect: TRect;
  public
    constructor Create(PageControl: TBorderLessPageControl;
      ParentItems: Pointer; DcefBrowser: Pointer; DefaultEncoding: ustring;
      PID: Integer; CreateByPopup: Boolean;
      Const DefaultURL: ustring = SBlankPageUrl; FShow: Boolean = True);
      reintroduce;
    destructor Destroy; override;

    procedure Show;
    procedure Close;
    procedure Load(Const URL: string);
    procedure SetFocus;
    procedure DevTools;

    procedure GoBack;
    procedure GoForward;
    procedure Reload;
    procedure ReloadIgnoreCache;
    procedure StopLoad;
    procedure Print;
    procedure SearchText;
    procedure ExecuteJavaScript(Const code: string);
    procedure AddZoomLevel;
    procedure ReduceZoomLevel;
    procedure ResetZoomLevel;
    procedure GetSourceInNewPage;
{$IFDEF DELPHI14_UP}
    procedure RunInRenderProcess(AProc: TRenderProcessCallbackA;
      AData: Pointer);
    function GetSource(var SourceText: string;
      Const TimeOut: Integer = 1000): Boolean;
    function GetText(var aText: string; Const TimeOut: Integer = 1000): Boolean;
{$ENDIF}
    property PageIndex: Integer read GetPageIndex;
    property PageID: Integer read FPageID;
    property Browser: ICefBrowser read GetBrowser;
    property CreateByPopup: Boolean read FCreateByPopup;
    property isLoading: Boolean read GetIsLoading;
    property canGoBack: Boolean read GetCanGoBack;
    property canGoForward: Boolean read GetCanGoForward;
    property URL: string read GetUrl;
    property ZoomLevel: string read GetZoomLevel;
    property title: String read GetTitle write SetTitle;
    property TabVisible: Boolean read GetTabVisible write SetTabVisible;
    property ClientRect: TRect read GetClientRect;
  end;

  TPageContainerLocker = class
  private
    FCS: TRTLCriticalSection;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Lock();
    procedure UnLock();
  end;

{$IFDEF DELPHI14_UP}

  TDynStrArr = array of string;
  TDynIntArr = array of Integer;

  // 线程安全
  TPageContainer = class
  private
    FItems: TList<TBrowserPage>;
    function GetItems(Index: Integer): TBrowserPage;
    function GetCount: Integer;
  public
    constructor Create();
    destructor Destroy; override;

    procedure FreeAndDelElement(Index: Integer); overload;
    procedure FreeAndDelElement(Const ExceptPageID: Integer;
      var Urls: TDynStrArr; var IDs: TDynIntArr); overload;
    procedure FreeAndDelElement(var DelIDArr: TDynIntArr; var Urls: TDynStrArr;
      var IDs: TDynIntArr); overload;
    function Add(const value: TBrowserPage): Integer;
    procedure Clear; overload;
    procedure Clear(var Urls: TDynStrArr; var IDs: TDynIntArr); overload;
    function IndexOf(const value: TBrowserPage): Integer;
    function GetPageIdByIndex(Index: Integer): Integer;
    function GetPageIndexByID(aID: Integer): Integer;
    function GetPageByID(aID: Integer): TBrowserPage;

    property Items[Index: Integer]: TBrowserPage read GetItems;
    property Count: Integer read GetCount;
  end;
{$ELSE}

  TPageContainer = class
  private
    FItems: Array of TBrowserPage;
    function GetCount: Integer;
  public
    constructor Create();
    destructor Destroy;
    function Add(aBrowserPage: TBrowserPage): Integer;
    function IndexOf(aBrowserPage: TBrowserPage): Integer;
    procedure FreeAndDelElement(Index: Integer);
    function GetByIndex(Index: Integer): TBrowserPage;
    property Count: Integer read GetCount;
  end;
{$ENDIF}

  TCustomDcefBrowser = class(TWinControl)
  private
    FPageControl: TBorderLessPageControl;
    FPageItems: TPageContainer;
    FClosedPageURL: TStringList;

    FOptions: TDcefBrowserOptions;
    FBasicOptions: TChromiumOptions;
    FBasicFontOptions: TChromiumFontOptions;

    FDefaultUrl: ustring;
    FDefaultEncoding: ustring;
    FActivePageID: Integer;
    FAddUpPageID: Integer;

    FOnPageChanged: TOnPageChanged;
    FOnPageChanging: TOnPageChanging;
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

    function GetPageByIndex(Index: Integer): TBrowserPage;
    function GetActivePageItem: TBrowserPage;
    function GetActivePageIndex: Integer;
    function GetPageCount: Integer;
    function GetActivePageIsLoading: Boolean;
    function GetActivePageCanGoBack: Boolean;
    function GetActivePageCanGoForward: Boolean;
    function GetNewPageID: Integer;
    function GetClosePageCount: Integer;
    function GetZoomLevel: string;
    procedure SetTabVisible(const value: Boolean);
  protected
    // swish changed:
    FTabVisible: Boolean;
    FLastWndProc: TWndMethod;
    FParentForm: TCustomForm;
    procedure HookWndProc;
    procedure UnhookWndProc;
    procedure WndProc(var AMsg: TMessage); override;
    procedure FormWndProc(var AMsg: TMessage);
    // changed end

    procedure doOnPageChanging(Sender: TObject; var Allow: Boolean);
    // swish add
    procedure doOnPageChanged(Sender: TObject); // swish add
    procedure doOnPageLoadingStateChange(const PageID: Integer;
      const Browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure doOnPageStateChange(const PageID: Integer;
      const Kind: TBrowserDataChangeKind; const value: string;
      const PageActived: Boolean);
    procedure doOnPageAdd(const PageID: Integer; Const AddAtLast: Boolean);
    procedure doOnPageClose(const ClosePageIDArr: Array of Integer;
      Const ShowPageID: Integer);

    procedure doOnLoadStart(const PageIndex: Integer;
      const Browser: ICefBrowser; const frame: ICefFrame);
    procedure doOnLoadEnd(const PageIndex: Integer; const Browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: Integer);
    procedure doOnLoadError(const PageIndex: Integer;
      const Browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
      const errorText, failedUrl: ustring);
    procedure doOnBeforeBrowse(const PageIndex: Integer;
      const Browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; isRedirect: Boolean; var Cancel: Boolean);

    procedure doOnPreKeyEvent(const PageIndex: Integer;
      const Browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle; var isKeyboardShortcut: Boolean;
      var Cancel: Boolean);
    procedure doOnKeyEvent(const PageIndex: Integer; const Browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle; var Cancel: Boolean);

    procedure doOnBeforeResourceLoad(const PageIndex: Integer;
      const Browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; var CancelLoad: Boolean);
    procedure doOnGetResourceHandler(const PageIndex: Integer;
      const Browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; var ResourceHandler: ICefResourceHandler);
    procedure doOnResourceRedirect(const PageIndex: Integer;
      const Browser: ICefBrowser; const frame: ICefFrame; const oldUrl: ustring;
      var newUrl: ustring);

    procedure doOnGotFocus(const PageIndex: Integer; const Browser: ICefBrowser;
      var CancelEventBuiltIn: Boolean);
    procedure doOnSetFocus(const PageIndex: Integer; const Browser: ICefBrowser;
      source: TCefFocusSource; var CancelFocus: Boolean);
    procedure doOnTakeFocus(const PageIndex: Integer;
      const Browser: ICefBrowser; next: Boolean);

    procedure doOnBeforeContextMenu(const PageIndex: Integer;
      const Browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure doOnContextMenuCommand(const PageIndex: Integer;
      const Browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: TCefEventFlags; var CancelEventBuiltIn: Boolean);
    procedure doOnContextMenuDismissed(const PageIndex: Integer;
      const Browser: ICefBrowser; const frame: ICefFrame);

    procedure doOnJsdialog(const PageIndex: Integer; const Browser: ICefBrowser;
      const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
      const messageText, defaultPromptText: ustring;
      const callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean);
    procedure doOnBeforeUnloadDialog(const PageIndex: Integer;
      const Browser: ICefBrowser; const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean);
    procedure doOnDialogClosed(const PageIndex: Integer;
      const Browser: ICefBrowser);

    procedure doOnPluginCrashed(const PageIndex: Integer;
      const Browser: ICefBrowser; const pluginPath: ustring);
    procedure doOnBeforePluginLoad(const PageIndex: Integer;
      const Browser: ICefBrowser; const URL, policyUrl: ustring;
      const info: ICefWebPluginInfo; var CancelLoad: Boolean);

    { procedure doOnDownloadUpdated(Const DcefItemIndex: Integer;
      Const Kind: TBrowserDownloadUpdatedKind); }
    procedure doOnBeforeDownload(const PageIndex: Integer;
      const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const suggestedName: ustring; const callback: ICefBeforeDownloadCallback;
      var CancelBuiltinPro: Boolean);
    procedure doOnDownloadUpdated(const PageIndex: Integer;
      const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback);
    procedure doOnGetAuthCredentials(const PageIndex: Integer;
      const Browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean;
      const host: ustring; port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback; var CancelEventBuiltIn: Boolean);
    procedure doOnConsoleMessage(const PageIndex: Integer;
      const Browser: ICefBrowser; const Message, source: ustring; line: Integer;
      var CancelEventBuiltIn: Boolean);
    procedure doOnProtocolExecution(const PageIndex: Integer;
      const Browser: ICefBrowser; const URL: ustring;
      out allowOsExecution: Boolean);
    procedure doOnFileDialog(const PageIndex: Integer;
      const Browser: ICefBrowser; mode: TCefFileDialogMode;
      const title, defaultFileName: ustring; acceptTypes: TStrings;
      const callback: ICefFileDialogCallback; out Result: Boolean);

    procedure doOnRequestGeolocationPermission(const PageIndex: Integer;
      const Browser: ICefBrowser; const requestingUrl: ustring;
      requestId: Integer; const callback: ICefGeolocationCallback;
      out Result: Boolean);
    procedure doOnCancelGeolocationPermission(const PageIndex: Integer;
      const Browser: ICefBrowser; const requestingUrl: ustring;
      requestId: Integer);

    procedure doOnQuotaRequest(const PageIndex: Integer;
      const Browser: ICefBrowser; const originUrl: ustring; newSize: Int64;
      const callback: ICefQuotaCallback; out Result: Boolean);
    procedure doOnCertificateError(const PageIndex: Integer;
      certError: TCefErrorCode; const requestUrl: ustring;
      const callback: ICefAllowCertificateErrorCallback; out Result: Boolean);

    procedure doOnDragEnter(const PageIndex: Integer;
      const Browser: ICefBrowser; const dragData: ICefDragData;
      mask: TCefDragOperations; out Result: Boolean);
    procedure doOnStartDragging(const PageIndex: Integer;
      const Browser: ICefBrowser; const dragData: ICefDragData;
      allowedOps: TCefDragOperations; x, y: Integer; out Result: Boolean);
    procedure doOnUpdateDragCursor(const PageIndex: Integer;
      const Browser: ICefBrowser; operation: TCefDragOperation);
    procedure doOnCursorChange(const PageIndex: Integer;
      const Browser: ICefBrowser; cursor: TCefCursorHandle;
      cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddPage(Const URL: string = ''; FShow: Boolean = True;
      AddAtLast: Boolean = True): Integer;
    function ClosePage(PageIndex: Integer;
      Const AutoChooseShowPageID: Boolean = True;
      Const ShowPageID: Integer = -1): Integer; overload;
    function ClosePage(ArrayPageIndex: Array of Integer; ShowPageID: Integer)
      : Integer; overload;
    procedure CloseAllOtherPage(PageID: Integer);
    procedure CloseAllPage(IsTrigClosePageEvent: Boolean);
    procedure ShowPage(PageID: Integer);
    procedure Load(Const URL: string);
    procedure SetPageFocus;
    procedure CopyPage(PageID: Integer; Const FShow: Boolean = True);
    procedure DownloadFile(FileURL: string);

    procedure DevTools;
    procedure GoHome;
    procedure GoBack;
    procedure GoForward;
    procedure Reload;
    procedure ReloadIgnoreCache;
    procedure ReOpenClosedPage;
    procedure StopLoad;
    procedure Print;
    procedure SearchText;
    procedure ExecuteJavaScript(Const code: string);
    procedure AddZoomLevel;
    procedure ReduceZoomLevel;
    procedure ResetZoomLevel;
    procedure GetSourceInNewPage;
{$IFDEF DELPHI14_UP}
    function GetSource(var SourceText: string;
      Const TimeOut: Integer = 1000): Boolean;
    function GetText(var aText: string; Const TimeOut: Integer = 1000): Boolean;
{$ENDIF}
    function GetPageByID(ID: Integer): TBrowserPage;
    function PageIDToIndex(PageID: Integer): Integer;
    function PageIndexToID(PageIndex: Integer): Integer;

    class procedure CreateDefaultRenderProcess;
{$IFDEF DELPHI14_UP}
    class procedure RegisterClasses(const aObjList: array of TClass);
{$ENDIF}
    property Pages[Index: Integer]: TBrowserPage read GetPageByIndex;
    property ActivePage: TBrowserPage read GetActivePageItem;
    property ActivePageIndex: Integer read GetActivePageIndex;
    property ActivePageID: Integer read FActivePageID write FActivePageID;
    property PageCount: Integer read GetPageCount;
    property ClosedPageCount: Integer read GetClosePageCount;
    property DefaultURL: ustring read FDefaultUrl write FDefaultUrl;
    property DefaultEncoding: ustring read FDefaultEncoding
      write FDefaultEncoding;
    property Options: TDcefBrowserOptions read FOptions write FOptions;
    property BasicOptions: TChromiumOptions read FBasicOptions
      write FBasicOptions;
    property BasicFontOptions: TChromiumFontOptions read FBasicFontOptions;

    property isLoading: Boolean read GetActivePageIsLoading;
    property canGoBack: Boolean read GetActivePageCanGoBack;
    property canGoForward: Boolean read GetActivePageCanGoForward;
    property ZoomLevel: string read GetZoomLevel;

    property OnPageChanging: TOnPageChanging read FOnPageChanging
      write FOnPageChanging;
    property OnPageChanged: TOnPageChanged read FOnPageChanged
      write FOnPageChanged;
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
    property TabVisible: Boolean read FTabVisible write SetTabVisible;
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

    property Options;
    property BasicOptions;
    property BasicFontOptions;

    property OnPageLoadingStateChange;
    property OnPageStateChange;
    property OnPageAdd;
    property OnPageClose;
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
  end;

  TDefaultRenderProcessHandler = class(TCefRenderProcessHandlerOwn)
  protected
    FClasses: array of TClass;
    procedure OnWebKitInitialized; override;
    function OnProcessMessageReceived(const Browser: ICefBrowser;
      sourceProcess: TCefProcessId; const Message: ICefProcessMessage)
      : Boolean; override;
  end;

var
  PagesHelper: TPageContainerLocker;

implementation

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}

uses
{$IFDEF DELPHI16_UP}
  Vcl.AppEvnts;
{$ELSE}
  AppEvnts;
{$ENDIF}

var
  CefInstances: Integer = 0;
  CefTimer: UINT = 0;
{$ENDIF}

type
  TVCLDcefClientHandler = class(TCustomClientHandler)
  public
    constructor Create(const crm: IChromiumEvents; renderer: Boolean); override;
    destructor Destroy; override;
  end;

  { TVCLClientHandler }

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}

var
  looping: Boolean = False;

procedure TimerProc(HWND: HWND; uMsg: UINT; idEvent: Pointer;
  dwTime: DWORD); stdcall;
begin
  if looping then
    Exit;
  if CefInstances > 0 then
  begin
    looping := True;
    try
      CefDoMessageLoopWork

      // avan lau 2015-08-04
      // 非阻塞模式，在Win8-64bit系统下，且为多进程模式（CefSingleProcess := False;）
      // 极易出现程序无响应的状况，故采用CefRunMessageLoop替代（阻塞）
      // CefRunMessageLoop;
      // BccSafe 2015-08-12
      // 使用CefRunMessageLoop方法将会导致多个问题，目前暂无解决方案
      // 故还原 如果你使用时出现了如avan lau的状况，可以手动更改
    finally
      looping := False;
    end;
  end;
end;
{$ENDIF}

constructor TVCLDcefClientHandler.Create(const crm: IChromiumEvents;
  renderer: Boolean);
begin
  inherited Create(crm, renderer);
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  if CefInstances = 0 then
    CefTimer := SetTimer(0, 0, 10, @TimerProc);
  InterlockedIncrement(CefInstances);
{$ENDIF}
end;

destructor TVCLDcefClientHandler.Destroy;
begin
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  InterlockedDecrement(CefInstances);
  if CefInstances = 0 then
  begin
    KillTimer(0, CefTimer);
    // avan lau 2015-08-04
    // 搭配CefRunMessageLoop使用
    // CefQuitMessageLoop;
    // BccSafe 2015-08-12
    // 还原 理由见TimerProc内
  end;
{$ENDIF}
  inherited;
end;

{ TPageContainer }

{$IFDEF DELPHI14_UP}

function TPageContainer.GetItems(Index: Integer): TBrowserPage;
begin
  Result := nil;
  PagesHelper.Lock;
  try
    if (Index > -1) and (Index < FItems.Count) then
      Result := FItems[Index];
  finally
    PagesHelper.UnLock;
  end;
end;

function TPageContainer.GetCount: Integer;
begin
  PagesHelper.Lock;
  try
    Result := FItems.Count;
  finally
    PagesHelper.UnLock;
  end;
end;

constructor TPageContainer.Create();
begin
  FItems := TList<TBrowserPage>.Create;
end;

destructor TPageContainer.Destroy;
begin
  Clear;
  FItems.Free;
end;

procedure TPageContainer.FreeAndDelElement(Index: Integer);
begin
  PagesHelper.Lock;
  try
    FItems[Index].Free;
    FItems.Delete(Index);
  finally
    PagesHelper.UnLock;
  end;
end;

procedure TPageContainer.FreeAndDelElement(Const ExceptPageID: Integer;
  var Urls: TDynStrArr; var IDs: TDynIntArr);
var
  Index: Integer;
begin
  PagesHelper.Lock;
  try
    for Index := FItems.Count - 1 downto 0 do
    begin
      if FItems[Index].PageID <> ExceptPageID then
      begin
        SetLength(Urls, Length(Urls) + 1);
        SetLength(IDs, Length(IDs) + 1);
        Urls[High(Urls)] := FItems[Index].URL;
        IDs[High(IDs)] := FItems[Index].PageID;
        FItems[Index].Free;
        FItems.Delete(Index);
      end;
    end;
  finally
    PagesHelper.UnLock;
  end;
end;

procedure TPageContainer.FreeAndDelElement(var DelIDArr: TDynIntArr;
  var Urls: TDynStrArr; var IDs: TDynIntArr);
var
  Index, ii, TempInt: Integer;
begin
  for Index := Low(DelIDArr) to High(DelIDArr) do
    for ii := Index + 1 to High(DelIDArr) do
      if DelIDArr[Index] > DelIDArr[ii] then
      begin
        TempInt := DelIDArr[Index];
        DelIDArr[Index] := DelIDArr[ii];
        DelIDArr[ii] := TempInt;
      end;

  PagesHelper.Lock;
  try
    for Index := High(DelIDArr) downto Low(DelIDArr) do
    begin
      SetLength(Urls, Length(Urls) + 1);
      SetLength(IDs, Length(IDs) + 1);
      Urls[High(Urls)] := FItems[Index].URL;
      IDs[High(IDs)] := FItems[Index].PageID;
      FItems[Index].Free;
      FItems.Delete(Index);
    end;
  finally
    PagesHelper.UnLock;
  end;
end;

function TPageContainer.Add(const value: TBrowserPage): Integer;
begin
  PagesHelper.Lock;
  try
    Result := FItems.Add(value);
  finally
    PagesHelper.UnLock;
  end;
end;

function TPageContainer.IndexOf(const value: TBrowserPage): Integer;
begin
  PagesHelper.Lock;
  try
    Result := FItems.IndexOf(value);
  finally
    PagesHelper.UnLock;
  end;
end;

function TPageContainer.GetPageIdByIndex(Index: Integer): Integer;
begin
  Result := -1;
  PagesHelper.Lock;
  try
    if (Index > -1) and (Index < FItems.Count) then
      Result := FItems[Index].PageID;
  finally
    PagesHelper.UnLock;
  end;
end;

function TPageContainer.GetPageIndexByID(aID: Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  PagesHelper.Lock;
  try
    for Index := 0 to FItems.Count - 1 do
    begin
      if FItems[Index].PageID = aID then
      begin
        Result := Index;
        Break;
      end;
    end;
  finally
    PagesHelper.UnLock;
  end;
end;

function TPageContainer.GetPageByID(aID: Integer): TBrowserPage;
var
  Index: Integer;
begin
  Result := nil;
  PagesHelper.Lock;
  try
    for Index := 0 to FItems.Count - 1 do
    begin
      if FItems[Index].PageID = aID then
      begin
        Result := FItems[Index];
        Break;
      end;
    end;
  finally
    PagesHelper.UnLock;
  end;
end;

procedure TPageContainer.Clear;
var
  Index: Integer;
begin
  PagesHelper.Lock;
  try
    for Index := FItems.Count - 1 downto 0 do
    begin
      FItems[Index].Free;
      FItems.Delete(Index);
    end;
  finally
    PagesHelper.UnLock;
  end;
end;

procedure TPageContainer.Clear(var Urls: TDynStrArr; var IDs: TDynIntArr);
var
  Index: Integer;
begin
  PagesHelper.Lock;
  try
    for Index := FItems.Count - 1 downto 0 do
    begin
      SetLength(Urls, Length(Urls) + 1);
      Urls[High(Urls)] := FItems[Index].URL;
      SetLength(IDs, Length(IDs) + 1);
      IDs[High(IDs)] := FItems[Index].PageID;
      FItems[Index].Free;
      FItems.Delete(Index);
    end;
  finally
    PagesHelper.UnLock;
  end;
end;

{$ELSE}

constructor TPageContainer.Create();
begin

end;

destructor TPageContainer.Destroy;
var
  Index: Integer;
begin
  for Index := High(FItems) downto Low(FItems) do
    FItems[Index].Free;
  SetLength(FItems, 0);
end;

function TPageContainer.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TPageContainer.GetByIndex(Index: Integer): TBrowserPage;
begin
  Result := FItems[Index];
end;

function TPageContainer.Add(aBrowserPage: TBrowserPage): Integer;
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := aBrowserPage;
  Result := High(FItems);
end;

function TPageContainer.IndexOf(aBrowserPage: TBrowserPage): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := Low(FItems) to High(FItems) do
    if FItems[Index] = aBrowserPage then
    begin
      Result := Index;
      Break;
    end;
end;

procedure TPageContainer.FreeAndDelElement(Index: Integer);
var
  Count: Cardinal;
begin
  GetByIndex(Index).Free;
  Count := Length(FItems);
  if (Count = 0) or (Index < 0) or (Index >= Count) then
    Exit;
  Move(FItems[Index + 1], FItems[Index], (Count - Index) *
    SizeOf(TBrowserPage));
  SetLength(FItems, Count - 1);
end;
{$ENDIF}
{ TDcefBrowser }

function TCustomDcefBrowser.AddPage(Const URL: string = '';
  FShow: Boolean = True; AddAtLast: Boolean = True): Integer;
var
  BrowserPageItem: TBrowserPage;
  URLStr: string;
  PageID: Integer;
begin
  URLStr := IfThen(SameText(URL, ''), FDefaultUrl, URL);
  PageID := GetNewPageID;

  BrowserPageItem := TBrowserPage.Create(FPageControl, FPageItems, Self,
    FDefaultEncoding, PageID, False, URLStr, FShow);
  Result := FPageItems.Add(BrowserPageItem);
  doOnPageAdd(PageID, AddAtLast);
end;

function TCustomDcefBrowser.PageIDToIndex(PageID: Integer): Integer;
begin
  Result := FPageItems.GetPageIndexByID(PageID);
end;

function TCustomDcefBrowser.PageIndexToID(PageIndex: Integer): Integer;
begin
  Result := FPageItems.GetPageIdByIndex(PageIndex);
end;

procedure TCustomDcefBrowser.Print;
begin
  if ActivePage <> nil then
    ActivePage.Print;
end;

procedure TCustomDcefBrowser.ReduceZoomLevel;
begin
  if ActivePage <> nil then
    ActivePage.ReduceZoomLevel;
end;

procedure TCustomDcefBrowser.Reload;
begin
  if ActivePage <> nil then
    ActivePage.Reload;
end;

procedure TCustomDcefBrowser.ReloadIgnoreCache;
begin
  if ActivePage <> nil then
    ActivePage.ReloadIgnoreCache;
end;

procedure TCustomDcefBrowser.ReOpenClosedPage;
begin
  if FClosedPageURL.Count > 0 then
  begin
    AddPage(FClosedPageURL[0]);
    FClosedPageURL.Delete(0);
  end;
end;

procedure TCustomDcefBrowser.ResetZoomLevel;
begin
  if ActivePage <> nil then
    ActivePage.ResetZoomLevel;
end;

procedure TCustomDcefBrowser.SearchText;
begin
  if ActivePage <> nil then
    ActivePage.SearchText;
end;

procedure TCustomDcefBrowser.SetPageFocus;
begin
  if ActivePage <> nil then
    ActivePage.SetFocus;
end;

procedure TCustomDcefBrowser.SetTabVisible(const value: Boolean);
var
  ALastActive: TTabSheet;
  i: Integer;
begin
  if FTabVisible <> value then
  begin
    ALastActive := FPageControl.ActivePage;
    FTabVisible := value;
    if value then
      FPageControl.TabHeight := 24
    else
      FPageControl.TabHeight := 0;
    for i := 0 to FPageControl.PageCount - 1 do
      Pages[i].TabVisible := value;
    FPageControl.ActivePage := ALastActive;
  end;
end;

procedure TCustomDcefBrowser.ShowPage(PageID: Integer);
var
  Page: TBrowserPage;
begin
  Page := Pages[PageIDToIndex(PageID)];
  if Page <> nil then
    Page.Show;
end;

procedure TCustomDcefBrowser.StopLoad;
begin
  if ActivePage <> nil then
    ActivePage.StopLoad;
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
    HookWndProc; // Added by swish
end;

procedure TCustomDcefBrowser.CloseAllOtherPage(PageID: Integer);
var
  Index: Integer;
  Urls: TDynStrArr;
  ClosePageIDArr: TDynIntArr;
begin
  FPageItems.FreeAndDelElement(PageID, Urls, ClosePageIDArr);
  for Index := Low(Urls) to High(Urls) do
    FClosedPageURL.Add(Urls[Index]);

  doOnPageClose(ClosePageIDArr, PageID);
  SetLength(Urls, 0);
  SetLength(ClosePageIDArr, 0);
end;

procedure TCustomDcefBrowser.CloseAllPage(IsTrigClosePageEvent: Boolean);
var
  Index: Integer;
  Urls: TDynStrArr;
  ClosePageIDArr: TDynIntArr;
begin
  FPageItems.Clear(Urls, ClosePageIDArr);
  for Index := Low(Urls) to High(Urls) do
    FClosedPageURL.Add(Urls[Index]);

  if IsTrigClosePageEvent then
    doOnPageClose(ClosePageIDArr, -1);
  SetLength(Urls, 0);
  SetLength(ClosePageIDArr, 0);
end;

function TCustomDcefBrowser.ClosePage(ArrayPageIndex: Array of Integer;
  ShowPageID: Integer): Integer;
var
  Index: Integer;
  Urls: TDynStrArr;
  DelIDArr, ClosePageIDArr: TDynIntArr;
begin
  Result := PageIDToIndex(ShowPageID);
  for Index := Low(ArrayPageIndex) to High(ArrayPageIndex) do
  begin
    SetLength(DelIDArr, Length(DelIDArr) + 1);
    DelIDArr[High(DelIDArr)] := ArrayPageIndex[Index];
  end;
  FPageItems.FreeAndDelElement(DelIDArr, Urls, ClosePageIDArr);

  for Index := Low(Urls) to High(Urls) do
    FClosedPageURL.Add(Urls[Index]);

  doOnPageClose(ClosePageIDArr, ShowPageID);
  SetLength(DelIDArr, 0);
  SetLength(Urls, 0);
  SetLength(ClosePageIDArr, 0);
end;

function TCustomDcefBrowser.ClosePage(PageIndex: Integer;
  Const AutoChooseShowPageID: Boolean = True;
  Const ShowPageID: Integer = -1): Integer;
var
  FLastIndex: Boolean;
  Page: TBrowserPage;
  ClosePageArr: Array of Integer;
begin
  Result := -1;
  if (PageIndex > -1) and (PageIndex < PageCount) then
  begin
    FLastIndex := PageIndex = (PageCount - 1);
    Page := Pages[PageIndex];
    if Page <> nil then
    begin
      FClosedPageURL.Add(Page.URL);
      Result := PageIndexToID(IfThen(FLastIndex, PageIndex - 1, PageIndex));
      SetLength(ClosePageArr, Length(ClosePageArr) + 1);
      ClosePageArr[High(ClosePageArr)] := Page.PageID;
      doOnPageClose(ClosePageArr, IfThen(AutoChooseShowPageID, Result,
        ShowPageID));
      FPageItems.FreeAndDelElement(PageIndex);
      if (PageCount > 0) then
        ShowPage(IfThen(AutoChooseShowPageID, Result, ShowPageID))
      else if FOptions.ExitPagesClosed then
      begin
        CefShutdown;
        ExitProcess(0);
      end;
    end;
  end;
end;

procedure TCustomDcefBrowser.CopyPage(PageID: Integer;
  Const FShow: Boolean = True);
var
  Page: TBrowserPage;
begin
  Page := GetPageByID(PageID);
  if Page <> nil then
    AddPage(Page.URL, FShow, False);
end;

constructor TCustomDcefBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultUrl := SBlankPageUrl;
  FActivePageID := -1;
  FAddUpPageID := -1;
  FTabVisible := False;

  FPageControl := TBorderLessPageControl.Create(Self);
  FPageControl.Parent := TWinControl(Self);
  FPageControl.Align := alClient;
  FPageControl.OnChange := doOnPageChanged;
  FPageControl.OnChanging := doOnPageChanging;
  FOptions := TDcefBrowserOptions.Create;
  FBasicFontOptions := TChromiumFontOptions.Create;
  FBasicOptions := TChromiumOptions.Create;

  FPageItems := TPageContainer.Create;
  FClosedPageURL := TStringList.Create;
end;

class procedure TCustomDcefBrowser.CreateDefaultRenderProcess;
begin
  if not Assigned(CefRenderProcessHandler) then
    CefRenderProcessHandler := TDefaultRenderProcessHandler.Create;
end;

procedure TCustomDcefBrowser.DevTools;
begin
  if ActivePage <> nil then
    ActivePage.DevTools;
end;

{$IFDEF DELPHI14_UP}

class procedure TCustomDcefBrowser.RegisterClasses(const aObjList
  : array of TClass);
var
  i, J, C: Integer;
  AFound: Boolean;
  ARender: TDefaultRenderProcessHandler;
begin
  CreateDefaultRenderProcess;
  ARender := CefRenderProcessHandler as TDefaultRenderProcessHandler;
  C := Length(ARender.FClasses);
  SetLength(ARender.FClasses, C + Length(aObjList));
  for i := 0 to High(aObjList) do
  begin
    AFound := False;
    for J := 0 to C - 1 do
    begin
      if ARender.FClasses[J] = aObjList[i] then
      begin
        AFound := True;
        Break;
      end;
    end;
    if not AFound then
    begin
      ARender.FClasses[C] := aObjList[i];
      Inc(C);
    end;
  end;
  SetLength(ARender.FClasses, C);
end;
{$ENDIF}

destructor TCustomDcefBrowser.Destroy;
begin
  FPageItems.Free;
  FPageControl.Free;
  FClosedPageURL.Free;
  FOptions.Free;
  FBasicFontOptions.Free;
  FBasicOptions.Free;
  UnhookWndProc;
  // Added by swish
  // if Not (csDesigning in ComponentState) then
  // CefQuitMessageLoop;
  // BccSafe 2015-08-12
  // 还原 理由见TimerProc内
  inherited;
end;

procedure TCustomDcefBrowser.doOnPageStateChange(const PageID: Integer;
  const Kind: TBrowserDataChangeKind; const value: string;
  const PageActived: Boolean);
begin
  if Assigned(FOnPageStateChange) and (PageID > -1) then
    FOnPageStateChange(PageID, Kind, value, PageActived);
end;

procedure TCustomDcefBrowser.doOnPluginCrashed(const PageIndex: Integer;
  const Browser: ICefBrowser; const pluginPath: ustring);
begin
  if Assigned(FOnPluginCrashed) and (PageIndex > -1) then
    FOnPluginCrashed(PageIndex, Browser, pluginPath);
end;

procedure TCustomDcefBrowser.DownloadFile(FileURL: string);
begin
  if ActivePage <> nil then
    ActivePage.FBasicBrowser.FBrowser.host.StartDownload(FileURL);
end;

procedure TCustomDcefBrowser.AddZoomLevel;
begin
  if ActivePage <> nil then
    ActivePage.AddZoomLevel;
end;

procedure TCustomDcefBrowser.ExecuteJavaScript(const code: string);
begin
  if ActivePage <> nil then
    ActivePage.ExecuteJavaScript(code);
end;

procedure TCustomDcefBrowser.FormWndProc(var AMsg: TMessage);
var
  ActivateMsg: TWMActivateApp;
  FormHandle: HWND;
begin
  if Not(csDestroying in ComponentState) then
  begin
    FormHandle := GetParentForm(Self).Handle;

    case AMsg.Msg of
      WM_NCACTIVATE:
        begin
          if (AMsg.wParam = 0) and (AMsg.lParam <> 0) then
          begin
            AMsg.wParam := 1;
          end;
        end;
      WM_ACTIVATEAPP:
        begin
          ActivateMsg := TWMActivateApp(AMsg);
          if Not ActivateMsg.Active then
          begin
            if GetForegroundWindow = FormHandle then
              PostMessage(FormHandle, WM_NCACTIVATE, 0, 0);
          end
          else if (FPageItems.Count > 0) and (ActivePage <> nil) then
          begin
            if ActivePage.FCreateByPopup and (FormHandle <> 0) then
            begin
              if GetForegroundWindow = ActivePage.Browser.host.WindowHandle then
              begin
                SetForegroundWindow(ActivePage.Browser.host.WindowHandle);
                SetWindowPos(FormHandle, HWND_TOP, 0, 0, 0, 0,
                  SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
              end;
            end
            else if GetForegroundWindow = FormHandle then
              ActivePage.FBasicBrowser.SetFocus;
          end;
        end;
    end;
  end;

  FLastWndProc(AMsg);
end;

procedure TCustomDcefBrowser.doOnDialogClosed(const PageIndex: Integer;
  const Browser: ICefBrowser);
begin
  if Assigned(FOnDialogClosed) and (PageIndex > -1) then
    FOnDialogClosed(PageIndex, Browser);
end;

procedure TCustomDcefBrowser.doOnDownloadUpdated(const PageIndex: Integer;
  const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin
  if Assigned(FOnDownloadUpdated) and (PageIndex > -1) then
    FOnDownloadUpdated(PageIndex, Browser, downloadItem, callback);
end;

{
  procedure TCustomDcefBrowser.doOnDownloadUpdated(const DcefItemIndex: Integer;
  const Kind: TBrowserDownloadUpdatedKind);
  begin
  if Assigned(FOnDownloadUpdated) and (DcefItemIndex > -1) then
  FOnDownloadUpdated(DcefItemIndex, Kind);
  end;
}

procedure TCustomDcefBrowser.doOnDragEnter(const PageIndex: Integer;
  const Browser: ICefBrowser; const dragData: ICefDragData;
  mask: TCefDragOperations; out Result: Boolean);
begin
  if Assigned(FOnDragEnter) and (PageIndex > -1) then
    FOnDragEnter(PageIndex, Browser, dragData, mask, Result);
end;

procedure TCustomDcefBrowser.doOnFileDialog(const PageIndex: Integer;
  const Browser: ICefBrowser; mode: TCefFileDialogMode;
  const title, defaultFileName: ustring; acceptTypes: TStrings;
  const callback: ICefFileDialogCallback; out Result: Boolean);
begin
  if Assigned(FOnFileDialog) and (PageIndex > -1) then
    FOnFileDialog(PageIndex, Browser, mode, title, defaultFileName, acceptTypes,
      callback, Result);
end;

procedure TCustomDcefBrowser.doOnPageAdd(const PageID: Integer;
  Const AddAtLast: Boolean);
begin
  if Assigned(FOnPageAdd) and (PageID > -1) then
    FOnPageAdd(PageID, AddAtLast);
end;

procedure TCustomDcefBrowser.doOnBeforeBrowse(const PageIndex: Integer;
  const Browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; isRedirect: Boolean; var Cancel: Boolean);
begin
  if Assigned(FOnBeforeBrowse) and (PageIndex > -1) then
    FOnBeforeBrowse(PageIndex, Browser, frame, request, isRedirect, Cancel);
end;

procedure TCustomDcefBrowser.doOnBeforeContextMenu(const PageIndex: Integer;
  const Browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  if Assigned(FOnBeforeContextMenu) and (PageIndex > -1) then
    FOnBeforeContextMenu(PageIndex, Browser, frame, params, model);
end;

procedure TCustomDcefBrowser.doOnBeforeDownload(const PageIndex: Integer;
  const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const suggestedName: ustring; const callback: ICefBeforeDownloadCallback;
  var CancelBuiltinPro: Boolean);
begin
  if Assigned(FOnBeforeDownload) then
    FOnBeforeDownload(PageIndex, Browser, downloadItem, suggestedName, callback,
      CancelBuiltinPro);
end;

procedure TCustomDcefBrowser.doOnBeforePluginLoad(const PageIndex: Integer;
  const Browser: ICefBrowser; const URL, policyUrl: ustring;
  const info: ICefWebPluginInfo; var CancelLoad: Boolean);
begin
  if Assigned(FOnBeforePluginLoad) and (PageIndex > -1) then
    FOnBeforePluginLoad(PageIndex, Browser, URL, policyUrl, info, CancelLoad);
end;

procedure TCustomDcefBrowser.doOnBeforeResourceLoad(const PageIndex: Integer;
  const Browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; var CancelLoad: Boolean);
begin
  if Assigned(FOnBeforeResourceLoad) and (PageIndex > -1) then
    FOnBeforeResourceLoad(PageIndex, Browser, frame, request, CancelLoad);
end;

procedure TCustomDcefBrowser.doOnBeforeUnloadDialog(const PageIndex: Integer;
  const Browser: ICefBrowser; const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnBeforeUnloadDialog) and (PageIndex > -1) then
    FOnBeforeUnloadDialog(PageIndex, Browser, messageText, isReload, callback,
      CancelEventBuiltIn);
end;

procedure TCustomDcefBrowser.doOnCancelGeolocationPermission(const PageIndex
  : Integer; const Browser: ICefBrowser; const requestingUrl: ustring;
  requestId: Integer);
begin
  if Assigned(FOnCancelGeolocationPermission) and (PageIndex > -1) then
    FOnCancelGeolocationPermission(PageIndex, Browser, requestingUrl,
      requestId);
end;

procedure TCustomDcefBrowser.doOnCertificateError(const PageIndex: Integer;
  certError: TCefErrorCode; const requestUrl: ustring;
  const callback: ICefAllowCertificateErrorCallback; out Result: Boolean);
begin
  if Assigned(FOnCertificateError) then
    FOnCertificateError(PageIndex, certError, requestUrl, callback, Result);
end;

procedure TCustomDcefBrowser.doOnConsoleMessage(const PageIndex: Integer;
  const Browser: ICefBrowser; const Message, source: ustring; line: Integer;
  var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnConsoleMessage) and (PageIndex > -1) then
    FOnConsoleMessage(PageIndex, Browser, message, source, line,
      CancelEventBuiltIn);
end;

procedure TCustomDcefBrowser.doOnContextMenuCommand(const PageIndex: Integer;
  const Browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: TCefEventFlags; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnContextMenuCommand) and (PageIndex > -1) then
    FOnContextMenuCommand(PageIndex, Browser, frame, params, commandId,
      eventFlags, CancelEventBuiltIn);
end;

procedure TCustomDcefBrowser.doOnContextMenuDismissed(const PageIndex: Integer;
  const Browser: ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnContextMenuDismissed) and (PageIndex > -1) then
    FOnContextMenuDismissed(PageIndex, Browser, frame);
end;

procedure TCustomDcefBrowser.doOnCursorChange(const PageIndex: Integer;
  const Browser: ICefBrowser; cursor: TCefCursorHandle;
  cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
begin
  if Assigned(FOnCursorChange) then
    FOnCursorChange(PageIndex, Browser, cursor, cursorType, customCursorInfo);
end;

procedure TCustomDcefBrowser.doOnPageChanged(Sender: TObject);
begin
  if FTabVisible then
    FActivePageID := PageIndexToID(FPageControl.TabIndex);

  if Assigned(FOnPageChanged) then
    FOnPageChanged(Sender);
end;

procedure TCustomDcefBrowser.doOnPageChanging(Sender: TObject;
  var Allow: Boolean);
begin
  if Assigned(FOnPageChanging) then
    FOnPageChanging(Sender, Allow);
end;

procedure TCustomDcefBrowser.doOnPageClose(const ClosePageIDArr
  : Array of Integer; Const ShowPageID: Integer);
begin
  if Assigned(FOnPageClose) then
    FOnPageClose(ClosePageIDArr, ShowPageID);
end;

procedure TCustomDcefBrowser.doOnPageLoadingStateChange(const PageID: Integer;
  const Browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if Assigned(FOnPageLoadingStateChange) and (PageID > -1) then
    FOnPageLoadingStateChange(PageID, Browser, isLoading, canGoBack,
      canGoForward);
end;

procedure TCustomDcefBrowser.doOnGetAuthCredentials(const PageIndex: Integer;
  const Browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean;
  const host: ustring; port: Integer; const realm, scheme: ustring;
  const callback: ICefAuthCallback; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnGetAuthCredentials) and (PageIndex > -1) then
    FOnGetAuthCredentials(PageIndex, Browser, frame, isProxy, host, port, realm,
      scheme, callback, CancelEventBuiltIn);
end;

procedure TCustomDcefBrowser.doOnGetResourceHandler(const PageIndex: Integer;
  const Browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; var ResourceHandler: ICefResourceHandler);
begin
  if Assigned(FOnGetResourceHandler) and (PageIndex > -1) then
    FOnGetResourceHandler(PageIndex, Browser, frame, request, ResourceHandler);
end;

procedure TCustomDcefBrowser.doOnGotFocus(const PageIndex: Integer;
  const Browser: ICefBrowser; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnGotFocus) and (PageIndex > -1) then
    FOnGotFocus(PageIndex, Browser, CancelEventBuiltIn);
end;

procedure TCustomDcefBrowser.doOnJsdialog(const PageIndex: Integer;
  const Browser: ICefBrowser; const originUrl, acceptLang: ustring;
  dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
  const callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnJsdialog) and (PageIndex > -1) then
    FOnJsdialog(PageIndex, Browser, originUrl, acceptLang, dialogType,
      messageText, defaultPromptText, callback, CancelEventBuiltIn);
end;

procedure TCustomDcefBrowser.doOnKeyEvent(const PageIndex: Integer;
  const Browser: ICefBrowser; const event: PCefKeyEvent;
  osEvent: TCefEventHandle; var Cancel: Boolean);
begin
  if Assigned(FOnKeyEvent) and (PageIndex > -1) then
    FOnKeyEvent(PageIndex, Browser, event, osEvent, Cancel);
end;

procedure TCustomDcefBrowser.doOnLoadEnd(const PageIndex: Integer;
  const Browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  if Assigned(FOnLoadEnd) and (PageIndex > -1) then
    FOnLoadEnd(PageIndex, Browser, frame, httpStatusCode);
end;

procedure TCustomDcefBrowser.doOnLoadError(const PageIndex: Integer;
  const Browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
begin
  if Assigned(FOnLoadError) and (PageIndex > -1) then
    FOnLoadError(PageIndex, Browser, frame, errorCode, errorText, failedUrl);
end;

procedure TCustomDcefBrowser.doOnLoadStart(const PageIndex: Integer;
  const Browser: ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnLoadStart) and (PageIndex > -1) then
    FOnLoadStart(PageIndex, Browser, frame);
end;

procedure TCustomDcefBrowser.doOnPreKeyEvent(const PageIndex: Integer;
  const Browser: ICefBrowser; const event: PCefKeyEvent;
  osEvent: TCefEventHandle; var isKeyboardShortcut, Cancel: Boolean);
begin
  if Assigned(FOnPreKeyEvent) and (PageIndex > -1) then
    FOnPreKeyEvent(PageIndex, Browser, event, osEvent,
      isKeyboardShortcut, Cancel);
end;

procedure TCustomDcefBrowser.doOnProtocolExecution(const PageIndex: Integer;
  const Browser: ICefBrowser; const URL: ustring;
  out allowOsExecution: Boolean);
begin
  if Assigned(FOnProtocolExecution) and (PageIndex > -1) then
    FOnProtocolExecution(PageIndex, Browser, URL, allowOsExecution);
end;

procedure TCustomDcefBrowser.doOnQuotaRequest(const PageIndex: Integer;
  const Browser: ICefBrowser; const originUrl: ustring; newSize: Int64;
  const callback: ICefQuotaCallback; out Result: Boolean);
begin
  if Assigned(FOnQuotaRequest) and (PageIndex > -1) then
    FOnQuotaRequest(PageIndex, Browser, originUrl, newSize, callback, Result);
end;

procedure TCustomDcefBrowser.doOnRequestGeolocationPermission(const PageIndex
  : Integer; const Browser: ICefBrowser; const requestingUrl: ustring;
  requestId: Integer; const callback: ICefGeolocationCallback;
  out Result: Boolean);
begin
  if Assigned(FOnRequestGeolocationPermission) and (PageIndex > -1) then
    FOnRequestGeolocationPermission(PageIndex, Browser, requestingUrl,
      requestId, callback, Result);
end;

procedure TCustomDcefBrowser.doOnResourceRedirect(const PageIndex: Integer;
  const Browser: ICefBrowser; const frame: ICefFrame; const oldUrl: ustring;
  var newUrl: ustring);
begin
  if Assigned(FOnResourceRedirect) and (PageIndex > -1) then
    FOnResourceRedirect(PageIndex, Browser, frame, oldUrl, newUrl);
end;

procedure TCustomDcefBrowser.doOnSetFocus(const PageIndex: Integer;
  const Browser: ICefBrowser; source: TCefFocusSource;
  var CancelFocus: Boolean);
begin
  if Assigned(FOnSetFocus) and (PageIndex > -1) then
    FOnSetFocus(PageIndex, Browser, source, CancelFocus);
end;

procedure TCustomDcefBrowser.doOnStartDragging(const PageIndex: Integer;
  const Browser: ICefBrowser; const dragData: ICefDragData;
  allowedOps: TCefDragOperations; x, y: Integer; out Result: Boolean);
begin
  if Assigned(FOnStartDragging) and (PageIndex > -1) then
    FOnStartDragging(PageIndex, Browser, dragData, allowedOps, x, y, Result);
end;

procedure TCustomDcefBrowser.doOnTakeFocus(const PageIndex: Integer;
  const Browser: ICefBrowser; next: Boolean);
begin
  if Assigned(FOnTakeFocus) and (PageIndex > -1) then
    FOnTakeFocus(PageIndex, Browser, next);
end;

procedure TCustomDcefBrowser.doOnUpdateDragCursor(const PageIndex: Integer;
  const Browser: ICefBrowser; operation: TCefDragOperation);
begin
  if Assigned(FOnUpdateDragCursor) and (PageIndex > -1) then
    FOnUpdateDragCursor(PageIndex, Browser, operation);
end;

function TCustomDcefBrowser.GetActivePageCanGoBack: Boolean;
var
  APage: TBrowserPage;
begin
  APage := ActivePage;
  Result := (APage <> nil) and (APage.canGoBack);
end;

function TCustomDcefBrowser.GetActivePageCanGoForward: Boolean;
var
  APage: TBrowserPage;
begin
  APage := ActivePage;
  Result := (APage <> nil) and (APage.canGoForward);
end;

function TCustomDcefBrowser.GetActivePageIndex: Integer;
begin
  Result := PageIDToIndex(FActivePageID);
end;

function TCustomDcefBrowser.GetActivePageIsLoading: Boolean;
var
  APage: TBrowserPage;
begin
  APage := ActivePage;
  Result := (APage <> nil) and (APage.isLoading);
end;

function TCustomDcefBrowser.GetActivePageItem: TBrowserPage;
begin
  Result := Pages[ActivePageIndex];
end;

function TCustomDcefBrowser.GetClosePageCount: Integer;
begin
  Result := 0;
  if Assigned(FClosedPageURL) then
    Result := FClosedPageURL.Count;
end;

function TCustomDcefBrowser.GetNewPageID: Integer;
begin
  Inc(FAddUpPageID);
  Result := FAddUpPageID;
end;

function TCustomDcefBrowser.GetPageByID(ID: Integer): TBrowserPage;
begin
  Result := FPageItems.GetPageByID(ID);
end;

function TCustomDcefBrowser.GetPageByIndex(Index: Integer): TBrowserPage;
begin
  Result := FPageItems.Items[Index];
end;

function TCustomDcefBrowser.GetPageCount: Integer;
begin
  Result := 0;
  if Assigned(FPageItems) then
    Result := FPageItems.Count;
end;

{$IFDEF DELPHI14_UP}

function TCustomDcefBrowser.GetSource(var SourceText: string;
  Const TimeOut: Integer = 1000): Boolean;
begin
  Result := False;
  if ActivePage <> nil then
    Result := ActivePage.GetSource(SourceText, TimeOut);
end;

{$ENDIF}

procedure TCustomDcefBrowser.GetSourceInNewPage;
begin
  if ActivePage <> nil then
    AddPage('view-source:' + ActivePage.URL);
end;

{$IFDEF DELPHI14_UP}

function TCustomDcefBrowser.GetText(var aText: string;
  const TimeOut: Integer): Boolean;
begin
  Result := False;
  if ActivePage <> nil then
    Result := ActivePage.GetText(aText, TimeOut);
end;

{$ENDIF}

function TCustomDcefBrowser.GetZoomLevel: string;
begin
  Result := '';
  if ActivePage <> nil then
    Result := ActivePage.ZoomLevel;
end;

procedure TCustomDcefBrowser.GoBack;
begin
  if ActivePage <> nil then
    ActivePage.GoBack;
end;

procedure TCustomDcefBrowser.GoForward;
begin
  if ActivePage <> nil then
    ActivePage.GoForward
end;

procedure TCustomDcefBrowser.GoHome;
begin
  if ActivePage <> nil then
    Load(FDefaultUrl);
end;

procedure TCustomDcefBrowser.HookWndProc;
var
  AForm: TCustomForm;
begin
  AForm := GetParentForm(Self);
  if AForm <> FParentForm then
    UnhookWndProc;
  FParentForm := AForm;
  FLastWndProc := FParentForm.WindowProc;
  FParentForm.WindowProc := FormWndProc;
end;

procedure TCustomDcefBrowser.Load(const URL: string);
begin
  if FPageItems.Count > 0 then
    ActivePage.Load(URL)
  else
    AddPage(URL, True);
end;

{ TBrowserPageItem }

constructor TBasicBrowser.Create(AOwner: TComponent;
  ParentBrowserPage: TBrowserPage; DcefBrowser: TCustomDcefBrowser;
  DefaultEncoding: ustring; PID: Integer; CreateByPopup: Boolean;
  Const DefaultURL: ustring = SBlankPageUrl; FShow: Boolean = True);
var
  FCreateByPopup: Boolean;
begin
  inherited Create(AOwner);
  FParentBrowserPage := ParentBrowserPage;
  FDcefBrowser := DcefBrowser;
  FPageID := PID;
  FBrowserId := -1;
  FBrowser := nil;

  FDefaultUrl := DefaultURL;
  FLastTitle := SLoadingText;
  FLastAddress := DefaultURL;
  FDefaultEncoding := DefaultEncoding;
  FUserStyleSheetLocation := '';
  FCreateByPopup := CreateByPopup;

  FIsLoading := True;
  FCanGoForward := False;
  FCanGoBack := False;

  FBasicDcefBrowserEvents := TBasicDcefBrowserEvents.Create;
  if PID <> -1 then
    LoadBrowserEvents;

  if not(csDesigning in ComponentState) and Not FCreateByPopup then
    FClientHandler := TVCLDcefClientHandler.Create
      (FBasicDcefBrowserEvents, False);
  UpdatePageInfo(True);
end;

procedure TBasicBrowser.CreateBrowser;
var
  info: TCefWindowInfo;
  settings: TCefBrowserSettings;
  rect: TRect;
begin
  FillChar(info, SizeOf(info), 0);
  rect := ClientRect;
  info.Style := WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or
    WS_TABSTOP;
  info.parent_window := Handle;
  info.x := left;
  info.y := top;
  info.Width := rect.right - rect.left;
  info.Height := rect.bottom - rect.top;
  FillChar(settings, SizeOf(TCefBrowserSettings), 0);
  settings.size := SizeOf(TCefBrowserSettings);
  GetSettings(settings);
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  CefBrowserHostCreate(@info, FClientHandler, FDefaultUrl, @settings, nil);
{$ELSE}
  CefLoadLibDefault;
  FBrowser := CefBrowserHostCreateSync(@info, FClientHandler, FDefaultUrl,
    @settings, nil);
  FBrowserId := FBrowser.Identifier;
{$ENDIF}
end;

destructor TBasicBrowser.Destroy;
begin
  if FBrowserId > -1 then
  begin
    if FBrowser <> nil then
    begin
      FBrowser.StopLoad;
      FBrowser.host.CloseBrowser(False);
    end;
    FBasicDcefBrowserEvents := nil;
    FBasicDcefBrowserEvents.Free;
    if FClientHandler <> nil then
      (FClientHandler as ICefClientHandler).Disconnect;
    FClientHandler := nil;
    FBrowser := nil;
  end;
  inherited;
end;

function TBasicBrowser.GetActived: Boolean;
begin
  Result := TCustomDcefBrowser(FDcefBrowser).ActivePageID = PageID;
end;

function TBasicBrowser.GetPageIndex: Integer;
begin
  Result := -1;
  if (not(csDestroying in ComponentState)) and Assigned(FParentBrowserPage) then
    Result := FParentBrowserPage.PageIndex;
end;

procedure TBasicBrowser.GetSettings(var settings: TCefBrowserSettings);
begin
  Assert(settings.size >= SizeOf(settings));
  with TCustomDcefBrowser(FDcefBrowser) do
  begin
    settings.windowless_frame_rate := FBasicOptions.WindowlessFrameRate;

    settings.standard_font_family :=
      CefString(FBasicFontOptions.StandardFontFamily);
    settings.fixed_font_family := CefString(FBasicFontOptions.FixedFontFamily);
    settings.serif_font_family := CefString(FBasicFontOptions.SerifFontFamily);
    settings.sans_serif_font_family :=
      CefString(FBasicFontOptions.SansSerifFontFamily);
    settings.cursive_font_family :=
      CefString(FBasicFontOptions.CursiveFontFamily);
    settings.fantasy_font_family :=
      CefString(FBasicFontOptions.FantasyFontFamily);
    settings.default_font_size := FBasicFontOptions.DefaultFontSize;
    settings.default_fixed_font_size := FBasicFontOptions.DefaultFixedFontSize;
    settings.minimum_font_size := FBasicFontOptions.MinimumFontSize;
    settings.minimum_logical_font_size :=
      FBasicFontOptions.MinimumLogicalFontSize;
    settings.remote_fonts := FBasicFontOptions.RemoteFonts;
    settings.default_encoding := CefString(DefaultEncoding);

    settings.javascript := FBasicOptions.javascript;
    settings.javascript_open_windows := FBasicOptions.JavascriptOpenWindows;
    settings.javascript_close_windows := FBasicOptions.JavascriptCloseWindows;
    settings.javascript_access_clipboard :=
      FBasicOptions.JavascriptAccessClipboard;
    settings.javascript_dom_paste := FBasicOptions.JavascriptDomPaste;
    settings.caret_browsing := FBasicOptions.CaretBrowsing;
    settings.java := FBasicOptions.java;
    settings.plugins := FBasicOptions.plugins;
    settings.universal_access_from_file_urls :=
      FBasicOptions.UniversalAccessFromFileUrls;
    settings.file_access_from_file_urls := FBasicOptions.FileAccessFromFileUrls;
    settings.web_security := FBasicOptions.WebSecurity;
    settings.image_loading := FBasicOptions.ImageLoading;
    settings.image_shrink_standalone_to_fit :=
      FBasicOptions.ImageShrinkStandaloneToFit;
    settings.text_area_resize := FBasicOptions.TextAreaResize;
    settings.tab_to_links := FBasicOptions.TabToLinks;
    settings.local_storage := FBasicOptions.LocalStorage;
    settings.databases := FBasicOptions.databases;
    settings.application_cache := FBasicOptions.ApplicationCache;
    settings.webgl := FBasicOptions.webgl;
    settings.background_color := FBasicOptions.BackgroundColor;
  end;
end;

procedure TBasicBrowser.Resize;
var
  brws: ICefBrowser;
  rect: TRect;
  hdwp: THandle;
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    brws := FBrowser;
    if (brws <> nil) and (brws.host.WindowHandle <> INVALID_HANDLE_VALUE) then
    begin
      rect := GetClientRect;
      hdwp := BeginDeferWindowPos(1);
      try
        hdwp := DeferWindowPos(hdwp, brws.host.WindowHandle, 0, rect.left,
          rect.top, rect.right - rect.left, rect.bottom - rect.top,
          SWP_NOZORDER);
      finally
        EndDeferWindowPos(hdwp);
      end;
    end;
  end;
end;

procedure TBasicBrowser.UpdatePageInfo(FCheckActived: Boolean);
begin
  if (Not FCheckActived) or Actived then
  begin
    TCustomDcefBrowser(FDcefBrowser).doOnPageStateChange(PageID,
      BrowserDataChange_Title, FLastTitle, Actived);
    TCustomDcefBrowser(FDcefBrowser).doOnPageStateChange(PageID,
      BrowserDataChange_Address, FLastAddress, Actived);
  end;
end;

procedure TBasicBrowser.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        if (FBrowser <> nil) and (FBrowser.host.WindowHandle <> 0) then
          PostMessage(FBrowser.host.WindowHandle, WM_SETFOCUS,
            Message.wParam, 0);
        inherited WndProc(Message);
      end;
    WM_ERASEBKGND:
      if (csDesigning in ComponentState) or (FBrowser = nil) then
        inherited WndProc(Message);
    CM_WANTSPECIALKEY:
      if not(TWMKey(Message).CharCode in [VK_LEFT .. VK_DOWN]) then
        Message.Result := 1
      else
        inherited WndProc(Message);
    WM_GETDLGCODE:
      Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  else
    inherited WndProc(Message);
  end;
end;

procedure TBasicBrowser.LoadBrowserEvents;
begin
  FBasicDcefBrowserEvents.OnBeforePopup := BrowserBeforePopup;
  FBasicDcefBrowserEvents.OnAfterCreated := BrowserAfterCreated;
  FBasicDcefBrowserEvents.OnClose := BrowserClose;
  FBasicDcefBrowserEvents.OnStatusMessage := BrowserStatusMessage;
  FBasicDcefBrowserEvents.OnAddressChange := BrowserAddressChange;
  FBasicDcefBrowserEvents.OnTitleChange := BrowserTitleChange;
  FBasicDcefBrowserEvents.OnLoadingStateChange := BrowserLoadingStateChange;
  FBasicDcefBrowserEvents.OnLoadStart := BrowserLoadStart;
  FBasicDcefBrowserEvents.OnLoadEnd := BrowserLoadEnd;
  FBasicDcefBrowserEvents.OnLoadError := BrowserLoadError;
  FBasicDcefBrowserEvents.OnBeforeBrowse := BrowserBeforeBrowse;
  FBasicDcefBrowserEvents.OnPreKeyEvent := BrowserPreKeyEvent;
  FBasicDcefBrowserEvents.OnKeyEvent := BrowserKeyEvent;
  FBasicDcefBrowserEvents.OnBeforeResourceLoad := BrowserBeforeResourceLoad;
  FBasicDcefBrowserEvents.OnGetResourceHandler := BrowserGetResourceHandler;
  FBasicDcefBrowserEvents.OnResourceRedirect := BrowserResourceRedirect;
  FBasicDcefBrowserEvents.OnGotFocus := BrowserGotFocus;
  FBasicDcefBrowserEvents.OnSetFocus := BrowserSetFocus;
  FBasicDcefBrowserEvents.OnTakeFocus := BrowserTakeFocus;
  FBasicDcefBrowserEvents.OnBeforeContextMenu := BrowserBeforeContextMenu;
  FBasicDcefBrowserEvents.OnContextMenuCommand := BrowserContextMenuCommand;
  FBasicDcefBrowserEvents.OnContextMenuDismissed := BrowserContextMenuDismissed;
  FBasicDcefBrowserEvents.OnJsdialog := BrowserJsdialog;
  FBasicDcefBrowserEvents.OnBeforeUnloadDialog := BrowserBeforeUnloadDialog;
  FBasicDcefBrowserEvents.OnDialogClosed := BrowserDialogClosed;
  FBasicDcefBrowserEvents.OnPluginCrashed := BrowserPluginCrashed;
  FBasicDcefBrowserEvents.OnBeforePluginLoad := BrowserBeforePluginLoad;
  FBasicDcefBrowserEvents.OnBeforeDownload := BrowserBeforeDownload;
  FBasicDcefBrowserEvents.OnDownloadUpdated := BrowserDownloadUpdated;
  FBasicDcefBrowserEvents.OnGetAuthCredentials := BrowserGetAuthCredentials;
  FBasicDcefBrowserEvents.OnConsoleMessage := BrowserConsoleMessage;
  FBasicDcefBrowserEvents.OnProtocolExecution := BrowserProtocolExecution;
  FBasicDcefBrowserEvents.OnFileDialog := BrowserFileDialog;
  FBasicDcefBrowserEvents.OnRequestGeolocationPermission :=
    BrowserRequestGeolocationPermission;
  FBasicDcefBrowserEvents.OnCancelGeolocationPermission :=
    BrowserCancelGeolocationPermission;
  FBasicDcefBrowserEvents.OnQuotaRequest := BrowserQuotaRequest;
  FBasicDcefBrowserEvents.OnDragEnter := BrowserDragEnter;
  FBasicDcefBrowserEvents.OnCertificateError := BrowserCertificateError;
  FBasicDcefBrowserEvents.OnStartDragging := BrowserStartDragging;
  FBasicDcefBrowserEvents.OnUpdateDragCursor := BrowserUpdateDragCursor;
  FBasicDcefBrowserEvents.OnCursorChange := BrowserCursorChange;
end;

procedure TBasicBrowser.BrowserAddressChange(const Browser: ICefBrowser;
  const frame: ICefFrame; const URL: ustring);
begin
  if Not(csDestroying in ComponentState) then
  begin
    TCustomDcefBrowser(FDcefBrowser).doOnPageStateChange(PageID,
      BrowserDataChange_Address, URL, Actived);
    FLastAddress := URL;
  end;
end;

procedure TBasicBrowser.BrowserAfterCreated(const Browser: ICefBrowser);
var
  WinHandle: HWND;
  BrowserPage: TBrowserPage;
  MyDcefBrowser: TCustomDcefBrowser;
  // WindowStyle: Integer;
  // MyRect: TRect;
begin
  if csDestroying in ComponentState then
    Exit;

  MyDcefBrowser := TCustomDcefBrowser(FDcefBrowser);
  if Browser.IsPopup and (Not MyDcefBrowser.Options.PopupNewWin) then
  begin
    BrowserPage := TBrowserPage(FParentBrowserPage);
    if (BrowserPage.FBasicBrowser.FBrowserId = -1) and BrowserPage.FCreateByPopup
    then
    begin
      WinHandle := Browser.host.WindowHandle;
      BrowserPage.FBasicBrowser.FBrowserId := Browser.Identifier;
      BrowserPage.FBasicBrowser.FBrowser := Browser;

      Windows.SetParent(WinHandle, BrowserPage.FBrowserPanel.Handle);
      { SetWindowPos(WinHandle, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
        WindowStyle := GetWindowLong(WinHandle, GWL_STYLE);
        WindowStyle := WindowStyle and (not WS_CAPTION) and (not WS_BORDER) and
        (not WS_THICKFRAME);
        SetWindowLong(WinHandle, GWL_STYLE, WindowStyle);

        MyRect := BrowserPage.FBrowserPanel.ClientRect;
        MoveWindow(WinHandle, 0, 0, MyRect.Width, MyRect.Height + 1, True); }

      MyDcefBrowser.doOnPageAdd(PageID, True);
      BrowserPage.Show;
      BrowserPage.SetFocus;
    end;
  end;
end;

procedure TBasicBrowser.BrowserBeforeBrowse(const Browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean;
  out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnBeforeBrowse(PageIndex, Browser, frame,
      request, isRedirect, Result);
end;

procedure TBasicBrowser.BrowserBeforeContextMenu(const Browser: ICefBrowser;
  const frame: ICefFrame; const params: ICefContextMenuParams;
  const model: ICefMenuModel);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnBeforeContextMenu(PageIndex, Browser,
      frame, params, model);
end;

procedure TBasicBrowser.BrowserBeforeDownload(const Browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
var
  MyOptions: TDcefBrowserOptions;
  // DownLoadItemIndex: Integer;
  SaveFullPath: string;
  FCancelBuiltInPro: Boolean;

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
  if csDestroying in ComponentState then
    Exit;
  { if Not browser.HasDocument then
    TBrowserPage(FParentBrowserPage).Close; }
  // DownloadManager := TCustomDcefBrowser(FDcefBrowser).DownloadManager;
  // DownLoadItemIndex := DownloadManager.ItemsIDToIndex(downloadItem.ID);
  // if DownLoadItemIndex <> -1 then
  // begin
  // DownloadManager.Items[DownLoadItemIndex].UpdataFileName(SaveFullPath);
  //
  // end; // else

  FCancelBuiltInPro := True;
  TCustomDcefBrowser(FDcefBrowser).doOnBeforeDownload(PageIndex, Browser,
    downloadItem, suggestedName, callback, FCancelBuiltInPro);
  if FCancelBuiltInPro then
  begin
    MyOptions := TCustomDcefBrowser(FDcefBrowser).Options;
    if Not DirectoryExists(MyOptions.DownLoadPath) then
      CreateDir(MyOptions.DownLoadPath);
    SaveFullPath := DealExistsFile(MyOptions.DownLoadPath + suggestedName);

    callback.Cont(SaveFullPath, Not MyOptions.AutoDown);
  end;
end;

procedure TBasicBrowser.BrowserBeforePluginLoad(const Browser: ICefBrowser;
  const URL, policyUrl: ustring; const info: ICefWebPluginInfo;
  out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnBeforePluginLoad(PageIndex, Browser,
      URL, policyUrl, info, Result);
end;

procedure TBasicBrowser.BrowserBeforePopup(const Browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var noJavascriptAccess: Boolean; out Result: Boolean);
var
  Point: TPoint;
  MyBrowserPage: TBrowserPage;
  NewBrowserPage: TBrowserPage;
  MyDcefBrowser: TCustomDcefBrowser;
begin
  if csDestroying in ComponentState then
  begin
    Result := False;
    Exit;
  end;

  MyBrowserPage := TBrowserPage(FParentBrowserPage);
  MyDcefBrowser := TCustomDcefBrowser(FDcefBrowser);

  if (Browser.Identifier = FBrowserId) and
    (Not SameText(targetUrl, SBlankPageUrl)) and
    (Not MyDcefBrowser.Options.PopupNewWin) then
  begin
    NewBrowserPage := TBrowserPage.Create(MyBrowserPage.FPageControl,
      MyBrowserPage.FParentItems, FDcefBrowser, FDefaultEncoding,
      MyDcefBrowser.GetNewPageID, True, targetUrl, False);
    NewBrowserPage.FBasicBrowser.GetSettings(settings);
    MyBrowserPage.FParentItems.Add(NewBrowserPage);

    client := TVCLDcefClientHandler.Create
      (NewBrowserPage.FBasicBrowser.FBasicDcefBrowserEvents, False);

    Point.x := MyBrowserPage.FBrowserPanel.left;
    Point.y := MyBrowserPage.FBrowserPanel.top;
    Point := MyBrowserPage.FBrowserPanel.ClientToScreen(Point);
    with windowInfo do
    begin
      parent_window := MyBrowserPage.FBrowserPanel.Handle;
      x := Point.x;
      y := Point.y;
      Width := MyBrowserPage.FBrowserPanel.Width;
      Height := MyBrowserPage.FBrowserPanel.Height;
      window_name := CefString('New BrowserWindow');
      Style := WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or
        WS_TABSTOP;
      { Style := WS_VISIBLE // or WS_POPUP;
        ex_style := ex_style and (not WS_CAPTION) and (not WS_BORDER) and
        (not WS_THICKFRAME); // WS_EX_TOPMOST or WS_EX_NOACTIVATE;
        ex_style := WS_EX_NOACTIVATE; }
    end;
  end;
  Result := False;
end;

procedure TBasicBrowser.BrowserBeforeResourceLoad(const Browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest; out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnBeforeResourceLoad(PageIndex, Browser,
      frame, request, Result);
end;

procedure TBasicBrowser.BrowserBeforeUnloadDialog(const Browser: ICefBrowser;
  const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback; out Result: Boolean);
var
  CancelEventBuiltIn, IsContinue: Boolean;
begin
  if csDestroying in ComponentState then
    Exit;

  CancelEventBuiltIn := False;
  TCustomDcefBrowser(FDcefBrowser).doOnBeforeUnloadDialog(PageIndex, Browser,
    messageText, isReload, callback, CancelEventBuiltIn);

  if Not CancelEventBuiltIn then
  begin
{$IFDEF DELPHI14_UP}
    TThread.Synchronize(nil,
      procedure
      begin
{$ENDIF}
        IsContinue := MessageBox(0,
          PChar(messageText + #13#10 + SUnloadDialogText),
          PChar(SUnloadDialogTitle), MB_OKCANCEL) = idOk;
{$IFDEF DELPHI14_UP}
      end);
{$ENDIF}
    callback.Cont(IsContinue, '');
  end;
  Result := True;
end;

procedure TBasicBrowser.BrowserCancelGeolocationPermission
  (const Browser: ICefBrowser; const requestingUrl: ustring;
requestId: Integer);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnCancelGeolocationPermission(PageIndex,
      Browser, requestingUrl, requestId);
end;

procedure TBasicBrowser.BrowserCertificateError(certError: TCefErrorCode;
const requestUrl: ustring; const callback: ICefAllowCertificateErrorCallback;
out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnCertificateError(PageIndex, certError,
      requestUrl, callback, Result);
end;

procedure TBasicBrowser.BrowserClose(const Browser: ICefBrowser;
out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
  begin
    TBrowserPage(FParentBrowserPage).Close;
    Result := True;
  end;
end;

procedure TBasicBrowser.BrowserConsoleMessage(const Browser: ICefBrowser;
const Message, source: ustring; line: Integer; out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnConsoleMessage(PageIndex, Browser,
      message, source, line, Result);
end;

procedure TBasicBrowser.BrowserContextMenuCommand(const Browser: ICefBrowser;
const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer;
eventFlags: TCefEventFlags; out Result: Boolean);
var
  CancelEventBuiltIn: Boolean;
begin
  CancelEventBuiltIn := False;
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnContextMenuCommand(PageIndex, Browser,
      frame, params, commandId, eventFlags, CancelEventBuiltIn);
  Result := CancelEventBuiltIn;
end;

procedure TBasicBrowser.BrowserContextMenuDismissed(const Browser: ICefBrowser;
const frame: ICefFrame);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnContextMenuDismissed(PageIndex,
      Browser, frame);
end;

procedure TBasicBrowser.BrowserCursorChange(const Browser: ICefBrowser;
cursor: TCefCursorHandle; cursorType: TCefCursorType;
const customCursorInfo: PCefCursorInfo);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnCursorChange(PageIndex, Browser,
      cursor, cursorType, customCursorInfo);
end;

procedure TBasicBrowser.BrowserDialogClosed(const Browser: ICefBrowser);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnDialogClosed(PageIndex, Browser);
end;

procedure TBasicBrowser.BrowserDownloadUpdated(const Browser: ICefBrowser;
const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback);
{ var
  MyDcefBrowser: TCustomDcefBrowser;
  DownloadManager: TDcefBrowserDownloadManager;
  DcefDownloadItem: TDcefDownloadItem;
  DownLoadItemIndex: Integer;
  DownloadComplete: Boolean; }
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnDownloadUpdated(PageIndex, Browser,
      downloadItem, callback);
  { if downloadItem.IsValid then
    begin
    MyDcefBrowser := TCustomDcefBrowser(FDcefBrowser);
    DownloadManager := MyDcefBrowser.DownloadManager;

    DownLoadItemIndex := DownloadManager.ItemsIDToIndex(downloadItem.ID);
    if DownLoadItemIndex = -1 then
    DownloadManager.AddItem(downloadItem, MyDcefBrowser.OnDownloadUpdated)
    else
    begin
    DcefDownloadItem := DownloadManager.Items[DownLoadItemIndex];
    DcefDownloadItem.UpdateInfo(downloadItem, callback, DownloadComplete);

    // if TBrowserPage(FParentBrowserPage).Hided and DownloadComplete then
    //  begin
    //  FBrowser.host.ParentWindowWillClose;
    //  FBrowser.host.CloseBrowser(True);
    //  end;
    end;
    end; }
end;

procedure TBasicBrowser.BrowserDragEnter(const Browser: ICefBrowser;
const dragData: ICefDragData; mask: TCefDragOperations; out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnDragEnter(PageIndex, Browser, dragData,
      mask, Result);
end;

procedure TBasicBrowser.BrowserFileDialog(const Browser: ICefBrowser;
mode: TCefFileDialogMode; const title, defaultFileName: ustring;
acceptTypes: TStrings; const callback: ICefFileDialogCallback;
out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnFileDialog(PageIndex, Browser, mode,
      title, defaultFileName, acceptTypes, callback, Result);
end;

procedure TBasicBrowser.BrowserGetAuthCredentials(const Browser: ICefBrowser;
const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer;
const realm, scheme: ustring; const callback: ICefAuthCallback;
out Result: Boolean);
var
  UserName, Password: string;
  TempBool, CancelEventBuiltIn: Boolean;
begin
  if csDestroying in ComponentState then
    Exit;

  CancelEventBuiltIn := False;
  TCustomDcefBrowser(FDcefBrowser).doOnGetAuthCredentials(PageIndex, Browser,
    frame, isProxy, host, port, realm, scheme, callback, CancelEventBuiltIn);

  if CancelEventBuiltIn then
    Result := True
  else
  begin
{$IFDEF DELPHI14_UP}
    TThread.Synchronize(nil,
      procedure
      begin
{$ENDIF}
        with TPasswordDialogForm.Create do
          try
            if ShowModal = mrOk then
            begin
              UserName := FormUserName;
              Password := FormPassword;
              TempBool := True;
            end
            else
              TempBool := False;
          finally
            Free;
          end;
{$IFDEF DELPHI14_UP}
      end);
{$ENDIF}
    Result := TempBool;
    if TempBool = True then
      callback.Cont(UserName, Password);
  end;
end;

procedure TBasicBrowser.BrowserGetResourceHandler(const Browser: ICefBrowser;
const frame: ICefFrame; const request: ICefRequest;
out Result: ICefResourceHandler);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnGetResourceHandler(PageIndex, Browser,
      frame, request, Result);
end;

procedure TBasicBrowser.BrowserGotFocus(const Browser: ICefBrowser);
var
  CancelEventBuiltIn: Boolean;
begin
  if Not(csDestroying in ComponentState) then
  begin
    TCustomDcefBrowser(FDcefBrowser).doOnGotFocus(PageIndex, Browser,
      CancelEventBuiltIn);

    if Not CancelEventBuiltIn then
      TBrowserPage(FParentBrowserPage).BringSearchBarToFront;
  end;
end;

procedure TBasicBrowser.BrowserJsdialog(const Browser: ICefBrowser;
const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
const messageText, defaultPromptText: ustring; callback: ICefJsDialogCallback;
out suppressMessage, Result: Boolean);
var
  CancelEventBuiltIn, IsContinue: Boolean;
  UserInput: string;
begin
  if csDestroying in ComponentState then
    Exit;

  CancelEventBuiltIn := False;
  IsContinue := True;
  TCustomDcefBrowser(FDcefBrowser).doOnJsdialog(PageIndex, Browser, originUrl,
    acceptLang, dialogType, messageText, defaultPromptText, callback,
    CancelEventBuiltIn);

  if Not CancelEventBuiltIn then
  begin // 这里不能使用MessageBox，得使用模态窗口
    case dialogType of
      JSDIALOGTYPE_ALERT:
{$IFDEF DELPHI14_UP}
        TThread.Synchronize(nil,
          procedure
          begin
{$ENDIF}
            ShowMessage(messageText);
{$IFDEF DELPHI14_UP}
          end);
{$ENDIF}
      JSDIALOGTYPE_CONFIRM:
        begin
{$IFDEF DELPHI14_UP}
          TThread.Synchronize(nil,
            procedure
            begin
{$ENDIF}
              MyConfirm(originUrl + SDialogTitleSuffix, messageText,
                IsContinue);
              callback.Cont(IsContinue, '');
{$IFDEF DELPHI14_UP}
            end);
{$ENDIF}
        end;
      JSDIALOGTYPE_PROMPT:
        begin
{$IFDEF DELPHI14_UP}
          TThread.Synchronize(nil,
            procedure
            begin
{$ENDIF}
              UserInput := defaultPromptText;
              IsContinue := InputQuery(PChar(originUrl + SDialogTitleSuffix),
                PChar(messageText), UserInput);
              callback.Cont(IsContinue, UserInput);
{$IFDEF DELPHI14_UP}
            end);
{$ENDIF}
        end;
    end;
  end;

  case dialogType of
    JSDIALOGTYPE_ALERT:
      begin
        suppressMessage := True;
        Result := False;
      end;
    JSDIALOGTYPE_CONFIRM:
      begin
        suppressMessage := False;
        Result := True;
      end;
    JSDIALOGTYPE_PROMPT:
      begin
        suppressMessage := False;
        Result := True;
      end;
  end;
end;

procedure TBasicBrowser.BrowserKeyEvent(const Browser: ICefBrowser;
const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnKeyEvent(PageIndex, Browser, event,
      osEvent, Result);
end;

procedure TBasicBrowser.BrowserLoadEnd(const Browser: ICefBrowser;
const frame: ICefFrame; httpStatusCode: Integer);
begin
  if Not(csDestroying in ComponentState) then
  begin
    if SameText(FLastTitle, '') then
      FLastTitle := SNoTitleText;
    TCustomDcefBrowser(FDcefBrowser).doOnLoadEnd(PageIndex, Browser, frame,
      httpStatusCode);
  end;
end;

procedure TBasicBrowser.BrowserLoadError(const Browser: ICefBrowser;
const frame: ICefFrame; errorCode: Integer;
const errorText, failedUrl: ustring);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnLoadError(PageIndex, Browser, frame,
      errorCode, errorText, failedUrl);
end;

procedure TBasicBrowser.BrowserLoadingStateChange(const Browser: ICefBrowser;
isLoading, canGoBack, canGoForward: Boolean);
begin
  if (Not(csDestroying in ComponentState)) and (Browser.Identifier = FBrowserId)
  then
  begin
    FIsLoading := isLoading;
    FCanGoForward := canGoForward;
    FCanGoBack := canGoBack;
    TCustomDcefBrowser(FDcefBrowser).doOnPageLoadingStateChange(PageID, Browser,
      isLoading, canGoBack, canGoForward);
  end;
end;

procedure TBasicBrowser.BrowserLoadStart(const Browser: ICefBrowser;
const frame: ICefFrame);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnLoadStart(PageIndex, Browser, frame);
end;

procedure TBasicBrowser.BrowserTakeFocus(const Browser: ICefBrowser;
next: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnTakeFocus(PageIndex, Browser, next);
end;

procedure TBasicBrowser.BrowserTitleChange(const Browser: ICefBrowser;
const title: ustring);
begin
  if Not(csDestroying in ComponentState) then
  begin
    TCustomDcefBrowser(FDcefBrowser).doOnPageStateChange(PageID,
      BrowserDataChange_Title, title, Actived);
    FLastTitle := title;
    TBrowserPage(FParentBrowserPage).title := title;
  end;
end;

procedure TBasicBrowser.BrowserUpdateDragCursor(const Browser: ICefBrowser;
operation: TCefDragOperation);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnUpdateDragCursor(PageIndex, Browser,
      operation);
end;

procedure TBasicBrowser.BrowserPluginCrashed(const Browser: ICefBrowser;
const pluginPath: ustring);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnPluginCrashed(PageIndex, Browser,
      pluginPath);
end;

procedure TBasicBrowser.BrowserPreKeyEvent(const Browser: ICefBrowser;
const event: PCefKeyEvent; osEvent: TCefEventHandle;
out isKeyboardShortcut, Result: Boolean);
begin
  if csDestroying in ComponentState then
  begin
    Result := False;
    Exit;
  end;

  TCustomDcefBrowser(FDcefBrowser).doOnPreKeyEvent(PageIndex, Browser, event,
    osEvent, isKeyboardShortcut, Result);

  if (event.windows_key_code = 123) and (event.Kind = KEYEVENT_KEYUP) and
    TCustomDcefBrowser(FDcefBrowser).Options.DevToolsEnable then
    TBrowserPage(FParentBrowserPage).DevTools; // F12

  if (event.windows_key_code = 116) and (event.Kind = KEYEVENT_KEYUP) then
    TBrowserPage(FParentBrowserPage).ReloadIgnoreCache; // F5

  if (event.windows_key_code = 70) and
    (EVENTFLAG_CONTROL_DOWN in event.modifiers) then
    TBrowserPage(FParentBrowserPage).SearchText; // Ctrl+F

  if (event.windows_key_code = 115) and (EVENTFLAG_ALT_DOWN in event.modifiers)
  then
    Result := True; // Alt+F4 这里得屏蔽掉 不然用户按下这组合键会使整个程序关闭
end;

procedure TBasicBrowser.BrowserProtocolExecution(const Browser: ICefBrowser;
const URL: ustring; out allowOsExecution: Boolean);
begin
  if Not(csDestroying in ComponentState) then
  begin
    allowOsExecution := True;
    TCustomDcefBrowser(FDcefBrowser).doOnProtocolExecution(PageIndex, Browser,
      URL, allowOsExecution);
  end;
end;

procedure TBasicBrowser.BrowserQuotaRequest(const Browser: ICefBrowser;
const originUrl: ustring; newSize: Int64; const callback: ICefQuotaCallback;
out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnQuotaRequest(PageIndex, Browser,
      originUrl, newSize, callback, Result);
end;

procedure TBasicBrowser.BrowserRequestGeolocationPermission
  (const Browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer;
const callback: ICefGeolocationCallback; out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnRequestGeolocationPermission(PageIndex,
      Browser, requestingUrl, requestId, callback, Result);
end;

procedure TBasicBrowser.BrowserResourceRedirect(const Browser: ICefBrowser;
const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnResourceRedirect(PageIndex, Browser,
      frame, oldUrl, newUrl);
end;

procedure TBasicBrowser.BrowserSetFocus(const Browser: ICefBrowser;
source: TCefFocusSource; out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnSetFocus(PageIndex, Browser,
      source, Result);
end;

procedure TBasicBrowser.BrowserStartDragging(const Browser: ICefBrowser;
const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer;
out Result: Boolean);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnStartDragging(PageIndex, Browser,
      dragData, allowedOps, x, y, Result);
end;

procedure TBasicBrowser.BrowserStatusMessage(const Browser: ICefBrowser;
const value: ustring);
begin
  if Not(csDestroying in ComponentState) then
    TCustomDcefBrowser(FDcefBrowser).doOnPageStateChange(PageID,
      BrowserDataChange_StatusMessage, value, Actived);
end;

{ TBrowserPage }

procedure TBrowserPage.AddZoomLevel;
begin
  if Browser.host.ZoomLevel < 9 then
    Browser.host.ZoomLevel := Browser.host.ZoomLevel + 1;
end;

procedure TBrowserPage.BringSearchBarToFront;
begin
  if FCreateByPopup and Assigned(FSearchTextBar) then
    Windows.BringWindowToTop(FSearchTextBar.Handle);
end;

procedure TBrowserPage.BrowserPanelResize(Sender: TObject);
begin
  if Assigned(FSearchTextBar) and (FSearchTextBar.Visible) then
    FSearchTextBar.left := FPageControl.Width - FSearchTextBar.Width - 20;

  BringSearchBarToFront;

  if FCreateByPopup and Assigned(FBasicBrowser) and
    (FBasicBrowser.FBrowser <> nil) then
  begin
    MoveBrowserWin(0);

    if FPageControl.ActivePageIndex = FTabsheet.TabIndex then
      SetFocus;
  end;
end;

procedure TBrowserPage.Close;
begin
  TCustomDcefBrowser(FDcefBrowser).ClosePage(PageIndex);
end;

constructor TBrowserPage.Create(PageControl: TBorderLessPageControl;
ParentItems: Pointer; DcefBrowser: Pointer; DefaultEncoding: ustring;
PID: Integer; CreateByPopup: Boolean; Const DefaultURL: ustring = SBlankPageUrl;
FShow: Boolean = True);
begin
  inherited Create;
  FPageControl := PageControl;
  FDcefBrowser := DcefBrowser;
  FParentItems := ParentItems;
  FPageID := PID;
  FDefaultEncoding := DefaultEncoding;
  FCreateByPopup := CreateByPopup;

  CreateTabSheet(FShow, PID);

  if Not FCreateByPopup then
  begin
    FBasicBrowser := TBasicBrowser.Create(FBrowserPanel, Pointer(Self),
      DcefBrowser, DefaultEncoding, FPageID, CreateByPopup, DefaultURL, FShow);
    FBasicBrowser.Parent := FBrowserPanel;
    FBasicBrowser.Align := alClient;
    FBasicBrowser.CreateBrowser;
  end
  else
  begin
    FBasicBrowser := TBasicBrowser.Create(nil, Pointer(Self), DcefBrowser,
      DefaultEncoding, FPageID, CreateByPopup, DefaultURL, FShow);
  end;
end;

procedure TBrowserPage.CreateTabSheet(FShow: Boolean; PID: Integer);
var
  FormerActivePageIndex: Integer;
begin
  FormerActivePageIndex := TCustomDcefBrowser(FDcefBrowser).ActivePageIndex;

  FTabsheet := TTabSheet.Create(nil);
  with FTabsheet do
  begin
    PageControl := FPageControl;
    Caption := '';
    // TabVisible := TCustomDcefBrowser(FDcefBrowser).TabVisible;
  end;
  FBrowserPanel := TDcefBPanel.Create(FTabsheet);
  with FBrowserPanel do
  begin
    Parent := FTabsheet;
    Align := alClient;
    BevelOuter := bvNone;
{$IFDEF DELPHI14_UP}
    ShowCaption := False;
{$ENDIF}
    OnResize := BrowserPanelResize;
  end;

  if FShow or (FormerActivePageIndex <= -1) then
  begin
    FPageControl.ActivePage := FTabsheet;
    TCustomDcefBrowser(FDcefBrowser).ActivePageID := PID;
  end
  else
    FPageControl.ActivePageIndex := FormerActivePageIndex;
end;

procedure TBrowserPage.DebugPanelResize(Sender: TObject);
var
  MyRect: TRect;
begin
  if Assigned(FDevToolsPanel) and (FDebugWinHandle <> 0) then
  begin
    MyRect := FDevToolsPanel.ClientRect;
    MoveWindow(FDebugWinHandle, 0, 0,
{$IFDEF DELPHI14_UP}MyRect.Width{$ELSE}MyRect.right - MyRect.left{$ENDIF},
{$IFDEF DELPHI14_UP}MyRect.Height{$ELSE}MyRect.bottom - MyRect.top{$ENDIF} +
      1, True);
  end;
end;

procedure TBrowserPage.DevTools;
begin
  if Assigned(FDevToolsPanel) then
  begin
    if Assigned(Browser) then
      FDevTools.CloseDevTools(Browser);
    FSplitter.Free;
    FSplitter := nil;
    FDevTools.Free;
    FDevTools := nil;
    FDevToolsPanel.Free;
    FDevToolsPanel := nil;
  end
  else
  begin
    FSplitter := TSplitter.Create(FTabsheet);
    with FSplitter do
    begin
      Parent := FTabsheet;
      Align := alBottom;
      Height := 3;
      cursor := crVSplit;
    end;

    FDevToolsPanel := TDcefBPanel.Create(FTabsheet);
    with FDevToolsPanel do
    begin
      Parent := FTabsheet;
      Align := alBottom;
      BevelOuter := bvNone;
      Height := Trunc(FPageControl.Height * 0.4);
      OnResize := DebugPanelResize;
    end;

    FDevTools := TChromiumDevTools.Create(FDevToolsPanel);
    with FDevTools do
    begin
      Parent := FDevToolsPanel;
      Align := alClient;
      if Assigned(FBasicBrowser.FBrowser) then
        ShowDevTools(Browser);
    end;
  end;
end;

destructor TBrowserPage.Destroy;
begin
  FBasicBrowser.Free;
  if Assigned(FSearchTextBar) then
    FSearchTextBar.Free;
  if Assigned(FDevToolsPanel) then
    FDevToolsPanel.Free;
  if Assigned(FBrowserPanel) then
    FBrowserPanel.Free;
  if Assigned(FSplitter) then
    FSplitter.Free;
  if Assigned(FTabsheet) then
  begin
    // swish:如果直接设置PageControl=nil，在程序退出时会操作PageControl的Handle从而出错
    if FTabsheet.PageControl.HandleAllocated then
      FTabsheet.PageControl := nil
    else
      PPointer(@FTabsheet.PageControl)^ := nil;
    // changed done
    FTabsheet.Free;
  end;
  inherited;
end;

procedure TBrowserPage.ExecuteJavaScript(Const code: string);
begin
  FBasicBrowser.FBrowser.MainFrame.ExecuteJavaScript(code, 'about:blank', 0);
end;

function TBrowserPage.GetBrowser: ICefBrowser;
begin
  Result := nil;
  if Assigned(FBasicBrowser) then
    Result := FBasicBrowser.FBrowser;
end;

function TBrowserPage.GetCanGoBack: Boolean;
begin
  Result := FBasicBrowser.canGoBack;
end;

function TBrowserPage.GetCanGoForward: Boolean;
begin
  Result := FBasicBrowser.canGoForward;
end;

function TBrowserPage.GetClientRect: TRect;
begin
  Result := FBrowserPanel.ClientRect;
end;

function TBrowserPage.GetTitle: String;
begin
  Result := FTabsheet.Caption;
end;

function TBrowserPage.GetIsLoading: Boolean;
begin
  Result := FBasicBrowser.isLoading;
end;

function TBrowserPage.GetPageIndex: Integer;
begin
  Result := FParentItems.IndexOf(Self);
end;

{$IFDEF DELPHI14_UP}

function TBrowserPage.GetSource(var SourceText: string;
Const TimeOut: Integer = 1000): Boolean;
var
  WaitThread: TThread;
  Msg: TMsg;
  TempSource: string;
begin
  SourceText := '';
  TempSource := '';
  { if browser.isLoading then
    Exit; }

  Browser.MainFrame.GetSourceProc(
    procedure(const Str: ustring)
    begin
      TempSource := Str;
    end);

  WaitThread := TThread.CreateAnonymousThread(
    procedure
    var
      MyTime: Integer;
    begin
      MyTime := 0;
      while MyTime < TimeOut do
      begin
        Inc(MyTime, 5);
        Sleep(5);
      end;
    end);
  WaitThread.FreeOnTerminate := True;
  WaitThread.Start;

  while (TempSource = '') and Assigned(WaitThread) and
    (Not WaitThread.Finished) do
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;

  SourceText := TempSource;
  Result := Not SameText(SourceText, '');
end;
{$ENDIF}

procedure TBrowserPage.GetSourceInNewPage;
begin
  TCustomDcefBrowser(FDcefBrowser).GetSourceInNewPage;
end;

function TBrowserPage.GetTabVisible: Boolean;
begin
  Result := FTabsheet.TabVisible;
end;

{$IFDEF DELPHI14_UP}

function TBrowserPage.GetText(var aText: string;
const TimeOut: Integer): Boolean;
var
  WaitThread: TThread;
  Msg: TMsg;
  TempText: string;
begin
  aText := '';
  TempText := '';

  { if browser.isLoading then
    Exit; }

  Browser.MainFrame.GetTextProc(
    procedure(const Str: ustring)
    begin
      TempText := Str;
    end);

  WaitThread := TThread.CreateAnonymousThread(
    procedure
    var
      MyTime: Integer;
    begin
      MyTime := 0;
      while MyTime < TimeOut do
      begin
        Inc(MyTime, 5);
        Sleep(5);
      end;
    end);
  WaitThread.FreeOnTerminate := True;
  WaitThread.Start;

  while (TempText = '') and Assigned(WaitThread) and
    (Not WaitThread.Finished) do
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;

  aText := TempText;
  Result := Not SameText(aText, '');
end;

{$ENDIF}

function TBrowserPage.GetUrl: string;
begin
  Result := Browser.MainFrame.URL;
end;

function TBrowserPage.GetZoomLevel: string;
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
  if DoubleIndex(Browser.host.ZoomLevel, DoubleCases, ZoomIndex) then
    Result := ResultCases[ZoomIndex]
  else
    Result := FloatToStr(Browser.host.ZoomLevel);
end;

procedure TBrowserPage.GoBack;
begin
  FBasicBrowser.FBrowser.GoBack;
  SetFocus;
end;

procedure TBrowserPage.GoForward;
begin
  FBasicBrowser.FBrowser.GoForward;
  SetFocus;
end;

procedure TBrowserPage.Load(const URL: string);
begin
  if Assigned(FBasicBrowser) and Assigned(FBasicBrowser.FBrowser) then
    FBasicBrowser.FBrowser.MainFrame.LoadUrl(URL);
end;

procedure TBrowserPage.MoveBrowserWin(Method: Integer);
var
  MyRect: TRect;
begin
  FBasicBrowser.FBrowser.host.NotifyMoveOrResizeStarted;

  MyRect := FBrowserPanel.ClientRect;
  case Method of
    0:
      MoveWindow(FBasicBrowser.FBrowser.host.WindowHandle, 0, 0,
{$IFDEF DELPHI14_UP}MyRect.Width{$ELSE}MyRect.right - MyRect.left{$ENDIF},
{$IFDEF DELPHI14_UP}MyRect.Height{$ELSE}MyRect.bottom -
        MyRect.top{$ENDIF}, True);
    1:
      MoveWindow(FBasicBrowser.FBrowser.host.WindowHandle, 0, 0,
{$IFDEF DELPHI14_UP}MyRect.Width{$ELSE}MyRect.right - MyRect.left{$ENDIF},
{$IFDEF DELPHI14_UP}MyRect.Height{$ELSE}MyRect.bottom - MyRect.top{$ENDIF} +
        1, True);
  end;
end;

procedure TBrowserPage.Print;
begin
  FBasicBrowser.FBrowser.host.Print;
end;

procedure TBrowserPage.ReduceZoomLevel;
begin
  if Browser.host.ZoomLevel > -6 then
    Browser.host.ZoomLevel := Browser.host.ZoomLevel - 1;
end;

procedure TBrowserPage.Reload;
begin
  FBasicBrowser.FBrowser.Reload;
  FTabsheet.Invalidate;
  SetFocus;
end;

procedure TBrowserPage.ReloadIgnoreCache;
begin
  FBasicBrowser.FBrowser.ReloadIgnoreCache;
  SetFocus;
end;

procedure TBrowserPage.ResetZoomLevel;
begin
  Browser.host.ZoomLevel := 0;
end;

{$IFDEF DELPHI14_UP}

procedure TBrowserPage.RunInRenderProcess(AProc: TRenderProcessCallbackA;
AData: Pointer);
var
  AMsg: ICefProcessMessage;
  ATemp: Pointer;
begin
  if CefSingleProcess then
  begin
    AMsg := TCefProcessMessageRef.New('@dcefbrowser_runinrender');
    AMsg.ArgumentList.SetSize(3);
    // 这里使用字符串而不是整数，是因为JavaScript里没有64位整数
    AMsg.ArgumentList.SetString(0, IntToHex(IntPtr(Self), SizeOf(Pointer)));
    TRenderProcessCallbackA(ATemp) := AProc;
    AMsg.ArgumentList.SetString(1, IntToHex(IntPtr(Pointer(ATemp)),
      SizeOf(Pointer)));
    AMsg.ArgumentList.SetString(2, IntToHex(IntPtr(AData), SizeOf(Pointer)));
    Browser.SendProcessMessage(PID_RENDERER, AMsg);
  end
  else
    raise Exception.Create(SRunOnlyInSinglePro);
end;
{$ENDIF}

procedure TBrowserPage.SearchText;
begin
  if Not Assigned(FSearchTextBar) then
  begin
    FSearchTextBar := TSearchTextBar.Create(nil, FBasicBrowser.FBrowser.host);
    FSearchTextBar.Parent := FBrowserPanel;
  end;

  MoveBrowserWin(1); // 防止花屏的情况出现
  BrowserPanelResize(nil);
  FSearchTextBar.Clear;
  FSearchTextBar.Show;
  BringSearchBarToFront;
  FSearchTextBar.EditSetFocus;
end;

procedure TBrowserPage.SetTitle(const value: String);
begin
  FTabsheet.Caption := value;
end;

procedure TBrowserPage.SetFocus;
begin
  if Assigned(FBasicBrowser) and (FBasicBrowser.FBrowser <> nil) then
    if FCreateByPopup then
      FBasicBrowser.FBrowser.host.SetFocus(True)
    else
      FBasicBrowser.SetFocus;
end;

procedure TBrowserPage.SetTabVisible(const value: Boolean);
begin
  FTabsheet.TabVisible := value;
end;

procedure TBrowserPage.Show;
begin
  // FTabsheet.TabVisible := True;
  // FHided := False;
  // FTabsheet.Show;
  FPageControl.ActivePage := FTabsheet;
  TCustomDcefBrowser(FDcefBrowser).ActivePageID := PageID;
  SetFocus;
  FBasicBrowser.UpdatePageInfo(True);
end;

procedure TBrowserPage.StopLoad;
begin
  FBasicBrowser.FBrowser.StopLoad;
  SetFocus;
end;

{ TChromiumDevTools }

procedure TChromiumDevTools.CloseDevTools(const Browser: ICefBrowser);
begin
  if Browser <> nil then
  begin
    Windows.SetParent(GetWindow(Handle, GW_CHILD), 0);
    Browser.host.CloseDevTools;
  end;
end;

procedure TChromiumDevTools.Resize;
var
  rect: TRect;
  hdwp: THandle;
  hndl: THandle;
begin
  inherited;
  hndl := GetWindow(Handle, GW_CHILD);
  if hndl = 0 then
    Exit;

  rect := GetClientRect;
  hdwp := BeginDeferWindowPos(1);
  try
    hdwp := DeferWindowPos(hdwp, hndl, 0, rect.left, rect.top,
      rect.right - rect.left, rect.bottom - rect.top, SWP_NOZORDER);
  finally
    EndDeferWindowPos(hdwp);
  end;
end;

procedure TChromiumDevTools.ShowDevTools(const Browser: ICefBrowser;
inspectElementAt: PCefPoint);
var
  info: TCefWindowInfo;
  setting: TCefBrowserSettings;
  rect: TRect;
begin
  if Browser = nil then
    Exit;

  FillChar(info, SizeOf(info), 0);

  info.parent_window := Handle;
  info.Style := WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or
    WS_TABSTOP;
  rect := GetClientRect;
  info.x := rect.left;
  info.y := rect.top;
  info.Width := rect.right - rect.left;
  info.Height := rect.bottom - rect.top;
  info.window_name := CefString('DevTools');

  FillChar(setting, SizeOf(setting), 0);
  setting.size := SizeOf(setting);

  Browser.host.ShowDevTools(@info, TCefClientOwn.Create as ICefClient, @setting,
    inspectElementAt);
end;

procedure TChromiumDevTools.WndProc(var Message: TMessage);
var
  hndl: THandle;
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        hndl := GetWindow(Handle, GW_CHILD);
        if hndl <> 0 then
          PostMessage(hndl, WM_SETFOCUS, Message.wParam, 0);
        inherited WndProc(Message);
      end;
    WM_ERASEBKGND:
      begin
        hndl := GetWindow(Handle, GW_CHILD);
        if (csDesigning in ComponentState) or (hndl = 0) then
          inherited WndProc(Message);
      end;
    CM_WANTSPECIALKEY:
      if not(TWMKey(Message).CharCode in [VK_LEFT .. VK_DOWN]) then
        Message.Result := 1
      else
        inherited WndProc(Message);
    WM_GETDLGCODE:
      Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  else
    inherited WndProc(Message);
  end;
end;

{ TDefaultRenderProcessHandler }

function TDefaultRenderProcessHandler.OnProcessMessageReceived
  (const Browser: ICefBrowser; sourceProcess: TCefProcessId;
const Message: ICefProcessMessage): Boolean;
  procedure DoRunInRender;
  var
    ASender: TBrowserPage;
    ATemp: Pointer;
    AProc: TRenderProcessCallbackA;
    AData: Pointer;
  begin
    ASender := TBrowserPage
      (StrToInt64('$' + message.ArgumentList.GetString(0)));
    ATemp := Pointer(StrToInt64('$' + message.ArgumentList.GetString(1)));
    AProc := TRenderProcessCallbackA(ATemp);
    AData := Pointer(StrToInt64('$' + message.ArgumentList.GetString(2)));
    if Assigned(AProc) then
      AProc(ASender, Browser.MainFrame.GetV8Context, AData);
    TRenderProcessCallbackA(ATemp) := nil;
  end;

begin
  if message.Name = '@dcefbrowser_runinrender' then
  begin
    DoRunInRender;
    Result := True;
  end
  else
    Result := False;
end;

procedure TDefaultRenderProcessHandler.OnWebKitInitialized;
var
  Index: Integer;
begin
  inherited;
  for Index := Low(FClasses) to High(FClasses) do
  begin
{$IFDEF DELPHI14_UP}
    TCefRTTIExtension.Register(FClasses[Index].ClassName, FClasses[Index]
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}, True {$ENDIF});
{$ENDIF}
  end;
end;

{ TPageContainerLocker }

constructor TPageContainerLocker.Create;
begin
  InitializeCriticalSection(FCS);
end;

destructor TPageContainerLocker.Destroy;
begin
  DeleteCriticalSection(FCS);
  inherited;
end;

procedure TPageContainerLocker.Lock;
begin
  EnterCriticalSection(FCS);
end;

procedure TPageContainerLocker.UnLock;
begin
  LeaveCriticalSection(FCS);
end;

Initialization

PagesHelper := TPageContainerLocker.Create;

Finalization

PagesHelper.Free;

end.
