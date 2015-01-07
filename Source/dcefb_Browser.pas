unit dcefb_Browser;

// 基于Dcef3编写的 多标签多进程浏览器 框架
// 由于TChromium的OnBeforePopup事件不能方便地处理新的弹出窗口
// 开发较为麻烦,因此编写了TDcefBrowser
// By BccSafe
// Blog: http://www.bccsafe.com/

// 编程资质尚浅 若发现BUG或是设计缺陷 希望能联系我
// QQ: 1262807955
// Email: bccsafe5988@gmail.com

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.Dialogs, System.StrUtils, Generics.Collections,
  System.SysUtils, Winapi.Messages, System.Math, dcef3_cefgui, dcef3_ceflib,
  dcefb_BasicEvents, dcefb_BorderLessPC, dcefb_Events, dcefb_DlManager,
  dcefb_Options, dcefb_BasicDialog, dcefb_Panel;

{$I dcef3_cef.inc}

Const
  SBlankPageUrl = 'about:blank';
  SLoadingText = '正在加载';
  SNoTitleText = '无标题';
  SDialogTitleSuffix = ' 上的网页显示';
  SUnloadDialogTitle = '确认导航';
  SUnloadDialogText = '确定要离开此页吗？';

type
  TBasicDownloadOSR = class(TWinControl)
  private
    FDcefBrowser: Pointer;
    FBasicDcefBrowserEvents: TBasicDcefBrowserEvents;
    FBrowser: ICefBrowser;
    FClientHandler: ICefClient;
    FBrowserId: Integer;

    procedure BrowserBeforeDownload(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback);
    procedure BrowserDownloadUpdated(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback);

    procedure CreateBrowser;
    procedure GetSettings(var settings: TCefBrowserSettings);
  public
    constructor Create(AOwner: TComponent; DcefBrowser: Pointer); reintroduce;
    destructor Destroy; override;
    procedure Download(URL: string);
  end;

  TBasicBrowser = class(TWinControl)
  private
    FDcefBrowser: Pointer;
    FParentBrowserPage: Pointer;
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

    procedure BrowserLoadingStateChange(const browser: ICefBrowser;
      isLoading, canGoBack, canGoForward: Boolean);
    procedure BrowserStatusMessage(const browser: ICefBrowser;
      const value: ustring);
    procedure BrowserAddressChange(const browser: ICefBrowser;
      const frame: ICefFrame; const URL: ustring);
    procedure BrowserTitleChange(const browser: ICefBrowser;
      const title: ustring);

    procedure BrowserBeforePopup(const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; out Result: Boolean);
    procedure BrowserAfterCreated(const browser: ICefBrowser);
    procedure BrowserClose(const browser: ICefBrowser; out Result: Boolean);
    procedure BrowserBeforeBrowse(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean;
      out Result: Boolean);

    procedure BrowserPreKeyEvent(const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle;
      out isKeyboardShortcut: Boolean; out Result: Boolean);
    procedure BrowserKeyEvent(const browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean);

    procedure BrowserLoadStart(const browser: ICefBrowser;
      const frame: ICefFrame);
    procedure BrowserLoadEnd(const browser: ICefBrowser; const frame: ICefFrame;
      httpStatusCode: Integer);
    procedure BrowserLoadError(const browser: ICefBrowser;
      const frame: ICefFrame; errorCode: Integer;
      const errorText, failedUrl: ustring);

    procedure BrowserBeforeDownload(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback);
    procedure BrowserDownloadUpdated(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback);

    procedure BrowserBeforeResourceLoad(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; out Result: Boolean);
    procedure BrowserGetResourceHandler(const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      out Result: ICefResourceHandler);
    procedure BrowserResourceRedirect(const browser: ICefBrowser;
      const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring);

    procedure BrowserGotFocus(const browser: ICefBrowser);
    procedure BrowserSetFocus(const browser: ICefBrowser;
      source: TCefFocusSource; out Result: Boolean);
    procedure BrowserTakeFocus(const browser: ICefBrowser; next: Boolean);

    procedure BrowserBeforeContextMenu(const browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      const model: ICefMenuModel);
    procedure BrowserContextMenuCommand(const browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      commandId: Integer; eventFlags: TCefEventFlags; out Result: Boolean);
    procedure BrowserContextMenuDismissed(const browser: ICefBrowser;
      const frame: ICefFrame);

    procedure BrowserJsdialog(const browser: ICefBrowser;
      const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
      const messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean;
      out Result: Boolean);
    procedure BrowserBeforeUnloadDialog(const browser: ICefBrowser;
      const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback; out Result: Boolean);
    procedure BrowserDialogClosed(const browser: ICefBrowser);

    procedure BrowserPluginCrashed(const browser: ICefBrowser;
      const pluginPath: ustring);
    procedure BrowserBeforePluginLoad(const browser: ICefBrowser;
      const URL, policyUrl: ustring; const info: ICefWebPluginInfo;
      out Result: Boolean);

    procedure BrowserGetAuthCredentials(const browser: ICefBrowser;
      const frame: ICefFrame; isProxy: Boolean; const host: ustring;
      port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback; out Result: Boolean);
    procedure BrowserConsoleMessage(const browser: ICefBrowser;
      const message, source: ustring; line: Integer; out Result: Boolean);
    procedure BrowserProtocolExecution(const browser: ICefBrowser;
      const URL: ustring; out allowOsExecution: Boolean);

    procedure BrowserFileDialog(const browser: ICefBrowser;
      mode: TCefFileDialogMode; const title, defaultFileName: ustring;
      acceptTypes: TStrings; const callback: ICefFileDialogCallback;
      out Result: Boolean);

    procedure BrowserRequestGeolocationPermission(const browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer;
      const callback: ICefGeolocationCallback);
    procedure BrowserCancelGeolocationPermission(const browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer);

    procedure BrowserQuotaRequest(const browser: ICefBrowser;
      const originUrl: ustring; newSize: Int64;
      const callback: ICefQuotaCallback; out Result: Boolean);
    procedure BrowserGetCookieManager(out Result: ICefCookieManager);

    procedure BrowserDragEnter(const browser: ICefBrowser;
      const dragData: ICefDragData; mask: TCefDragOperations;
      out Result: Boolean);
    procedure BrowserStartDragging(const browser: ICefBrowser;
      const dragData: ICefDragData; allowedOps: TCefDragOperations;
      x, y: Integer; out Result: Boolean);
    procedure BrowserUpdateDragCursor(const browser: ICefBrowser;
      operation: TCefDragOperation);

    procedure LoadBrowserEvents;

    procedure GetSettings(var settings: TCefBrowserSettings);
    procedure CreateBrowser;

    function GetPageIndex: Integer;
    function GetActived: Boolean;
  protected
    procedure WndProc(var message: TMessage); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; ParentBrowserPage: Pointer;
      DcefBrowser: Pointer; DefaultEncoding: ustring; PID: Integer;
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

  TBrowserPage = class
  private
    FBasicBrowser: TBasicBrowser;
    FBrowserPanel: TDcefB_Panel;
    FDebugBrowserPanel: TDcefB_Panel;
    FSplitter: TSplitter;
    FTabsheet: TTabSheet;
    FSearchTextBar: TSearchTextBar;
    FPageID: Integer;
    FDefaultEncoding: ustring;
    FDebugWinHandle: HWND;

    FPageControl: TBorderLessPageControl;
    FDcefBrowser: Pointer;
    FParentItems: Pointer;

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
    procedure DebugTool;

    procedure GoBack;
    procedure GoForward;
    procedure Reload;
    procedure ReloadIgnoreCache;
    procedure StopLoad;
    procedure Print;
    procedure SearchText;
    procedure ExecuteJavaScript(Const code: string);
    procedure GetSource;
    procedure AddZoomLevel;
    procedure ReduceZoomLevel;
    procedure ResetZoomLevel;

    property PageIndex: Integer read GetPageIndex;
    property PageID: Integer read FPageID;
    property browser: ICefBrowser read GetBrowser;
    property CreateByPopup: Boolean read FCreateByPopup;
    property isLoading: Boolean read GetIsLoading;
    property canGoBack: Boolean read GetCanGoBack;
    property canGoForward: Boolean read GetCanGoForward;
    property URL: string read GetUrl;
    property ZoomLevel: string read GetZoomLevel;
  end;

  TCustomDcefBrowser = class(TWinControl)
  private
    FPageControl: TBorderLessPageControl;
    FDownloadManager: TDcefBrowserDownloadManager;
    FPageItems: TList<TBrowserPage>;
    FDownloadOSR: TBasicDownloadOSR;
    FClosedPageURL: TStringList;

    FOptions: TDcefBrowserOptions;
    FBasicOptions: TChromiumOptions;
    FBasicFontOptions: TChromiumFontOptions;

    FDefaultUrl: ustring;
    FDefaultEncoding: ustring;
    FActivePageID: Integer;
    FAddUpPageID: Integer;

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
    FOnGetCookieManager: TOnGetCookieManager;
    FOnDragEnter: TOnDragEnter;
    FOnStartDragging: TOnStartDragging;
    FOnUpdateDragCursor: TOnUpdateDragCursor;

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
  protected
    procedure doOnPageLoadingStateChange(const PageID: Integer;
      const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure doOnPageStateChange(const PageID: Integer;
      const Kind: TBrowserDataChangeKind; const value: string;
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
      const browser: ICefBrowser; const URL, policyUrl: ustring;
      const info: ICefWebPluginInfo; var CancelLoad: Boolean);

    procedure doOnDownloadUpdated(Const DcefItemIndex: Integer;
      Const Kind: TBrowserDownloadUpdatedKind);
    procedure doOnGetAuthCredentials(const PageIndex: Integer;
      const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean;
      const host: ustring; port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback; var CancelEventBuiltIn: Boolean);
    procedure doOnConsoleMessage(const PageIndex: Integer;
      const browser: ICefBrowser; const message, source: ustring;
      line: Integer);
    procedure doOnProtocolExecution(const PageIndex: Integer;
      const browser: ICefBrowser; const URL: ustring;
      out allowOsExecution: Boolean);
    procedure doOnFileDialog(const PageIndex: Integer;
      const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title, defaultFileName: ustring; acceptTypes: TStrings;
      const callback: ICefFileDialogCallback; out Result: Boolean);

    procedure doOnRequestGeolocationPermission(const PageIndex: Integer;
      const browser: ICefBrowser; const requestingUrl: ustring;
      requestId: Integer; const callback: ICefGeolocationCallback);
    procedure doOnCancelGeolocationPermission(const PageIndex: Integer;
      const browser: ICefBrowser; const requestingUrl: ustring;
      requestId: Integer);

    procedure doOnQuotaRequest(const PageIndex: Integer;
      const browser: ICefBrowser; const originUrl: ustring; newSize: Int64;
      const callback: ICefQuotaCallback; out Result: Boolean);
    procedure doOnGetCookieManager(const PageIndex: Integer;
      out Result: ICefCookieManager);

    procedure doOnDragEnter(const PageIndex: Integer;
      const browser: ICefBrowser; const dragData: ICefDragData;
      mask: TCefDragOperations; out Result: Boolean);
    procedure doOnStartDragging(const PageIndex: Integer;
      const browser: ICefBrowser; const dragData: ICefDragData;
      allowedOps: TCefDragOperations; x, y: Integer; out Result: Boolean);
    procedure doOnUpdateDragCursor(const PageIndex: Integer;
      const browser: ICefBrowser; operation: TCefDragOperation);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MainFormWndProc(var message: TMessage; const FormHandle: HWND);
    // 处理窗口MainForm(不是游览器窗口)焦点问题

    function AddPage(Const URL: string = ''; FShow: Boolean = True;
      AddAtLast: Boolean = True): Integer;
    function ClosePage(PageIndex: Integer;
      Const AutoChooseShowPageID: Boolean = True;
      Const ShowPageID: Integer = -1): Integer; overload;
    function ClosePage(ArrayPageID: Array of Integer; ShowPageID: Integer)
      : Integer; overload;
    procedure CloseAllOtherPage(PageID: Integer);
    procedure ShowPage(PageID: Integer);
    procedure Load(Const URL: string);
    procedure SetPageFocus;
    procedure CopyPage(PageID: Integer; Const FShow: Boolean = True);
    procedure DownloadFile(FileURL: string);
    procedure CheckDownProIsCreate;

    procedure DebugTool;
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
    procedure GetSource;
    procedure AddZoomLevel;
    procedure ReduceZoomLevel;
    procedure ResetZoomLevel;

    function GetPageByID(ID: Integer): TBrowserPage;
    function PageIDToIndex(PageID: Integer): Integer;
    function PageIndexToID(PageIndex: Integer): Integer;

    property Pages[Index: Integer]: TBrowserPage read GetPageByIndex;
    property ActivePage: TBrowserPage read GetActivePageItem;
    property ActivePageIndex: Integer read GetActivePageIndex;
    property ActivePageID: Integer read FActivePageID write FActivePageID;
    property PageCount: Integer read GetPageCount;
    property ClosedPageCount: Integer read GetClosePageCount;
    property DownloadManager: TDcefBrowserDownloadManager read FDownloadManager;
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
    property OnGetCookieManager: TOnGetCookieManager read FOnGetCookieManager
      write FOnGetCookieManager;
    property OnDragEnter: TOnDragEnter read FOnDragEnter write FOnDragEnter;
    property OnStartDragging: TOnStartDragging read FOnStartDragging
      write FOnStartDragging;
    property OnUpdateDragCursor: TOnUpdateDragCursor read FOnUpdateDragCursor
      write FOnUpdateDragCursor;
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
    property OnDownloadUpdated;
    property OnGetAuthCredentials;
    property OnConsoleMessage;
    property OnProtocolExecution;
    property OnFileDialog;
    property OnRequestGeolocationPermission;
    property OnCancelGeolocationPermission;
    property OnQuotaRequest;
    property OnGetCookieManager;
    property OnDragEnter;
    property OnStartDragging;
    property OnUpdateDragCursor;
  end;

implementation

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}

var
  CefInstances: Integer = 0;
  CefTimer: UINT = 0;
  looping: Boolean = False;
{$ENDIF}

type
  TVCLDcefClientHandler = class(TCustomClientHandler)
  public
    constructor Create(const crm: IChromiumEvents; renderer: Boolean); override;
    destructor Destroy; override;
  end;

  { TVCLClientHandler }

procedure TimerProc(HWND: HWND; uMsg: UINT; idEvent: Pointer;
  dwTime: DWORD); stdcall;
begin
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  if looping then
    Exit;
  if CefInstances > 0 then
  begin
    looping := True;
    try
      CefDoMessageLoopWork;
    finally
      looping := False;
    end;
  end;
{$ENDIF}
end;

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
    KillTimer(0, CefTimer);
{$ENDIF}
  inherited;
end;

{ TBasicDownloadBrowser }

procedure TBasicDownloadOSR.BrowserBeforeDownload(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
var
  MyOptions: TDcefBrowserOptions;
  DownloadManager: TDcefBrowserDownloadManager;
  DownLoadItemIndex: Integer;
  SaveFullPath: string;

  function RightPos(const SubStr, Str: string): Integer;
  var
    i, j, k, LenSub, LenS: Integer;
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
        for j := LenSub - 1 downto 1 do
        begin
          if Str[k] = SubStr[j] then
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

  { if Not browser.HasDocument then
    TBrowserPage(FParentBrowserPage).Close; }

  DownloadManager := TCustomDcefBrowser(FDcefBrowser).DownloadManager;
  DownLoadItemIndex := DownloadManager.ItemsIDToIndex(downloadItem.ID);
  if DownLoadItemIndex <> -1 then
  begin
    MyOptions := TCustomDcefBrowser(FDcefBrowser).Options;
    if Not DirectoryExists(MyOptions.DownLoadPath) then
      CreateDir(MyOptions.DownLoadPath);
    SaveFullPath := DealExistsFile(MyOptions.DownLoadPath + suggestedName);
    DownloadManager.Items[DownLoadItemIndex].UpdataFileName(SaveFullPath);
    callback.Cont(SaveFullPath, Not MyOptions.AutoDown);
  end; // else
end;

procedure TBasicDownloadOSR.BrowserDownloadUpdated(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
var
  MyDcefBrowser: TCustomDcefBrowser;
  DownloadManager: TDcefBrowserDownloadManager;
  DcefDownloadItem: TDcefDownloadItem;
  DownLoadItemIndex: Integer;
  DownloadComplete: Boolean;
begin
  if downloadItem.IsValid then
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

      { if TBrowserPage(FParentBrowserPage).Hided and DownloadComplete then
        begin
        FBrowser.host.ParentWindowWillClose;
        FBrowser.host.CloseBrowser(True);
        end; }
    end;

  end;
end;

constructor TBasicDownloadOSR.Create(AOwner: TComponent; DcefBrowser: Pointer);
begin
  FDcefBrowser := DcefBrowser;
  FBasicDcefBrowserEvents := TBasicDcefBrowserEvents.Create;
  FBasicDcefBrowserEvents.OnBeforeDownload := BrowserBeforeDownload;
  FBasicDcefBrowserEvents.OnDownloadUpdated := BrowserDownloadUpdated;

  if not(csDesigning in ComponentState) then
    FClientHandler := TVCLDcefClientHandler.Create
      (FBasicDcefBrowserEvents, True);

  FBrowserId := 0;
  FBrowser := nil;
end;

procedure TBasicDownloadOSR.CreateBrowser;
var
  info: TCefWindowInfo;
  settings: TCefBrowserSettings;
begin
  if not(csDesigning in ComponentState) then
  begin
    FillChar(info, SizeOf(info), 0);
    info.windowless_rendering_enabled := Ord(True);
    FillChar(settings, SizeOf(TCefBrowserSettings), 0);
    settings.size := SizeOf(TCefBrowserSettings);
    GetSettings(settings);
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    CefBrowserHostCreate(@info, FHandler, FDefaultUrl, @settings);
{$ELSE}
    FBrowser := CefBrowserHostCreateSync(@info, FClientHandler, '',
      @settings, nil);
    FBrowserId := FBrowser.Identifier;
{$ENDIF}
  end;
end;

destructor TBasicDownloadOSR.Destroy;
begin
  if FBrowser <> nil then
  begin
    FBrowser.StopLoad;
    FBrowser.host.CloseBrowser(False);
  end;

  if FClientHandler <> nil then
    (FClientHandler as ICefClientHandler).Disconnect;

  FClientHandler := nil;
  FBrowser := nil;
  FBasicDcefBrowserEvents := nil;
  FBasicDcefBrowserEvents.Free;
  inherited;
end;

procedure TBasicDownloadOSR.Download(URL: string);
begin
  FBrowser.host.StartDownload(URL);
end;

procedure TBasicDownloadOSR.GetSettings(var settings: TCefBrowserSettings);
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

procedure TCustomDcefBrowser.MainFormWndProc(var message: TMessage;
  const FormHandle: HWND);
var
  ActivateMsg: TWMActivateApp;
begin
  case Message.Msg of
    WM_NCACTIVATE:
      begin
        if (Message.wParam = 0) and (Message.lParam <> 0) then
        begin
          Message.wParam := 1;
        end;
      end;
    WM_ACTIVATEAPP:
      begin
        ActivateMsg := TWMActivateApp(Message);
        if Not ActivateMsg.Active then
        begin
          PostMessage(FormHandle, WM_NCACTIVATE, 0, 0);
        end
        else if (FPageItems.Count > 0) and (ActivePage <> nil) then
          if ActivePage.FCreateByPopup and (Options.MainFormWinHandle <> 0) then
          begin
            SetForegroundWindow(ActivePage.browser.host.WindowHandle);
            SetWindowPos(Options.MainFormWinHandle, HWND_TOP, 0, 0, 0, 0,
              SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
          end
          else
            ActivePage.FBasicBrowser.SetFocus;
      end;
  end;
end;

function TCustomDcefBrowser.PageIDToIndex(PageID: Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to FPageItems.Count - 1 do
    if FPageItems[Index].PageID = PageID then
    begin
      Result := Index;
      Break;
    end;
end;

function TCustomDcefBrowser.PageIndexToID(PageIndex: Integer): Integer;
begin
  Result := -1;
  if (PageIndex < FPageItems.Count) and (PageIndex > -1) then
    Result := FPageItems[PageIndex].PageID;
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

procedure TCustomDcefBrowser.ShowPage(PageID: Integer);
var
  PageIndex: Integer;
begin
  PageIndex := PageIDToIndex(PageID);
  if (PageIndex < PageCount) and (PageIndex > -1) then
    FPageItems[PageIndex].Show;
end;

procedure TCustomDcefBrowser.StopLoad;
begin
  if ActivePage <> nil then
    ActivePage.StopLoad;
end;

procedure TCustomDcefBrowser.CloseAllOtherPage(PageID: Integer);
var
  Index, ID: Integer;
begin
  for Index := FPageItems.Count - 1 Downto 0 do
  begin
    ID := Pages[Index].PageID;
    if PageID <> ID then
      ClosePage(Index, False, -1);
  end;
  ShowPage(PageID);
end;

function TCustomDcefBrowser.ClosePage(ArrayPageID: Array of Integer;
  ShowPageID: Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := Low(ArrayPageID) to High(ArrayPageID) do
    ClosePage(PageIDToIndex(ArrayPageID[Index]), False, -1);
  if (ShowPageID > -1) then
  begin
    Result := ShowPageID;
    ShowPage(ShowPageID);
  end;
end;

function TCustomDcefBrowser.ClosePage(PageIndex: Integer;
  Const AutoChooseShowPageID: Boolean = True;
  Const ShowPageID: Integer = -1): Integer;
var
  FLastIndex: Boolean;
begin
  Result := -1;
  if (PageIndex > -1) and (PageIndex < PageCount) then
  begin
    FLastIndex := PageIndex = (PageCount - 1);
    FClosedPageURL.Add(FPageItems[PageIndex].FBasicBrowser.FBrowser.
      MainFrame.URL);
    Result := PageIndexToID(IfThen(FLastIndex, PageIndex - 1, PageIndex));

    if AutoChooseShowPageID then
      doOnPageClose(FPageItems[PageIndex].PageID, Result)
    else if ShowPageID <> -1 then
      doOnPageClose(FPageItems[PageIndex].PageID, ShowPageID)
    else
      doOnPageClose(FPageItems[PageIndex].PageID, -1);

    FPageItems[PageIndex].Free;
    FPageItems.Delete(PageIndex);

    if (PageCount > 0) then
    begin
      if AutoChooseShowPageID then
        ShowPage(Result)
      else if ShowPageID <> -1 then
        ShowPage(ShowPageID);
    end
    else if FOptions.TerminateAppWhenAllPageClosed then
    begin
      CefShutdown;
      ExitProcess(0);
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

  FPageControl := TBorderLessPageControl.Create(Self);
  FPageControl.Parent := TWinControl(Self);
  FPageControl.Align := alClient;

  FOptions := TDcefBrowserOptions.Create;
  FBasicFontOptions := TChromiumFontOptions.Create;
  FBasicOptions := TChromiumOptions.Create;

  FPageItems := TList<TBrowserPage>.Create;
  FDownloadManager := TDcefBrowserDownloadManager.Create();
  FClosedPageURL := TStringList.Create;
end;

procedure TCustomDcefBrowser.DebugTool;
begin
  if ActivePage <> nil then
    ActivePage.DebugTool;
end;

destructor TCustomDcefBrowser.Destroy;
var
  i: Integer;
begin
  if Assigned(FPageItems) then
    for i := 0 to FPageItems.Count - 1 do
      FPageItems[i].Free;
  FPageItems.Free;
  FDownloadManager.Free;
  FClosedPageURL.Free;
  FPageControl.Free;
  FOptions.Free;
  FBasicFontOptions.Free;
  FBasicOptions.Free;
  FDownloadOSR.Free;
  inherited;
end;

procedure TCustomDcefBrowser.doOnPageStateChange(const PageID: Integer;
  const Kind: TBrowserDataChangeKind; const value: string;
  const PageActived: Boolean);
begin
  if Assigned(FOnPageStateChange) then
    FOnPageStateChange(PageID, Kind, value, PageActived);
end;

procedure TCustomDcefBrowser.doOnPluginCrashed(const PageIndex: Integer;
  const browser: ICefBrowser; const pluginPath: ustring);
begin
  if Assigned(FOnPluginCrashed) then
    FOnPluginCrashed(PageIndex, browser, pluginPath);
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

procedure TCustomDcefBrowser.CheckDownProIsCreate;
begin
  if Not Assigned(FDownloadOSR) then
  begin
    FDownloadOSR := TBasicDownloadOSR.Create(nil, Pointer(Self));
    FDownloadOSR.CreateBrowser;
  end;
end;

procedure TCustomDcefBrowser.ExecuteJavaScript(const code: string);
begin
  if ActivePage <> nil then
    ActivePage.ExecuteJavaScript(code);
end;

procedure TCustomDcefBrowser.doOnDialogClosed(const PageIndex: Integer;
  const browser: ICefBrowser);
begin
  if Assigned(FOnDialogClosed) then
    FOnDialogClosed(PageIndex, browser);
end;

procedure TCustomDcefBrowser.doOnDownloadUpdated(const DcefItemIndex: Integer;
  const Kind: TBrowserDownloadUpdatedKind);
begin
  if Assigned(FOnDownloadUpdated) then
    FOnDownloadUpdated(DcefItemIndex, Kind);
end;

procedure TCustomDcefBrowser.doOnDragEnter(const PageIndex: Integer;
  const browser: ICefBrowser; const dragData: ICefDragData;
  mask: TCefDragOperations; out Result: Boolean);
begin
  if Assigned(FOnDragEnter) then
    FOnDragEnter(PageIndex, browser, dragData, mask, Result);
end;

procedure TCustomDcefBrowser.doOnFileDialog(const PageIndex: Integer;
  const browser: ICefBrowser; mode: TCefFileDialogMode;
  const title, defaultFileName: ustring; acceptTypes: TStrings;
  const callback: ICefFileDialogCallback; out Result: Boolean);
begin
  if Assigned(FOnFileDialog) then
    FOnFileDialog(PageIndex, browser, mode, title, defaultFileName, acceptTypes,
      callback, Result);
end;

procedure TCustomDcefBrowser.doOnPageAdd(const PageID: Integer;
  Const AddAtLast: Boolean);
begin
  if Assigned(FOnPageAdd) then
    FOnPageAdd(PageID, AddAtLast);
end;

procedure TCustomDcefBrowser.doOnBeforeBrowse(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; isRedirect: Boolean; var Cancel: Boolean);
begin
  if Assigned(FOnBeforeBrowse) then
    FOnBeforeBrowse(PageIndex, browser, frame, request, isRedirect, Cancel);
end;

procedure TCustomDcefBrowser.doOnBeforeContextMenu(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  if Assigned(FOnBeforeContextMenu) then
    FOnBeforeContextMenu(PageIndex, browser, frame, params, model);
end;

procedure TCustomDcefBrowser.doOnBeforePluginLoad(const PageIndex: Integer;
  const browser: ICefBrowser; const URL, policyUrl: ustring;
  const info: ICefWebPluginInfo; var CancelLoad: Boolean);
begin
  if Assigned(FOnBeforePluginLoad) then
    FOnBeforePluginLoad(PageIndex, browser, URL, policyUrl, info, CancelLoad);
end;

procedure TCustomDcefBrowser.doOnBeforeResourceLoad(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; var CancelLoad: Boolean);
begin
  if Assigned(FOnBeforeResourceLoad) then
    FOnBeforeResourceLoad(PageIndex, browser, frame, request, CancelLoad);
end;

procedure TCustomDcefBrowser.doOnBeforeUnloadDialog(const PageIndex: Integer;
  const browser: ICefBrowser; const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnBeforeUnloadDialog) then
    FOnBeforeUnloadDialog(PageIndex, browser, messageText, isReload, callback,
      CancelEventBuiltIn);
end;

procedure TCustomDcefBrowser.doOnCancelGeolocationPermission(const PageIndex
  : Integer; const browser: ICefBrowser; const requestingUrl: ustring;
  requestId: Integer);
begin
  if Assigned(FOnCancelGeolocationPermission) then
    FOnCancelGeolocationPermission(PageIndex, browser, requestingUrl,
      requestId);
end;

procedure TCustomDcefBrowser.doOnConsoleMessage(const PageIndex: Integer;
  const browser: ICefBrowser; const message, source: ustring; line: Integer);
begin
  if Assigned(FOnConsoleMessage) then
    FOnConsoleMessage(PageIndex, browser, message, source, line);
end;

procedure TCustomDcefBrowser.doOnContextMenuCommand(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: TCefEventFlags; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnContextMenuCommand) then
    FOnContextMenuCommand(PageIndex, browser, frame, params, commandId,
      eventFlags, CancelEventBuiltIn);
end;

procedure TCustomDcefBrowser.doOnContextMenuDismissed(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnContextMenuDismissed) then
    FOnContextMenuDismissed(PageIndex, browser, frame);
end;

procedure TCustomDcefBrowser.doOnPageClose(const ClosePageID,
  ShowPageID: Integer);
begin
  if Assigned(FOnPageClose) then
    FOnPageClose(ClosePageID, ShowPageID);
end;

procedure TCustomDcefBrowser.doOnPageLoadingStateChange(const PageID: Integer;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if Assigned(FOnPageLoadingStateChange) then
    FOnPageLoadingStateChange(PageID, browser, isLoading, canGoBack,
      canGoForward);
end;

procedure TCustomDcefBrowser.doOnGetAuthCredentials(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean;
  const host: ustring; port: Integer; const realm, scheme: ustring;
  const callback: ICefAuthCallback; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnGetAuthCredentials) then
    FOnGetAuthCredentials(PageIndex, browser, frame, isProxy, host, port, realm,
      scheme, callback, CancelEventBuiltIn);
end;

procedure TCustomDcefBrowser.doOnGetCookieManager(const PageIndex: Integer;
  out Result: ICefCookieManager);
begin
  if Assigned(FOnGetCookieManager) then
    FOnGetCookieManager(PageIndex, Result);
end;

procedure TCustomDcefBrowser.doOnGetResourceHandler(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; var ResourceHandler: ICefResourceHandler);
begin
  if Assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(PageIndex, browser, frame, request, ResourceHandler);
end;

procedure TCustomDcefBrowser.doOnGotFocus(const PageIndex: Integer;
  const browser: ICefBrowser; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnGotFocus) then
    FOnGotFocus(PageIndex, browser, CancelEventBuiltIn);
end;

procedure TCustomDcefBrowser.doOnJsdialog(const PageIndex: Integer;
  const browser: ICefBrowser; const originUrl, acceptLang: ustring;
  dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
  const callback: ICefJsDialogCallback; var CancelEventBuiltIn: Boolean);
begin
  if Assigned(FOnJsdialog) then
    FOnJsdialog(PageIndex, browser, originUrl, acceptLang, dialogType,
      messageText, defaultPromptText, callback, CancelEventBuiltIn);
end;

procedure TCustomDcefBrowser.doOnKeyEvent(const PageIndex: Integer;
  const browser: ICefBrowser; const event: PCefKeyEvent;
  osEvent: TCefEventHandle; var Cancel: Boolean);
begin
  if Assigned(FOnKeyEvent) then
    FOnKeyEvent(PageIndex, browser, event, osEvent, Cancel);
end;

procedure TCustomDcefBrowser.doOnLoadEnd(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(PageIndex, browser, frame, httpStatusCode);
end;

procedure TCustomDcefBrowser.doOnLoadError(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
begin
  if Assigned(FOnLoadError) then
    FOnLoadError(PageIndex, browser, frame, errorCode, errorText, failedUrl);
end;

procedure TCustomDcefBrowser.doOnLoadStart(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnLoadStart) then
    FOnLoadStart(PageIndex, browser, frame);
end;

procedure TCustomDcefBrowser.doOnPreKeyEvent(const PageIndex: Integer;
  const browser: ICefBrowser; const event: PCefKeyEvent;
  osEvent: TCefEventHandle; var isKeyboardShortcut, Cancel: Boolean);
begin
  if Assigned(FOnPreKeyEvent) then
    FOnPreKeyEvent(PageIndex, browser, event, osEvent,
      isKeyboardShortcut, Cancel);
end;

procedure TCustomDcefBrowser.doOnProtocolExecution(const PageIndex: Integer;
  const browser: ICefBrowser; const URL: ustring;
  out allowOsExecution: Boolean);
begin
  if Assigned(FOnProtocolExecution) then
    FOnProtocolExecution(PageIndex, browser, URL, allowOsExecution);
end;

procedure TCustomDcefBrowser.doOnQuotaRequest(const PageIndex: Integer;
  const browser: ICefBrowser; const originUrl: ustring; newSize: Int64;
  const callback: ICefQuotaCallback; out Result: Boolean);
begin
  if Assigned(FOnQuotaRequest) then
    FOnQuotaRequest(PageIndex, browser, originUrl, newSize, callback, Result);
end;

procedure TCustomDcefBrowser.doOnRequestGeolocationPermission(const PageIndex
  : Integer; const browser: ICefBrowser; const requestingUrl: ustring;
  requestId: Integer; const callback: ICefGeolocationCallback);
begin
  if Assigned(FOnRequestGeolocationPermission) then
    FOnRequestGeolocationPermission(PageIndex, browser, requestingUrl,
      requestId, callback);
end;

procedure TCustomDcefBrowser.doOnResourceRedirect(const PageIndex: Integer;
  const browser: ICefBrowser; const frame: ICefFrame; const oldUrl: ustring;
  var newUrl: ustring);
begin
  if Assigned(FOnResourceRedirect) then
    FOnResourceRedirect(PageIndex, browser, frame, oldUrl, newUrl);
end;

procedure TCustomDcefBrowser.doOnSetFocus(const PageIndex: Integer;
  const browser: ICefBrowser; source: TCefFocusSource;
  var CancelFocus: Boolean);
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(PageIndex, browser, source, CancelFocus);
end;

procedure TCustomDcefBrowser.doOnStartDragging(const PageIndex: Integer;
  const browser: ICefBrowser; const dragData: ICefDragData;
  allowedOps: TCefDragOperations; x, y: Integer; out Result: Boolean);
begin
  if Assigned(FOnStartDragging) then
    FOnStartDragging(PageIndex, browser, dragData, allowedOps, x, y, Result);
end;

procedure TCustomDcefBrowser.doOnTakeFocus(const PageIndex: Integer;
  const browser: ICefBrowser; next: Boolean);
begin
  if Assigned(FOnTakeFocus) then
    FOnTakeFocus(PageIndex, browser, next);
end;

procedure TCustomDcefBrowser.doOnUpdateDragCursor(const PageIndex: Integer;
  const browser: ICefBrowser; operation: TCefDragOperation);
begin
  if Assigned(FOnUpdateDragCursor) then
    FOnUpdateDragCursor(PageIndex, browser, operation);
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
var
  Index: Integer;
begin
  Result := nil;
  if Assigned(FPageItems) then
  begin
    Index := ActivePageIndex;
    if Index > -1 then
      Result := FPageItems[Index];
  end;
end;

function TCustomDcefBrowser.GetClosePageCount: Integer;
begin
  Result := 0;
  if Assigned(FClosedPageURL) then
    Result := FClosedPageURL.Count;
end;

{ function TCustomDcefBrowser.GetHidedPageCount: Integer;
  var
  Index: Integer;
  begin
  Result := 0;
  if Assigned(FPageItems) then
  for Index := 0 to FPageItems.Count - 1 do
  if FPageItems[Index].Hided then
  Inc(Result);
  end; }

function TCustomDcefBrowser.GetNewPageID: Integer;
begin
  Inc(FAddUpPageID);
  Result := FAddUpPageID;
end;

function TCustomDcefBrowser.GetPageByID(ID: Integer): TBrowserPage;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to FPageItems.Count - 1 do
    if FPageItems[Index].PageID = ID then
    begin
      Result := FPageItems[Index];
      Break;
    end;
end;

function TCustomDcefBrowser.GetPageByIndex(Index: Integer): TBrowserPage;
begin
  Result := nil;
  if Assigned(FPageItems) and (FPageItems.Count > 0) then
    Result := FPageItems[Index];
end;

function TCustomDcefBrowser.GetPageCount: Integer;
begin
  Result := 0;
  if Assigned(FPageItems) then
    Result := FPageItems.Count;
end;

procedure TCustomDcefBrowser.GetSource;
begin
  if ActivePage <> nil then
    AddPage('view-source:' + ActivePage.URL);
end;

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

procedure TCustomDcefBrowser.Load(const URL: string);
begin
  if FPageItems.Count > 0 then
    ActivePage.Load(URL)
  else
    AddPage(URL, True);
end;

{ TBrowserPageItem }

constructor TBasicBrowser.Create(AOwner: TComponent; ParentBrowserPage: Pointer;
  DcefBrowser: Pointer; DefaultEncoding: ustring; PID: Integer;
  CreateByPopup: Boolean; Const DefaultURL: ustring = SBlankPageUrl;
  FShow: Boolean = True);
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
  CefBrowserHostCreate(@info, FClientHandler, FDefaultUrl, @settings,
    TCefRequestContextRef.CreateContext((FHandler as ICefClientHandler)
    .GetRequestContextHandler));
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
    if FClientHandler <> nil then
      (FClientHandler as ICefClientHandler).Disconnect;

    FClientHandler := nil;
    FBrowser := nil;
    FBasicDcefBrowserEvents := nil;
    FBasicDcefBrowserEvents.Free;
  end;
  inherited;
end;

function TBasicBrowser.GetActived: Boolean;
begin
  Result := TCustomDcefBrowser(FDcefBrowser).ActivePageID = PageID;
end;

function TBasicBrowser.GetPageIndex: Integer;
begin
  Result := TBrowserPage(FParentBrowserPage).PageIndex;
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

procedure TBasicBrowser.WndProc(var message: TMessage);
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
  FBasicDcefBrowserEvents.OnGetCookieManager := BrowserGetCookieManager;
  FBasicDcefBrowserEvents.OnDragEnter := BrowserDragEnter;
  FBasicDcefBrowserEvents.OnStartDragging := BrowserStartDragging;
  FBasicDcefBrowserEvents.OnUpdateDragCursor := BrowserUpdateDragCursor;
end;

procedure TBasicBrowser.BrowserAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const URL: ustring);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnPageStateChange(PageID,
    BrowserDataChange_Address, URL, Actived);
  FLastAddress := URL;
end;

procedure TBasicBrowser.BrowserAfterCreated(const browser: ICefBrowser);
var
  WinHandle: HWND;
  BrowserPage: TBrowserPage;
  MyDcefBrowser: TCustomDcefBrowser;
  // WindowStyle: Integer;
  // MyRect: TRect;
begin
  MyDcefBrowser := TCustomDcefBrowser(FDcefBrowser);
  if browser.IsPopup and (Not MyDcefBrowser.Options.PopupNewWindow) then
  begin
    BrowserPage := TBrowserPage(FParentBrowserPage);
    if (BrowserPage.FBasicBrowser.FBrowserId = -1) and BrowserPage.FCreateByPopup
    then
    begin
      WinHandle := browser.host.WindowHandle;
      BrowserPage.FBasicBrowser.FBrowserId := browser.Identifier;
      BrowserPage.FBasicBrowser.FBrowser := browser;

      Winapi.Windows.SetParent(WinHandle, BrowserPage.FBrowserPanel.Handle);
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

procedure TBasicBrowser.BrowserBeforeBrowse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean;
  out Result: Boolean);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnBeforeBrowse(PageIndex, browser, frame,
    request, isRedirect, Result);
end;

procedure TBasicBrowser.BrowserBeforeContextMenu(const browser: ICefBrowser;
  const frame: ICefFrame; const params: ICefContextMenuParams;
  const model: ICefMenuModel);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnBeforeContextMenu(PageIndex, browser,
    frame, params, model);
end;

procedure TBasicBrowser.BrowserBeforeDownload(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
var
  MyDcefBrowser: TCustomDcefBrowser;
begin
  MyDcefBrowser := TCustomDcefBrowser(FDcefBrowser);
  MyDcefBrowser.CheckDownProIsCreate;
  MyDcefBrowser.FDownloadOSR.Download(downloadItem.URL);
  if Not browser.HasDocument then
    TBrowserPage(FParentBrowserPage).Close;
end;

procedure TBasicBrowser.BrowserBeforePluginLoad(const browser: ICefBrowser;
  const URL, policyUrl: ustring; const info: ICefWebPluginInfo;
  out Result: Boolean);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnBeforePluginLoad(PageIndex, browser, URL,
    policyUrl, info, Result);
end;

procedure TBasicBrowser.BrowserBeforePopup(const browser: ICefBrowser;
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
  MyBrowserPage := TBrowserPage(FParentBrowserPage);
  MyDcefBrowser := TCustomDcefBrowser(FDcefBrowser);

  if (browser.Identifier = FBrowserId) and
    (Not SameText(targetUrl, SBlankPageUrl)) and
    (Not MyDcefBrowser.Options.PopupNewWindow) then
  begin
    NewBrowserPage := TBrowserPage.Create(MyBrowserPage.FPageControl,
      MyBrowserPage.FParentItems, FDcefBrowser, FDefaultEncoding,
      MyDcefBrowser.GetNewPageID, True, targetUrl, False);
    NewBrowserPage.FBasicBrowser.GetSettings(settings);
    TList<TBrowserPage>(MyBrowserPage.FParentItems).Add(NewBrowserPage);

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

procedure TBasicBrowser.BrowserBeforeResourceLoad(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest; out Result: Boolean);
{ //uses typinfo
  function EnumToStr(IEnum: TCefResourceType): string;
  begin
  Result := GetEnumName(Typeinfo(TCefResourceType), Ord(IEnum));
  end; }

begin
  // senddebuginfo(EnumToStr(request.ResourceType));
end;

procedure TBasicBrowser.BrowserBeforeUnloadDialog(const browser: ICefBrowser;
  const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback; out Result: Boolean);
var
  CancelEventBuiltIn, IsContinue: Boolean;
begin
  CancelEventBuiltIn := False;
  TCustomDcefBrowser(FDcefBrowser).doOnBeforeUnloadDialog(PageIndex, browser,
    messageText, isReload, callback, CancelEventBuiltIn);

  if Not CancelEventBuiltIn then
  begin
    TThread.Synchronize(nil,
      procedure
      begin
        IsContinue := MessageBox(0,
          PChar(messageText + #13#10 + SUnloadDialogText),
          PChar(SUnloadDialogTitle), MB_OKCANCEL) = idOk;
      end);
    callback.Cont(IsContinue, '');
  end;
  Result := True;
end;

procedure TBasicBrowser.BrowserCancelGeolocationPermission
  (const browser: ICefBrowser; const requestingUrl: ustring;
requestId: Integer);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnCancelGeolocationPermission(PageIndex,
    browser, requestingUrl, requestId);
end;

procedure TBasicBrowser.BrowserClose(const browser: ICefBrowser;
out Result: Boolean);
begin
  TBrowserPage(FParentBrowserPage).Close;
  Result := True;
end;

procedure TBasicBrowser.BrowserConsoleMessage(const browser: ICefBrowser;
const message, source: ustring; line: Integer; out Result: Boolean);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnConsoleMessage(PageIndex, browser,
    message, source, line);
  Result := True;
end;

procedure TBasicBrowser.BrowserContextMenuCommand(const browser: ICefBrowser;
const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer;
eventFlags: TCefEventFlags; out Result: Boolean);
var
  CancelEventBuiltIn: Boolean;
begin
  TCustomDcefBrowser(FDcefBrowser).doOnContextMenuCommand(PageIndex, browser,
    frame, params, commandId, eventFlags, CancelEventBuiltIn);
  Result := CancelEventBuiltIn;
end;

procedure TBasicBrowser.BrowserContextMenuDismissed(const browser: ICefBrowser;
const frame: ICefFrame);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnContextMenuDismissed(PageIndex,
    browser, frame);
end;

procedure TBasicBrowser.BrowserDialogClosed(const browser: ICefBrowser);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnDialogClosed(PageIndex, browser);
end;

procedure TBasicBrowser.BrowserDownloadUpdated(const browser: ICefBrowser;
const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback);
begin
  callback.Cancel;

end;

procedure TBasicBrowser.BrowserDragEnter(const browser: ICefBrowser;
const dragData: ICefDragData; mask: TCefDragOperations; out Result: Boolean);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnDragEnter(PageIndex, browser, dragData,
    mask, Result);
end;

procedure TBasicBrowser.BrowserFileDialog(const browser: ICefBrowser;
mode: TCefFileDialogMode; const title, defaultFileName: ustring;
acceptTypes: TStrings; const callback: ICefFileDialogCallback;
out Result: Boolean);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnFileDialog(PageIndex, browser, mode,
    title, defaultFileName, acceptTypes, callback, Result);
end;

procedure TBasicBrowser.BrowserGetAuthCredentials(const browser: ICefBrowser;
const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer;
const realm, scheme: ustring; const callback: ICefAuthCallback;
out Result: Boolean);
var
  UserName, Password: string;
  TempBool, CancelEventBuiltIn: Boolean;
begin
  CancelEventBuiltIn := False;
  TCustomDcefBrowser(FDcefBrowser).doOnGetAuthCredentials(PageIndex, browser,
    frame, isProxy, host, port, realm, scheme, callback, CancelEventBuiltIn);

  if CancelEventBuiltIn then
    Result := True
  else
  begin
    TThread.Synchronize(nil,
      procedure
      begin
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
          end
      end);
    Result := TempBool;
    if TempBool = True then
      callback.Cont(UserName, Password);
  end;
end;

procedure TBasicBrowser.BrowserGetCookieManager(out Result: ICefCookieManager);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnGetCookieManager(PageIndex, Result);
end;

procedure TBasicBrowser.BrowserGetResourceHandler(const browser: ICefBrowser;
const frame: ICefFrame; const request: ICefRequest;
out Result: ICefResourceHandler);
begin
  //
end;

procedure TBasicBrowser.BrowserGotFocus(const browser: ICefBrowser);
var
  CancelEventBuiltIn: Boolean;
begin
  TCustomDcefBrowser(FDcefBrowser).doOnGotFocus(PageIndex, browser,
    CancelEventBuiltIn);

  if Not CancelEventBuiltIn then
    TBrowserPage(FParentBrowserPage).BringSearchBarToFront;
end;

procedure TBasicBrowser.BrowserJsdialog(const browser: ICefBrowser;
const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
const messageText, defaultPromptText: ustring; callback: ICefJsDialogCallback;
out suppressMessage, Result: Boolean);
var
  CancelEventBuiltIn, IsContinue: Boolean;
  UserInput: string;
begin
  CancelEventBuiltIn := False;
  IsContinue := True;
  TCustomDcefBrowser(FDcefBrowser).doOnJsdialog(PageIndex, browser, originUrl,
    acceptLang, dialogType, messageText, defaultPromptText, callback,
    CancelEventBuiltIn);

  if Not CancelEventBuiltIn then
  begin // 这里不能使用MessageBox，得使用模态窗口
    case dialogType of
      JSDIALOGTYPE_ALERT:
        TThread.Synchronize(nil,
          procedure
          begin
            ShowMessage(messageText);
          end);
      JSDIALOGTYPE_CONFIRM:
        TThread.Synchronize(nil,
          procedure
          begin
            MyConfirm(originUrl + SDialogTitleSuffix, messageText, IsContinue);
            callback.Cont(IsContinue, '');
          end);
      JSDIALOGTYPE_PROMPT:
        TThread.Synchronize(nil,
          procedure
          begin
            UserInput := defaultPromptText;
            IsContinue := InputQuery(PChar(originUrl + SDialogTitleSuffix),
              PChar(messageText), UserInput);
            callback.Cont(IsContinue, UserInput);
          end);
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

procedure TBasicBrowser.BrowserKeyEvent(const browser: ICefBrowser;
const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnKeyEvent(PageIndex, browser, event,
    osEvent, Result);
end;

procedure TBasicBrowser.BrowserLoadEnd(const browser: ICefBrowser;
const frame: ICefFrame; httpStatusCode: Integer);
begin
  if SameText(FLastTitle, '') then
    FLastTitle := SNoTitleText;
  TCustomDcefBrowser(FDcefBrowser).doOnLoadEnd(PageIndex, browser, frame,
    httpStatusCode);
end;

procedure TBasicBrowser.BrowserLoadError(const browser: ICefBrowser;
const frame: ICefFrame; errorCode: Integer;
const errorText, failedUrl: ustring);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnLoadError(PageIndex, browser, frame,
    errorCode, errorText, failedUrl);
end;

procedure TBasicBrowser.BrowserLoadingStateChange(const browser: ICefBrowser;
isLoading, canGoBack, canGoForward: Boolean);
begin
  if browser.Identifier = FBrowserId then
  begin
    FIsLoading := isLoading;
    FCanGoForward := canGoForward;
    FCanGoBack := canGoBack;
    TCustomDcefBrowser(FDcefBrowser).doOnPageLoadingStateChange(PageID, browser,
      isLoading, canGoBack, canGoForward);
  end;
end;

procedure TBasicBrowser.BrowserLoadStart(const browser: ICefBrowser;
const frame: ICefFrame);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnLoadStart(PageIndex, browser, frame);
end;

procedure TBasicBrowser.BrowserTakeFocus(const browser: ICefBrowser;
next: Boolean);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnTakeFocus(PageIndex, browser, next);
end;

procedure TBasicBrowser.BrowserTitleChange(const browser: ICefBrowser;
const title: ustring);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnPageStateChange(PageID,
    BrowserDataChange_Title, title, Actived);
  FLastTitle := title;
end;

procedure TBasicBrowser.BrowserUpdateDragCursor(const browser: ICefBrowser;
operation: TCefDragOperation);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnUpdateDragCursor(PageIndex, browser,
    operation);
end;

procedure TBasicBrowser.BrowserPluginCrashed(const browser: ICefBrowser;
const pluginPath: ustring);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnPluginCrashed(PageIndex, browser,
    pluginPath);
end;

procedure TBasicBrowser.BrowserPreKeyEvent(const browser: ICefBrowser;
const event: PCefKeyEvent; osEvent: TCefEventHandle;
out isKeyboardShortcut, Result: Boolean);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnPreKeyEvent(PageIndex, browser, event,
    osEvent, isKeyboardShortcut, Result);

  if (event.windows_key_code = 123) and (event.Kind = KEYEVENT_KEYUP) and
    TCustomDcefBrowser(FDcefBrowser).Options.DebugToolAvailable then
    TBrowserPage(FParentBrowserPage).DebugTool; // F12

  if (event.windows_key_code = 116) and (event.Kind = KEYEVENT_KEYUP) then
    TBrowserPage(FParentBrowserPage).ReloadIgnoreCache; // F5

  if (event.windows_key_code = 70) and
    (EVENTFLAG_CONTROL_DOWN in event.modifiers) then
    TBrowserPage(FParentBrowserPage).SearchText; // Ctrl+F

  if (event.windows_key_code = 115) and (EVENTFLAG_ALT_DOWN in event.modifiers)
  then
    Result := True; // Alt+F4 这里得屏蔽掉 不然用户按下这组合键会使整个程序关闭
end;

procedure TBasicBrowser.BrowserProtocolExecution(const browser: ICefBrowser;
const URL: ustring; out allowOsExecution: Boolean);
begin
  allowOsExecution := True;
  TCustomDcefBrowser(FDcefBrowser).doOnProtocolExecution(PageIndex, browser,
    URL, allowOsExecution);
end;

procedure TBasicBrowser.BrowserQuotaRequest(const browser: ICefBrowser;
const originUrl: ustring; newSize: Int64; const callback: ICefQuotaCallback;
out Result: Boolean);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnQuotaRequest(PageIndex, browser,
    originUrl, newSize, callback, Result);
end;

procedure TBasicBrowser.BrowserRequestGeolocationPermission
  (const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer;
const callback: ICefGeolocationCallback);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnRequestGeolocationPermission(PageIndex,
    browser, requestingUrl, requestId, callback);
end;

procedure TBasicBrowser.BrowserResourceRedirect(const browser: ICefBrowser;
const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnResourceRedirect(PageIndex, browser,
    frame, oldUrl, newUrl);
end;

procedure TBasicBrowser.BrowserSetFocus(const browser: ICefBrowser;
source: TCefFocusSource; out Result: Boolean);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnSetFocus(PageIndex, browser,
    source, Result);
end;

procedure TBasicBrowser.BrowserStartDragging(const browser: ICefBrowser;
const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer;
out Result: Boolean);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnStartDragging(PageIndex, browser,
    dragData, allowedOps, x, y, Result);
end;

procedure TBasicBrowser.BrowserStatusMessage(const browser: ICefBrowser;
const value: ustring);
begin
  TCustomDcefBrowser(FDcefBrowser).doOnPageStateChange(PageID,
    BrowserDataChange_StatusMessage, value, Actived);
end;

{ TBrowserPage }

procedure TBrowserPage.AddZoomLevel;
begin
  if browser.host.ZoomLevel < 9 then
    browser.host.ZoomLevel := browser.host.ZoomLevel + 1;
end;

procedure TBrowserPage.BringSearchBarToFront;
begin
  if FCreateByPopup and Assigned(FSearchTextBar) then
    Winapi.Windows.BringWindowToTop(FSearchTextBar.Handle);
end;

procedure TBrowserPage.BrowserPanelResize(Sender: TObject);
begin
  if Assigned(FSearchTextBar) and (FSearchTextBar.Visible) then
    FSearchTextBar.left := FBrowserPanel.Width - FSearchTextBar.Width - 20;

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
    // Tabvisible := False;
  end;
  FBrowserPanel := TDcefB_Panel.Create(FTabsheet);
  with FBrowserPanel do
  begin
    Parent := FTabsheet;
    Align := alClient;
    BevelOuter := bvNone;
    ShowCaption := False;
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
begin
  if Assigned(FDebugBrowserPanel) and (FDebugWinHandle <> 0) then
    MoveBrowserWin(0);
end;

procedure TBrowserPage.DebugTool;
var
  info: TCefWindowInfo;
  setting: TCefBrowserSettings;
  Point: TPoint;
  WinHandle: HWND;
  WindowStyle: Integer;
  FDebugClientHandler: ICefClient;
  FBasicDcefBrowserEvents: TBasicDcefBrowserEvents;
begin
  if Assigned(FDebugBrowserPanel) then
  begin
    FDebugBrowserPanel.Free;
    FDebugBrowserPanel := nil;
    FSplitter.Free;
    FSplitter := nil;
    browser.host.CloseDevTools;
  end
  else
  begin
    FSplitter := TSplitter.Create(FTabsheet);
    with FSplitter do
    begin
      Parent := FTabsheet;
      Align := alBottom;
      Height := 3;
      Cursor := crVSplit;
    end;

    FDebugBrowserPanel := TDcefB_Panel.Create(FTabsheet);
    with FDebugBrowserPanel do
    begin
      Parent := FTabsheet;
      Align := alBottom;
      Height := Trunc(FPageControl.Height * 0.4);
      OnResize := DebugPanelResize;

      if Assigned(FBasicBrowser.FBrowser) then
      begin
        FBasicDcefBrowserEvents := TBasicDcefBrowserEvents.Create;
        FDebugClientHandler := TVCLDcefClientHandler.Create
          (FBasicDcefBrowserEvents, True);

        Point.x := FDebugBrowserPanel.left;
        Point.y := FDebugBrowserPanel.top;
        Point := FPageControl.ClientToScreen(Point);

        FillChar(info, SizeOf(info), 0);
        info.Style := WS_OVERLAPPEDWINDOW or WS_CLIPCHILDREN or
          WS_CLIPSIBLINGS or WS_VISIBLE;
        info.x := Point.x;
        info.y := Point.y;
        info.Width := FDebugBrowserPanel.Width;
        info.Height := FDebugBrowserPanel.Height;
        info.window_name := CefString('DevTools' + inttostr(PageIndex));
        FillChar(setting, SizeOf(setting), 0);
        setting.size := SizeOf(setting);
        browser.host.ShowDevTools(@info, FDebugClientHandler, @setting);

        WinHandle := FindWindow(nil, PChar('DevTools' + inttostr(PageIndex)));
        if WinHandle > 0 then
        begin
          FDebugWinHandle := WinHandle;
          Winapi.Windows.SetParent(WinHandle, FDebugBrowserPanel.Handle);
          SetWindowPos(WinHandle, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
          WindowStyle := GetWindowLong(WinHandle, GWL_STYLE);
          WindowStyle := WindowStyle and (not WS_CAPTION) and (not WS_BORDER)
            and (not WS_THICKFRAME);
          SetWindowLong(WinHandle, GWL_STYLE, WindowStyle);
          MoveBrowserWin(1);
        end;
      end;
    end;
  end;
end;

destructor TBrowserPage.Destroy;
begin
  FBasicBrowser.Free;
  FSearchTextBar.Free;
  FDebugBrowserPanel.Free;
  FBrowserPanel.Free;
  FSplitter.Free;
  FTabsheet.Free;
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

function TBrowserPage.GetIsLoading: Boolean;
begin
  Result := FBasicBrowser.isLoading;
end;

function TBrowserPage.GetPageIndex: Integer;
begin
  Result := TList<TBrowserPage>(FParentItems).IndexOf(Self);
end;

procedure TBrowserPage.GetSource;
begin
  TCustomDcefBrowser(FDcefBrowser).GetSource;
end;

function TBrowserPage.GetUrl: string;
begin
  Result := browser.MainFrame.URL;
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
  if DoubleIndex(browser.host.ZoomLevel, DoubleCases, ZoomIndex) then
    Result := ResultCases[ZoomIndex]
  else
    Result := FloatToStr(browser.host.ZoomLevel);
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
  MyRect := FBrowserPanel.ClientRect;
  case Method of
    0:
      MoveWindow(FBasicBrowser.FBrowser.host.WindowHandle, 0, 0, MyRect.Width,
        MyRect.Height, True);
    1:
      MoveWindow(FBasicBrowser.FBrowser.host.WindowHandle, 0, 0, MyRect.Width,
        MyRect.Height + 1, True);
  end;
end;

procedure TBrowserPage.Print;
begin
  FBasicBrowser.FBrowser.host.Print;
end;

procedure TBrowserPage.ReduceZoomLevel;
begin
  if browser.host.ZoomLevel > -6 then
    browser.host.ZoomLevel := browser.host.ZoomLevel - 1;
end;

procedure TBrowserPage.Reload;
begin
  FBasicBrowser.FBrowser.Reload;
  SetFocus;
end;

procedure TBrowserPage.ReloadIgnoreCache;
begin
  FBasicBrowser.FBrowser.ReloadIgnoreCache;
  SetFocus;
end;

procedure TBrowserPage.ResetZoomLevel;
begin
  browser.host.ZoomLevel := 0;
end;

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

procedure TBrowserPage.SetFocus;
begin
  if Assigned(FBasicBrowser) and (FBasicBrowser.FBrowser <> nil) then
    if FCreateByPopup then
      FBasicBrowser.FBrowser.host.SetFocus(True)
    else
      FBasicBrowser.SetFocus;
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

end.
