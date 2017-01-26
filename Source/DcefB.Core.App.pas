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

{$IFDEF FPC}
{$MODE DELPHI}{$H+}
{$ENDIF}
unit DcefB.Core.App;
{$IFNDEF CPUX64}
{$ALIGN ON}
{$MINENUMSIZE 4}
{$ENDIF}
{$I DcefB.Dcef3.cef.inc}

interface

uses
{$IFDEF DELPHI14_UP}
  Rtti, TypInfo, Variants,
{$ENDIF}
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  Messages,
{$ENDIF}
  SysUtils, Classes, Math, SyncObjs
{$IFDEF MSWINDOWS}
    , Windows
{$ENDIF}
{$IFNDEF FPC}
{$ENDIF},
  DcefB.Cef3.Interfaces, DcefB.Cef3.Types, DcefB.Cef3.Api, DcefB.Cef3.Classes,
  DcefB.res, DcefB.Cef3.Helper, DcefB.Handler.ResourceBundle, DcefB.BaseObject,
  DcefB.Handler.BrowserProcess, DcefB.Handler.RenderProcess, DcefB.Events;

type
  TDcefBApp = class(TInterfacedObject)
  private
    FLibHandle: THandle;
    FCefIsMainProcess: Boolean;

    FCefLibrary: string;
    FCefCache: ustring;
    FCefUserDataPath: ustring;
    FCefUserAgent: ustring;
    FCefProductVersion: ustring;
    FCefLocale: ustring;
    FCefLogFile: ustring;
    FCefLogSeverity: TCefLogSeverity;
    FCefJavaScriptFlags: ustring;
    FCefResourcesDirPath: ustring;
    FCefLocalesDirPath: ustring;
    FCefPackLoadingDisabled: Boolean;
    FCefSingleProcess: Boolean;
    FCefNoSandbox: Boolean;
    FCefBrowserSubprocessPath: ustring;
    FCefCommandLineArgsDisabled: Boolean;
    FCefRemoteDebuggingPort: Integer;
    FCefGetDataResource: TGetDataResource;
    FCefGetLocalizedString: TGetLocalizedString;
    FCefGetDataResourceForScale: TGetDataResourceForScale;
    FCefUncaughtExceptionStackSize: Integer;
    FCefContextSafetyImplementation: Integer;
    FCefPersistSessionCookies: Boolean;
    FCefIgnoreCertificateErrors: Boolean;
    FCefBackgroundColor: TCefColor;
    FCefAcceptLanguageList: ustring;
    FCefWindowsSandboxInfo: Pointer;
    FCefWindowlessRenderingEnabled: Boolean;

    function CefLoadLib(const Cache: ustring = '';
      const UserDataPath: ustring = '';
      const UserAgent: ustring = ''; const ProductVersion: ustring = '';
      const locale: ustring = ''; const LogFile: ustring = '';
      const BrowserSubprocessPath: ustring = '';
      LogSeverity: TCefLogSeverity = LOGSEVERITY_DISABLE;
      JavaScriptFlags: ustring = ''; ResourcesDirPath: ustring = '';
      LocalesDirPath: ustring = ''; SingleProcess: Boolean = False;
      NoSandbox: Boolean = False; CommandLineArgsDisabled: Boolean = False;
      PackLoadingDisabled: Boolean = False; RemoteDebuggingPort: Integer = 0;
      UncaughtExceptionStackSize: Integer = 0;
      ContextSafetyImplementation: Integer = 0;
      persistSessionCookies: Boolean = False;
      IgnoreCertificateErrors: Boolean = False; BackgroundColor: TCefColor = 0;
      const AcceptLanguageList: ustring = '';
      WindowsSandboxInfo: Pointer = nil;
      WindowlessRenderingEnabled: Boolean = False): Boolean;

    function GetOnBeforeCommandLineProcessing: TOnBeforeCommandLineProcessing;
    function GetOnRegisterCustomSchemes: TOnRegisterCustomSchemes;
    procedure SetOnBeforeCommandLineProcessing(const Value
      : TOnBeforeCommandLineProcessing);
    procedure SetOnRegisterCustomSchemes(const Value: TOnRegisterCustomSchemes);
    function GetOnBeforeChildProcessLaunch: TOnBeforeChildProcessLaunch;
    function GetOnBeforeNavigation: TOnBeforeNavigation;
    function GetOnBrowserCreated: TOnBrowserCreated;
    function GetOnBrowserDestroyed: TOnBrowserDestroyed;
    function GetOnContextCreated: TOnContextCreated;
    function GetOnContextInitialized: TOnContextInitialized;
    function GetOnContextReleased: TOnContextReleased;
    function GetOnFocusedNodeChanged: TOnFocusedNodeChanged;
    function GetOnGetDataResource: TOnGetDataResource;
    function GetOnGetLocalizedString: TOnGetLocalizedString;
    function GetOnGetDataResourceForScale: TOnGetDataResourceForScale;
    function GetOnProcessMessageReceived: TOnProcessMessageReceived;
    function GetOnRenderProcessThreadCreated: TOnRenderProcessThreadCreated;
    function GetOnRenderThreadCreated: TOnRenderThreadCreated;
    function GetOnUncaughtException: TOnUncaughtException;
    function GetOnWebKitInitialized: TOnWebKitInitialized;
    procedure SetOnBeforeChildProcessLaunch(const Value
      : TOnBeforeChildProcessLaunch);
    procedure SetOnBeforeNavigation(const Value: TOnBeforeNavigation);
    procedure SetOnBrowserCreated(const Value: TOnBrowserCreated);
    procedure SetOnBrowserDestroyed(const Value: TOnBrowserDestroyed);
    procedure SetOnContextCreated(const Value: TOnContextCreated);
    procedure SetOnContextInitialized(const Value: TOnContextInitialized);
    procedure SetOnContextReleased(const Value: TOnContextReleased);
    procedure SetOnFocusedNodeChanged(const Value: TOnFocusedNodeChanged);
    procedure SetOnGetDataResource(const Value: TOnGetDataResource);
    procedure SetOnGetLocalizedString(const Value: TOnGetLocalizedString);
    procedure SetOnGetDataResourceForScale(const Value: TOnGetDataResourceForScale);
    procedure SetOnProcessMessageReceived(const Value
      : TOnProcessMessageReceived);
    procedure SetOnRenderProcessThreadCreated(const Value
      : TOnRenderProcessThreadCreated);
    procedure SetOnRenderThreadCreated(const Value: TOnRenderThreadCreated);
    procedure SetOnUncaughtException(const Value: TOnUncaughtException);
    procedure SetOnWebKitInitialized(const Value: TOnWebKitInitialized);
  public
    constructor create(); reintroduce;
    destructor Destroy; override;

    function Init: Boolean;
    function IsNeedInitInMainProcess: Boolean;

    function CefBrowserHostCreate(windowInfo: PCefWindowInfo;
      const client: ICefClient; const url: ustring;
      const settings: PCefBrowserSettings;
      const RequestContext: ICefRequestContext): Boolean;
    function CefBrowserHostCreateSync(windowInfo: PCefWindowInfo;
      const client: ICefClient; const url: ustring;
      const settings: PCefBrowserSettings;
      const RequestContext: ICefRequestContext): ICefBrowser;
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    procedure CefDoMessageLoopWork;
    procedure CefRunMessageLoop;
    procedure CefQuitMessageLoop;
    procedure CefSetOsModalLoop(loop: Boolean);
    procedure CefEnableHighDpiSupport;
{$ENDIF}
    procedure CefShutDown;
    function CefRegisterSchemeHandlerFactory(const schemeName,
      HostName: ustring; const Handler: TCefResourceHandlerClass)
      : Boolean; overload;

    function CefRegisterSchemeHandlerFactory(const schemeName,
      HostName: ustring; const factory: ICefSchemeHandlerFactory)
      : Boolean; overload;

    function CefClearSchemeHandlerFactories: Boolean;

    function CefAddCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol,
      TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
    function CefRemoveCrossOriginWhitelistEntry(const SourceOrigin,
      TargetProtocol, TargetDomain: ustring;
      AllowTargetSubdomains: Boolean): Boolean;
    function CefClearCrossOriginWhitelist: Boolean;

    function CefRegisterExtension(const name, code: ustring;
      const Handler: ICefv8Handler): Boolean; overload;
    procedure CefRegisterExtension(const name: string;
      const Value: TValue{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP};
      SyncMainThread: Boolean{$ENDIF}); overload;
    procedure RegisterClasses(const aObjList: array of TClass); overload;
    procedure RegisterClasses(const aRegParList
      : Array of TRegExtentionPar); overload;

    procedure CefVisitWebPluginInfo(const visitor: ICefWebPluginInfoVisitor);
    procedure CefVisitWebPluginInfoProc(const visitor
      : TCefWebPluginInfoVisitorProc);
    procedure CefRefreshWebPlugins;
    procedure CefAddWebPluginPath(const path: ustring);
    procedure CefAddWebPluginDirectory(const dir: ustring);
    procedure CefRemoveWebPluginPath(const path: ustring);
    procedure CefUnregisterInternalWebPlugin(const path: ustring);
    procedure CefForceWebPluginShutdown(const path: ustring);
    procedure CefRegisterWebPluginCrash(const path: ustring);
    procedure CefIsWebPluginUnstable(const path: ustring;
      const callback: ICefWebPluginUnstableCallback);
    procedure CefIsWebPluginUnstableProc(const path: ustring;
      const callback: TCefWebPluginIsUnstableProc);

    // -----------------
    property CefLibrary: string read FCefLibrary write FCefLibrary;
    property CefCache: ustring read FCefCache write FCefCache;
    property CefUserAgent: ustring read FCefUserAgent write FCefUserAgent;
    property CefProductVersion: ustring read FCefProductVersion
      write FCefProductVersion;
    property CefLocale: ustring read FCefLocale write FCefLocale;
    property CefLogFile: ustring read FCefLogFile write FCefLogFile;
    property CefLogSeverity: TCefLogSeverity read FCefLogSeverity
      write FCefLogSeverity;
    property CefJavaScriptFlags: ustring read FCefJavaScriptFlags
      write FCefJavaScriptFlags;
    property CefResourcesDirPath: ustring read FCefResourcesDirPath
      write FCefResourcesDirPath;
    property CefLocalesDirPath: ustring read FCefLocalesDirPath
      write FCefLocalesDirPath;
    property CefPackLoadingDisabled: Boolean read FCefPackLoadingDisabled
      write FCefPackLoadingDisabled;
    property CefSingleProcess: Boolean read FCefSingleProcess
      write FCefSingleProcess;
    property CefNoSandbox: Boolean read FCefNoSandbox write FCefNoSandbox;
    property CefBrowserSubprocessPath: ustring read FCefBrowserSubprocessPath
      write FCefBrowserSubprocessPath;
    property CefCommandLineArgsDisabled: Boolean
      read FCefCommandLineArgsDisabled write FCefCommandLineArgsDisabled;
    property CefRemoteDebuggingPort: Integer read FCefRemoteDebuggingPort
      write FCefRemoteDebuggingPort;
    property CefGetDataResource: TGetDataResource read FCefGetDataResource
      write FCefGetDataResource;
    property CefGetLocalizedString: TGetLocalizedString
      read FCefGetLocalizedString write FCefGetLocalizedString;
    property CefUncaughtExceptionStackSize: Integer
      read FCefUncaughtExceptionStackSize write FCefUncaughtExceptionStackSize;
    property CefContextSafetyImplementation: Integer
      read FCefContextSafetyImplementation
      write FCefContextSafetyImplementation;
    property CefPersistSessionCookies: Boolean read FCefPersistSessionCookies
      write FCefPersistSessionCookies;
    property CefIgnoreCertificateErrors: Boolean
      read FCefIgnoreCertificateErrors write FCefIgnoreCertificateErrors;
    property CefBackgroundColor: TCefColor read FCefBackgroundColor
      write FCefBackgroundColor;
    property CefWindowsSandboxInfo: Pointer read FCefWindowsSandboxInfo
      write FCefWindowsSandboxInfo;
    property CefWindowlessRenderingEnabled: Boolean
      read FCefWindowlessRenderingEnabled write FCefWindowlessRenderingEnabled;
    property CefAcceptLanguageList: ustring read FCefAcceptLanguageList write FCefAcceptLanguageList;

    property OnBeforeCommandLineProcessing: TOnBeforeCommandLineProcessing
      read GetOnBeforeCommandLineProcessing
      write SetOnBeforeCommandLineProcessing;
    property OnRegisterCustomSchemes: TOnRegisterCustomSchemes
      read GetOnRegisterCustomSchemes write SetOnRegisterCustomSchemes;

    property OnGetDataResource: TOnGetDataResource read GetOnGetDataResource
      write SetOnGetDataResource;
    property OnGetLocalizedString: TOnGetLocalizedString
      read GetOnGetLocalizedString write SetOnGetLocalizedString;
    property OnGetDataResourceForScale: TOnGetDataResourceForScale
      read GetOnGetDataResourceForScale write SetOnGetDataResourceForScale;

    property OnContextInitialized: TOnContextInitialized
      read GetOnContextInitialized write SetOnContextInitialized;
    property OnBeforeChildProcessLaunch: TOnBeforeChildProcessLaunch
      read GetOnBeforeChildProcessLaunch write SetOnBeforeChildProcessLaunch;
    property OnRenderProcessThreadCreated: TOnRenderProcessThreadCreated
      read GetOnRenderProcessThreadCreated
      write SetOnRenderProcessThreadCreated;

    property OnRenderThreadCreated: TOnRenderThreadCreated
      read GetOnRenderThreadCreated write SetOnRenderThreadCreated;
    property OnWebKitInitialized: TOnWebKitInitialized
      read GetOnWebKitInitialized write SetOnWebKitInitialized;
    property OnBrowserCreated: TOnBrowserCreated read GetOnBrowserCreated
      write SetOnBrowserCreated;
    property OnBrowserDestroyed: TOnBrowserDestroyed read GetOnBrowserDestroyed
      write SetOnBrowserDestroyed;
    property OnBeforeNavigation: TOnBeforeNavigation read GetOnBeforeNavigation
      write SetOnBeforeNavigation;
    property OnContextCreated: TOnContextCreated read GetOnContextCreated
      write SetOnContextCreated;
    property OnContextReleased: TOnContextReleased read GetOnContextReleased
      write SetOnContextReleased;
    property OnUncaughtException: TOnUncaughtException
      read GetOnUncaughtException write SetOnUncaughtException;
    property OnFocusedNodeChanged: TOnFocusedNodeChanged
      read GetOnFocusedNodeChanged write SetOnFocusedNodeChanged;
    property OnProcessMessageReceived: TOnProcessMessageReceived
      read GetOnProcessMessageReceived write SetOnProcessMessageReceived;
  end;

var
  DcefBApp: TDcefBApp;

implementation

var
  InnerDcefBResourceBundleHandler: IDcefBResourceBundleHandler;
  InnerDcefBBrowserProcessHandler: IDcefBBrowserProcessHandler;
  InnerDcefBRenderProcessHandler: IDcefBRenderProcessHandler;
  InnerCefOnBeforeCommandLineProcessing: TOnBeforeCommandLineProcessing;
  InnerCefOnRegisterCustomSchemes: TOnRegisterCustomSchemes;

type
  TInternalApp = class(TCefAppOwn)
  protected
    procedure OnBeforeCommandLineProcessing(const processType: ustring;
      const commandLine: ICefCommandLine); override;
    procedure OnRegisterCustomSchemes(const registrar
      : ICefSchemeRegistrar); override;
    function GetResourceBundleHandler: ICefResourceBundleHandler; override;
    function GetBrowserProcessHandler: ICefBrowserProcessHandler; override;
    function GetRenderProcessHandler: ICefRenderProcessHandler; override;
  end;

  { TDcefBApp }

function TDcefBApp.IsNeedInitInMainProcess: Boolean;
begin
  Result := (Not FCefSingleProcess) and (FLibHandle = 0);
end;

procedure TDcefBApp.RegisterClasses(const aRegParList
  : Array of TRegExtentionPar);
var
  i, J, C: Integer;
  AFound: Boolean;
  AItem: TRegExtentionPar;
begin
  C := Length(TDcefBRenderProcessHandler.RegParList);
  SetLength(TDcefBRenderProcessHandler.RegParList, C + Length(aRegParList));
  for i := 0 to High(aRegParList) do
  begin
    AFound := False;
    for J := 0 to C - 1 do
    begin
      AItem := TDcefBRenderProcessHandler.RegParList[J];
      if (AItem.name = aRegParList[i].name) and
        (AItem.code = aRegParList[i].code) then
      begin
        AFound := True;
        Break;
      end;
    end;
    if not AFound then
    begin
      TDcefBRenderProcessHandler.RegParList[C] := aRegParList[i];
      Inc(C);
    end;
  end;
  SetLength(TDcefBRenderProcessHandler.RegParList, C);
end;

procedure TDcefBApp.RegisterClasses(const aObjList: array of TClass);
var
  aRegParList: Array of TRegExtentionPar;
  Index: Integer;
begin
  SetLength(aRegParList, Length(aObjList));
  for Index := Low(aObjList) to High(aObjList) do
  begin
    aRegParList[Index].name := aObjList[Index].ClassName;
    aRegParList[Index].code := '';
    aRegParList[Index].Handler := nil;
    aRegParList[Index].Value := aObjList[Index];
  end;
  RegisterClasses(aRegParList);
  SetLength(aRegParList, 0);
end;

procedure TDcefBApp.SetOnBeforeChildProcessLaunch
  (const Value: TOnBeforeChildProcessLaunch);
begin
  InnerDcefBBrowserProcessHandler.SetOnBeforeChildProcessLaunch(Value);
end;

procedure TDcefBApp.SetOnBeforeCommandLineProcessing
  (const Value: TOnBeforeCommandLineProcessing);
begin
  InnerCefOnBeforeCommandLineProcessing := Value;
end;

procedure TDcefBApp.SetOnBeforeNavigation(const Value: TOnBeforeNavigation);
begin
  InnerDcefBRenderProcessHandler.SetOnBeforeNavigation(Value);
end;

procedure TDcefBApp.SetOnBrowserCreated(const Value: TOnBrowserCreated);
begin
  InnerDcefBRenderProcessHandler.SetOnBrowserCreated(Value);
end;

procedure TDcefBApp.SetOnBrowserDestroyed(const Value: TOnBrowserDestroyed);
begin
  InnerDcefBRenderProcessHandler.SetOnBrowserDestroyed(Value);
end;

procedure TDcefBApp.SetOnContextCreated(const Value: TOnContextCreated);
begin
  InnerDcefBRenderProcessHandler.SetOnContextCreated(Value);
end;

procedure TDcefBApp.SetOnContextInitialized(const Value: TOnContextInitialized);
begin
  InnerDcefBBrowserProcessHandler.SetOnContextInitialized(Value);
end;

procedure TDcefBApp.SetOnContextReleased(const Value: TOnContextReleased);
begin
  InnerDcefBRenderProcessHandler.SetOnContextReleased(Value);
end;

procedure TDcefBApp.SetOnFocusedNodeChanged(const Value: TOnFocusedNodeChanged);
begin
  InnerDcefBRenderProcessHandler.SetOnFocusedNodeChanged(Value);
end;

procedure TDcefBApp.SetOnGetDataResource(const Value: TOnGetDataResource);
begin
  InnerDcefBResourceBundleHandler.SetOnGetDataResource(Value);
end;

procedure TDcefBApp.SetOnGetDataResourceForScale(
  const Value: TOnGetDataResourceForScale);
begin
  InnerDcefBResourceBundleHandler.SetOnGetDataResourceForScale(Value);
end;

procedure TDcefBApp.SetOnGetLocalizedString(const Value: TOnGetLocalizedString);
begin
  InnerDcefBResourceBundleHandler.SetOnGetLocalizedString(Value);
end;

procedure TDcefBApp.SetOnProcessMessageReceived(const Value
  : TOnProcessMessageReceived);
begin
  InnerDcefBRenderProcessHandler.SetOnProcessMessageReceived(Value);
end;

procedure TDcefBApp.SetOnRegisterCustomSchemes(const Value
  : TOnRegisterCustomSchemes);
begin
  InnerCefOnRegisterCustomSchemes := Value;
end;

procedure TDcefBApp.SetOnRenderProcessThreadCreated
  (const Value: TOnRenderProcessThreadCreated);
begin
  InnerDcefBBrowserProcessHandler.SetOnRenderProcessThreadCreated(Value);
end;

procedure TDcefBApp.SetOnRenderThreadCreated(const Value
  : TOnRenderThreadCreated);
begin
  InnerDcefBRenderProcessHandler.SetOnRenderThreadCreated(Value);
end;

procedure TDcefBApp.SetOnUncaughtException(const Value: TOnUncaughtException);
begin
  InnerDcefBRenderProcessHandler.SetOnUncaughtException(Value);
end;

procedure TDcefBApp.SetOnWebKitInitialized(const Value: TOnWebKitInitialized);
begin
  InnerDcefBRenderProcessHandler.SetOnWebKitInitialized(Value);
end;

constructor TDcefBApp.create;
begin
  FLibHandle := 0;
  FCefIsMainProcess := False;

  FCefLibrary := 'libcef.dll';
  FCefCache := '';
  FCefUserDataPath := '';
  FCefUserAgent := '';
  FCefProductVersion := '';
  FCefLocale := '';
  FCefLogFile := '';
  FCefLogSeverity := TCefLogSeverity.LOGSEVERITY_DISABLE;
  FCefJavaScriptFlags := '';
  FCefResourcesDirPath := '';
  FCefLocalesDirPath := '';
  FCefPackLoadingDisabled := False;
  FCefSingleProcess := True;
  FCefNoSandbox := False;
  FCefBrowserSubprocessPath := '';
  FCefCommandLineArgsDisabled := False;
  FCefRemoteDebuggingPort := 0;
  FCefGetDataResource := nil;
  FCefGetLocalizedString := nil;
  FCefUncaughtExceptionStackSize := 0;
  FCefContextSafetyImplementation := 0;
  FCefPersistSessionCookies := False;
  FCefIgnoreCertificateErrors := False;
  FCefBackgroundColor := 0;
  FCefAcceptLanguageList := '';
  FCefWindowsSandboxInfo := nil;
  FCefWindowlessRenderingEnabled := False;
end;

destructor TDcefBApp.Destroy;
begin

  inherited;
end;

function TDcefBApp.GetOnBeforeChildProcessLaunch: TOnBeforeChildProcessLaunch;
begin
  Result := InnerDcefBBrowserProcessHandler.GetOnBeforeChildProcessLaunch;
end;

function TDcefBApp.GetOnBeforeCommandLineProcessing
  : TOnBeforeCommandLineProcessing;
begin
  Result := InnerCefOnBeforeCommandLineProcessing;
end;

function TDcefBApp.GetOnBeforeNavigation: TOnBeforeNavigation;
begin
  Result := InnerDcefBRenderProcessHandler.GetOnBeforeNavigation;
end;

function TDcefBApp.GetOnBrowserCreated: TOnBrowserCreated;
begin
  Result := InnerDcefBRenderProcessHandler.GetOnBrowserCreated;
end;

function TDcefBApp.GetOnBrowserDestroyed: TOnBrowserDestroyed;
begin
  Result := InnerDcefBRenderProcessHandler.GetOnBrowserDestroyed;
end;

function TDcefBApp.GetOnContextCreated: TOnContextCreated;
begin
  Result := InnerDcefBRenderProcessHandler.GetOnContextCreated;
end;

function TDcefBApp.GetOnContextInitialized: TOnContextInitialized;
begin
  Result := InnerDcefBBrowserProcessHandler.GetOnContextInitialized;
end;

function TDcefBApp.GetOnContextReleased: TOnContextReleased;
begin
  Result := InnerDcefBRenderProcessHandler.GetOnContextReleased;
end;

function TDcefBApp.GetOnFocusedNodeChanged: TOnFocusedNodeChanged;
begin
  Result := InnerDcefBRenderProcessHandler.GetOnFocusedNodeChanged;
end;

function TDcefBApp.GetOnGetDataResource: TOnGetDataResource;
begin
  Result := InnerDcefBResourceBundleHandler.GetOnGetDataResource;
end;

function TDcefBApp.GetOnGetDataResourceForScale: TOnGetDataResourceForScale;
begin
  Result := InnerDcefBResourceBundleHandler.GetOnGetDataResourceForScale;
end;

function TDcefBApp.GetOnGetLocalizedString: TOnGetLocalizedString;
begin
  Result := InnerDcefBResourceBundleHandler.GetOnGetLocalizedString;
end;

function TDcefBApp.GetOnProcessMessageReceived: TOnProcessMessageReceived;
begin
  Result := InnerDcefBRenderProcessHandler.GetOnProcessMessageReceived;
end;

function TDcefBApp.GetOnRegisterCustomSchemes: TOnRegisterCustomSchemes;
begin
  Result := InnerCefOnRegisterCustomSchemes;
end;

function TDcefBApp.GetOnRenderProcessThreadCreated
  : TOnRenderProcessThreadCreated;
begin
  Result := InnerDcefBBrowserProcessHandler.GetOnRenderProcessThreadCreated;
end;

function TDcefBApp.GetOnRenderThreadCreated: TOnRenderThreadCreated;
begin
  Result := InnerDcefBRenderProcessHandler.GetOnRenderThreadCreated;
end;

function TDcefBApp.GetOnUncaughtException: TOnUncaughtException;
begin
  Result := InnerDcefBRenderProcessHandler.GetOnUncaughtException;
end;

function TDcefBApp.GetOnWebKitInitialized: TOnWebKitInitialized;
begin
  Result := InnerDcefBRenderProcessHandler.GetOnWebKitInitialized;
end;

function TDcefBApp.Init: Boolean;
begin
  Result := CefLoadLib(FCefCache, FCefUserDataPath, FCefUserAgent, FCefProductVersion, FCefLocale,
    FCefLogFile, FCefBrowserSubprocessPath, FCefLogSeverity,
    FCefJavaScriptFlags, FCefResourcesDirPath, FCefLocalesDirPath,
    FCefSingleProcess, FCefNoSandbox, FCefCommandLineArgsDisabled,
    FCefPackLoadingDisabled, FCefRemoteDebuggingPort,
    FCefUncaughtExceptionStackSize, FCefContextSafetyImplementation,
    FCefPersistSessionCookies, FCefIgnoreCertificateErrors, FCefBackgroundColor, FCefAcceptLanguageList,
    FCefWindowsSandboxInfo, FCefWindowlessRenderingEnabled);
end;

function TDcefBApp.CefBrowserHostCreate(windowInfo: PCefWindowInfo;
  const client: ICefClient; const url: ustring;
  const settings: PCefBrowserSettings;
  const RequestContext: ICefRequestContext): Boolean;
var
  u: TCefString;
begin
  if FLibHandle = 0 then
    raise exception.create(EXP_CEFLIBNOTLOAD);

  u := TCef3Helper.CefString(url);
  Result := cef_browser_host_create_browser(windowInfo,
    TCef3Helper.CefGetData(client), @u, settings,
    TCef3Helper.CefGetData(RequestContext)) <> 0;
end;

function TDcefBApp.CefBrowserHostCreateSync(windowInfo: PCefWindowInfo;
  const client: ICefClient; const url: ustring;
  const settings: PCefBrowserSettings; const RequestContext: ICefRequestContext)
  : ICefBrowser;
var
  u: TCefString;
begin
  if FLibHandle = 0 then
    raise exception.create(EXP_CEFLIBNOTLOAD);
  u := TCef3Helper.CefString(url);
  Result := TCefBrowserRef.UnWrap(cef_browser_host_create_browser_sync
    (windowInfo, TCef3Helper.CefGetData(client), @u, settings,
    TCef3Helper.CefGetData(RequestContext)));
end;

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}

procedure TDcefBApp.CefDoMessageLoopWork;
begin
  if FLibHandle > 0 then
    cef_do_message_loop_work;
end;

procedure TDcefBApp.CefEnableHighDpiSupport;
begin
  cef_enable_highdpi_support();
end;

procedure TDcefBApp.CefRunMessageLoop;
begin
  if FLibHandle > 0 then
    cef_run_message_loop;
end;

procedure TDcefBApp.CefQuitMessageLoop;
begin
  cef_quit_message_loop;
end;

procedure TDcefBApp.CefSetOsModalLoop(loop: Boolean);
begin
  cef_set_osmodal_loop(ord(loop));
end;
{$ENDIF}

procedure TDcefBApp.CefShutDown;
begin
  if FLibHandle <> 0 then
  begin
    if FCefIsMainProcess then
      cef_shutdown;
    FreeLibrary(FLibHandle);
    FLibHandle := 0;
  end;
end;

function TDcefBApp.CefRegisterSchemeHandlerFactory(const schemeName,
  HostName: ustring; const Handler: TCefResourceHandlerClass): Boolean;
var
  s, h: TCefString;
begin
  if FLibHandle = 0 then
    raise exception.create(EXP_CEFLIBNOTLOAD);
  s := TCef3Helper.CefString(schemeName);
  h := TCef3Helper.CefString(HostName);
  Result := cef_register_scheme_handler_factory(@s, @h,
    TCef3Helper.CefGetData(TCefSchemeHandlerFactoryOwn.create(Handler)
    as ICefBase)) <> 0;
end;

procedure TDcefBApp.CefRegisterExtension(const name: string;
  const Value: TValue);
begin
  CefRegisterExtension(name,
    format('__defineSetter__(''%s'', function(v){native function $s();$s(v)});__defineGetter__(''%0:s'', function(){native function $g();return $g()});',
    [name]), TCefRTTIExtension.create(Value
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    , SyncMainThread
{$ENDIF}
    ) as ICefv8Handler);
end;

function TDcefBApp.CefRegisterSchemeHandlerFactory(const schemeName,
  HostName: ustring; const factory: ICefSchemeHandlerFactory): Boolean;
var
  s, h: TCefString;
begin
  if FLibHandle = 0 then
    raise exception.create(EXP_CEFLIBNOTLOAD);
  s := TCef3Helper.CefString(schemeName);
  h := TCef3Helper.CefString(HostName);
  Result := cef_register_scheme_handler_factory(@s, @h,
    TCef3Helper.CefGetData(factory as ICefBase)) <> 0;
end;

function TDcefBApp.CefClearSchemeHandlerFactories: Boolean;
begin
  if FLibHandle = 0 then
    raise exception.create(EXP_CEFLIBNOTLOAD);
  Result := cef_clear_scheme_handler_factories <> 0;
end;

function TDcefBApp.CefAddCrossOriginWhitelistEntry(const SourceOrigin,
  TargetProtocol, TargetDomain: ustring;
  AllowTargetSubdomains: Boolean): Boolean;
var
  so, tp, td: TCefString;
begin
  if FLibHandle = 0 then
    raise exception.create(EXP_CEFLIBNOTLOAD);
  so := TCef3Helper.CefString(SourceOrigin);
  tp := TCef3Helper.CefString(TargetProtocol);
  td := TCef3Helper.CefString(TargetDomain);
  Result := cef_add_cross_origin_whitelist_entry(@so, @tp, @td,
    ord(AllowTargetSubdomains)) <> 0;
end;

function TDcefBApp.CefRemoveCrossOriginWhitelistEntry(const SourceOrigin,
  TargetProtocol, TargetDomain: ustring;
  AllowTargetSubdomains: Boolean): Boolean;
var
  so, tp, td: TCefString;
begin
  if FLibHandle = 0 then
    raise exception.create(EXP_CEFLIBNOTLOAD);
  so := TCef3Helper.CefString(SourceOrigin);
  tp := TCef3Helper.CefString(TargetProtocol);
  td := TCef3Helper.CefString(TargetDomain);
  Result := cef_remove_cross_origin_whitelist_entry(@so, @tp, @td,
    ord(AllowTargetSubdomains)) <> 0;
end;

function TDcefBApp.CefClearCrossOriginWhitelist: Boolean;
begin
  if FLibHandle = 0 then
    raise exception.create(EXP_CEFLIBNOTLOAD);
  Result := cef_clear_cross_origin_whitelist <> 0;
end;

function TDcefBApp.CefRegisterExtension(const name, code: ustring;
  const Handler: ICefv8Handler): Boolean;
var
  n, C: TCefString;
begin
  if FLibHandle = 0 then
    raise exception.create(EXP_CEFLIBNOTLOAD);
  n := TCef3Helper.CefString(name);
  C := TCef3Helper.CefString(code);
  Result := cef_register_extension(@n, @C,
    TCef3Helper.CefGetData(Handler)) <> 0;
end;

procedure TDcefBApp.CefVisitWebPluginInfo(const visitor
  : ICefWebPluginInfoVisitor);
begin
  cef_visit_web_plugin_info(TCef3Helper.CefGetData(visitor));
end;

procedure TDcefBApp.CefVisitWebPluginInfoProc(const visitor
  : TCefWebPluginInfoVisitorProc);
begin
  CefVisitWebPluginInfo(TCefFastWebPluginInfoVisitor.create(visitor));
end;

procedure TDcefBApp.CefRefreshWebPlugins;
begin
  cef_refresh_web_plugins();
end;

procedure TDcefBApp.CefAddWebPluginPath(const path: ustring);
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(path);
  cef_add_web_plugin_path(@p);
end;

procedure TDcefBApp.CefAddWebPluginDirectory(const dir: ustring);
var
  d: TCefString;
begin
  d := TCef3Helper.CefString(dir);
  cef_add_web_plugin_directory(@d);
end;

procedure TDcefBApp.CefRemoveWebPluginPath(const path: ustring);
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(path);
  cef_remove_web_plugin_path(@p);
end;

procedure TDcefBApp.CefUnregisterInternalWebPlugin(const path: ustring);
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(path);
  cef_unregister_internal_web_plugin(@p);
end;

procedure TDcefBApp.CefForceWebPluginShutdown(const path: ustring);
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(path);
  cef_force_web_plugin_shutdown(@p);
end;


procedure TDcefBApp.CefRegisterWebPluginCrash(const path: ustring);
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(path);
  cef_register_web_plugin_crash(@p);
end;

procedure TDcefBApp.CefIsWebPluginUnstable(const path: ustring;
  const callback: ICefWebPluginUnstableCallback);
var
  p: TCefString;
begin
  p := TCef3Helper.CefString(path);
  cef_is_web_plugin_unstable(@p, TCef3Helper.CefGetData(callback));
end;

procedure TDcefBApp.CefIsWebPluginUnstableProc(const path: ustring;
  const callback: TCefWebPluginIsUnstableProc);
begin
  CefIsWebPluginUnstable(path, TCefFastWebPluginUnstableCallback.create
    (callback));
end;

function TDcefBApp.CefLoadLib(const Cache: ustring = '';
      const UserDataPath: ustring = '';
      const UserAgent: ustring = ''; const ProductVersion: ustring = '';
      const locale: ustring = ''; const LogFile: ustring = '';
      const BrowserSubprocessPath: ustring = '';
      LogSeverity: TCefLogSeverity = LOGSEVERITY_DISABLE;
      JavaScriptFlags: ustring = ''; ResourcesDirPath: ustring = '';
      LocalesDirPath: ustring = ''; SingleProcess: Boolean = False;
      NoSandbox: Boolean = False; CommandLineArgsDisabled: Boolean = False;
      PackLoadingDisabled: Boolean = False; RemoteDebuggingPort: Integer = 0;
      UncaughtExceptionStackSize: Integer = 0;
      ContextSafetyImplementation: Integer = 0;
      persistSessionCookies: Boolean = False;
      IgnoreCertificateErrors: Boolean = False; BackgroundColor: TCefColor = 0;
      const AcceptLanguageList: ustring = '';
      WindowsSandboxInfo: Pointer = nil;
      WindowlessRenderingEnabled: Boolean = False): Boolean;
var
  settings: TCefSettings;
  errcode: Integer;
  App: ICefApp;
begin
  if FLibHandle = 0 then
  begin
    // deactivate FPU exception FPU & SSE2
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
      exUnderflow, exPrecision]);

    FLibHandle := LoadLibrary(PChar(CefLibrary));
    if FLibHandle = 0 then
      RaiseLastOSError;

    cef_string_wide_set := GetProcAddress(FLibHandle, 'cef_string_wide_set');
    cef_string_utf8_set := GetProcAddress(FLibHandle, 'cef_string_utf8_set');
    cef_string_utf16_set := GetProcAddress(FLibHandle, 'cef_string_utf16_set');
    cef_string_wide_clear := GetProcAddress(FLibHandle,
      'cef_string_wide_clear');
    cef_string_utf8_clear := GetProcAddress(FLibHandle,
      'cef_string_utf8_clear');
    cef_string_utf16_clear := GetProcAddress(FLibHandle,
      'cef_string_utf16_clear');
    cef_string_wide_cmp := GetProcAddress(FLibHandle, 'cef_string_wide_cmp');
    cef_string_utf8_cmp := GetProcAddress(FLibHandle, 'cef_string_utf8_cmp');
    cef_string_utf16_cmp := GetProcAddress(FLibHandle, 'cef_string_utf16_cmp');
    cef_string_wide_to_utf8 := GetProcAddress(FLibHandle,
      'cef_string_wide_to_utf8');
    cef_string_utf8_to_wide := GetProcAddress(FLibHandle,
      'cef_string_utf8_to_wide');
    cef_string_wide_to_utf16 := GetProcAddress(FLibHandle,
      'cef_string_wide_to_utf16');
    cef_string_utf16_to_wide := GetProcAddress(FLibHandle,
      'cef_string_utf16_to_wide');
    cef_string_utf8_to_utf16 := GetProcAddress(FLibHandle,
      'cef_string_utf8_to_utf16');
    cef_string_utf16_to_utf8 := GetProcAddress(FLibHandle,
      'cef_string_utf16_to_utf8');
    cef_string_ascii_to_wide := GetProcAddress(FLibHandle,
      'cef_string_ascii_to_wide');
    cef_string_ascii_to_utf16 := GetProcAddress(FLibHandle,
      'cef_string_ascii_to_utf16');
    cef_string_userfree_wide_alloc := GetProcAddress(FLibHandle,
      'cef_string_userfree_wide_alloc');
    cef_string_userfree_utf8_alloc := GetProcAddress(FLibHandle,
      'cef_string_userfree_utf8_alloc');
    cef_string_userfree_utf16_alloc := GetProcAddress(FLibHandle,
      'cef_string_userfree_utf16_alloc');
    cef_string_userfree_wide_free := GetProcAddress(FLibHandle,
      'cef_string_userfree_wide_free');
    cef_string_userfree_utf8_free := GetProcAddress(FLibHandle,
      'cef_string_userfree_utf8_free');
    cef_string_userfree_utf16_free := GetProcAddress(FLibHandle,
      'cef_string_userfree_utf16_free');

{$IFDEF CEF_STRING_TYPE_UTF8}
    cef_string_set := cef_string_utf8_set;
    cef_string_clear := cef_string_utf8_clear;
    cef_string_userfree_alloc := cef_string_userfree_utf8_alloc;
    cef_string_userfree_free := cef_string_userfree_utf8_free;
    cef_string_from_ascii := cef_string_utf8_copy;
    cef_string_to_utf8 := cef_string_utf8_copy;
    cef_string_from_utf8 := cef_string_utf8_copy;
    cef_string_to_utf16 := cef_string_utf8_to_utf16;
    cef_string_from_utf16 := cef_string_utf16_to_utf8;
    cef_string_to_wide := cef_string_utf8_to_wide;
    cef_string_from_wide := cef_string_wide_to_utf8;
{$ENDIF}
{$IFDEF CEF_STRING_TYPE_UTF16}
    cef_string_set := cef_string_utf16_set;
    cef_string_clear := cef_string_utf16_clear;
    cef_string_userfree_alloc := cef_string_userfree_utf16_alloc;
    cef_string_userfree_free := cef_string_userfree_utf16_free;
    cef_string_from_ascii := cef_string_ascii_to_utf16;
    cef_string_to_utf8 := cef_string_utf16_to_utf8;
    cef_string_from_utf8 := cef_string_utf8_to_utf16;
    cef_string_to_utf16 := cef_string_utf16_copy;
    cef_string_from_utf16 := cef_string_utf16_copy;
    cef_string_to_wide := cef_string_utf16_to_wide;
    cef_string_from_wide := cef_string_wide_to_utf16;
{$ENDIF}
{$IFDEF CEF_STRING_TYPE_WIDE}
    cef_string_set := cef_string_wide_set;
    cef_string_clear := cef_string_wide_clear;
    cef_string_userfree_alloc := cef_string_userfree_wide_alloc;
    cef_string_userfree_free := cef_string_userfree_wide_free;
    cef_string_from_ascii := cef_string_ascii_to_wide;
    cef_string_to_utf8 := cef_string_wide_to_utf8;
    cef_string_from_utf8 := cef_string_utf8_to_wide;
    cef_string_to_utf16 := cef_string_wide_to_utf16;
    cef_string_from_utf16 := cef_string_utf16_to_wide;
    cef_string_to_wide := cef_string_wide_copy;
    cef_string_from_wide := cef_string_wide_copy;
{$ENDIF}
    cef_string_map_alloc := GetProcAddress(FLibHandle, 'cef_string_map_alloc');
    cef_string_map_size := GetProcAddress(FLibHandle, 'cef_string_map_size');
    cef_string_map_find := GetProcAddress(FLibHandle, 'cef_string_map_find');
    cef_string_map_key := GetProcAddress(FLibHandle, 'cef_string_map_key');
    cef_string_map_value := GetProcAddress(FLibHandle, 'cef_string_map_value');
    cef_string_map_append := GetProcAddress(FLibHandle,
      'cef_string_map_append');
    cef_string_map_clear := GetProcAddress(FLibHandle, 'cef_string_map_clear');
    cef_string_map_free := GetProcAddress(FLibHandle, 'cef_string_map_free');
    cef_string_list_alloc := GetProcAddress(FLibHandle,
      'cef_string_list_alloc');
    cef_string_list_size := GetProcAddress(FLibHandle, 'cef_string_list_size');
    cef_string_list_value := GetProcAddress(FLibHandle,
      'cef_string_list_value');
    cef_string_list_append := GetProcAddress(FLibHandle,
      'cef_string_list_append');
    cef_string_list_clear := GetProcAddress(FLibHandle,
      'cef_string_list_clear');
    cef_string_list_free := GetProcAddress(FLibHandle, 'cef_string_list_free');
    cef_string_list_copy := GetProcAddress(FLibHandle, 'cef_string_list_copy');
    cef_initialize := GetProcAddress(FLibHandle, 'cef_initialize');
    cef_execute_process := GetProcAddress(FLibHandle, 'cef_execute_process');
    cef_shutdown := GetProcAddress(FLibHandle, 'cef_shutdown');
    cef_do_message_loop_work := GetProcAddress(FLibHandle,
      'cef_do_message_loop_work');
    cef_run_message_loop := GetProcAddress(FLibHandle, 'cef_run_message_loop');
    cef_quit_message_loop := GetProcAddress(FLibHandle,
      'cef_quit_message_loop');
    cef_set_osmodal_loop := GetProcAddress(FLibHandle, 'cef_set_osmodal_loop');
    cef_enable_highdpi_support := GetProcAddress(FLibHandle, 'cef_enable_highdpi_support');
    cef_register_extension := GetProcAddress(FLibHandle,
      'cef_register_extension');
    cef_register_scheme_handler_factory := GetProcAddress(FLibHandle,
      'cef_register_scheme_handler_factory');
    cef_clear_scheme_handler_factories := GetProcAddress(FLibHandle,
      'cef_clear_scheme_handler_factories');
    cef_add_cross_origin_whitelist_entry := GetProcAddress(FLibHandle,
      'cef_add_cross_origin_whitelist_entry');
    cef_remove_cross_origin_whitelist_entry :=
      GetProcAddress(FLibHandle, 'cef_remove_cross_origin_whitelist_entry');
    cef_clear_cross_origin_whitelist := GetProcAddress(FLibHandle,
      'cef_clear_cross_origin_whitelist');
    cef_currently_on := GetProcAddress(FLibHandle, 'cef_currently_on');
    cef_post_task := GetProcAddress(FLibHandle, 'cef_post_task');
    cef_post_delayed_task := GetProcAddress(FLibHandle,
      'cef_post_delayed_task');
    cef_parse_url := GetProcAddress(FLibHandle, 'cef_parse_url');
    cef_create_url := GetProcAddress(FLibHandle, 'cef_create_url');
    cef_format_url_for_security_display := GetProcAddress(FLibHandle, 'cef_format_url_for_security_display');
    cef_get_mime_type := GetProcAddress(FLibHandle, 'cef_get_mime_type');
    cef_get_extensions_for_mime_type := GetProcAddress(FLibHandle,
      'cef_get_extensions_for_mime_type');
    cef_base64encode := GetProcAddress(FLibHandle, 'cef_base64encode');
    cef_base64decode := GetProcAddress(FLibHandle, 'cef_base64decode');
    cef_uriencode := GetProcAddress(FLibHandle, 'cef_uriencode');
    cef_uridecode := GetProcAddress(FLibHandle, 'cef_uridecode');	
    cef_parse_csscolor := GetProcAddress(FLibHandle, 'cef_parse_csscolor');
{$ifdef Win32}
    cef_parse_json := GetProcAddress(FLibHandle, 'cef_parse_json');
    cef_parse_jsonand_return_error := GetProcAddress(FLibHandle, 'cef_parse_jsonand_return_error');
    cef_write_json := GetProcAddress(FLibHandle, 'cef_write_json');
{$endif}
    cef_browser_host_create_browser := GetProcAddress(FLibHandle,
      'cef_browser_host_create_browser');
    cef_browser_host_create_browser_sync := GetProcAddress(FLibHandle,
      'cef_browser_host_create_browser_sync');
    cef_request_create := GetProcAddress(FLibHandle, 'cef_request_create');
    cef_post_data_create := GetProcAddress(FLibHandle, 'cef_post_data_create');
    cef_post_data_element_create := GetProcAddress(FLibHandle,
      'cef_post_data_element_create');
    cef_stream_reader_create_for_file := GetProcAddress(FLibHandle,
      'cef_stream_reader_create_for_file');
    cef_stream_reader_create_for_data := GetProcAddress(FLibHandle,
      'cef_stream_reader_create_for_data');
    cef_stream_reader_create_for_handler := GetProcAddress(FLibHandle,
      'cef_stream_reader_create_for_handler');
    cef_stream_writer_create_for_file := GetProcAddress(FLibHandle,
      'cef_stream_writer_create_for_file');
    cef_stream_writer_create_for_handler := GetProcAddress(FLibHandle,
      'cef_stream_writer_create_for_handler');
    cef_v8context_get_current_context := GetProcAddress(FLibHandle,
      'cef_v8context_get_current_context');
    cef_v8context_get_entered_context := GetProcAddress(FLibHandle,
      'cef_v8context_get_entered_context');
    cef_v8context_in_context := GetProcAddress(FLibHandle,
      'cef_v8context_in_context');
    cef_v8value_create_undefined := GetProcAddress(FLibHandle,
      'cef_v8value_create_undefined');
    cef_v8value_create_null := GetProcAddress(FLibHandle,
      'cef_v8value_create_null');
    cef_v8value_create_bool := GetProcAddress(FLibHandle,
      'cef_v8value_create_bool');
    cef_v8value_create_int := GetProcAddress(FLibHandle,
      'cef_v8value_create_int');
    cef_v8value_create_uint := GetProcAddress(FLibHandle,
      'cef_v8value_create_uint');
    cef_v8value_create_double := GetProcAddress(FLibHandle,
      'cef_v8value_create_double');
    cef_v8value_create_date := GetProcAddress(FLibHandle,
      'cef_v8value_create_date');
    cef_v8value_create_string := GetProcAddress(FLibHandle,
      'cef_v8value_create_string');
    cef_v8value_create_object := GetProcAddress(FLibHandle,
      'cef_v8value_create_object');
    cef_v8value_create_array := GetProcAddress(FLibHandle,
      'cef_v8value_create_array');
    cef_v8value_create_function := GetProcAddress(FLibHandle,
      'cef_v8value_create_function');
    cef_v8stack_trace_get_current := GetProcAddress(FLibHandle,
      'cef_v8stack_trace_get_current');
    cef_xml_reader_create := GetProcAddress(FLibHandle,
      'cef_xml_reader_create');
    cef_zip_reader_create := GetProcAddress(FLibHandle,
      'cef_zip_reader_create');

    cef_string_multimap_alloc := GetProcAddress(FLibHandle,
      'cef_string_multimap_alloc');
    cef_string_multimap_size := GetProcAddress(FLibHandle,
      'cef_string_multimap_size');
    cef_string_multimap_find_count := GetProcAddress(FLibHandle,
      'cef_string_multimap_find_count');
    cef_string_multimap_enumerate := GetProcAddress(FLibHandle,
      'cef_string_multimap_enumerate');
    cef_string_multimap_key := GetProcAddress(FLibHandle,
      'cef_string_multimap_key');
    cef_string_multimap_value := GetProcAddress(FLibHandle,
      'cef_string_multimap_value');
    cef_string_multimap_append := GetProcAddress(FLibHandle,
      'cef_string_multimap_append');
    cef_string_multimap_clear := GetProcAddress(FLibHandle,
      'cef_string_multimap_clear');
    cef_string_multimap_free := GetProcAddress(FLibHandle,
      'cef_string_multimap_free');

    cef_cookie_manager_get_global_manager := GetProcAddress(FLibHandle,
      'cef_cookie_manager_get_global_manager');
    cef_cookie_manager_create_manager := GetProcAddress(FLibHandle,
      'cef_cookie_manager_create_manager');

    cef_command_line_create := GetProcAddress(FLibHandle,
      'cef_command_line_create');

    cef_command_line_get_global := GetProcAddress(FLibHandle,
      'cef_command_line_get_global');

    cef_process_message_create := GetProcAddress(FLibHandle,
      'cef_process_message_create');

    cef_value_create := GetProcAddress(FLibHandle, 'cef_value_create');

    cef_binary_value_create := GetProcAddress(FLibHandle,
      'cef_binary_value_create');

    cef_dictionary_value_create := GetProcAddress(FLibHandle,
      'cef_dictionary_value_create');

    cef_list_value_create := GetProcAddress(FLibHandle,
      'cef_list_value_create');

    cef_get_path := GetProcAddress(FLibHandle, 'cef_get_path');

    cef_launch_process := GetProcAddress(FLibHandle, 'cef_launch_process');

    cef_response_create := GetProcAddress(FLibHandle, 'cef_response_create');

    cef_urlrequest_create := GetProcAddress(FLibHandle,
      'cef_urlrequest_create');

    cef_visit_web_plugin_info := GetProcAddress(FLibHandle,
      'cef_visit_web_plugin_info');
    cef_refresh_web_plugins := GetProcAddress(FLibHandle,
      'cef_refresh_web_plugins');
	cef_add_web_plugin_path := GetProcAddress(FLibHandle, 'cef_add_web_plugin_path');
    cef_add_web_plugin_directory := GetProcAddress(FLibHandle, 'cef_add_web_plugin_directory');
    cef_remove_web_plugin_path := GetProcAddress(FLibHandle, 'cef_remove_web_plugin_path');
    cef_unregister_internal_web_plugin := GetProcAddress(FLibHandle, 'cef_unregister_internal_web_plugin');
    cef_force_web_plugin_shutdown := GetProcAddress(FLibHandle, 'cef_force_web_plugin_shutdown');
    cef_register_web_plugin_crash := GetProcAddress(FLibHandle,
      'cef_register_web_plugin_crash');
    cef_is_web_plugin_unstable := GetProcAddress(FLibHandle,
      'cef_is_web_plugin_unstable');

    cef_get_geolocation := GetProcAddress(FLibHandle, 'cef_get_geolocation');

    cef_task_runner_get_for_current_thread := GetProcAddress(FLibHandle,
      'cef_task_runner_get_for_current_thread');
    cef_task_runner_get_for_thread := GetProcAddress(FLibHandle,
      'cef_task_runner_get_for_thread');

    cef_begin_tracing := GetProcAddress(FLibHandle, 'cef_begin_tracing');
    cef_end_tracing := GetProcAddress(FLibHandle, 'cef_end_tracing');
    cef_now_from_system_trace_time := GetProcAddress(FLibHandle,
      'cef_now_from_system_trace_time');

    cef_request_context_get_global_context := GetProcAddress(FLibHandle,
      'cef_request_context_get_global_context');
    cef_request_context_create_context := GetProcAddress(FLibHandle,
      'cef_request_context_create_context');

    create_context_shared := GetProcAddress(FLibHandle, 'create_context_shared');

    cef_get_min_log_level := GetProcAddress(FLibHandle,
      'cef_get_min_log_level');

    cef_get_vlog_level := GetProcAddress(FLibHandle, 'cef_get_vlog_level');
    cef_log := GetProcAddress(FLibHandle, 'cef_log');

    cef_get_current_platform_thread_id := GetProcAddress(FLibHandle,
      'cef_get_current_platform_thread_id');
    cef_get_current_platform_thread_handle := GetProcAddress(FLibHandle,
      'cef_get_current_platform_thread_handle');

    cef_trace_event_instant := GetProcAddress(FLibHandle,
      'cef_trace_event_instant');
    cef_trace_event_begin := GetProcAddress(FLibHandle,
      'cef_trace_event_begin');
    cef_trace_event_end := GetProcAddress(FLibHandle, 'cef_trace_event_end');
    cef_trace_counter := GetProcAddress(FLibHandle, 'cef_trace_counter');
    cef_trace_counter_id := GetProcAddress(FLibHandle, 'cef_trace_counter_id');
    cef_trace_event_async_begin := GetProcAddress(FLibHandle,
      'cef_trace_event_async_begin');
    cef_trace_event_async_step_into := GetProcAddress(FLibHandle,
      'cef_trace_event_async_step_into');
    cef_trace_event_async_step_past := GetProcAddress(FLibHandle,
      'cef_trace_event_async_step_past');
    cef_trace_event_async_end := GetProcAddress(FLibHandle,
      'cef_trace_event_async_end');

    cef_print_settings_create := GetProcAddress(FLibHandle,
      'cef_print_settings_create');

    cef_drag_data_create := GetProcAddress(FLibHandle, 'cef_drag_data_create');

    cef_resource_bundle_get_global := GetProcAddress(FLibHandle, 'cef_resource_bundle_get_global');

if not (
      Assigned(cef_string_wide_set) and
      Assigned(cef_string_utf8_set) and
      Assigned(cef_string_utf16_set) and
      Assigned(cef_string_wide_clear) and
      Assigned(cef_string_utf8_clear) and
      Assigned(cef_string_utf16_clear) and
      Assigned(cef_string_wide_cmp) and
      Assigned(cef_string_utf8_cmp) and
      Assigned(cef_string_utf16_cmp) and
      Assigned(cef_string_wide_to_utf8) and
      Assigned(cef_string_utf8_to_wide) and
      Assigned(cef_string_wide_to_utf16) and
      Assigned(cef_string_utf16_to_wide) and
      Assigned(cef_string_utf8_to_utf16) and
      Assigned(cef_string_utf16_to_utf8) and
      Assigned(cef_string_ascii_to_wide) and
      Assigned(cef_string_ascii_to_utf16) and
      Assigned(cef_string_userfree_wide_alloc) and
      Assigned(cef_string_userfree_utf8_alloc) and
      Assigned(cef_string_userfree_utf16_alloc) and
      Assigned(cef_string_userfree_wide_free) and
      Assigned(cef_string_userfree_utf8_free) and
      Assigned(cef_string_userfree_utf16_free) and
      Assigned(cef_string_map_alloc) and
      Assigned(cef_string_map_size) and
      Assigned(cef_string_map_find) and
      Assigned(cef_string_map_key) and
      Assigned(cef_string_map_value) and
      Assigned(cef_string_map_append) and
      Assigned(cef_string_map_clear) and
      Assigned(cef_string_map_free) and
      Assigned(cef_string_list_alloc) and
      Assigned(cef_string_list_size) and
      Assigned(cef_string_list_value) and
      Assigned(cef_string_list_append) and
      Assigned(cef_string_list_clear) and
      Assigned(cef_string_list_free) and
      Assigned(cef_string_list_copy) and
      Assigned(cef_initialize) and
      Assigned(cef_execute_process) and
      Assigned(cef_shutdown) and
      Assigned(cef_do_message_loop_work) and
      Assigned(cef_run_message_loop) and
      Assigned(cef_quit_message_loop) and
      Assigned(cef_set_osmodal_loop) and
      Assigned(cef_enable_highdpi_support) and
      Assigned(cef_register_extension) and
      Assigned(cef_register_scheme_handler_factory) and
      Assigned(cef_clear_scheme_handler_factories) and
      Assigned(cef_add_cross_origin_whitelist_entry) and
      Assigned(cef_remove_cross_origin_whitelist_entry) and
      Assigned(cef_clear_cross_origin_whitelist) and
      Assigned(cef_currently_on) and
      Assigned(cef_post_task) and
      Assigned(cef_post_delayed_task) and
      Assigned(cef_parse_url) and
      Assigned(cef_create_url) and
      Assigned(cef_format_url_for_security_display) and
      Assigned(cef_get_mime_type) and
      Assigned(cef_get_extensions_for_mime_type) and
      Assigned(cef_base64encode) and
      Assigned(cef_base64decode) and
      Assigned(cef_uriencode) and
      Assigned(cef_uridecode) and
      Assigned(cef_parse_csscolor) and
{$ifdef Win32}
      Assigned(cef_parse_json) and
      Assigned(cef_parse_jsonand_return_error) and
      Assigned(cef_write_json) and
{$endif}
      Assigned(cef_browser_host_create_browser) and
      Assigned(cef_browser_host_create_browser_sync) and
      Assigned(cef_request_create) and
      Assigned(cef_post_data_create) and
      Assigned(cef_post_data_element_create) and
      Assigned(cef_stream_reader_create_for_file) and
      Assigned(cef_stream_reader_create_for_data) and
      Assigned(cef_stream_reader_create_for_handler) and
      Assigned(cef_stream_writer_create_for_file) and
      Assigned(cef_stream_writer_create_for_handler) and
      Assigned(cef_v8context_get_current_context) and
      Assigned(cef_v8context_get_entered_context) and
      Assigned(cef_v8context_in_context) and
      Assigned(cef_v8value_create_undefined) and
      Assigned(cef_v8value_create_null) and
      Assigned(cef_v8value_create_bool) and
      Assigned(cef_v8value_create_int) and
      Assigned(cef_v8value_create_uint) and
      Assigned(cef_v8value_create_double) and
      Assigned(cef_v8value_create_date) and
      Assigned(cef_v8value_create_string) and
      Assigned(cef_v8value_create_object) and
      Assigned(cef_v8value_create_array) and
      Assigned(cef_v8value_create_function) and
      Assigned(cef_v8stack_trace_get_current) and
      Assigned(cef_xml_reader_create) and
      Assigned(cef_zip_reader_create) and
      Assigned(cef_string_multimap_alloc) and
      Assigned(cef_string_multimap_size) and
      Assigned(cef_string_multimap_find_count) and
      Assigned(cef_string_multimap_enumerate) and
      Assigned(cef_string_multimap_key) and
      Assigned(cef_string_multimap_value) and
      Assigned(cef_string_multimap_append) and
      Assigned(cef_string_multimap_clear) and
      Assigned(cef_string_multimap_free) and
      Assigned(cef_cookie_manager_get_global_manager) and
      Assigned(cef_cookie_manager_create_manager) and
      Assigned(cef_command_line_create) and
      Assigned(cef_command_line_get_global) and
      Assigned(cef_process_message_create) and
      Assigned(cef_value_create) and
      Assigned(cef_binary_value_create) and
      Assigned(cef_dictionary_value_create) and
      Assigned(cef_list_value_create) and
      Assigned(cef_get_path) and
      Assigned(cef_launch_process) and
      Assigned(cef_response_create) and
      Assigned(cef_urlrequest_create) and
      Assigned(cef_visit_web_plugin_info) and
      Assigned(cef_refresh_web_plugins) and
      Assigned(cef_add_web_plugin_path) and
      Assigned(cef_add_web_plugin_directory) and
      Assigned(cef_remove_web_plugin_path) and
      Assigned(cef_unregister_internal_web_plugin) and
      Assigned(cef_force_web_plugin_shutdown) and
      Assigned(cef_register_web_plugin_crash) and
      Assigned(cef_is_web_plugin_unstable) and
      Assigned(cef_get_geolocation) and
      Assigned(cef_task_runner_get_for_current_thread) and
      Assigned(cef_task_runner_get_for_thread) and
      Assigned(cef_begin_tracing) and
      Assigned(cef_end_tracing) and
      Assigned(cef_now_from_system_trace_time) and
      Assigned(cef_request_context_get_global_context) and
      Assigned(cef_request_context_create_context) and
      Assigned(create_context_shared) and
      Assigned(cef_get_min_log_level) and
      Assigned(cef_get_vlog_level) and
      Assigned(cef_log) and
      Assigned(cef_get_current_platform_thread_id) and
      Assigned(cef_get_current_platform_thread_handle) and
      Assigned(cef_trace_event_instant) and
      Assigned(cef_trace_event_begin) and
      Assigned(cef_trace_event_end) and
      Assigned(cef_trace_counter) and
      Assigned(cef_trace_counter_id) and
      Assigned(cef_trace_event_async_begin) and
      Assigned(cef_trace_event_async_step_into) and
      Assigned(cef_trace_event_async_step_past) and
      Assigned(cef_trace_event_async_end) and
      Assigned(cef_print_settings_create) and
      Assigned(cef_drag_data_create) and
      Assigned(cef_resource_bundle_get_global)
    )
    then
      raise ECefException.create('Invalid CEF Library version');

    FillChar(settings, SizeOf(settings), 0);
    settings.size := SizeOf(settings);
    settings.single_process := ord(SingleProcess);
    settings.no_sandbox := ord(NoSandbox);
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    settings.multi_threaded_message_loop := ord(False);
{$ELSE}
    settings.multi_threaded_message_loop := ord(True);
{$ENDIF}
    settings.windowless_rendering_enabled := ord(WindowlessRenderingEnabled);
    settings.cache_path := TCef3Helper.CefString(Cache);
    settings.user_data_path := TCef3Helper.CefString(UserDataPath);
    settings.persist_session_cookies := ord(persistSessionCookies);
    settings.browser_subprocess_path :=
      TCef3Helper.CefString(BrowserSubprocessPath);
    settings.command_line_args_disabled := ord(CommandLineArgsDisabled);
    settings.user_agent := TCef3Helper.CefString(UserAgent);
    settings.product_version := TCef3Helper.CefString(ProductVersion);
    settings.locale := TCef3Helper.CefString(locale);
    settings.log_file := TCef3Helper.CefString(LogFile);
    settings.log_severity := LogSeverity;
    settings.javascript_flags := TCef3Helper.CefString(JavaScriptFlags);
    settings.resources_dir_path := TCef3Helper.CefString(ResourcesDirPath);
    settings.locales_dir_path := TCef3Helper.CefString(LocalesDirPath);
    settings.pack_loading_disabled := ord(PackLoadingDisabled);
    settings.remote_debugging_port := RemoteDebuggingPort;
    settings.uncaught_exception_stack_size := UncaughtExceptionStackSize;
    settings.context_safety_implementation := ContextSafetyImplementation;
    settings.ignore_certificate_errors := ord(IgnoreCertificateErrors);
    settings.background_color := BackgroundColor;
    settings.accept_language_list := TCef3Helper.CefString(AcceptLanguageList);
    App := TInternalApp.create;
    errcode := cef_execute_process(@HInstance, TCef3Helper.CefGetData(App),
      WindowsSandboxInfo);
    if errcode >= 0 then
    begin
      Result := False;
      exit;
    end;
    cef_initialize(@HInstance, @settings, TCef3Helper.CefGetData(App),
      WindowsSandboxInfo);
    FCefIsMainProcess := True;
  end;
  Result := True;
end;

{ TInternalApp }

procedure TInternalApp.OnBeforeCommandLineProcessing(const processType: ustring;
  const commandLine: ICefCommandLine);
begin
  if Assigned(InnerCefOnBeforeCommandLineProcessing) then
    InnerCefOnBeforeCommandLineProcessing(processType, commandLine);
end;

procedure TInternalApp.OnRegisterCustomSchemes(const registrar
  : ICefSchemeRegistrar);
begin
  if Assigned(InnerCefOnRegisterCustomSchemes) then
    InnerCefOnRegisterCustomSchemes(registrar);
end;

function TInternalApp.GetResourceBundleHandler: ICefResourceBundleHandler;
begin
  Result := InnerDcefBResourceBundleHandler;
end;

function TInternalApp.GetBrowserProcessHandler: ICefBrowserProcessHandler;
begin
  Result := InnerDcefBBrowserProcessHandler;
end;

function TInternalApp.GetRenderProcessHandler: ICefRenderProcessHandler;
begin
  Result := InnerDcefBRenderProcessHandler;
end;

initialization

IsMultiThread := True;
DcefBApp := TDcefBApp.create;
InnerDcefBResourceBundleHandler := TDcefBResourceBundleHandler.create;
InnerDcefBBrowserProcessHandler := TDcefBBrowserProcessHandler.create;
InnerDcefBRenderProcessHandler := TDcefBRenderProcessHandler.create;

finalization

InnerDcefBResourceBundleHandler := nil;
InnerDcefBBrowserProcessHandler := nil;
InnerDcefBRenderProcessHandler := nil;
InnerCefOnBeforeCommandLineProcessing := nil;
InnerCefOnRegisterCustomSchemes := nil;
DcefBApp.CefShutDown;
DcefBApp.Free;

end.
