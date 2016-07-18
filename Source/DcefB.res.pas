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

unit DcefB.res;

interface

uses
  Messages;

Const
  WM_LoadingStateChange = WM_USER + 1;
  WM_WindowCheck = WM_USER + 2;
  WM_CreateWindow = WM_USER + 3;
  WM_NewBrowser = WM_USER + 4;
  WM_LoadStart = WM_USER + 5;
  WM_LoadEnd = WM_USER + 6;
  WM_LoadError = WM_USER + 7;
  WM_SetActive = WM_USER + 8;
  WM_GotFocus = WM_USER + 9;
  WM_SetFocus = WM_USER + 10;
  WM_TakeFocus = WM_USER + 11;
  WM_BeforeContextMenu = WM_USER + 12;
  WM_ContextMenuCommand = WM_USER + 13;
  WM_ContextMenuDismissed = WM_USER + 14;
  WM_FileDialog = WM_USER + 15;
  WM_KeyEvent = WM_USER + 16;
  WM_PreKeyEvent = WM_USER + 17;
  WM_AddressChange = WM_USER + 18;
  WM_ConsoleMessage = WM_USER + 19;
  WM_StatusMessage = WM_USER + 20;
  WM_TitleChange = WM_USER + 21;
  WM_Tooltip = WM_USER + 22;
  WM_BeforeDownload = WM_USER + 23;
  WM_DownloadUpdated = WM_USER + 24;
  WM_RequestGeolocationPermission = WM_USER + 25;
  WM_CancelGeolocationPermission = WM_USER + 26;
  WM_Jsdialog = WM_USER + 27;
  WM_BeforeUnloadDialog = WM_USER + 28;
  WM_ResetDialogState = WM_USER + 29;
  WM_DialogClosed = WM_USER + 30;
  WM_DoClose = WM_USER + 31;
  WM_BeforeClose = WM_USER + 32;

  WM_BeforeBrowse = WM_USER + 34;
  WM_BeforeResourceLoad = WM_USER + 35;
  WM_GetResourceHandler = WM_USER + 36;
  WM_ResourceRedirect = WM_USER + 37;
  WM_GetAuthCredentials = WM_USER + 38;
  WM_QuotaRequest = WM_USER + 39;
  WM_ProtocolExecution = WM_USER + 40;
  WM_BeforePluginLoad = WM_USER + 41;
  WM_CertificateError = WM_USER + 42;
  WM_PluginCrashed = WM_USER + 43;
  WM_RenderProcessTerminated = WM_USER + 44;
  WM_DragEnter = WM_USER + 45;

  WM_DevTools = WM_USER + 46;
  WM_RefreshIgnoreCache = WM_USER + 47;
  WM_SearchText = WM_USER + 48;

  WM_JsExtention = WM_USER + 49;
  WM_JsExtHelperReady = WM_USER + 50;

  WM_FaviconUrlChange = WM_USER + 51;
  WM_FullScreenModeChange = WM_USER + 52;

  State_IsLoading = $001;
  State_CanGoBack = $002;
  State_CanGoForward = $004;

Const
  SBlankPageUrl = 'about:blank';
  SLoadingText = '正在加载';
  SNoTitleText = '无标题';
  SDialogTitleSuffix = ' 上的网页显示';
  SUnloadDialogTitle = '确认导航';
  SUnloadDialogText = '确定要离开此页吗？';
  SRunOnlyInSinglePro = '暂时只支持单进程模式';

  SVarTypeMismatch = '变量 %s 不存在或类型不匹配';
  SJSException =
    '执行脚本时发生异常：'#13#10'信息：%s'#13#10'位置:第 %d 行 %d 列'#13#10'脚本:'#13#10'%s';
  SCantToVariant = '指定的JavaScript变量无法转换为 Variant';

  EXP_CEFLIBNOTLOAD = 'TDcefBApp.LoadLib must be called first';
  EXP_CEFNOTLOADINMAINPRO =
    'TDcefBApp.LoadLib must be called first in the main process';

  RUNINRENDER_MSG = '@dcefbrowser_runinrender';
  JSEXTENTION_TORENDER_MSG = '@dcefbrowser_torender_jsextention';
  JSEXTENTION_TOBROWSER_MEG = '@dcefbrowser_tobrowser_jsextention';

implementation

end.
