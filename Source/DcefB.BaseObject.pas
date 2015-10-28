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

unit DcefB.BaseObject;

interface

uses
  Classes, Rtti, TypInfo, Variants,
  DcefB.Cef3.Types, DcefB.Cef3.Classes, DcefB.Cef3.Interfaces;

type
{$IF RTLVersion<25}
  IntPtr = Integer;
{$ENDIF}
  Pustring = ^ustring;
  PICefFrame = ^ICefFrame;
  PICefFileDialogCallback = ^ICefFileDialogCallback;
  PCefFocusSource = ^TCefFocusSource;
  PICefContextMenuParams = ^ICefContextMenuParams;
  PICefMenuModel = ^ICefMenuModel;
  PCefEventFlags = ^TCefEventFlags;
  PICefDownloadItem = ^ICefDownloadItem;
  PICefBeforeDownloadCallback = ^ICefBeforeDownloadCallback;
  PICefDownloadItemCallback = ^ICefDownloadItemCallback;
  PICefGeolocationCallback = ^ICefGeolocationCallback;
  PICefJsDialogCallback = ^ICefJsDialogCallback;
  PCefJsDialogType = ^TCefJsDialogType;
  PICefDragData = ^ICefDragData;
  PCefDragOperations = ^TCefDragOperations;
  PBeforePopupArgs = ^TBeforePopupArgs;
  PFileDialogArgs = ^TFileDialogArgs;
  PLoadEndArgs = ^TLoadEndArgs;
  PLoadErrorArgs = ^TLoadErrorArgs;
  PSetFocusArgs = ^TSetFocusArgs;
  PTakeFocusArgs = ^TTakeFocusArgs;
  PBeforeContextMenuArgs = ^TBeforeContextMenuArgs;
  PContextMenuCommandArgs = ^TContextMenuCommandArgs;
  PKeyEventArgs = ^TKeyEventArgs;
  PPreKeyEventArgs = ^TPreKeyEventArgs;
  // PAddressChangeArgs = ^TAddressChangeArgs;
  PConsoleMessageArgs = ^TConsoleMessageArgs;
  PTooltipArgs = ^TTooltipArgs;
  PBeforeDownloadArgs = ^TBeforeDownloadArgs;
  PDownloadUpdatedArgs = ^TDownloadUpdatedArgs;
  PRequestGeolocationPermissionArgs = ^TRequestGeolocationPermissionArgs;
  PCancelGeolocationPermissionArgs = ^TCancelGeolocationPermissionArgs;
  PBeforeUnloadDialogArgs = ^TBeforeUnloadDialogArgs;
  PJsdialogArgs = ^TJsdialogArgs;
  PDragEnterArgs = ^TDragEnterArgs;
  PICefClient = ^ICefClient;
  PCefBrowserSettings = ^TCefBrowserSettings;

  TDynStrArr = Array of string;
  TDynIntArr = Array of Integer;
  TDynClassArr = Array of TClass;

  TBeforePopupArgs = record
    frame: PICefFrame;
    targetUrl, targetFrameName: Pustring;
    popupFeatures: PCefPopupFeatures;
    windowInfo: PCefWindowInfo;
    client: PICefClient;
    settings: PCefBrowserSettings;
    noJavascriptAccess: PBoolean;
    Result: PBoolean;
    CancelDefaultEvent: Boolean;
  end;

  TFileDialogArgs = record
    mode: ^TCefFileDialogMode;
    title: Pustring;
    defaultFileName: Pustring;
    acceptTypes: ^TStrings;
    callback: PICefFileDialogCallback;
    Result: PBoolean;
  end;

  TLoadEndArgs = record
    frame: PICefFrame;
    httpStatusCode: Integer;
  end;

  TLoadErrorArgs = record
    frame: PICefFrame;
    errorCode: Integer;
    errorText: Pustring;
    failedUrl: Pustring;
    CancelDefaultEvent: Boolean;
  end;

  TSetFocusArgs = record
    source: PCefFocusSource;
    Result: PBoolean;
    CancelDefaultEvent: Boolean;
  end;

  TTakeFocusArgs = record
    next: Boolean;
  end;

  TBeforeContextMenuArgs = record
    frame: PICefFrame;
    params: PICefContextMenuParams;
    model: PICefMenuModel;
  end;

  TContextMenuCommandArgs = record
    frame: PICefFrame;
    params: PICefContextMenuParams;
    commandId: Integer;
    eventFlags: PCefEventFlags;
    Result: PBoolean;
  end;

  TKeyEventArgs = record
    event: PCefKeyEvent;
    osEvent: TCefEventHandle;
    Result: PBoolean;
  end;

  TPreKeyEventArgs = record
    event: PCefKeyEvent;
    osEvent: TCefEventHandle;
    isKeyboardShortcut: Boolean;
    Result: PBoolean;
    CancelDefaultEvent: Boolean;
  end;

  { TAddressChangeArgs = record
    frame: PICefFrame;
    url: Pustring;
    end; }

  TConsoleMessageArgs = record
    message: Pustring;
    source: Pustring;
    line: Integer;
    Result: PBoolean;
  end;

  TTooltipArgs = record
    text: Pustring;
    Result: PBoolean;
  end;

  TBeforeDownloadArgs = record
    downloadItem: PICefDownloadItem;
    suggestedName: Pustring;
    callback: PICefBeforeDownloadCallback;
    CancelDefaultEvent: Boolean;
  end;

  TDownloadUpdatedArgs = record
    downloadItem: PICefDownloadItem;
    callback: PICefDownloadItemCallback;
  end;

  TRequestGeolocationPermissionArgs = record
    requestingUrl: Pustring;
    requestId: Integer;
    callback: PICefGeolocationCallback;
    Result: PBoolean;
  end;

  TCancelGeolocationPermissionArgs = record
    requestingUrl: Pustring;
    requestId: Integer;
  end;

  TBeforeUnloadDialogArgs = record
    messageText: Pustring;
    isReload: Boolean;
    callback: PICefJsDialogCallback;
    Result: PBoolean;
    CancelDefaultEvent: Boolean;
  end;

  TJsdialogArgs = record
    originUrl: Pustring;
    acceptLang: Pustring;
    dialogType: PCefJsDialogType;
    messageText: Pustring;
    defaultPromptText: Pustring;
    callback: PICefJsDialogCallback;
    suppressMessage: PBoolean;
    Result: PBoolean;
    CancelDefaultEvent: Boolean;
  end;

  TDragEnterArgs = record
    dragData: PICefDragData;
    mask: PCefDragOperations;
    Result: PBoolean;
  end;

  TRegExtentionPar = record
    name: ustring;
    code: ustring;
    Handler: ICefv8Handler;
    value: TValue;
  end;

implementation

end.
