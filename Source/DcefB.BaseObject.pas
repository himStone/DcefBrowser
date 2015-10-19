(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.BaseObject;

interface

uses
  System.Classes,
  DcefB.Dcef3.CefLib;

type
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
  //PAddressChangeArgs = ^TAddressChangeArgs;
  PConsoleMessageArgs = ^TConsoleMessageArgs;
  PTooltipArgs = ^TTooltipArgs;
  PBeforeDownloadArgs = ^TBeforeDownloadArgs;
  PDownloadUpdatedArgs = ^TDownloadUpdatedArgs;
  PRequestGeolocationPermissionArgs = ^TRequestGeolocationPermissionArgs;
  PCancelGeolocationPermissionArgs = ^TCancelGeolocationPermissionArgs;
  PBeforeUnloadDialogArgs = ^TBeforeUnloadDialogArgs;
  PJsdialogArgs = ^TJsdialogArgs;
  PDragEnterArgs = ^TDragEnterArgs;

  TDynStrArr = Array of string;
  TDynIntArr = Array of Integer;
  TDynClassArr = Array of TClass;

  TBeforePopupArgs = record
    popupFeatures: PCefPopupFeatures;
    windowInfo: PCefWindowInfo;
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

  {TAddressChangeArgs = record
    frame: PICefFrame;
    url: Pustring;
  end;      }

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

implementation

end.
