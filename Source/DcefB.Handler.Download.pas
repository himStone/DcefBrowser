(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Handler.Download;

interface

uses
  Winapi.Windows, System.Classes,
  DcefB.Dcef3.CefLib, DcefB.Events, DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBDownloadHandler = class(TCefDownloadHandlerOwn)
  private
    FEvents: IDcefBEvents;
  protected
    procedure OnBeforeDownload(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback); override;
    procedure OnDownloadUpdated(const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback); override;
  public
    constructor Create(aDcefBEvents: IDcefBEvents); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomDownloadHandler }

constructor TDcefBDownloadHandler.Create(aDcefBEvents: IDcefBEvents);
begin
  inherited Create;
  FEvents := aDcefBEvents;
end;

destructor TDcefBDownloadHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

procedure TDcefBDownloadHandler.OnBeforeDownload(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
var
  // PArgs: PBeforeDownloadArgs;
  CancelDefaultEvent: Boolean;
begin
  inherited;
  { New(PArgs);
    PArgs.downloadItem := @downloadItem;
    PArgs.suggestedName := @suggestedName;
    PArgs.callback := @callback;
    PArgs.CancelDefaultEvent := False;
    TDcefBUtils.SendMsg(browser, WM_BeforeDownload, LParam(PArgs));
    Dispose(PArgs); }
  FEvents.doOnBeforeDownload(browser, downloadItem, suggestedName, callback,
    CancelDefaultEvent);
end;

procedure TDcefBDownloadHandler.OnDownloadUpdated(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
{ var
  PArgs: PDownloadUpdatedArgs; }
begin
  inherited;
  { New(PArgs);
    PArgs.downloadItem := @downloadItem;
    PArgs.callback := @callback;
    TDcefBUtils.SendMsg(browser, WM_DownloadUpdated, LParam(PArgs));
    Dispose(PArgs); }
  FEvents.doOnDownloadUpdated(browser, downloadItem, callback);
end;

end.
