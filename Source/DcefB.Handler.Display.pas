(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Handler.Display;

interface

uses
  Winapi.Windows, System.Classes,
  DcefB.Dcef3.CefLib, DcefB.Events, DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBDisplayHandler = class(TCefDisplayHandlerOwn)
  private
    FEvents: IDcefBEvents;
  protected
    procedure OnAddressChange(const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring); override;
    procedure OnTitleChange(const browser: ICefBrowser;
      const title: ustring); override;
    function OnTooltip(const browser: ICefBrowser; var text: ustring)
      : Boolean; override;
    procedure OnStatusMessage(const browser: ICefBrowser;
      const value: ustring); override;
    function OnConsoleMessage(const browser: ICefBrowser;
      const message, source: ustring; line: Integer): Boolean; override;
  public
    constructor Create(aDcefBEvents: IDcefBEvents); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCustomDisplayHandler }

constructor TDcefBDisplayHandler.Create(aDcefBEvents: IDcefBEvents);
begin
  inherited Create;
  FEvents := aDcefBEvents;
end;

destructor TDcefBDisplayHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

procedure TDcefBDisplayHandler.OnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
var
  PArgs: PAddressChangeArgs;
begin
  inherited;
  New(PArgs);
  PArgs.frame := @frame;
  PArgs.url := @url;
  TDcefBUtils.SendMsg(browser, WM_AddressChange, LParam(PArgs));
  Dispose(PArgs);
end;

function TDcefBDisplayHandler.OnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: Integer): Boolean;
var
  PArgs: PConsoleMessageArgs;
begin
  Result := False;
  New(PArgs);
  PArgs.message := @message;
  PArgs.source := @source;
  PArgs.line := line;
  PArgs.Result := @Result;
  TDcefBUtils.SendMsg(browser, WM_ConsoleMessage, LParam(PArgs));
  Dispose(PArgs);
end;

procedure TDcefBDisplayHandler.OnStatusMessage(const browser: ICefBrowser;
  const value: ustring);
begin
  inherited;
  TDcefBUtils.SendMsg(browser, WM_StatusMessage, LParam(@value));
end;

procedure TDcefBDisplayHandler.OnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin
  inherited;
  TDcefBUtils.SendMsg(browser, WM_TitleChange, LParam(@title));
end;

function TDcefBDisplayHandler.OnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
var
  PArgs: PTooltipArgs;
begin
  Result := False;
  New(PArgs);
  PArgs.text := @text;
  PArgs.Result := @Result;
  TDcefBUtils.SendMsg(browser, WM_Tooltip, LParam(PArgs));
  Dispose(PArgs);
end;

end.
