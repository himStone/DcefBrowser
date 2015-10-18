(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Handler.Dialog;

interface

uses
  Winapi.Windows, System.Classes,
  DcefB.Dcef3.CefLib, DcefB.Events, DcefB.res, DcefB.Utils, DcefB.BaseObject;

type
  TDcefBDialogHandler = class(TCefDialogHandlerOwn)
  private
    FEvents: IDcefBEvents;
  protected
    function OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title: ustring; const defaultFileName: ustring;
      acceptTypes: TStrings; const callback: ICefFileDialogCallback)
      : Boolean; override;
  public
    constructor Create(aDcefBEvents: IDcefBEvents); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDcefBDialogHandler }

constructor TDcefBDialogHandler.Create(aDcefBEvents: IDcefBEvents);
begin
  inherited Create;
  FEvents := aDcefBEvents;
end;

destructor TDcefBDialogHandler.Destroy;
begin
  FEvents := nil;
  inherited;
end;

function TDcefBDialogHandler.OnFileDialog(const browser: ICefBrowser;
  mode: TCefFileDialogMode; const title, defaultFileName: ustring;
  acceptTypes: TStrings; const callback: ICefFileDialogCallback): Boolean;
var
  PArgs: PFileDialogArgs;
begin
  Result := False;
  new(PArgs);
  PArgs.mode := @mode;
  PArgs.title := @title;
  PArgs.defaultFileName := @defaultFileName;
  PArgs.acceptTypes := @acceptTypes;
  PArgs.callback := @callback;
  PArgs.Result := @Result;
  Dispose(PArgs);
end;

end.
