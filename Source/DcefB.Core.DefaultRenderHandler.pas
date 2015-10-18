(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.Core.DefaultRenderHandler;

interface

uses
  System.Classes, System.SysUtils, DcefB.Dcef3.CefLib, DcefB.BaseObject,
  DcefB.res;

type
  TRenderProcessCallbackA = reference to procedure(aBrowser: ICefBrowser;
    aContext: ICefv8Context; aData: Pointer);

  TDefaultRenderProcessHandler = class(TCefRenderProcessHandlerOwn)
  protected
    procedure OnWebKitInitialized; override;
    function OnProcessMessageReceived(const Browser: ICefBrowser;
      sourceProcess: TCefProcessId; const Message: ICefProcessMessage)
      : Boolean; override;
  public
    class var Classes: TDynClassArr;
  end;

implementation

{ TDefaultRenderProcessHandler }

function TDefaultRenderProcessHandler.OnProcessMessageReceived
  (const Browser: ICefBrowser; sourceProcess: TCefProcessId;
  const Message: ICefProcessMessage): Boolean;
  procedure DoRunInRender;
  var
    aBrowser: ICefBrowser;
    ATemp: Pointer;
    AProc: TRenderProcessCallbackA;
    aData: Pointer;
  begin
    aBrowser := ICefBrowser
      (Pointer(StrToInt64('$' + message.ArgumentList.GetString(0))));
    ATemp := Pointer(StrToInt64('$' + message.ArgumentList.GetString(1)));
    AProc := TRenderProcessCallbackA(ATemp);
    aData := Pointer(StrToInt64('$' + message.ArgumentList.GetString(2)));
    if Assigned(AProc) then
      AProc(aBrowser, Browser.MainFrame.GetV8Context, aData);
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
  for Index := Low(Classes) to High(Classes) do
  begin
    TCefRTTIExtension.Register(Classes[Index].ClassName, Classes[Index]
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}, True {$ENDIF});
  end;
end;

end.
