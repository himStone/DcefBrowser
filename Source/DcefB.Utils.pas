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

unit DcefB.Utils;

interface

uses
  Windows, Classes, Variants, SysUtils, ComObj, SyncObjs,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Types, DcefB.Cef3.Classes, DcefB.res;

type
  TFaviconReady = procedure(aUrl: string; aStream: TStream) of object;

  TDcefBUtils = record
    class function GetCefParentWindow(aBrowser: ICefBrowser): HWND; static;
    class function SendMsg(aBrowser: ICefBrowser; Msg: UINT; LParam: LParam)
      : Boolean; static;
    class function GetGuid: string; static;
    class function ToVariant(V: Icefv8Value): Variant; static;
    class procedure SetCefValueData(aData: ICefListValue;
      const aValueIndex: Integer; const aValue: Variant); static;
    class procedure SetVariantData(var aValue: Variant; aCefList: ICefListValue;
      const aIndex: Integer); static;
    class function MsgWaitForEvent(AEvent: TEvent; ATimeout: Cardinal)
      : TWaitResult; static;
  end;

  TFaviconGetter = class(TCefUrlrequestClientOwn)
  private
    fCallback: TFaviconReady;
    fStream: TMemoryStream;
    fUrlRequest: ICefUrlRequest;
  protected
    procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer;
      dataLength: NativeUInt); override;
    procedure OnRequestComplete(const request: ICefUrlRequest); override;
  public
    constructor Create(Url: String; Callback: TFaviconReady);

    procedure Cancel;
  end;

implementation

{ TDcefBUtils }

class function TDcefBUtils.GetCefParentWindow(aBrowser: ICefBrowser): HWND;
var
  h: HWND;
begin
  Result := GetParent(aBrowser.host.WindowHandle);
end;

class function TDcefBUtils.GetGuid: string;
var
  aGuid: TGUID;
begin
  CreateGUID(aGuid);
  Result := GUIDToString(aGuid);
end;

//Copy From QWorker.pas
class function TDcefBUtils.MsgWaitForEvent(AEvent: TEvent; ATimeout: Cardinal)
  : TWaitResult;
var
  T: Cardinal;
{$IFDEF MSWINDOWS}
  AHandles: array [0 .. 0] of THandle;
  rc: DWORD;
{$ENDIF}
  procedure ProcessAppMessage;
{$IFDEF MSWINDOWS}
  var
    AMsg: Msg;
{$ENDIF}
  begin
{$IFDEF MSWINDOWS}
    while PeekMessage(AMsg, 0, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage(AMsg);
      DispatchMessage(AMsg);
    end;
{$ELSE}
    Application.ProcessMessages;
{$ENDIF}
  end;

begin
  if GetCurrentThreadId <> MainThreadId then
    Result := AEvent.WaitFor(ATimeout)
  else
  begin
{$IFDEF MSWINDOWS}
    Result := wrTimeout;
    AHandles[0] := AEvent.Handle;
    repeat
      T := GetTickCount;
      rc := MsgWaitForMultipleObjects(1, AHandles[0], False, ATimeout,
        QS_ALLINPUT);
      if rc = WAIT_OBJECT_0 + 1 then
      begin
        ProcessAppMessage;
        T := GetTickCount - T;
        if ATimeout > T then
          Dec(ATimeout, T)
        else
        begin
          Result := wrTimeout;
          Break;
        end;
      end
      else
      begin
        case rc of
          WAIT_ABANDONED:
            Result := wrAbandoned;
          WAIT_OBJECT_0:
            Result := wrSignaled;
          WAIT_TIMEOUT:
            Result := wrTimeout;
          WAIT_FAILED:
            Result := wrError;
          WAIT_IO_COMPLETION:
            Result := wrIOCompletion;
        end;
        Break;
      end;
    until False;
{$ELSE}
    repeat
      // 每隔10毫秒检查一下是否有消息需要处理，有则处理，无则进入下一个等待
      T := GetTimestamp;
      Result := AEvent.WaitFor(10);
      if Result = wrTimeout then
      begin
        T := (GetTimestamp - T) div 10;
        ProcessAppMessage;
        if ATimeout > T then
          Dec(ATimeout, T)
        else
          Break;
      end
      else
        Break;
    until False;
{$ENDIF}
  end;
end;

class function TDcefBUtils.SendMsg(aBrowser: ICefBrowser; Msg: UINT;
  LParam: LParam): Boolean;
begin
  Result := SendMessage(GetCefParentWindow(aBrowser), Msg, WParam(@aBrowser),
    LParam) <> S_FALSE;
end;

class procedure TDcefBUtils.SetCefValueData(aData: ICefListValue;
  const aValueIndex: Integer; const aValue: Variant);
begin
  case TVarData(aValue).vType of
    varInteger, varInt64:
      aData.SetInt(aValueIndex, aValue);
    varDouble:
      aData.SetDouble(aValueIndex, aValue);
    varBoolean:
      aData.SetBool(aValueIndex, aValue);
    varUString, varString:
      aData.SetString(aValueIndex, aValue)
  end;
end;

class procedure TDcefBUtils.SetVariantData(var aValue: Variant;
  aCefList: ICefListValue; const aIndex: Integer);
begin
  case aCefList.GetType(aIndex) of
    VTYPE_BOOL:
      aValue := aCefList.GetBool(aIndex);
    VTYPE_INT:
      aValue := aCefList.GetInt(aIndex);
    VTYPE_DOUBLE:
      aValue := aCefList.GetDouble(aIndex);
    VTYPE_STRING:
      aValue := aCefList.GetString(aIndex);
  end;
end;

class function TDcefBUtils.ToVariant(V: Icefv8Value): Variant;
var
  i: Integer;
  procedure AsObject;
  var
    AList: TStringList;
    AVal: Icefv8Value;
    i, T, c: Integer;
    AValues: array of Variant;
  begin
    AList := TStringList.Create;
    try
      V.GetKeys(AList);
      SetLength(AValues, AList.Count shl 1);
      c := 0;
      for i := 0 to AList.Count do
      begin
        AVal := V.GetValueByIndex(i);
        if not AVal.IsFunction then
        begin
          T := c shl 1;
          AValues[T] := AList[i];
          AValues[T + 1] := ToVariant(AVal);
          Inc(c);
        end;
      end;
      SetLength(AValues, c shl 1);
      Result := VarArrayOf(AValues);
    finally
      FreeAndNil(AList);
    end;
  end;

begin
  if V.IsString then
    Result := V.GetStringValue
  else if V.IsBool then
    Result := V.GetBoolValue
  else if V.IsInt then
    Result := V.GetIntValue
  else if V.IsUInt then
    Result := V.GetUIntValue
  else if V.IsDouble then
    Result := V.GetDoubleValue
  else if V.IsUndefined then
    Result := Unassigned
  else if V.IsNull then
    Result := Null
  else if V.IsFunction then
    Result := V.GetFunctionName
  else if V.IsArray then
  begin
    Result := VarArrayCreate([0, V.GetArrayLength], varVariant);
    for i := 0 to V.GetArrayLength - 1 do
    begin
      Result[i] := ToVariant(V.GetValueByIndex(i));
    end;
  end
  else if V.IsObject then
  begin
    AsObject;
  end
  else
    raise Exception.Create(SCantToVariant);
end;

{ TFaviconGetter }

procedure TFaviconGetter.OnDownloadData(const request: ICefUrlRequest; data: Pointer;
      dataLength: NativeUInt);
begin
  fStream.WriteBuffer(data^, dataLength);
end;

procedure TFaviconGetter.OnRequestComplete(const request: ICefUrlRequest);
begin
  If Assigned(fCallback) then
  begin
    If request.GetRequestStatus = UR_SUCCESS then
    begin
      fStream.Position := 0;
      try
        fCallback(request.GetRequest.Url, fStream);
      except
      end;
    end;
  end;

  fStream.Free;
end;

constructor TFaviconGetter.Create(Url: String; Callback: TFaviconReady);
Var
  Request: ICefRequest;
begin
  inherited Create;

  fCallback := Callback;
  fStream := TMemoryStream.Create;
  Request := TCefRequestRef.New;
  Request.Url := Url;
  fUrlRequest := TCefUrlRequestRef.New(Request, Self, nil);
end;

procedure TFaviconGetter.Cancel;
begin
  fCallback := nil;
  fUrlRequest.Cancel;
end;

end.
