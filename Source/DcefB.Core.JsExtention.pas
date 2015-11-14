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

unit DcefB.Core.JsExtention;

interface

uses
  SysUtils, Generics.Collections,
  DcefB.Cef3.Interfaces, DcefB.Cef3.Classes, DcefB.Cef3.types, DcefB.res,
  DcefB.BaseObject, DcefB.Utils, DcefB.Locker;

type
  {
    JsExtention -> RenderProcess
    Parameters:
    0.string(JsExtention ID)
    1.TJsExtentionAction
    2.string(Js Variable Name or JsScript)
    3.string(Js Variable Value)

    RenderProcess -> BrowserProcess
    Parameters:
    0.string(JsExtention ID)
    1.Variant(Js Result Value)
    2.string(Exception Hint)
  }

  TJsExtentionProc = reference to procedure(const aData: Variant);

  TDcefBJsExtention = class(TInterfacedObject)
  private
    FHandlers: TDictionary<string, TJsExtentionProc>;

    procedure SendMsgToRenderProcess(aBrowser: ICefBrowser;
      const aAction: TJsExtentionAction; const aId, aText: string); overload;
    procedure SendMsgToRenderProcess(aBrowser: ICefBrowser;
      const aAction: TJsExtentionAction; const aId, aText: string;
      const aValue: Variant); overload;
    procedure InnerExec(aPorc: TJsExtentionProc; aBrowser: ICefBrowser;
      const aAction: TJsExtentionAction; const aText: string); overload;
    procedure InnerExec(aPorc: TJsExtentionProc; aBrowser: ICefBrowser;
      const aAction: TJsExtentionAction; const aText: string;
      const aValue: Variant); overload;
    procedure RegisterHandler(aId: string; aProc: TJsExtentionProc);
  public
    constructor Create();
    destructor Destroy; override;

    procedure GetExecuteScriptResultProc(aBrowser: ICefBrowser;
      const aScript: String; aPorc: TJsExtentionProc);
    procedure GetDefinedProc(aBrowser: ICefBrowser; const aName: String;
      aPorc: TJsExtentionProc);
    procedure GetAsBooleanProc(aBrowser: ICefBrowser; const aName: String;
      aPorc: TJsExtentionProc);
    procedure GetAsFloatProc(aBrowser: ICefBrowser; const aName: String;
      aPorc: TJsExtentionProc);
    procedure GetAsIntegerProc(aBrowser: ICefBrowser; const aName: String;
      aPorc: TJsExtentionProc);
    procedure GetAsStringProc(aBrowser: ICefBrowser; const aName: String;
      aPorc: TJsExtentionProc);
    procedure SetAsBooleanProc(aBrowser: ICefBrowser; const aName: String;
      const Value: Boolean; aPorc: TJsExtentionProc);
    procedure SetAsFloatProc(aBrowser: ICefBrowser; const aName: String;
      const Value: Double; aPorc: TJsExtentionProc);
    procedure SetAsIntegerProc(aBrowser: ICefBrowser; const aName: String;
      const Value: Int64; aPorc: TJsExtentionProc);
    procedure SetAsStringProc(aBrowser: ICefBrowser; const aName: String;
      const Value: String; aPorc: TJsExtentionProc);

    procedure UnRegisterHandler(aId, aExpStr: string; aValue: Variant);
  end;

implementation

{ TDcefBJsExtention }

constructor TDcefBJsExtention.Create();
begin
  inherited Create;
  FHandlers := TDictionary<string, TJsExtentionProc>.Create();
end;

destructor TDcefBJsExtention.Destroy;
begin
  FHandlers.Free;
  inherited;
end;

procedure TDcefBJsExtention.GetExecuteScriptResultProc(aBrowser: ICefBrowser;
  const aScript: String; aPorc: TJsExtentionProc);
begin
  InnerExec(aPorc, aBrowser, JSEA_SCRIPT, aScript);
end;

procedure TDcefBJsExtention.GetAsBooleanProc(aBrowser: ICefBrowser;
  const aName: String; aPorc: TJsExtentionProc);
begin
  InnerExec(aPorc, aBrowser, JSEA_GET, aName);
end;

procedure TDcefBJsExtention.GetAsFloatProc(aBrowser: ICefBrowser;
  const aName: String; aPorc: TJsExtentionProc);
begin
  InnerExec(aPorc, aBrowser, JSEA_GET, aName);
end;

procedure TDcefBJsExtention.GetAsIntegerProc(aBrowser: ICefBrowser;
  const aName: String; aPorc: TJsExtentionProc);
begin
  InnerExec(aPorc, aBrowser, JSEA_GET, aName);
end;

procedure TDcefBJsExtention.GetAsStringProc(aBrowser: ICefBrowser;
  const aName: String; aPorc: TJsExtentionProc);
begin
  InnerExec(aPorc, aBrowser, JSEA_GET, aName);
end;

procedure TDcefBJsExtention.GetDefinedProc(aBrowser: ICefBrowser;
  const aName: String; aPorc: TJsExtentionProc);
begin
  InnerExec(aPorc, aBrowser, JSEA_GETDEFINED, aName);
end;

procedure TDcefBJsExtention.RegisterHandler(aId: string;
  aProc: TJsExtentionProc);
begin
  JsExtention.Enter;
  try
    FHandlers.Add(aId, aProc);
  finally
    JsExtention.Exit;
  end;
end;

procedure TDcefBJsExtention.SendMsgToRenderProcess(aBrowser: ICefBrowser;
  const aAction: TJsExtentionAction; const aId, aText: string;
  const aValue: Variant);
var
  AMsg: ICefProcessMessage;
begin
  if aBrowser <> nil then
  begin
    AMsg := TCefProcessMessageRef.New(JSEXTENTION_TORENDER_MSG);
    AMsg.ArgumentList.SetSize(4);
    AMsg.ArgumentList.SetString(0, aId);
    AMsg.ArgumentList.SetInt(1, Integer(aAction));
    AMsg.ArgumentList.SetString(2, aText);
    TDcefBUtils.SetCefValueData(AMsg.ArgumentList, 3, aValue);
    aBrowser.SendProcessMessage(PID_RENDERER, AMsg);
  end;
end;

procedure TDcefBJsExtention.SendMsgToRenderProcess(aBrowser: ICefBrowser;
  const aAction: TJsExtentionAction; const aId, aText: string);
var
  AMsg: ICefProcessMessage;
begin
  if aBrowser <> nil then
  begin
    AMsg := TCefProcessMessageRef.New(JSEXTENTION_TORENDER_MSG);
    AMsg.ArgumentList.SetSize(3);
    AMsg.ArgumentList.SetString(0, aId);
    AMsg.ArgumentList.SetInt(1, Integer(aAction));
    AMsg.ArgumentList.SetString(2, aText);
    aBrowser.SendProcessMessage(PID_RENDERER, AMsg);
  end;
end;

procedure TDcefBJsExtention.SetAsBooleanProc(aBrowser: ICefBrowser;
  const aName: String; const Value: Boolean; aPorc: TJsExtentionProc);
begin
  InnerExec(aPorc, aBrowser, JSEA_SET, aName, Value);
end;

procedure TDcefBJsExtention.SetAsFloatProc(aBrowser: ICefBrowser;
  const aName: String; const Value: Double; aPorc: TJsExtentionProc);
begin
  InnerExec(aPorc, aBrowser, JSEA_SET, aName, Value);
end;

procedure TDcefBJsExtention.SetAsIntegerProc(aBrowser: ICefBrowser;
  const aName: String; const Value: Int64; aPorc: TJsExtentionProc);
begin
  InnerExec(aPorc, aBrowser, JSEA_SET, aName, Value);
end;

procedure TDcefBJsExtention.SetAsStringProc(aBrowser: ICefBrowser;
  const aName, Value: String; aPorc: TJsExtentionProc);
begin
  InnerExec(aPorc, aBrowser, JSEA_SET, aName, Value);
end;

procedure TDcefBJsExtention.InnerExec(aPorc: TJsExtentionProc;
  aBrowser: ICefBrowser; const aAction: TJsExtentionAction;
  const aText: string);
var
  aId: string;
begin
  aId := TDcefBUtils.GetGuid;
  RegisterHandler(aId, aPorc);
  SendMsgToRenderProcess(aBrowser, aAction, aId, aText);
end;

procedure TDcefBJsExtention.InnerExec(aPorc: TJsExtentionProc;
  aBrowser: ICefBrowser; const aAction: TJsExtentionAction; const aText: string;
  const aValue: Variant);
var
  aId: string;
begin
  aId := TDcefBUtils.GetGuid;
  RegisterHandler(aId, aPorc);
  SendMsgToRenderProcess(aBrowser, aAction, aId, aText, aValue);
end;

procedure TDcefBJsExtention.UnRegisterHandler(aId, aExpStr: string;
  aValue: Variant);
var
  aPorc: TJsExtentionProc;
begin
  JsExtention.Enter;
  try
    if FHandlers.TryGetValue(aId, aPorc) and Assigned(aPorc) then
    begin
      if aExpStr <> '' then
        raise Exception.Create(aExpStr);

      aPorc(aValue);
    end;
    FHandlers.Remove(aId);
  finally
    JsExtention.Exit;
  end;
end;

end.
