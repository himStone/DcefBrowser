unit jsvarhelper;

{ This unit created by swish and can be distribute with bccsafe's dcefbrowser

  2015.10.26
  ==========
  * Execute script code wrapped by try/finally make the SetReady always called
  + Add global DVars.Notify function make notify from js more easy
}
interface

uses
  classes, sysutils, types, syncobjs, variants, system.Generics.collections,
  DcefB.Core.App, DcefB.Cef3.Interfaces, DcefB.Cef3.classes,
  DcefB.Core.DcefBrowser;

type
  TJSNotify = procedure(S: String; var AHandled: Boolean) of object;

  TJsVars = class
  protected
    FEvent: TEvent;
    FValue: Variant;
    FBrowser: TDcefBrowser;
    procedure SetReady; inline;
    function Wait(ATimeout: Cardinal = INFINITE): TWaitResult;
    function GetAsBoolean(AName: String): Boolean;
    function GetAsFloat(AName: String): Double;
    function GetAsInteger(AName: String): Int64;
    function GetAsString(AName: String): String;
    procedure SetAsBoolean(AName: String; const Value: Boolean);
    procedure SetAsFloat(AName: String; const Value: Double);
    procedure SetAsInteger(AName: String; const Value: Int64);
    procedure SetAsString(AName: String; const Value: String);
    function GetDefined(AName: String): Boolean;
  public
    constructor Create(ABrowser: TDcefBrowser); overload;
    destructor Destroy; override;
    procedure ExecuteScript(const AScript, AUrl: String;
      AFromLine: Integer = 0); overload;
    function ExecuteScript(const AScript: String): Variant; overload;
    class procedure RegisterJsNotify(ANotify: TJSNotify);
    class procedure UnregisterJsNotify(ANotify: TJSNotify);
    property Defined[AName: String]: Boolean read GetDefined;
    property AsBoolean[AName: String]: Boolean read GetAsBoolean
      write SetAsBoolean;
    property AsInteger[AName: String]: Int64 read GetAsInteger
      write SetAsInteger;
    property AsFloat[AName: String]: Double read GetAsFloat write SetAsFloat;
    property AsString[AName: String]: String read GetAsString write SetAsString;
    property Browser: TDcefBrowser read FBrowser write FBrowser;
  end;

implementation

uses windows;

type

  DVars = class
  protected
    FHandlers: TList<TJSNotify>;
    FLocker: TCriticalSection;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure ProcessNotify(S: String);
    procedure Register(ANotify: TJSNotify);
    procedure Unregister(ANotify: TJSNotify);
    class procedure Notify(S: String);
  end;

var
  VarHelper: TJsVars;
  _DVars: DVars;

resourcestring
  SCantToVariant = '指定的JavaScript变量无法转换为 Variant';
  SJSException =
    '执行脚本时发生异常：'#13#10'信息：%s'#13#10'位置:第 %d 行 %d 列'#13#10'脚本:'#13#10'%s';
  SVarTypeMismatch = '变量 %s 不存在或类型不匹配';

function ToVariant(V: Icefv8Value): Variant;
var
  i: Integer;
  procedure AsObject;
  var
    AList: TStringList;
    AVal: Icefv8Value;
    i, t, c: Integer;
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
          t := c shl 1;
          AValues[t] := AList[i];
          AValues[t + 1] := ToVariant(AVal);
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

{ TJsVars }

constructor TJsVars.Create(ABrowser: TDcefBrowser);
begin
  inherited Create;
  FBrowser := ABrowser;
  FEvent := TEvent.Create(nil, false, false, '');
end;

destructor TJsVars.Destroy;
begin
  FreeAndNil(FEvent);
  inherited;
end;

function TJsVars.ExecuteScript(const AScript: String): Variant;
var
  AExcept: Exception;
begin
  VarHelper := Self;
  AExcept := nil;
  VarClear(Result);
  FBrowser.RunInRenderProcess(
    procedure(ABrowser: ICefBrowser; aContext: ICefv8Context; aData: Pointer)
    var
      AResult: Icefv8Value;
      AException: ICefv8Exception;
    begin
      try
        if Assigned(aContext) and aContext.Enter then
        begin
          if not aContext.Eval(AScript, AResult, AException) then
          begin
            AExcept := Exception.CreateFmt(SJSException, [AException.Message,
              AException.LineNumber, AException.StartColumn,
              AException.SourceLine]);
          end
          else
            PVariant(aData)^ := ToVariant(AResult);
          aContext.Exit;
        end;
      finally
        VarHelper.SetReady;
      end;
    end, @Result);
  Wait;
  if AExcept <> nil then
    raise AExcept;
end;

procedure TJsVars.ExecuteScript(const AScript, AUrl: String;
AFromLine: Integer);
var
  S: String;
begin
  VarHelper := Self;
  S := 'try {'#13#10 + S + '}#13#10finaly{TJsDHelper.SetReady();}';
  FBrowser.ExecuteJavaScript(S);
  Wait;
end;

function TJsVars.GetAsBoolean(AName: String): Boolean;
begin
  Result := false;
  VarHelper := Self;
  FBrowser.RunInRenderProcess(
    procedure(ABrowser: ICefBrowser; aContext: ICefv8Context; aData: Pointer)
    var
      AValue: Icefv8Value;
      AException: ICefv8Exception;
    begin
      try
        if Assigned(aContext) and aContext.Enter then
        begin
          AValue := aContext.Global.GetValueByKey(AName);
          if AValue.IsValid then
          begin
            if AValue.IsUndefined then
            begin
              if aContext.Eval(AName, AValue, AException) then
                PBoolean(aData)^ := ToVariant(AValue)
              else
                raise Exception.CreateFmt(SVarTypeMismatch, [AName]);
            end
            else
              PBoolean(aData)^ := ToVariant(AValue)
          end
          else
            raise Exception.CreateFmt(SVarTypeMismatch, [AName]);
        end;
      finally
        VarHelper.SetReady;
      end;
    end, @Result);
  Wait;
end;

function TJsVars.GetAsFloat(AName: String): Double;
begin
  Result := 0;
  VarHelper := Self;
  FBrowser.RunInRenderProcess(
    procedure(ABrowser: ICefBrowser; aContext: ICefv8Context; aData: Pointer)
    var
      AValue: Icefv8Value;
      AException: ICefv8Exception;
    begin
      try
        if Assigned(aContext) and aContext.Enter then
        begin
          AValue := aContext.Global.GetValueByKey(AName);
          if AValue.IsValid then
          begin
            if AValue.IsUndefined then
            begin
              if aContext.Eval(AName, AValue, AException) then
                PDouble(aData)^ := ToVariant(AValue)
              else
                raise Exception.CreateFmt(SVarTypeMismatch, [AName]);
            end
            else
              PDouble(aData)^ := ToVariant(AValue)
          end
          else
            raise Exception.CreateFmt(SVarTypeMismatch, [AName]);
        end;
      finally
        VarHelper.SetReady;
      end;
    end, @Result);
  Wait;
end;

function TJsVars.GetAsInteger(AName: String): Int64;
begin
  Result := 0;
  VarHelper := Self;
  FBrowser.RunInRenderProcess(
    procedure(ABrowser: ICefBrowser; aContext: ICefv8Context; aData: Pointer)
    var
      AValue: Icefv8Value;
      AException: ICefv8Exception;
    begin
      try
        if Assigned(aContext) and aContext.Enter then
        begin
          AValue := aContext.Global.GetValueByKey(AName);
          if AValue.IsValid then
          begin
            if AValue.IsUndefined then
            begin
              if aContext.Eval(AName, AValue, AException) then
                PInteger(aData)^ := ToVariant(AValue)
              else
                raise Exception.CreateFmt(SVarTypeMismatch, [AName]);
            end
            else
              PInteger(aData)^ := ToVariant(AValue)
          end
          else
            raise Exception.CreateFmt(SVarTypeMismatch, [AName]);
        end;
      finally
        VarHelper.SetReady;
      end;
    end, @Result);
  Wait;
end;

function TJsVars.GetAsString(AName: String): String;
begin
  Result := '';
  VarHelper := Self;
  FBrowser.RunInRenderProcess(
    procedure(ABrowser: ICefBrowser; aContext: ICefv8Context; aData: Pointer)
    var
      AValue: Icefv8Value;
      AException: ICefv8Exception;
    begin
      try
        if Assigned(aContext) and aContext.Enter then
        begin
          AValue := aContext.Global.GetValueByKey(AName);
          if AValue.IsValid then
          begin
            if AValue.IsUndefined then
            begin
              if aContext.Eval(AName, AValue, AException) then
                PString(aData)^ := ToVariant(AValue)
              else
                raise Exception.CreateFmt(SVarTypeMismatch, [AName]);
            end
            else
              PString(aData)^ := ToVariant(AValue)
          end
          else
            raise Exception.CreateFmt(SVarTypeMismatch, [AName]);
        end;
      finally
        VarHelper.SetReady;
      end;
    end, @Result);
  Wait;
end;

function TJsVars.GetDefined(AName: String): Boolean;
begin
  Result := false;
  VarHelper := Self;
  FBrowser.RunInRenderProcess(
    procedure(ABrowser: ICefBrowser; aContext: ICefv8Context; aData: Pointer)
    begin
      if Assigned(aContext) then
      begin
        if aContext.Enter then
        begin
          PBoolean(aData)^ := aContext.Global.HasValueByKey(AName);
          aContext.Exit;
        end;
      end;
      VarHelper.SetReady;
    end, @Result);
  Wait;
end;

class procedure TJsVars.RegisterJsNotify(ANotify: TJSNotify);
begin
  if Assigned(ANotify) then
    _DVars.Register(ANotify);
end;

procedure TJsVars.SetAsBoolean(AName: String; const Value: Boolean);
begin
  VarHelper := Self;
  FBrowser.RunInRenderProcess(
    procedure(ABrowser: ICefBrowser; aContext: ICefv8Context; aData: Pointer)
    begin
      try
        if Assigned(aContext) and aContext.Enter then
        begin
          aContext.Global.SetValueByKey(AName,
            TCefv8ValueRef.NewBool(Value), []);
          aContext.Exit;
        end;
      finally
        VarHelper.SetReady;
      end;
    end, nil);
  Wait;
end;

procedure TJsVars.SetAsFloat(AName: String; const Value: Double);
begin
  VarHelper := Self;
  FBrowser.RunInRenderProcess(
    procedure(ABrowser: ICefBrowser; aContext: ICefv8Context; aData: Pointer)
    begin
      try
        if Assigned(aContext) and aContext.Enter then
        begin
          aContext.Global.SetValueByKey(AName,
            TCefv8ValueRef.NewDouble(Value), []);
          aContext.Exit;
        end;
      finally
        VarHelper.SetReady;
      end;
    end, nil);
  Wait;
end;

procedure TJsVars.SetAsInteger(AName: String; const Value: Int64);
begin
  VarHelper := Self;
  FBrowser.RunInRenderProcess(
    procedure(ABrowser: ICefBrowser; aContext: ICefv8Context; aData: Pointer)
    begin
      try
        if Assigned(aContext) and aContext.Enter then
        begin
          aContext.Global.SetValueByKey(AName,
            TCefv8ValueRef.NewInt(Value), []);
          aContext.Exit;
        end;
      finally
        VarHelper.SetReady;
      end;
    end, nil);
  Wait;
end;

procedure TJsVars.SetAsString(AName: String; const Value: String);
begin
  VarHelper := Self;
  FBrowser.RunInRenderProcess(
    procedure(ABrowser: ICefBrowser; aContext: ICefv8Context; aData: Pointer)
    begin
      try
        if Assigned(aContext) and aContext.Enter then
        begin
          aContext.Global.SetValueByKey(AName,
            TCefv8ValueRef.NewString(Value), []);
          aContext.Exit;
        end;
      finally
        VarHelper.SetReady;
      end;
    end, nil);
  Wait;
end;

procedure TJsVars.SetReady;
begin
  FEvent.SetEvent;
end;

class procedure TJsVars.UnregisterJsNotify(ANotify: TJSNotify);
begin
  if Assigned(ANotify) then
    _DVars.Unregister(ANotify);
end;

function TJsVars.Wait(ATimeout: Cardinal): TWaitResult;
begin
  Result := FEvent.WaitFor(INFINITE);
end;
{ DVars }

constructor DVars.Create;
begin
  inherited;
  FLocker := TCriticalSection.Create;
  FHandlers := TList<TJSNotify>.Create;
end;

destructor DVars.Destroy;
begin
  FreeAndNil(FLocker);
  FreeAndNil(FHandlers);
  inherited;
end;

class procedure DVars.Notify(S: String);
begin
  _DVars.ProcessNotify(S);
end;

procedure DVars.ProcessNotify(S: String);
var
  i: Integer;
  AHandled: Boolean;
begin
  FLocker.Enter;
  try
    AHandled := false;
    for i := 0 to FHandlers.Count - 1 do
    begin
      FHandlers[i](S, AHandled);
      if AHandled then
        Break;
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure DVars.Register(ANotify: TJSNotify);
begin
  FLocker.Enter;
  try
    FHandlers.Add(ANotify);
  finally
    FLocker.Leave;
  end;
end;

procedure DVars.Unregister(ANotify: TJSNotify);
var
  i: Integer;
begin
  FLocker.Enter;
  try
    for i := 0 to FHandlers.Count - 1 do
    begin
      if TMethod(FHandlers[i]) = TMethod(ANotify) then
      begin
        FHandlers.Delete(i);
        Exit;
      end;
    end;
  finally
    FLocker.Leave;
  end;
end;

initialization

_DVars := DVars.Create;
DcefbApp.RegisterClasses([DVars]);

finalization

FreeAndNil(_DVars);

end.
