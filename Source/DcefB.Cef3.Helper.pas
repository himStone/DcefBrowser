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

unit DcefB.Cef3.Helper;

{$IFDEF FPC}
{$MODE DELPHI}{$H+}
{$ENDIF}
{$I DcefB.Dcef3.cef.inc}

interface

uses
  SysUtils, Classes
{$IFDEF MSWINDOWS}
    , Windows
{$ENDIF}
{$IFNDEF FPC}
{$ENDIF}, DcefB.Cef3.Interfaces, DcefB.Cef3.Types, DcefB.Cef3.Api;

type
  TCef3Helper = class
    class function CefGetData(const i: ICefBase): Pointer;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    class function CefGetObject(ptr: Pointer): TObject;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    class function CefString(const str: ustring): TCefString; overload;
    class function CefString(const str: PCefString): ustring; overload;
{$IFDEF MSWINDOWS}
    class function CefCurrentlyOn(threadId: TCefThreadId): Boolean;
    class procedure CefPostTask(threadId: TCefThreadId; const task: ICefTask);
    class procedure CefPostDelayedTask(threadId: TCefThreadId;
      const task: ICefTask; delayMs: Int64);
    class function CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
    class function SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
    class function CefTimeToDateTime(const dt: TCefTime): TDateTime;
    class function DateTimeToCefTime(dt: TDateTime): TCefTime;
{$ELSE}
    class function CefTimeToDateTime(const dt: TCefTime): TDateTime;
    class function DateTimeToCefTime(dt: TDateTime): TCefTime;
{$ENDIF}
    class function CefStringAlloc(const str: ustring): TCefString;

    class function CefUserFreeString(const str: ustring): PCefStringUserFree;

    class function CefParseUrl(const url: ustring;
      var parts: TUrlParts): Boolean;
    class function CefCreateUrl(var parts: TUrlParts): ustring;
    class function CefFormatUrlForSecurityDisplay(const originUrl, languages: string): string;
    class function CefGetMimeType(const extension: ustring): ustring;
    class procedure CefGetExtensionsForMimeType(const mimeType: ustring;
      extensions: TStringList);

    function CefBase64Encode(const data: Pointer; dataSize: NativeUInt): ustring;
    function CefBase64Decode(const data: ustring): ICefBinaryValue;
    function CefUriEncode(const text: ustring; usePlus: Boolean): ustring;
    function CefUriDecode(const text: ustring; convertToUtf8: Boolean;
      unescapeRule: TCefUriUnescapeRule): ustring;
    function CefParseCssColor(const str: ustring; strict: Boolean; out color: TCefColor): Boolean;
    {$ifdef Win32}
    function CefParseJson(const jsonString: ustring; options: TCefJsonParserOptions): ICefValue;
    function CefParseJsonAndReturnError(const jsonString: ustring; options: TCefJsonParserOptions;
      out errorCodeOut: TCefJsonParserError; out errorMsgOut: ustring): ICefValue;
    function CefWriteJson(const node: ICefValue; options: TCefJsonWriterOptions): ustring;
    {$endif}

    class function CefStringClearAndGet(var str: TCefString): ustring;
    class procedure CefStringFree(const str: PCefString);
    class function CefStringFreeAndGet(const str: PCefStringUserFree): ustring;
    class procedure CefStringSet(const str: PCefString; const value: ustring);

    class function CefGetPath(key: TCefPathKey; out path: ustring): Boolean;

    class function CefBeginTracing(const categories: ustring;
      const callback: ICefCompletionCallback): Boolean;
    class function CefEndTracing(const tracingFile: ustring;
      const callback: ICefEndTracingCallback): Boolean;
    class function CefNowFromSystemTraceTime: Int64;

    class function CefGetGeolocation(const callback
      : ICefGetGeolocationCallback): Boolean;
  end;

implementation

uses DcefB.Cef3.Classes;

{$IFDEF MSWINDOWS}
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation
  : PTimeZoneInformation; lpLocalTime, lpUniversalTime: PSystemTime): BOOL;
  stdcall; external 'kernel32.dll';

function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation
  : PTimeZoneInformation; lpUniversalTime, lpLocalTime: PSystemTime): BOOL;
  stdcall; external 'kernel32.dll';
{$ENDIF}

procedure _free_string(str: PChar16); stdcall;
begin
  if str <> nil then
    FreeMem(str);
end;

class function TCef3Helper.CefGetData(const i: ICefBase): Pointer;
begin
  if i <> nil then
    Result := i.Wrap
  else
    Result := nil;
end;

class function TCef3Helper.CefGetObject(ptr: Pointer): TObject;
begin
  Dec(PByte(ptr), SizeOf(Pointer));
  Result := TObject(PPointer(ptr)^);
end;

class function TCef3Helper.CefString(const str: ustring): TCefString;
begin
  Result.str := PChar16(PWideChar(str));
  Result.length := length(str);
  Result.dtor := nil;
end;

class function TCef3Helper.CefString(const str: PCefString): ustring;
begin
  if str <> nil then
    SetString(Result, str.str, str.length)
  else
    Result := '';
end;

{$IFDEF MSWINDOWS}

class function TCef3Helper.CefCurrentlyOn(threadId: TCefThreadId): Boolean;
begin
  Result := cef_currently_on(threadId) <> 0;
end;

class procedure TCef3Helper.CefPostTask(threadId: TCefThreadId;
  const task: ICefTask);
begin
  cef_post_task(threadId, TCef3Helper.CefGetData(task));
end;

class procedure TCef3Helper.CefPostDelayedTask(threadId: TCefThreadId;
  const task: ICefTask; delayMs: Int64);
begin
  cef_post_delayed_task(threadId, TCef3Helper.CefGetData(task), delayMs);
end;

class function TCef3Helper.CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
begin
  Result.wYear := dt.year;
  Result.wMonth := dt.month;
  Result.wDayOfWeek := dt.day_of_week;
  Result.wDay := dt.day_of_month;
  Result.wHour := dt.hour;
  Result.wMinute := dt.minute;
  Result.wSecond := dt.second;
  Result.wMilliseconds := dt.millisecond;
end;

class function TCef3Helper.SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
begin
  Result.year := dt.wYear;
  Result.month := dt.wMonth;
  Result.day_of_week := dt.wDayOfWeek;
  Result.day_of_month := dt.wDay;
  Result.hour := dt.wHour;
  Result.minute := dt.wMinute;
  Result.second := dt.wSecond;
  Result.millisecond := dt.wMilliseconds;
end;

class function TCef3Helper.CefTimeToDateTime(const dt: TCefTime): TDateTime;
var
  st: TSystemTime;
begin
  st := CefTimeToSystemTime(dt);
  SystemTimeToTzSpecificLocalTime(nil, @st, @st);
  Result := SystemTimeToDateTime(st);
end;

class function TCef3Helper.DateTimeToCefTime(dt: TDateTime): TCefTime;
var
  st: TSystemTime;
begin
  DateTimeToSystemTime(dt, st);
  TzSpecificLocalTimeToSystemTime(nil, @st, @st);
  Result := SystemTimeToCefTime(st);
end;
{$ELSE}

class function TCef3Helper.CefTimeToDateTime(const dt: TCefTime): TDateTime;
begin
  Result := EncodeDate(dt.year, dt.month, dt.day_of_month) +
    EncodeTime(dt.hour, dt.minute, dt.second, dt.millisecond);
end;

class function TCef3Helper.DateTimeToCefTime(dt: TDateTime): TCefTime;
var
  year, month, Day, hour, Min, Sec, MSec: word;
begin
  DecodeDate(dt, year, month, Day);
  DecodeTime(dt, hour, Min, Sec, MSec);
  Result.year := year;
  Result.month := month;
  Result.day_of_week := DayOfWeek(dt);
  Result.day_of_month := month;
  Result.hour := hour;
  Result.minute := Min;
  Result.second := Sec;
  Result.millisecond := MSec;
end;
{$ENDIF}

class function TCef3Helper.CefStringAlloc(const str: ustring): TCefString;
begin
  FillChar(Result, SizeOf(Result), 0);
  if str <> '' then
    cef_string_from_wide(PWideChar(str), length(str), @Result);
end;

function TCef3Helper.CefUriDecode(const text: ustring; convertToUtf8: Boolean;
  unescapeRule: TCefUriUnescapeRule): ustring;
var
  s: TCefString;
begin
  s := CefString(text);
  Result := CefStringFreeAndGet(cef_uridecode(@s, Ord(convertToUtf8), unescapeRule));
end;

function TCef3Helper.CefParseCssColor(const str: ustring; strict: Boolean; out color: TCefColor): Boolean;
var
  s: TCefString;
begin
  s := CefString(str);
  Result := cef_parse_csscolor(@s, Ord(strict), @color) <> 0;
end;


function TCef3Helper.CefUriEncode(const text: ustring;
  usePlus: Boolean): ustring;
var
  s: TCefString;
begin
  s := CefString(text);
  Result := CefStringFreeAndGet(cef_uriencode(@s, Ord(usePlus)));
end;


class function TCef3Helper.CefUserFreeString(const str: ustring)
  : PCefStringUserFree;
begin
  Result := cef_string_userfree_alloc;
  Result.length := length(str);
  GetMem(Result.str, Result.length * SizeOf(TCefChar));
  Move(PCefChar(str)^, Result.str^, Result.length * SizeOf(TCefChar));
  Result.dtor := @_free_string;
end;

{$ifdef Win32}
function TCef3Helper.CefParseJson(const jsonString: ustring;
  options: TCefJsonParserOptions): ICefValue;
var
  s: TCefString;
begin
  s := CefString(jsonString);
  Result := TCefValueRef.UnWrap(cef_parse_json(@s, options));
end;

function TCef3Helper.CefParseJsonAndReturnError(const jsonString: ustring;
  options: TCefJsonParserOptions; out errorCodeOut: TCefJsonParserError;
  out errorMsgOut: ustring): ICefValue;
var
  s, e: TCefString;
begin
  s := CefString(jsonString);
  FillChar(e, SizeOf(e), 0);
  Result := TCefValueRef.UnWrap(cef_parse_jsonand_return_error(@s, options,
    @errorCodeOut, @e));
  errorMsgOut := CefString(@e);
end;

function TCef3Helper.CefWriteJson(const node: ICefValue;
  options: TCefJsonWriterOptions): ustring;
begin
  Result := CefStringFreeAndGet(cef_write_json(CefGetData(node), options));
end;
{$endif}

class function TCef3Helper.CefParseUrl(const url: ustring;
  var parts: TUrlParts): Boolean;
var
  u: TCefString;
  p: TCefUrlParts;
begin
  FillChar(p, SizeOf(p), 0);
  u := CefString(url);
  Result := cef_parse_url(@u, p) <> 0;
  if Result then
  begin
    // parts.spec := CefString(@p.spec);
    parts.scheme := CefString(@p.scheme);
    parts.username := CefString(@p.username);
    parts.password := CefString(@p.password);
    parts.host := CefString(@p.host);
    parts.port := CefString(@p.port);
    parts.origin := CefString(@p.origin);
    parts.path := CefString(@p.path);
    parts.query := CefString(@p.query);
  end;
end;

class function TCef3Helper.CefCreateUrl(var parts: TUrlParts): ustring;
var
  p: TCefUrlParts;
  u: TCefString;
begin
  FillChar(p, SizeOf(p), 0);
  p.spec := CefString(parts.spec);
  p.scheme := CefString(parts.scheme);
  p.username := CefString(parts.username);
  p.password := CefString(parts.password);
  p.host := CefString(parts.host);
  p.port := CefString(parts.port);
  p.origin := CefString(parts.origin);
  p.path := CefString(parts.path);
  p.query := CefString(parts.query);
  FillChar(u, SizeOf(u), 0);
  if cef_create_url(@p, @u) <> 0 then
    Result := CefString(@u)
  else
    Result := '';
end;

class function TCef3Helper.CefGetMimeType(const extension: ustring): ustring;
var
  s: TCefString;
begin
  s := CefString(extension);
  Result := CefStringFreeAndGet(cef_get_mime_type(@s))
end;

class procedure TCef3Helper.CefGetExtensionsForMimeType(const mimeType: ustring;
  extensions: TStringList);
var
  list: TCefStringList;
  s, str: TCefString;
  i: Integer;
begin
  list := cef_string_list_alloc();
  try
    s := CefString(mimeType);
    cef_get_extensions_for_mime_type(@s, list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      extensions.Add(CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

class function TCef3Helper.CefStringClearAndGet(var str: TCefString): ustring;
begin
  Result := TCef3Helper.CefString(@str);
  cef_string_clear(@str);
end;

class procedure TCef3Helper.CefStringFree(const str: PCefString);
begin
  if str <> nil then
    cef_string_clear(str);
end;

class function TCef3Helper.CefStringFreeAndGet
  (const str: PCefStringUserFree): ustring;
begin
  if str <> nil then
  begin
    Result := TCef3Helper.CefString(PCefString(str));
    cef_string_userfree_free(str);
  end
  else
    Result := '';
end;

class procedure TCef3Helper.CefStringSet(const str: PCefString;
  const value: ustring);
begin
  if str <> nil then
    cef_string_set(PWideChar(value), length(value), str, 1);
end;

class function TCef3Helper.CefGetPath(key: TCefPathKey; out path: ustring): Boolean;
var
  p: TCefString;
begin
  p := CefString('');
  Result := cef_get_path(key, @p) <> 0;
  path := CefStringClearAndGet(p);
end;

function TCef3Helper.CefBase64Decode(const data: ustring): ICefBinaryValue;
var
  s: TCefString;
begin
  s := CefString(data);
  Result := TCefBinaryValueRef.UnWrap(cef_base64decode(@s));
end;

function TCef3Helper.CefBase64Encode(const data: Pointer;
  dataSize: NativeUInt): ustring;
begin
  Result:= CefStringFreeAndGet(cef_base64encode(data, dataSize));
end;

class function TCef3Helper.CefBeginTracing(const categories: ustring;
  const callback: ICefCompletionCallback): Boolean;
var
  c: TCefString;
begin
  c := CefString(categories);
  Result := cef_begin_tracing(@c, CefGetData(callback)) <> 0;
end;

class function TCef3Helper.CefEndTracing(const tracingFile: ustring;
  const callback: ICefEndTracingCallback): Boolean;
var
  s: TCefString;
begin
  s := CefString(tracingFile);
  Result := cef_end_tracing(@s, CefGetData(callback)) <> 0;
end;

class function TCef3Helper.CefFormatUrlForSecurityDisplay(
  const originUrl, languages: string): string;
var
  o, l: TCefString;
begin
  o := CefString(originUrl);
  l := CefString(languages);
  Result := CefStringFreeAndGet(cef_format_url_for_security_display(@o, @l));
end;

class function TCef3Helper.CefNowFromSystemTraceTime: Int64;
begin
  Result := cef_now_from_system_trace_time();
end;

class function TCef3Helper.CefGetGeolocation(const callback: ICefGetGeolocationCallback): Boolean;
begin
  Result := cef_get_geolocation(CefGetData(callback)) <> 0;
end;


end.
