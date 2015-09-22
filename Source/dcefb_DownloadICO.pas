unit dcefb_DownloadICO;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.ComCtrls,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.ImgList,
  Vcl.Graphics, System.StrUtils, Generics.Collections,
  System.SysUtils, Winapi.Messages, System.Math, RegularExpressions,
  WinInet, debuginfo;

function MatchIco(source: string; BrowserURL: string): string;
function WinInet_HttpGet(const URL: string; Stream: TStream): Boolean;
function test: string;

implementation

function DealURLText(str: string; FAdd: Boolean; Const AddStr: string = '/favicon.ico'): string;
var
  int: Integer;
  tempstr: string;
begin
  if pos('http://', str) > 0 then
  begin
    Delete(str, 1, 7);
    tempstr := 'http://';
  end;
  if pos('https://', str) > 0 then
  begin
    Delete(str, 1, 8);
    tempstr := 'https://';
  end;
  int := pos('/', str);
  if int > 0 then
    str := copy(str, 0, int - 1);
  if tempstr <> '' then
    Result := tempstr + str
  else
    Result := 'http://' + str;
  if FAdd then
    Result := Result + AddStr;
end;

function MatchIco(source: string; BrowserURL: string): string;
var
  reg: TRegEx;
  match: TMatch;
  PosInt: Integer;
  IcoStr, URLStr: string;

  function RightPos(const SubStr, str: string): Integer;
  var
    i, j, k, LenSub, LenS: Integer;
  begin
    Result := 0;
    k := 0;
    LenSub := Length(SubStr);
    LenS := Length(str);
    if (LenSub = 0) or (LenS = 0) or (LenSub > LenS) then
      Exit;
    for i := LenS downto 1 do
    begin
      if str[i] = SubStr[LenSub] then
      begin
        k := i - 1;
        for j := LenSub - 1 downto 1 do
        begin
          if str[k] = SubStr[j] then
            Dec(k)
          else
            Break;
        end;
      end;
      if i - k = LenSub then
      begin
        Result := k + 1;
        Exit;
      end;
    end;
  end;

const // 正则写的应该有问题...勉强实现
  Pattern = '(href\s?=\s?["|''|\s]?)(\S+/)(\S+\.ico|(.ico[^"^''^ ])+)';
begin
  reg := TRegEx.Create(Pattern, [roIgnoreCase]);
  match := reg.match(source);
  if match.Success then
  begin
    IcoStr := trim(match.value);
    Delete(IcoStr, 1, 6); // 去 href="

    if copy(IcoStr, 0, 1) = '/' then
      IcoStr := DealURLText(BrowserURL, True, IcoStr)
    else if (pos('http://', IcoStr) <= 0) and (pos('https://', IcoStr) <= 0) then
    begin
     // PosInt := RightPos('/', IcoStr);
     // Delete(IcoStr, PosInt + 1, Length(IcoStr) - PosInt);

      URLStr := BrowserURL;

      while copy(IcoStr, 1, 3) = '../' do
      begin
        Delete(IcoStr, 1, 3);
        if Copy(URLStr, Length(URLStr), 1) = '/' then
          Delete(URLStr, Length(URLStr) , 1);
        PosInt := RightPos('/', URLStr);
        Delete(URLStr, PosInt + 1, Length(URLStr) - PosInt);
      end;

    end;
    Result := URLStr + IcoStr;

  end
  else
    Result := DealURLText(BrowserURL, True);
end;

function test: string;
begin
  Result := MatchIco('href="../../fangyuan.ico"', 'wwww.baidu.com');
end;

function WinInet_HttpGet(const URL: string; Stream: TStream): Boolean; overload;
const
  BuffSize = 2048;
var
  hInter: HINTERNET;
  UrlHandle: HINTERNET;
  BytesRead: dWord;
  dwindex, dwcodelen: dWord;
  Buffer: Pointer;
  dwcode: array [1 .. 20] of Char;
  res: PChar;
begin
  Result := False;
  hInter := InternetOpen('Mozilla/3.0', INTERNET_OPEN_TYPE_PRECONFIG,
    nil, nil, 0);
  if Assigned(hInter) then
  begin
    Stream.Seek(0, 0);
    GetMem(Buffer, BuffSize);
    try
      UrlHandle := InternetOpenUrl(hInter, PWideChar(URL), nil, 0,
        INTERNET_FLAG_RELOAD, 0);
      dwindex := 0;
      dwcodelen := 10;
      HttpQueryInfo(UrlHandle, HTTP_QUERY_STATUS_CODE, @dwcode,
        dwcodelen, dwindex);
      res := PChar(@dwcode);
      if (res = '200') or (res = '302') then // 文件存在
      begin
        Result := True;
        if Assigned(UrlHandle) then
        begin
          repeat
            InternetReadFile(UrlHandle, Buffer, BuffSize, BytesRead);
            if BytesRead > 0 then
              Stream.WriteBuffer(Buffer^, BytesRead);
          until BytesRead = 0;
          InternetCloseHandle(UrlHandle);
        end;
      end
      else
        Result := False;
    finally
      FreeMem(Buffer);
    end;
    InternetCloseHandle(hInter);
  end
end;

function WinInet_HttpGet(const URL: string): string; overload;
Var
  StringStream: TStringStream;
begin
  Result := '';
  StringStream := TStringStream.Create;
  try
    WinInet_HttpGet(URL, StringStream);
    if StringStream.Size > 0 then
    begin
      StringStream.Seek(0, 0);
      Result := StringStream.ReadString(StringStream.Size);
    end;
  finally
    StringStream.Free;
  end;
end;

end.
