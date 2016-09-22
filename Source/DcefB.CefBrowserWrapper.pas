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

unit DcefB.CefBrowserWrapper;

interface

uses
  Windows, Classes, Generics.Collections, Vcl.Graphics,
  DcefB.Cef3.Interfaces, DcefB.res, DcefB.Handler.Basic, DcefB.Events, DcefB.Utils;

type
  TBrowserWrapperDic = class;

  TCefBrowserWrapper = class
  private
    FParent: TBrowserWrapperDic;
    FBrowser: ICefBrowser;
    FLoadingState: Integer;
    FLastTitle: string;
    FLastAddress: string;
    FLastFavUrl: string;
    FFaviconGetter: TFaviconGetter;
    FFavicon: TIcon;
    procedure SetLastFavUrl(const Value: string);
    procedure IconReady(aUrl: string; aStream: TStream);
    procedure SetFavicon(const Value: TIcon);
  public
    constructor Create(aBrowser: ICefBrowser; aParent: TBrowserWrapperDic);
    destructor Destroy; override;

    property LoadingState: Integer read FLoadingState write FLoadingState;
    property LastTitle: string read FLastTitle write FLastTitle;
    property LastFavUrl: string read FLastFavUrl write SetLastFavUrl;
    property LastAddress: string read FLastAddress write FLastAddress;
    property Favicon: TIcon read FFavicon write SetFavicon;
    property Browser: ICefBrowser read FBrowser;
  end;

  TBrowserWrapperDic = class(TObjectDictionary<Integer, TCefBrowserWrapper>)
  private
    FEvents: IDcefBrowser;
  public
    constructor Create(aEvents: IDcefBrowser);
    destructor Destroy; override;
    procedure Add(aBrowser: ICefBrowser);
    procedure Clear;
  end;

implementation

{ ICefBrowserWapper }

constructor TCefBrowserWrapper.Create(aBrowser: ICefBrowser; aParent: TBrowserWrapperDic);
begin
  FBrowser := aBrowser;
  FParent := aParent;
  FLastTitle := SLoadingText;
  FLastAddress := '';
  FLastFavUrl := '';
  FLoadingState := 0 or State_IsLoading;
end;

destructor TCefBrowserWrapper.Destroy;
begin
  if Assigned(FFaviconGetter) then
    FFaviconGetter.Cancel;
  //FFaviconGetter.Free;
  FFavicon.Free;
  FBrowser := nil;
  inherited;
end;

procedure TCefBrowserWrapper.IconReady(aUrl: string; aStream: TStream);

  function IsIcon(Stream: TStream): Boolean;
  var
    Image: TMemoryStream;
    CI: TCursorOrIcon;
  begin
    Result := true;
    Image := TMemoryStream.Create;
    try
      Image.SetSize(Stream.Size - Stream.Position);
      Stream.ReadBuffer(Image.Memory^, Image.Size);
      Image.ReadBuffer(CI, SizeOf(CI));
      if not(CI.wType in [RC3_STOCKICON, RC3_ICON]) then
        Result := False;
    except
      Image.Free;
    end;
  end;

begin
  if FLastFavUrl = aUrl then
  begin
    if IsIcon(aStream) then
    begin
      if Assigned(FFavicon) then
      begin
        FFavicon.Free;
        FFavicon := nil;
      end;
      aStream.Seek(0, 0);
      FFavicon := TIcon.Create;
      FFavicon.LoadFromStream(aStream);
      FParent.FEvents.doOnFaviconChange(FBrowser, aUrl, FFavicon);
    end;
  end;
end;

procedure TCefBrowserWrapper.SetFavicon(const Value: TIcon);
begin
  FFavicon := Value;
end;

procedure TCefBrowserWrapper.SetLastFavUrl(const Value: string);
begin
  FLastFavUrl := Value;
  FFaviconGetter := TFaviconGetter.Create(Value, IconReady);
end;

{ TBrowserWrapperDic }

procedure TBrowserWrapperDic.Add(aBrowser: ICefBrowser);
var
  aCefBrowserWrapper: TCefBrowserWrapper;
begin
  if (aBrowser <> nil) and (aBrowser.Identifier <> -1) then
  begin
    aCefBrowserWrapper := TCefBrowserWrapper.Create(aBrowser, Self);
    inherited Add(aBrowser.Identifier, aCefBrowserWrapper);
  end;
end;

procedure TBrowserWrapperDic.Clear;
var
  Item: TCefBrowserWrapper;
  //handle: HWND;
begin
  for Item in inherited Values do
  begin
    //handle := Item.Browser.host.WindowHandle;
    Item.Browser.StopLoad;
    //DestroyWindow(Item.Browser.host.WindowHandle);
  end;
  inherited;
end;

constructor TBrowserWrapperDic.Create(aEvents: IDcefBrowser);
begin
  inherited Create([doOwnsValues]);
  FEvents := aEvents;
end;

destructor TBrowserWrapperDic.Destroy;
begin
  FEvents := nil;
  inherited;
end;

end.
