(*
  Delphi Multi-tab Chromium Browser Frame

  Unit owner : BccSafe
  QQ: 1262807955
  Email: bccsafe5988@gmail.com
  Web site   : http://www.bccsafe.com
  Repository : https://github.com/bccsafe/DcefBrowser
*)

unit DcefB.CefBrowserWrapper;

interface

uses
  System.Classes, Generics.Collections,
  DcefB.Dcef3.CefLib, DcefB.res;

type
  TCefBrowserWrapper = class
  private
    FBrowser: ICefBrowser;
    FLoadingState: Integer;
    FLastTitle: string;
    FLastAddress: string;
  public
    constructor Create(aBrowser: ICefBrowser);
    destructor Destroy;

    property LoadingState: Integer read FLoadingState write FLoadingState;
    property LastTitle: string read FLastTitle write FLastTitle;
    property LastAddress: string read FLastAddress write FLastAddress;
    property Browser: ICefBrowser read FBrowser;
  end;

  TBrowserWrapperDic = class(TDictionary<Integer, TCefBrowserWrapper>)
  public
    function Add(aBrowser: ICefBrowser): Integer;
  end;

implementation

{ ICefBrowserWapper }

constructor TCefBrowserWrapper.Create(aBrowser: ICefBrowser);
begin
  FBrowser := aBrowser;
  FLastTitle := SLoadingText;
  FLastAddress := '';
  FLoadingState := 0 or State_IsLoading;
end;

destructor TCefBrowserWrapper.Destroy;
begin
  FBrowser := nil;
end;

{ TBrowserWrapperDic }

function TBrowserWrapperDic.Add(aBrowser: ICefBrowser): Integer;
var
  aCefBrowserWrapper: TCefBrowserWrapper;
begin
  aCefBrowserWrapper := TCefBrowserWrapper.Create(aBrowser);
  inherited Add(aBrowser.Identifier, aCefBrowserWrapper);
end;

end.
