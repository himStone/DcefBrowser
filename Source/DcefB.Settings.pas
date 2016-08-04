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

unit DcefB.Settings;

interface

uses
  Classes, SysUtils, DcefB.Cef3.Types;

type
  PChromiumOptions = ^TChromiumOptions;
  PChromiumFontOptions = ^TChromiumFontOptions;
  PDcefBOptions = ^TDcefBOptions;

  TChromiumOptions = class(TPersistent)
  private
    FWindowlessFrameRate: Integer;
    FJavascript: TCefState;
    FJavascriptOpenWindows: TCefState;
    FJavascriptCloseWindows: TCefState;
    FJavascriptAccessClipboard: TCefState;
    FJavascriptDomPaste: TCefState;
    FCaretBrowsing: TCefState;
    FPlugins: TCefState;
    FUniversalAccessFromFileUrls: TCefState;
    FFileAccessFromFileUrls: TCefState;
    FWebSecurity: TCefState;
    FImageLoading: TCefState;
    FImageShrinkStandaloneToFit: TCefState;
    FTextAreaResize: TCefState;
    FTabToLinks: TCefState;
    FLocalStorage: TCefState;
    FDatabases: TCefState;
    FApplicationCache: TCefState;
    FWebgl: TCefState;
    FBackgroundColor: TCefColor;
    FAcceptLanguageList: ustring;
  public
    constructor Create; virtual;
  published
    property Javascript: TCefState read FJavascript write FJavascript
      default STATE_DEFAULT;
    property JavascriptOpenWindows: TCefState read FJavascriptOpenWindows
      write FJavascriptOpenWindows default STATE_DEFAULT;
    property JavascriptCloseWindows: TCefState read FJavascriptCloseWindows
      write FJavascriptCloseWindows default STATE_DEFAULT;
    property JavascriptAccessClipboard: TCefState
      read FJavascriptAccessClipboard write FJavascriptAccessClipboard
      default STATE_DEFAULT;
    property JavascriptDomPaste: TCefState read FJavascriptDomPaste
      write FJavascriptDomPaste default STATE_DEFAULT;
    property CaretBrowsing: TCefState read FCaretBrowsing write FCaretBrowsing
      default STATE_DEFAULT;
    property Plugins: TCefState read FPlugins write FPlugins
      default STATE_DEFAULT;
    property UniversalAccessFromFileUrls: TCefState
      read FUniversalAccessFromFileUrls write FUniversalAccessFromFileUrls
      default STATE_DEFAULT;
    property FileAccessFromFileUrls: TCefState read FFileAccessFromFileUrls
      write FFileAccessFromFileUrls default STATE_DEFAULT;
    property WebSecurity: TCefState read FWebSecurity write FWebSecurity
      default STATE_DEFAULT;
    property ImageLoading: TCefState read FImageLoading write FImageLoading
      default STATE_DEFAULT;
    property ImageShrinkStandaloneToFit: TCefState
      read FImageShrinkStandaloneToFit write FImageShrinkStandaloneToFit
      default STATE_DEFAULT;
    property TextAreaResize: TCefState read FTextAreaResize
      write FTextAreaResize default STATE_DEFAULT;
    property TabToLinks: TCefState read FTabToLinks write FTabToLinks
      default STATE_DEFAULT;
    property LocalStorage: TCefState read FLocalStorage write FLocalStorage
      default STATE_DEFAULT;
    property Databases: TCefState read FDatabases write FDatabases
      default STATE_DEFAULT;
    property ApplicationCache: TCefState read FApplicationCache
      write FApplicationCache default STATE_DEFAULT;
    property Webgl: TCefState read FWebgl write FWebgl default STATE_DEFAULT;
    property BackgroundColor: TCefColor read FBackgroundColor
      write FBackgroundColor default 0;
    property AcceptLanguageList: ustring read FAcceptLanguageList write FAcceptLanguageList;
    property WindowlessFrameRate: Integer read FWindowlessFrameRate
      write FWindowlessFrameRate default 30;
  end;

  TChromiumFontOptions = class(TPersistent)
  private
    FStandardFontFamily: ustring;
    FCursiveFontFamily: ustring;
    FSansSerifFontFamily: ustring;
    FMinimumLogicalFontSize: Integer;
    FFantasyFontFamily: ustring;
    FSerifFontFamily: ustring;
    FDefaultFixedFontSize: Integer;
    FDefaultFontSize: Integer;
    FRemoteFontsDisabled: TCefState;
    FFixedFontFamily: ustring;
    FMinimumFontSize: Integer;
  public
    constructor Create; virtual;
  published
    property StandardFontFamily: ustring read FStandardFontFamily;
    property FixedFontFamily: ustring read FFixedFontFamily
      write FFixedFontFamily;
    property SerifFontFamily: ustring read FSerifFontFamily
      write FSerifFontFamily;
    property SansSerifFontFamily: ustring read FSansSerifFontFamily
      write FSansSerifFontFamily;
    property CursiveFontFamily: ustring read FCursiveFontFamily
      write FCursiveFontFamily;
    property FantasyFontFamily: ustring read FFantasyFontFamily
      write FFantasyFontFamily;
    property DefaultFontSize: Integer read FDefaultFontSize
      write FDefaultFontSize default 0;
    property DefaultFixedFontSize: Integer read FDefaultFixedFontSize
      write FDefaultFixedFontSize default 0;
    property MinimumFontSize: Integer read FMinimumFontSize
      write FMinimumFontSize default 0;
    property MinimumLogicalFontSize: Integer read FMinimumLogicalFontSize
      write FMinimumLogicalFontSize default 0;
    property RemoteFonts: TCefState read FRemoteFontsDisabled
      write FRemoteFontsDisabled default STATE_DEFAULT;
  end;

  TDcefBOptions = class(TPersistent)
  private
    FDevToolsEnable: Boolean;
    FPopupNewWin: Boolean;
    FCloseWPagesClosed: Boolean;
    FDownLoadPath: string;
    FAutoDown: Boolean;
    FShowLoadError: Boolean;
    procedure SetDownLoadPath(const Value: string);
    procedure SetShowLoadError(const Value: Boolean);
  public
    constructor Create; virtual;
  published
    /// <summary>Is enable development tools, Default: True/summary>
    property DevToolsEnable: Boolean read FDevToolsEnable write FDevToolsEnable
      default True;
    /// <summary>Is always popup new window, Default: False/summary>
    property PopupNewWin: Boolean read FPopupNewWin write FPopupNewWin
      default False;
    /// <summary>Do close when all browser closed, Default: False/summary>
    property CloseWPagesClosed: Boolean read FCloseWPagesClosed
      write FCloseWPagesClosed default True;
    /// <summary>Is auto download file in default folder, Default: False/summary>
    property AutoDown: Boolean read FAutoDown write FAutoDown default False;
    /// <summary>Download Path, Default: (Current folder) + Download /summary>
    property DownLoadPath: string read FDownLoadPath write SetDownLoadPath;
    /// <summary>Is show load Error, Default: False /summary>
    property ShowLoadError: Boolean read FShowLoadError write SetShowLoadError
      default False;
  end;

implementation

{ TChromiumOptions }

constructor TChromiumOptions.Create;
begin
  FWindowlessFrameRate := 30;
  FJavascript := STATE_DEFAULT;
  FJavascriptOpenWindows := STATE_DEFAULT;
  FJavascriptCloseWindows := STATE_DEFAULT;
  FJavascriptAccessClipboard := STATE_DEFAULT;
  FJavascriptDomPaste := STATE_DEFAULT;
  FCaretBrowsing := STATE_DEFAULT;
  FPlugins := STATE_DEFAULT;
  FUniversalAccessFromFileUrls := STATE_DEFAULT;
  FFileAccessFromFileUrls := STATE_DEFAULT;
  FWebSecurity := STATE_DEFAULT;
  FImageLoading := STATE_DEFAULT;
  FImageShrinkStandaloneToFit := STATE_DEFAULT;
  FTextAreaResize := STATE_DEFAULT;
  FTabToLinks := STATE_DEFAULT;
  FLocalStorage := STATE_DEFAULT;
  FDatabases := STATE_DEFAULT;
  FApplicationCache := STATE_DEFAULT;
  FWebgl := STATE_DEFAULT;
  FBackgroundColor := 0;
end;

{ TChromiumFontOptions }

constructor TChromiumFontOptions.Create;
begin
  FStandardFontFamily := '';
  FCursiveFontFamily := '';
  FSansSerifFontFamily := '';
  FMinimumLogicalFontSize := 0;
  FFantasyFontFamily := '';
  FSerifFontFamily := '';
  FDefaultFixedFontSize := 0;
  FDefaultFontSize := 0;
  FRemoteFontsDisabled := STATE_DEFAULT;
  FFixedFontFamily := '';
  FMinimumFontSize := 0;
end;

{ TDcefBOptions }

constructor TDcefBOptions.Create;
begin
  FDownLoadPath := ExtractFilePath(Paramstr(0)) + 'Download\';
end;

procedure TDcefBOptions.SetDownLoadPath(const Value: string);
begin
  if DirectoryExists(Value) then
  begin
    if SameText(Copy(Value, Length(Value), 1), '\') then
      FDownLoadPath := Value
    else
      FDownLoadPath := Value + '\';
  end;
end;

procedure TDcefBOptions.SetShowLoadError(const Value: Boolean);
begin
  FShowLoadError := Value;
end;

end.
