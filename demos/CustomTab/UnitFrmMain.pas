unit UnitFrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, ExtCtrls, Actions, ActnList,
  DcefB.Events, DcefB.Core.DcefBrowser, DcefB.Cef3.Interfaces, DcefB.Cef3.Types;

type
  TMainForm = class(TForm)
    MyCustomTabs: TPageControl;
    DcefBrowser1: TDcefBrowser;
    Panel1: TPanel;
    Button4: TButton;
    Button1: TButton;
    CheckBox1: TCheckBox;
    AddressEdit: TEdit;
    AddButton: TButton;
    LoadButton: TButton;
    ActionList1: TActionList;
    ActIsLoading: TAction;
    ActCanGoBack: TAction;
    ActCanGoForward: TAction;
    StatusPanel: TPanel;
    PopupMenuPageControl: TPopupMenu;
    N1: TMenuItem;
    procedure AddButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActIsLoadingUpdate(Sender: TObject);
    procedure ActCanGoBackUpdate(Sender: TObject);
    procedure ActCanGoForwardUpdate(Sender: TObject);
    procedure ActCanGoBackExecute(Sender: TObject);
    procedure ActCanGoForwardExecute(Sender: TObject);
    procedure MyCustomTabsChange(Sender: TObject);
    procedure N1Click(Sender: TObject);
    function GetTabsheetByBrowserID(Const aBrowserID: Integer): TTabsheet;
    procedure DcefBrowser1AddBrowser(const browser: ICefBrowser);
    procedure DcefBrowser1CloseBrowser(const CloseBrowserIdArr
      : array of Integer; const ShowBrowserId: Integer);
    procedure DcefBrowser1StateChange(const browser: ICefBrowser;
      const Kind: TBrowserDataChangeKind; const Value: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ActCanGoBackExecute(Sender: TObject);
begin
  DcefBrowser1.GoBack;
end;

procedure TMainForm.ActCanGoBackUpdate(Sender: TObject);
begin
  if Assigned(DcefBrowser1) then
    ActCanGoBack.Enabled := DcefBrowser1.canGoBack;
end;

procedure TMainForm.ActCanGoForwardExecute(Sender: TObject);
begin
  DcefBrowser1.GoForward;
end;

procedure TMainForm.ActCanGoForwardUpdate(Sender: TObject);
begin
  if Assigned(DcefBrowser1) then
    ActCanGoForward.Enabled := DcefBrowser1.canGoForward;
end;

procedure TMainForm.ActIsLoadingUpdate(Sender: TObject);
begin
  if Assigned(DcefBrowser1) then
    ActIsLoading.Enabled := DcefBrowser1.isLoading;
end;

procedure TMainForm.AddButtonClick(Sender: TObject);
begin
  DcefBrowser1.AddPage(AddressEdit.Text);
end;

procedure TMainForm.LoadButtonClick(Sender: TObject);
begin
  DcefBrowser1.Load(AddressEdit.Text);
end;

procedure TMainForm.N1Click(Sender: TObject);

  function GetShowBrowserId(const CloseTabIndex: Integer): Integer;
  var
    LastIndex: Boolean;
    Index: Integer;
  begin
    LastIndex := CloseTabIndex = (MyCustomTabs.PageCount - 1);
    if LastIndex then
      Index := CloseTabIndex - 1
    else
      Index := CloseTabIndex;

    if (Index > -1) and (Index < MyCustomTabs.PageCount) then
      Result := MyCustomTabs.Pages[Index].Tag
    else
      Result := -1;
  end;

begin
  DcefBrowser1.CloseBrowser(MyCustomTabs.ActivePage.Tag,
    GetShowBrowserId(MyCustomTabs.ActivePageIndex));
end;

function TMainForm.GetTabsheetByBrowserID(Const aBrowserID: Integer): TTabsheet;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to MyCustomTabs.PageCount - 1 do
    if MyCustomTabs.Pages[i].Tag = aBrowserID then
    begin
      Result := MyCustomTabs.Pages[i];
      Break;
    end;
end;

procedure TMainForm.DcefBrowser1AddBrowser(const browser: ICefBrowser);
var
  NewTab: TTabsheet;
begin
  NewTab := TTabsheet.Create(MyCustomTabs);
  NewTab.Caption := '';
  NewTab.Tag := browser.Identifier;
  NewTab.PageControl := MyCustomTabs;

  MyCustomTabs.ActivePageIndex := NewTab.PageIndex;
end;

procedure TMainForm.DcefBrowser1CloseBrowser(const CloseBrowserIdArr
  : array of Integer; const ShowBrowserId: Integer);
var
  Index: Integer;
  MyShowTabsheet, MyCloseTabsheet: TTabsheet;
begin
  MyShowTabsheet := GetTabsheetByBrowserID(ShowBrowserId);
  for Index := Low(CloseBrowserIdArr) to High(CloseBrowserIdArr) do
  begin
    MyCloseTabsheet := GetTabsheetByBrowserID(CloseBrowserIdArr[Index]);
    if MyCloseTabsheet <> nil then
      MyCloseTabsheet.Free;
  end;

  if (MyShowTabsheet <> nil) and (MyShowTabsheet <> MyCustomTabs.ActivePage)
  then
    MyCustomTabs.ActivePage := MyShowTabsheet;
end;

procedure TMainForm.DcefBrowser1StateChange(const browser: ICefBrowser;
  const Kind: TBrowserDataChangeKind; const Value: string);
var
  MyTabsheet: TTabsheet;
begin
  if browser.Identifier = DcefBrowser1.ActiveBrowserId then
    case Kind of
      BrowserDataChange_StatusMessage:
        StatusPanel.Caption := '   ' + Value;
      BrowserDataChange_Address:
        AddressEdit.Text := Value;
      BrowserDataChange_Title:
        MainForm.Caption := Value;
    end;
  if BrowserDataChange_Title = Kind then
  begin
    MyTabsheet := GetTabsheetByBrowserID(browser.Identifier);
    if MyTabsheet <> nil then
      MyTabsheet.Caption := Value;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // DcefBrowser1.TabVisible := False;
  DcefBrowser1.ChromiumOptions.JavascriptOpenWindows := STATE_DISABLED;
  DcefBrowser1.DcefBOptions.AutoDown := False;
  AddButton.Click;
end;

procedure TMainForm.MyCustomTabsChange(Sender: TObject);
begin
  DcefBrowser1.ShowBrowser(MyCustomTabs.ActivePage.Tag);
end;

end.
