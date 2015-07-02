unit UnitFrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.Menus, Vcl.StdCtrls, Vcl.ExtCtrls, System.Actions,
  Vcl.ActnList, dcef3_ceflib, dcefb_Events, dcefb_Browser;

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
    procedure DcefBrowser1PageAdd(const PageID: Integer;
      Const AddAtLast: Boolean);
    procedure MyCustomTabsChange(Sender: TObject);
    procedure N1Click(Sender: TObject);
    function GetTabsheetByPageID(Const PageID: Integer): TTabsheet;
    procedure DcefBrowser1PageStateChange(const PageID: Integer;
      const Kind: TBrowserDataChangeKind; const Value: string;
      const PageActived: Boolean);
    procedure DcefBrowser1PageClose(
      const ClosePageIDArr: TArray<System.Integer>; const ShowPageID: Integer);
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
  DcefBrowser1.AddPage(AddressEdit.Text, True);
end;

procedure TMainForm.LoadButtonClick(Sender: TObject);
begin
  DcefBrowser1.Load(AddressEdit.Text);
end;

procedure TMainForm.N1Click(Sender: TObject);
begin
  DcefBrowser1.ClosePage(DcefBrowser1.ActivePageIndex);
end;

function TMainForm.GetTabsheetByPageID(Const PageID: Integer): TTabsheet;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to MyCustomTabs.PageCount - 1 do
    if MyCustomTabs.Pages[i].Tag = PageID then
    begin
      Result := MyCustomTabs.Pages[i];
      Break;
    end;
end;

procedure TMainForm.DcefBrowser1PageAdd(const PageID: Integer;
  Const AddAtLast: Boolean);
var
  NewTab: TTabsheet;
begin
  NewTab := TTabsheet.Create(MyCustomTabs);
  NewTab.Caption := '';
  NewTab.Tag := PageID;
  // NewTab.Parent := MyCustomTabs;
  NewTab.PageControl := MyCustomTabs;

  MyCustomTabs.ActivePageIndex := NewTab.PageIndex;
end;

procedure TMainForm.DcefBrowser1PageClose(
  const ClosePageIDArr: TArray<System.Integer>; const ShowPageID: Integer);
var
  Index: Integer;
  MyShowTabsheet, MyCloseTabsheet: TTabsheet;
begin
  MyShowTabsheet := GetTabsheetByPageID(ShowPageID);
  for Index := Low(ClosePageIDArr) to High(ClosePageIDArr) do
  begin
    MyCloseTabsheet := GetTabsheetByPageID(Index);
    if MyCloseTabsheet <> nil then
      MyCloseTabsheet.Free;
  end;

  if (MyShowTabsheet <> nil) and (MyShowTabsheet <> MyCustomTabs.ActivePage) then
    MyCustomTabs.ActivePage := MyShowTabsheet;
end;

procedure TMainForm.DcefBrowser1PageStateChange(const PageID: Integer;
  const Kind: TBrowserDataChangeKind; const Value: string;
  const PageActived: Boolean);
var
  MyTabsheet: TTabsheet;
begin
  if PageActived then
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
    MyTabsheet := GetTabsheetByPageID(PageID);
    if MyTabsheet <> nil then
      MyTabsheet.Caption := Value;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DcefBrowser1.TabVisible := False;
  DcefBrowser1.BasicOptions.JavascriptOpenWindows := STATE_DISABLED;
  DcefBrowser1.Options.AutoDown := False;
  AddButton.Click;
end;

procedure TMainForm.MyCustomTabsChange(Sender: TObject);
begin
  DcefBrowser1.ShowPage(MyCustomTabs.ActivePage.Tag);
end;

end.
