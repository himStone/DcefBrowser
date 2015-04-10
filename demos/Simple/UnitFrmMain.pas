unit UnitFrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.Menus, Vcl.StdCtrls, Vcl.ExtCtrls, System.Actions,
  Vcl.ActnList, dcef3_ceflib, dcefb_Events, dcefb_Browser;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
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
    procedure FormDestroy(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActIsLoadingUpdate(Sender: TObject);
    procedure ActCanGoBackUpdate(Sender: TObject);
    procedure ActCanGoForwardUpdate(Sender: TObject);
    procedure ActCanGoBackExecute(Sender: TObject);
    procedure ActCanGoForwardExecute(Sender: TObject);
    procedure DcefBrowser1PageAdd(const PageID: Integer; Const AddAtLast: Boolean);
    procedure PageControlChange(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure DcefBrowser1PageClose(const ClosePageID,
      ShowPageID: Integer);
    function GetTabsheetByPageID(Const PageID: Integer): TTabsheet;
    procedure DcefBrowser1PageStateChange(const PageID: Integer;
      const Kind: TBrowserDataChangeKind; const Value: string;
      const PageActived: Boolean);
  protected
    procedure WndProc(var Message: TMessage); override;
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
  for i := 0 to PageControl.PageCount - 1 do
    if PageControl.Pages[i].Tag = PageID then
    begin
      Result := PageControl.Pages[i];
      Break;
    end;
end;

procedure TMainForm.DcefBrowser1PageAdd(const PageID: Integer; Const AddAtLast: Boolean);
var
  NewTab: TTabsheet;
begin
  NewTab := TTabsheet.Create(PageControl);
  NewTab.Caption := '';
  NewTab.Tag := PageID;
  // NewTab.Parent := PageControl;
  NewTab.PageControl := PageControl;

  PageControl.ActivePageIndex := NewTab.PageIndex;
end;

procedure TMainForm.DcefBrowser1PageClose(const ClosePageID,
  ShowPageID: Integer);
var
  MyShowTabsheet, MyCloseTabsheet: TTabsheet;
begin
  MyShowTabsheet := GetTabsheetByPageID(ShowPageID);
  MyCloseTabsheet := GetTabsheetByPageID(ClosePageID);

  if MyCloseTabsheet <> nil then
    MyCloseTabsheet.Free;
  if (MyShowTabsheet <> nil) and (MyShowTabsheet <> PageControl.ActivePage) then
    PageControl.ActivePage := MyShowTabsheet;
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
  DcefBrowser1.BasicOptions.JavascriptOpenWindows := STATE_DISABLED;
  DcefBrowser1.Options.FrmWinHandle := Handle;
  AddButton.Click;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DcefBrowser1.Free;
  // 不管是拖控件上去的还是动态创建的TDcefBrowser
  // 都要手动释放TDcefBrowser
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  DcefBrowser1.ShowPage(PageControl.ActivePage.Tag);
end;

procedure TMainForm.WndProc(var Message: TMessage);
begin
  if Assigned(DcefBrowser1) then
    DcefBrowser1.MainFormWndProc(Message, Handle);

  inherited WndProc(Message);
end;

end.
