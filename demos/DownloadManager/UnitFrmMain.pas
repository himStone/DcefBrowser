unit UnitFrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus,
  System.UITypes, dcef3_ceflib, dcefb_Browser, dcefb_Events, dcefb_DlManager;

type
  TMainForm = class(TForm)
    DcefBrowser1: TDcefBrowser;
    DownloadItemsListview: TListView;
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    Splitter1: TSplitter;
    PopupMenuDownload: TPopupMenu;
    N1: TMenuItem;
    Button2: TButton;
    N2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    function GetListItemByID(ID: Integer): TListItem;
    procedure UpdataDownListViewItem(Item: TListItem; DownItemIndex: Integer;
      Kind: TBrowserDownloadUpdatedKind; State: string);
    procedure LoadDownloadItems;
    procedure N1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DownloadItemsListviewContextPopup(Sender: TObject;
      MousePos: TPoint; var Handled: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure DcefBrowser1DownloadUpdated(const DcefItemIndex: Integer;
      const Kind: TBrowserDownloadUpdatedKind);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

Const
  SFileState_Start = '已开始';
  SFileState_Complete = '已完成';
  SFileState_Cancel = '已取消';
  SFileState_Delete = '已删除';

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function SizeConversion(Bytes: Int64): String;
begin
  Result := '0 B';
  if Bytes < 1024 then
    Result := IntToStr(Bytes) + ' B'
  else if Bytes < 1048576 then
    Result := FloatToStrF(Bytes / 1024, ffFixed, 10, 1) + ' KB'
  else if Bytes < 1073741824 then
    Result := FloatToStrF(Bytes / 1048576, ffFixed, 10, 1) + ' MB'
  else if Bytes > 1073741824 then
    Result := FloatToStrF(Bytes / 1073741824, ffFixed, 10, 1) + ' GB';
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  DcefBrowser1.Load(Edit1.Text);
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  Page: TBrowserPage;
begin
  Page := DcefBrowser1.ActivePage;
  if Page <> nil then Page.Close;
end;

procedure TMainForm.DcefBrowser1DownloadUpdated(const DcefItemIndex: Integer;
  const Kind: TBrowserDownloadUpdatedKind);
var
  Item: TListItem;
begin
  case Kind of
    BrowserDownloadUpdated_Start:
      begin
        Item := DownloadItemsListview.Items.Add;
        UpdataDownListViewItem(Item, DcefItemIndex, Kind, SFileState_Start);
      end;
    BrowserDownloadUpdated_Progress:
      begin
        Item := GetListItemByID(DcefBrowser1.DownloadManager.Items
          [DcefItemIndex].ID);
        if Item <> nil then
          UpdataDownListViewItem(Item, DcefItemIndex, Kind, SFileState_Start);
      end;
    BrowserDownloadUpdated_End:
      begin
        Item := GetListItemByID(DcefBrowser1.DownloadManager.Items
          [DcefItemIndex].ID);
        if Item <> nil then
          UpdataDownListViewItem(Item, DcefItemIndex, Kind,
            SFileState_Complete);
      end;
    BrowserDownloadUpdated_Canceled:
      begin
        Item := GetListItemByID(DcefBrowser1.DownloadManager.Items
          [DcefItemIndex].ID);
        if Item <> nil then
          UpdataDownListViewItem(Item, DcefItemIndex, Kind, SFileState_Cancel);
      end;
  end;
end;

procedure TMainForm.DownloadItemsListviewContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  PopupMenuDownload.Items.Items[0].Enabled :=
    (DownloadItemsListview.Selected <> nil) and
    (DownloadItemsListview.Selected.SubItems[0] = SFileState_Start);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  TThread.Synchronize(nil,
    procedure
    begin
    end);
  if DcefBrowser1.DownloadManager.Downloading then
  begin
    if MessageDlg('正在下载文件 您希望退出DcefBrowser并取消下载么', mtInformation, [mbYes, mbNO],
      0) = mrYes then
    begin
      CanClose := False;
      Hide;
      DcefBrowser1.DownloadManager.CancelAllDownloadAndSave;
      DcefBrowser1.DownloadManager.AppExitWhenAllItemStopDownload;
    end
    else
      CanClose := False;
  end
  else
    CanClose := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DcefBrowser1.BasicOptions.JavascriptOpenWindows := STATE_DISABLED;
  DcefBrowser1.Options.MainFormWinHandle := Handle;
  DcefBrowser1.Options.TerminateAppWhenAllPageClosed := False;

  // DcefBrowser1.Options.DownLoadPath
  // DcefBrowser1.Options.AutoDown
  // 关于下载文件的设置 具体请看UnitDcefBrowserDownloadManager内注释
  // 这里不特意设置 全部使用默认的

  Button1.Click;

  LoadDownloadItems; // 载入下载记录
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DcefBrowser1.Free;
  // 不管是拖控件上去的还是动态创建的TDcefBrowser
  // 都要手动释放TDcefBrowser
end;

function TMainForm.GetListItemByID(ID: Integer): TListItem;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to DownloadItemsListview.Items.Count - 1 do
    if DownloadItemsListview.Items.Item[Index].Caption = IntToStr(ID) then
    begin
      Result := DownloadItemsListview.Items.Item[Index];
      Break;
    end;
end;

procedure TMainForm.LoadDownloadItems;
var
  Index: Integer;
  Item: TListItem;
  DcefDownLoadItem: TDcefDownloadItem;
begin
  DownloadItemsListview.Items.BeginUpdate;
  DownloadItemsListview.Clear;
  for Index := 0 to DcefBrowser1.DownloadManager.ItemsCount - 1 do
  begin
    DcefDownLoadItem := DcefBrowser1.DownloadManager.Items[Index];

    Item := DownloadItemsListview.Items.Add;
    Item.Caption := IntToStr(DcefDownLoadItem.ID);
    if DcefDownLoadItem.Complete then
    begin
      if FileExists(DcefDownLoadItem.FullPath) then
        Item.SubItems.Add(SFileState_Complete)
      else
        Item.SubItems.Add(SFileState_Delete);
    end
    else
      Item.SubItems.Add(SFileState_Cancel);
    Item.SubItems.Add(DcefDownLoadItem.FileName);
    Item.SubItems.Add(SizeConversion(DcefDownLoadItem.CurrentSpeed) + '/s');
    Item.SubItems.Add(IntToStr(DcefDownLoadItem.PercentComplete) + '%');
    Item.SubItems.Add(SizeConversion(DcefDownLoadItem.TotalBytes));
    Item.SubItems.Add(SizeConversion(DcefDownLoadItem.ReceivedBytes));
    Item.SubItems.Add(FormatDateTime('yyyy-mm-dd HH:MM:ss',
      DcefDownLoadItem.StartTime));
    Item.SubItems.Add(FormatDateTime('yyyy-mm-dd HH:MM:ss',
      DcefDownLoadItem.EndTime));
    Item.SubItems.Add(DcefDownLoadItem.FullPath);
    Item.SubItems.Add(DcefDownLoadItem.Url);
    Item.SubItems.Add(DcefDownLoadItem.MimeType);
  end;
  DownloadItemsListview.Items.EndUpdate;
end;

procedure TMainForm.N1Click(Sender: TObject);
begin
  if DownloadItemsListview.Selected <> nil then
    if Not DcefBrowser1.DownloadManager.CancelDownload
      (StrToIntDef(DownloadItemsListview.Selected.Caption, -1)) then
      ShowMessage('暂停失败');
end;

procedure TMainForm.N2Click(Sender: TObject);
begin
  DcefBrowser1.DownloadManager.Clear;
  LoadDownloadItems;
end;

procedure TMainForm.UpdataDownListViewItem(Item: TListItem;
DownItemIndex: Integer; Kind: TBrowserDownloadUpdatedKind; State: string);
var
  DcefDownLoadItem: TDcefDownloadItem;
begin
  DcefDownLoadItem := DcefBrowser1.DownloadManager.Items[DownItemIndex];
  if Kind = BrowserDownloadUpdated_Start then
  begin
    Item.Caption := IntToStr(DcefDownLoadItem.ID);
    Item.SubItems.Add(State);
    Item.SubItems.Add(DcefDownLoadItem.FileName);
    Item.SubItems.Add('');
    Item.SubItems.Add('');
    Item.SubItems.Add(SizeConversion(DcefDownLoadItem.TotalBytes));
    Item.SubItems.Add('');
    Item.SubItems.Add(FormatDateTime('yyyy-mm-dd HH:MM:ss',
      DcefDownLoadItem.StartTime));
    Item.SubItems.Add('');
    Item.SubItems.Add(DcefDownLoadItem.FullPath);
    Item.SubItems.Add(DcefDownLoadItem.Url);
    Item.SubItems.Add(DcefDownLoadItem.MimeType);
  end
  else if (Kind = BrowserDownloadUpdated_End) or
    (Kind = BrowserDownloadUpdated_Canceled) then
    Item.SubItems[7] := FormatDateTime('yyyy-mm-dd HH:MM:ss',
      DcefDownLoadItem.EndTime);

  Item.SubItems[0] := State;
  Item.SubItems[2] := SizeConversion(DcefDownLoadItem.CurrentSpeed) + '/s';
  Item.SubItems[3] := IntToStr(DcefDownLoadItem.PercentComplete) + '%';
  Item.SubItems[5] := SizeConversion(DcefDownLoadItem.ReceivedBytes);
end;

end.
