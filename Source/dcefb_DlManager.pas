unit dcefb_DlManager;

interface

uses
  Winapi.Windows, System.Classes, Generics.Collections, System.StrUtils,
  System.SysUtils, dcef3_ceflib, dcefb_Events;

type
  TDcefDownloadItem = class
  private
    FPDownloadManager: Pointer;
    FOnDownloadUpdated: TOnDownloadUpdated;
    FCefDownloadItemCallback: ICefDownloadItemCallback;
    FParentItems: Pointer;
    FSendStartDownloadMSG: Boolean;
    FID: Integer;
    FCurrentSpeed: Integer;
    FCanceled: Boolean;

    FFileName: string;
    FComplete: Boolean;
    FPercentComplete: Integer;
    FTotalBytes: Int64;
    FReceivedBytes: Int64;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FFullPath: string;
    FUrl: string;
    FMimeType: string;

    function GetItemIndex: Integer;
    procedure DoCancelAndSave;
    procedure doOnDownloadUpdated(Const DcefItemIndex: Integer;
      Const Kind: TBrowserDownloadUpdatedKind);
  public
    constructor Create(PDownloadManager: Pointer);
    destructor Destroy; override;

    function DoCancel: Boolean;
    procedure UpdateInfo(const CefDownloadItem: ICefDownloadItem;
      const Callback: ICefDownloadItemCallback; var FDownloadComplete: Boolean);
    procedure UpdataFileName(const MyFullPath: string);

    property SendStartDownloadMSG: Boolean read FSendStartDownloadMSG
      write FSendStartDownloadMSG;
    property ID: Integer read FID;
    property CurrentSpeed: Integer read FCurrentSpeed;
    property ItemIndex: Integer read GetItemIndex;

    property FileName: string read FFileName write FFileName;
    property Complete: Boolean read FComplete write FComplete;
    property Canceled: Boolean read FCanceled write FCanceled;
    property PercentComplete: Integer read FPercentComplete
      write FPercentComplete;
    property TotalBytes: Int64 read FTotalBytes write FTotalBytes;
    property ReceivedBytes: Int64 read FReceivedBytes write FReceivedBytes;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read FEndTime write FEndTime;
    property FullPath: string read FFullPath write FFullPath;
    property Url: string read FUrl write FUrl;
    property MimeType: string read FMimeType write FMimeType;
  end;

  TDcefBrowserDownloadManager = class
  private
    FDownloadItems: TObjectList<TDcefDownloadItem>;

    function GetDownloadItem(Index: Integer): TDcefDownloadItem;
    function GetItemsCount: Integer;
    function GetDownloading: Boolean;
  public
    constructor Create();
    destructor Destroy; override;

    function AddItem(const CefDownloadItem: ICefDownloadItem;
      const OnDownloadUpdated: TOnDownloadUpdated): Integer;
    function CancelDownload(ID: Integer): Boolean;
    procedure CancelAllDownloadAndSave;
    procedure AppExitWhenAllItemStopDownload;
    function ItemsIdToIndex(ID: Integer): Integer;
    function GetItemByID(ID: Integer): TDcefDownloadItem;
    procedure Clear;

    property Items[Index: Integer]: TDcefDownloadItem read GetDownloadItem;
    property ItemsCount: Integer read GetItemsCount;
    property Downloading: Boolean read GetDownloading;
  end;

implementation


{ TDcefBrowserDownloadManager }

function TDcefBrowserDownloadManager.AddItem(const CefDownloadItem: ICefDownloadItem;
  const OnDownloadUpdated: TOnDownloadUpdated): Integer;
var
  downloadItem: TDcefDownloadItem;
begin
  downloadItem := TDcefDownloadItem.Create(Pointer(Self));
  downloadItem.FOnDownloadUpdated := OnDownloadUpdated;
  downloadItem.FParentItems := Pointer(FDownloadItems);
  downloadItem.FSendStartDownloadMSG := False;
  downloadItem.FID := CefDownloadItem.ID;

  // DownloadItem.FileName := CefDownloadItem.SuggestedFileName;
  downloadItem.TotalBytes := CefDownloadItem.TotalBytes;
  downloadItem.StartTime := CefDownloadItem.StartTime;
  downloadItem.FullPath := CefDownloadItem.FullPath;
  downloadItem.Url := CefDownloadItem.Url;
  downloadItem.MimeType := CefDownloadItem.MimeType;

  Result := FDownloadItems.Add(downloadItem);
end;

procedure TDcefBrowserDownloadManager.AppExitWhenAllItemStopDownload;
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      Index: Integer;
      Item: TDcefDownloadItem;
    begin
      for Index := 0 to FDownloadItems.Count - 1 do
      begin
        Item := FDownloadItems[Index];
        if Item.ID = -1 then
          Continue
        else if (Not Item.Complete) and (Not Item.Canceled) then
        begin
          while (Not Item.Complete) and (Not Item.Canceled) do
            sleep(200);
          Continue;
        end
        else
          Continue;
      end;
      // Exit APP
      CefShutDown;
      ExitProcess(0);
    end).Start;
end;

procedure TDcefBrowserDownloadManager.CancelAllDownloadAndSave;
var
  Index: Integer;
begin
  for Index := 0 to FDownloadItems.Count - 1 do
    if Not FDownloadItems[Index].Complete then
      FDownloadItems[Index].DoCancelAndSave;
end;

function TDcefBrowserDownloadManager.CancelDownload(ID: Integer): Boolean;
var
  ItemIndex: Integer;
begin
  Result := False;
  ItemIndex := ItemsIdToIndex(ID);
  if ItemIndex > -1 then
    Result := FDownloadItems[ItemIndex].DoCancel;
end;

procedure TDcefBrowserDownloadManager.Clear;
var
  Index: Integer;
  Item: TDcefDownloadItem;
begin
  for Index := FDownloadItems.Count - 1 downto 0 do
  begin
    Item := FDownloadItems[Index];
    if (Item.ID = -1) or (Item.Complete) then
      FDownloadItems.Delete(Index);
  end;
end;

constructor TDcefBrowserDownloadManager.Create();
begin
  inherited Create;
  FDownloadItems := TObjectList<TDcefDownloadItem>.Create(True);
end;

destructor TDcefBrowserDownloadManager.Destroy;
begin
  FDownloadItems.Free;
  inherited;
end;

function TDcefBrowserDownloadManager.GetDownloading: Boolean;
var
  Index: Integer;
  Item: TDcefDownloadItem;
begin
  Result := False;
  for Index := 0 to FDownloadItems.Count - 1 do
  begin
    Item := FDownloadItems[Index];
    if (Item.ID <> -1) and (Not Item.Complete) and (Not Item.Canceled) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TDcefBrowserDownloadManager.GetDownloadItem(Index: Integer)
  : TDcefDownloadItem;
begin
  Result := nil;
  if (Index > -1) and (Index < FDownloadItems.Count) then
    Result := FDownloadItems[Index];
end;

function TDcefBrowserDownloadManager.GetItemByID(ID: Integer)
  : TDcefDownloadItem;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to FDownloadItems.Count - 1 do
    if FDownloadItems.Items[Index].ID = ID then
    begin
      Result := FDownloadItems.Items[Index];
      Break;
    end;
end;

function TDcefBrowserDownloadManager.GetItemsCount: Integer;
begin
  Result := FDownloadItems.Count;
end;

function TDcefBrowserDownloadManager.ItemsIdToIndex(ID: Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to FDownloadItems.Count - 1 do
    if FDownloadItems[Index].FID = ID then
    begin
      Result := Index;
      Break;
    end;
end;

{ TDownloadItem }

function TDcefDownloadItem.DoCancel: Boolean;
begin
  Result := False;
  if FCefDownloadItemCallback <> nil then
  begin
    // EndTime := Now; Save;
    FCefDownloadItemCallback.cancel;
    // 这里做停止下载的动作 判读在UpdataInfo内
    Result := True;
  end;
end;

procedure TDcefDownloadItem.DoCancelAndSave;
begin
  DoCancel;
  if EndTime = 0 then
    EndTime := Now;
end;

procedure TDcefDownloadItem.doOnDownloadUpdated(const DcefItemIndex: Integer;
const Kind: TBrowserDownloadUpdatedKind);
begin
  if Assigned(FOnDownloadUpdated) then
    FOnDownloadUpdated(DcefItemIndex, Kind);
end;

function TDcefDownloadItem.GetItemIndex: Integer;
begin
  Result := TObjectList<TDcefDownloadItem>(FParentItems).IndexOf(Self);
end;

constructor TDcefDownloadItem.Create(PDownloadManager: Pointer);
begin
  inherited Create;
  FPDownloadManager := PDownloadManager;
  FSendStartDownloadMSG := False;
  FID := -1;
  FCurrentSpeed := 0;
  FFileName := '';
  FComplete := False;
  FCanceled := False;
  FPercentComplete := 0;
  FTotalBytes := 0;
  FReceivedBytes := 0;
  FStartTime := 0;
  FEndTime := 0;
  FFullPath := '';
  FUrl := '';
  FMimeType := '';
end;

destructor TDcefDownloadItem.Destroy;
begin
  FCefDownloadItemCallback := nil;
  inherited;
end;

procedure TDcefDownloadItem.UpdataFileName(const MyFullPath: string);
begin
  if Not SameText(MyFullPath, '') then
  begin
    FFullPath := MyFullPath;
    FFileName := ExtractFileName(MyFullPath);
  end;
end;

procedure TDcefDownloadItem.UpdateInfo(const CefDownloadItem: ICefDownloadItem;
const Callback: ICefDownloadItemCallback; var FDownloadComplete: Boolean);
begin
  FComplete := False;
  FCurrentSpeed := CefDownloadItem.CurrentSpeed;
  FPercentComplete := CefDownloadItem.PercentComplete;
  FReceivedBytes := CefDownloadItem.ReceivedBytes;
  FComplete := CefDownloadItem.IsComplete;
  if CefDownloadItem.EndTime > 0 then
    FEndTime := CefDownloadItem.EndTime;

  if (Not CefDownloadItem.IsInProgress) and (CefDownloadItem.IsComplete) then
  // 下载完成
  begin
    FDownloadComplete := True;
    doOnDownloadUpdated(ItemIndex, BrowserDownloadUpdated_End);
  end
  else
  begin
    if (Not SendStartDownloadMSG) and (Not SameText(FileName, '')) then
    // 下载[真正]开始
    begin
      SendStartDownloadMSG := True;
      FCefDownloadItemCallback := Callback;
      doOnDownloadUpdated(ItemIndex, BrowserDownloadUpdated_Start);
    end
    else
    begin
      if CefDownloadItem.IsInProgress then // 正在下载
        doOnDownloadUpdated(ItemIndex, BrowserDownloadUpdated_Progress)
      else // 下载停止
      begin
        Complete := False;
        Canceled := True;
        EndTime := Now;
        doOnDownloadUpdated(ItemIndex, BrowserDownloadUpdated_Canceled);
      end;
    end;
  end;
end;

end.
