object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DownloadManager'
  ClientHeight = 591
  ClientWidth = 1121
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #24494#36719#38597#40657
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 17
  object Splitter1: TSplitter
    Left = 0
    Top = 438
    Width = 1121
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsLine
    ExplicitTop = 436
  end
  object DcefBrowser1: TDcefBrowser
    Left = 0
    Top = 38
    Width = 1121
    Height = 400
    TabOrder = 0
    Align = alClient
    DefaultURL = 'about:blank'
    Options.TerminateAppWhenAllPageClosed = True
    Options.TerminateAppWhenDownloading = False
    Options.PopupNewWindow = False
    Options.DebugToolAvailable = True
    Options.MainFormWinHandle = 0
    Options.AutoDown = True
    Options.DownLoadPath = 'H:\program files (x86)\Embarcadero\Studio\14.0\Bin\Download\'
    OnDownloadUpdated = DcefBrowser1DownloadUpdated
  end
  object DownloadItemsListview: TListView
    Left = 0
    Top = 441
    Width = 1121
    Height = 150
    Align = alBottom
    Columns = <
      item
        Caption = 'ID'
      end
      item
        Caption = 'State'
        Width = 80
      end
      item
        Caption = 'FileName'
        Width = 150
      end
      item
        Caption = 'CurrentSpeed'
        Width = 100
      end
      item
        Caption = 'PercentComplete'
        Width = 120
      end
      item
        Caption = 'TotalBytes'
        Width = 100
      end
      item
        Caption = 'ReceivedBytes'
        Width = 100
      end
      item
        Caption = 'StartTime'
        Width = 100
      end
      item
        Caption = 'EndTime'
        Width = 100
      end
      item
        Caption = 'FullPath'
        Width = 200
      end
      item
        Caption = 'URL'
        Width = 200
      end
      item
        Caption = 'MimeType'
        Width = 150
      end>
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenuDownload
    TabOrder = 1
    ViewStyle = vsReport
    OnContextPopup = DownloadItemsListviewContextPopup
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1121
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 2
    object Edit1: TEdit
      Left = 8
      Top = 7
      Width = 897
      Height = 25
      TabOrder = 0
      Text = 'pc.qq.com'
    end
    object Button1: TButton
      Left = 1031
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 919
      Top = 7
      Width = 106
      Height = 25
      Caption = 'CloseActivePage'
      TabOrder = 2
      OnClick = Button2Click
    end
  end
  object PopupMenuDownload: TPopupMenu
    Left = 152
    Top = 144
    object N1: TMenuItem
      Caption = #26242#20572
      OnClick = N1Click
    end
    object N2: TMenuItem
      Caption = #28165#31354#35760#24405
      OnClick = N2Click
    end
  end
end
