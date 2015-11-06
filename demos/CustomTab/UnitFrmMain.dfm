object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Custom Tab For Browser'
  ClientHeight = 543
  ClientWidth = 1062
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #24494#36719#38597#40657
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 17
  object MyCustomTabs: TPageControl
    Left = 0
    Top = 36
    Width = 1062
    Height = 22
    Align = alTop
    PopupMenu = PopupMenuPageControl
    TabOrder = 0
    TabWidth = 150
    OnChange = MyCustomTabsChange
  end
  object DcefBrowser1: TDcefBrowser
    Left = 0
    Top = 58
    Width = 1062
    Height = 462
    TabOrder = 1
    Align = alClient
    DefaultURL = 'about:blank'
    DcefBOptions.DevToolsEnable = False
    DcefBOptions.CloseWPagesClosed = False
    DcefBOptions.DownLoadPath = 'C:\Program Files (x86)\Embarcadero\Studio\14.0\bin\Download\'
    OnStateChange = DcefBrowser1StateChange
    OnAddBrowser = DcefBrowser1AddBrowser
    OnCloseBrowser = DcefBrowser1CloseBrowser
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1062
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    Caption = 'ToolPanel'
    ShowCaption = False
    TabOrder = 2
    object Button4: TButton
      Left = 4
      Top = 6
      Width = 75
      Height = 25
      Action = ActCanGoBack
      TabOrder = 0
    end
    object Button1: TButton
      Left = 85
      Top = 5
      Width = 75
      Height = 25
      Action = ActCanGoForward
      TabOrder = 1
    end
    object CheckBox1: TCheckBox
      Left = 170
      Top = 10
      Width = 74
      Height = 16
      Action = ActIsLoading
      TabOrder = 2
    end
    object AddressEdit: TEdit
      Left = 252
      Top = 6
      Width = 661
      Height = 25
      TabOrder = 3
      Text = 'www.baidu.com'
    end
    object AddButton: TButton
      Left = 919
      Top = 6
      Width = 75
      Height = 25
      Caption = 'AddPage'
      TabOrder = 4
      OnClick = AddButtonClick
    end
    object LoadButton: TButton
      Left = 1000
      Top = 6
      Width = 49
      Height = 25
      Caption = 'load'
      TabOrder = 5
      OnClick = LoadButtonClick
    end
  end
  object StatusPanel: TPanel
    Left = 0
    Top = 520
    Width = 1062
    Height = 23
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 3
  end
  object ActionList1: TActionList
    Left = 200
    Top = 177
    object ActIsLoading: TAction
      Caption = 'IsLoading'
      OnUpdate = ActIsLoadingUpdate
    end
    object ActCanGoBack: TAction
      Caption = 'GoBack'
      OnExecute = ActCanGoBackExecute
      OnUpdate = ActCanGoBackUpdate
    end
    object ActCanGoForward: TAction
      Caption = 'GoForward'
      OnExecute = ActCanGoForwardExecute
      OnUpdate = ActCanGoForwardUpdate
    end
  end
  object PopupMenuPageControl: TPopupMenu
    AutoHotkeys = maManual
    Left = 240
    Top = 176
    object N1: TMenuItem
      Caption = 'Close'
      OnClick = N1Click
    end
  end
end
