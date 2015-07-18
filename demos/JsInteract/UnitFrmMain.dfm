object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 551
  ClientWidth = 738
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DcefBrowser1: TDcefBrowser
    Left = 0
    Top = 0
    Width = 738
    Height = 336
    TabOrder = 0
    Align = alClient
    DefaultURL = 'about:blank'
    Options.ExitPagesClosed = True
    Options.PopupNewWin = False
    Options.DevToolsEnable = True
    Options.AutoDown = False
    Options.DownLoadPath = 'C:\Program Files (x86)\Embarcadero\Studio\14.0\bin\Download\'
    ExplicitHeight = 368
  end
  object Panel1: TPanel
    Left = 0
    Top = 336
    Width = 738
    Height = 215
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object Edit1: TEdit
      Left = 8
      Top = 6
      Width = 65
      Height = 21
      TabOrder = 0
      Text = 'TestValue'
    end
    object Button1: TButton
      Left = 79
      Top = 4
      Width = 75
      Height = 25
      Caption = 'SetJsValue'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Memo1: TMemo
      Left = 0
      Top = 69
      Width = 738
      Height = 146
      Align = alBottom
      ScrollBars = ssVertical
      TabOrder = 2
      ExplicitTop = 37
    end
    object Button2: TButton
      Left = 160
      Top = 4
      Width = 75
      Height = 25
      Caption = 'GetJsValue'
      TabOrder = 3
      OnClick = Button2Click
    end
    object Edit2: TEdit
      Left = 8
      Top = 42
      Width = 121
      Height = 21
      TabOrder = 4
      Text = 'RunErrorJS();'
    end
    object Button3: TButton
      Left = 135
      Top = 40
      Width = 75
      Height = 25
      Caption = 'RunJs'
      TabOrder = 5
      OnClick = Button3Click
    end
  end
end
