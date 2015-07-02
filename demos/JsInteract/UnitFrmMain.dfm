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
  PixelsPerInch = 96
  TextHeight = 13
  object DcefBrowser1: TDcefBrowser
    Left = 0
    Top = 0
    Width = 738
    Height = 368
    TabOrder = 0
    Align = alClient
    DefaultURL = 'about:blank'
    Options.ExitPagesClosed = True
    Options.PopupNewWin = False
    Options.DevToolsEnable = True
    Options.AutoDown = False
    Options.DownLoadPath = 'C:\Program Files (x86)\Embarcadero\Studio\14.0\bin\Download\'
  end
  object Panel1: TPanel
    Left = 0
    Top = 368
    Width = 738
    Height = 183
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object Edit1: TEdit
      Left = 8
      Top = 6
      Width = 634
      Height = 21
      TabOrder = 0
      Text = 'TTestExtension.SendJsStr(%s, "Send Msg");'
    end
    object Button1: TButton
      Left = 648
      Top = 4
      Width = 75
      Height = 25
      Caption = 'GetJsResult'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Memo1: TMemo
      Left = 0
      Top = 37
      Width = 738
      Height = 146
      Align = alBottom
      ScrollBars = ssVertical
      TabOrder = 2
    end
  end
end
