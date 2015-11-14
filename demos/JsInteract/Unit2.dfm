object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'JsInteract2'
  ClientHeight = 545
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
    Height = 179
    TabOrder = 0
    Align = alClient
    DefaultURL = 'about:blank'
    DcefBOptions.DevToolsEnable = False
    DcefBOptions.CloseWPagesClosed = False
    DcefBOptions.DownLoadPath = 'C:\Program Files (x86)\Embarcadero\Studio\14.0\bin\Download\'
  end
  object Panel1: TPanel
    Left = 0
    Top = 179
    Width = 738
    Height = 366
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object Memo1: TMemo
      Left = 0
      Top = 121
      Width = 738
      Height = 245
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object GroupBox1: TGroupBox
      Left = 0
      Top = 0
      Width = 738
      Height = 121
      Align = alTop
      Caption = 'Method 2'
      TabOrder = 1
      object Label1: TLabel
        Left = 11
        Top = 24
        Width = 430
        Height = 13
        Caption = 
          'Use DcefBrowser.JsExtention, support single and multi process mo' +
          'de but less easy to use'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object Button1: TButton
        Left = 11
        Top = 49
        Width = 78
        Height = 25
        Caption = 'SetTest'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button3: TButton
        Left = 138
        Top = 86
        Width = 75
        Height = 25
        Caption = 'RunJs'
        TabOrder = 1
        OnClick = Button3Click
      end
      object Edit2: TEdit
        Left = 11
        Top = 88
        Width = 121
        Height = 21
        TabOrder = 2
        Text = 'RunErrorJS();'
      end
      object Button2: TButton
        Left = 95
        Top = 49
        Width = 75
        Height = 25
        Caption = 'GetTest'
        TabOrder = 3
        OnClick = Button2Click
      end
    end
  end
end
