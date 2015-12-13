unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  DcefB.Core.DcefBrowser, DcefB.Core.App, Vcl.ComCtrls;

  //jsvarhelper
  //only support single process mode, but code more beautifule!


type
  TForm2 = class(TForm)
    DcefBrowser1: TDcefBrowser;
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Edit2: TEdit;
    Button3: TButton;
    GroupBox1: TGroupBox;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add('Set JsVar [StrValue, True, 1.1, 100], Start');
  DcefBrowser1.JsExtention.SetAsString(DcefBrowser1.ActiveBrowser, 'Str',
    'StrValue');
  DcefBrowser1.JsExtention.SetAsBoolean(DcefBrowser1.ActiveBrowser,
    'Boolean', True);
  DcefBrowser1.JsExtention.SetAsFloat(DcefBrowser1.ActiveBrowser, 'Float', 1.1);
  DcefBrowser1.JsExtention.SetAsInteger(DcefBrowser1.ActiveBrowser,
    'Integer', 100);
  Memo1.Lines.Add('Set JsVar [StrValue, True, 1.1, 100], End');
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  Str: string;
  Bool: Boolean;
  Int: Integer;
  Float: Double;
begin
  Memo1.Lines.Add('Get JsVar Start!');

  Str := DcefBrowser1.JsExtention.GetAsString
    (DcefBrowser1.ActiveBrowser, 'Str');
  Bool := DcefBrowser1.JsExtention.GetAsBoolean(DcefBrowser1.ActiveBrowser,
    'Boolean');
  Float := DcefBrowser1.JsExtention.GetAsFloat
    (DcefBrowser1.ActiveBrowser, 'Float');
  Int := DcefBrowser1.JsExtention.GetAsInteger(DcefBrowser1.ActiveBrowser,
    'Integer');

  Memo1.Lines.Add('Get Str -> ' + Str);
  Memo1.Lines.Add('Get Boolean -> ' + BoolToStr(Bool, True));
  Memo1.Lines.Add('Get Float -> ' + FloatToStr(Float));
  Memo1.Lines.Add('Get Integer -> ' + IntToStr(Int));
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Add('GetExecuteScriptResult -> ' +
    DcefBrowser1.JsExtention.GetExecuteScriptResult(DcefBrowser1.ActiveBrowser,
    Edit2.Text));
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  if DcefBApp.CefSingleProcess then
    Caption := 'JsInteract (Run in single process mode)'
  else
    Caption := 'JsInteract (Run in Multi-process mode)';

  DcefBrowser1.AddPage('http://www.baidu.com');
end;

end.
