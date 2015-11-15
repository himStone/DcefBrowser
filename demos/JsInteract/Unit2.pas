unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  DcefB.Core.DcefBrowser, DcefB.Core.App, Vcl.ComCtrls;

type
  TForm2 = class(TForm)
    DcefBrowser1: TDcefBrowser;
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Edit2: TEdit;
    Button3: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
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

  DcefBrowser1.JsExtention.SetAsStringProc(DcefBrowser1.ActiveBrowser, 'Str',
    'StrValue',
    procedure(const aData: Variant)
    begin
      Memo1.Lines.Add('Set Str Success');
    end);
  DcefBrowser1.JsExtention.SetAsBooleanProc(DcefBrowser1.ActiveBrowser,
    'Boolean', True,
    procedure(const aData: Variant)
    begin
      Memo1.Lines.Add('Set Boolean Success');
    end);
  DcefBrowser1.JsExtention.SetAsFloatProc(DcefBrowser1.ActiveBrowser,
    'Float', 1.1,
    procedure(const aData: Variant)
    begin
      Memo1.Lines.Add('Set Float Success');
    end);
  DcefBrowser1.JsExtention.SetAsIntegerProc(DcefBrowser1.ActiveBrowser,
    'Integer', 100,
    procedure(const aData: Variant)
    begin
      Memo1.Lines.Add('Set Integer Success');
    end);
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Add('Get JsVar Start!');

  DcefBrowser1.JsExtention.GetAsStringProc(DcefBrowser1.ActiveBrowser, 'Str',
    procedure(const aData: Variant)
    begin
      Memo1.Lines.Add('Get Str -> ' + string(aData));
    end);
  DcefBrowser1.JsExtention.GetAsBooleanProc(DcefBrowser1.ActiveBrowser,
    'Boolean',
    procedure(const aData: Variant)
    begin
      Memo1.Lines.Add('Get Boolean -> ' + string(aData));
    end);
  DcefBrowser1.JsExtention.GetAsFloatProc(DcefBrowser1.ActiveBrowser, 'Float',
    procedure(const aData: Variant)
    begin
      Memo1.Lines.Add('Get Float -> ' + string(aData));
    end);
  DcefBrowser1.JsExtention.GetAsIntegerProc(DcefBrowser1.ActiveBrowser,
    'Integer',
    procedure(const aData: Variant)
    begin
      Memo1.Lines.Add('Get Integer -> ' + string(aData));
    end);
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  DcefBrowser1.JsExtention.GetExecuteScriptResultProc(DcefBrowser1.ActiveBrowser, Edit2.Text,
    procedure(const aData: Variant)
    begin
      Memo1.Lines.Add('GetExecuteScriptResult -> ' + aData.AsString);
    end);
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
