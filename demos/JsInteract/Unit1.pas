unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  DcefB.Core.DcefBrowser, DcefB.Core.App, jsvarhelper, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    DcefBrowser1: TDcefBrowser;
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Edit2: TEdit;
    Button3: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    JsVars: TJsVars;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add('Set JsVar [StrValue, True, 1.1, 100]');
  JsVars.AsString['Str'] := 'StrValue';
  JsVars.AsBoolean['Boolean'] := True;
  JsVars.AsFloat['Float'] := 1.1;
  JsVars.AsInteger['Integer'] := 100;

  Memo1.Lines.Add('---------Set complete,Do GetTest---------');

  Memo1.Lines.Add('Get Str -> ' + JsVars.AsString['Str']);
  Memo1.Lines.Add('Get Boolean -> ' + BooltoStr(JsVars.AsBoolean['Boolean'], True));
  Memo1.Lines.Add('Get Float -> ' + FloatToStr(JsVars.AsFloat['Float']));
  Memo1.Lines.Add('Get Integer -> ' + IntToStr(JsVars.AsInteger['Integer']));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  JsVars.ExecuteScript(Edit2.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if DcefBApp.CefSingleProcess then
    Caption := 'JsInteract (Run in single process mode)'
  else
    Caption := 'JsInteract (Run in Multi-process mode)';

  DcefBrowser1.AddPage('http://www.baidu.com');
  JsVars := TJsVars.Create(DcefBrowser1);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  JsVars.Free;
end;

end.
