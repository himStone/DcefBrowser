unit UnitFrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  dcefb_Browser, dcef3_ceflib, jsvarhelper;

type
  TMainForm = class(TForm)
    DcefBrowser1: TDcefBrowser;
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Edit2: TEdit;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    JsVars: TJsVars;
    { Private declarations }
  public
    { Public declarations }
  end;
var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
begin
  JsVars.AsString['TestStr'] := Edit1.Text;
  Memo1.Lines.Add(Format('Set TestStr = %s', [Edit1.Text]));
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Add('Get TestStr = ' + JsVars.AsString['TestStr']);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  JsVars.ExecuteScript(Edit2.Text);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DcefBrowser1.TabVisible := True;
  DcefBrowser1.AddPage('http://www.baidu.com');
  JsVars := TJsVars.Create(DcefBrowser1);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  JsVars.Free;
end;

end.
