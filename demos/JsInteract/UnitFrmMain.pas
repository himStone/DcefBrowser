unit UnitFrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  dcefb_Browser, dcef3_ceflib;

type
  TMainForm = class(TForm)
    DcefBrowser1: TDcefBrowser;
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;
    { Private declarations }
  public
    { Public declarations }
  end;

  TTestExtension = class
    class procedure SendJsStr(FormHandle: Integer; JsStr: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function ByteLength(const S: string): Integer;
begin
  Result := Length(S) * SizeOf(Char);
end;

procedure SendStrToForm(FormHandle: HWND; Str: string);
var
  Size: Integer;
  CopyDataStruct: TCopyDataStruct;
begin
  if FormHandle > 0 then
  begin
    Size := ByteLength(Str) + 2;
    CopyDataStruct.lpData := PChar(Str + #0);
    CopyDataStruct.dwData := WM_COPYDATA;
    CopyDataStruct.cbData := Size;
    SendMessage(FormHandle, WM_COPYDATA, 0, Integer(@CopyDataStruct));
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  JsCode: string;
begin
  JsCode := Format(Edit1.Text, [IntToStr(Handle)]);
  DcefBrowser1.ExecuteJavaScript(JsCode);
  Memo1.Lines.Add('执行JS ' + JsCode);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DcefBrowser1.TabVisible := True;
  DcefBrowser1.AddPage('http://www.baidu.com');
end;

procedure TMainForm.WMCopyData(var Message: TWMCopyData);
begin
  memo1.Lines.Add('收到消息 ' + PChar(Message.CopyDataStruct.lpData));
end;

{ TDefaultDcefbExtension }

class procedure TTestExtension.SendJsStr(FormHandle: Integer;
  JsStr: string);
begin
  SendStrToForm(FormHandle, JsStr);
end;

end.
