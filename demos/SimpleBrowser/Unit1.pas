unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  DcefB.Dcef3.CefLib, DcefB.Core.DcefBrowser;

type
  TForm1 = class(TForm)
    DcefBrowser1: TDcefBrowser;
    Panel1: TPanel;
    AddressEdit: TEdit;
    AddButton: TButton;
    Panel2: TPanel;
    Button4: TButton;
    Button1: TButton;
    Panel3: TPanel;
    Button2: TButton;
    procedure AddressEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DcefBrowser1LoadEnd(const browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: Integer);
    procedure FormCreate(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateStates;
    procedure DoPageChanged(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AddButtonClick(Sender: TObject);
begin
  DcefBrowser1.AddPage();
  AddressEdit.Text := 'about:blank';
end;

procedure TForm1.AddressEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    DcefBrowser1.Load(AddressEdit.Text);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DcefBrowser1.GoForward;
  UpdateStates;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  DcefBrowser1.Load(AddressEdit.Text);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  DcefBrowser1.GoBack;
  UpdateStates;
end;

procedure TForm1.DcefBrowser1LoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  UpdateStates;
  if DcefBrowser1.ActiveBrowserId = browser.Identifier then
    Caption := DcefBrowser1.Title;
end;

procedure TForm1.DoPageChanged(Sender: TObject);
begin
  Caption := DcefBrowser1.Title;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DcefBrowser1.CreateDefaultTabsControl();
  DcefBrowser1.OnDefaultTabChanged := DoPageChanged;
end;

procedure TForm1.UpdateStates;
begin
  Button4.Enabled := DcefBrowser1.canGoBack;
  Button1.Enabled := DcefBrowser1.canGoForward;
end;

// demo create by swish

end.
