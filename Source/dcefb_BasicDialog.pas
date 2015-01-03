unit dcefb_BasicDialog;

interface

uses
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.Consts, System.Classes,
  Vcl.Graphics, Vcl.Dialogs, Vcl.ExtCtrls, Winapi.Windows, System.Math,
  dcef3_ceflib;

type
  TPasswordDialogForm = class
  private
    FForm: TForm;
    FLabelPassword: TLabel;
    FEditPassword: TEdit;
    FOKBtn: TButton;
    FCancelBtn: TButton;
    FLabelUserName: TLabel;
    FEditUserName: TEdit;
    function GetFormPassword: string;
    function GetFormUserName: string;
  public
    constructor Create;
    destructor Destroy; override;
    function ShowModal: Integer;
    property FormUserName: string read GetFormUserName;
    property FormPassword: string read GetFormPassword;
  end;

  TSearchTextBar = class(TPanel)
  private
    FEditSearch: TEdit;
    FButtonGoBack: TButton;
    FButtonGoForward: TButton;
    FButtonClose: TButton;
    FCefBrowserHost: ICefBrowserHost;
    procedure ButtonGoBackClick(Sender: TObject);
    procedure ButtonGoForwardClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; CefBrowserHost: ICefBrowserHost);
      reintroduce;
    procedure EditSetFocus;
    destructor Destroy; override;
    procedure Clear;
  end;

procedure MyConfirm(const ACaption: string; const AText: string;
  var BoolValue: Boolean); // 仿造自InputQuery

implementation

{ TPasswordDlg }

constructor TPasswordDialogForm.Create;
begin
  FForm := TForm.Create(nil);
  with FForm do
  begin
    Left := 245;
    Top := 108;
    BorderStyle := bsDialog;
    Caption := #36523#20221#39564#35777;
    ClientHeight := 128;
    ClientWidth := 233;
    Color := clBtnFace;
    Font.Charset := DEFAULT_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -12;
    Font.Name := #24494#36719#38597#40657;
    Font.Style := [];
    OldCreateOrder := True;
    Position := poScreenCenter;
    PixelsPerInch := 96;
  end;

  FLabelPassword := TLabel.Create(nil);
  FLabelUserName := TLabel.Create(nil);
  FEditPassword := TEdit.Create(nil);
  FEditUserName := TEdit.Create(nil);
  FOKBtn := TButton.Create(nil);
  FCancelBtn := TButton.Create(nil);

  with FLabelPassword do
  begin
    Parent := FForm;
    Left := 8;
    Top := 49;
    Width := 24;
    Height := 13;
    Caption := '密码';
  end;
  with FLabelUserName do
  begin
    Parent := FForm;
    Left := 8;
    Top := 3;
    Width := 36;
    Height := 13;
    Caption := '用户名';
  end;
  with FEditPassword do
  begin
    Parent := FForm;
    Left := 24;
    Top := 68;
    Width := 201;
    Height := 21;
    PasswordChar := '*';
    TabOrder := 1;
  end;
  with FOKBtn do
  begin
    Parent := FForm;
    Left := 62;
    Top := 98;
    Width := 75;
    Height := 25;
    Caption := '确定';
    Default := True;
    ModalResult := 1;
    TabOrder := 2;
  end;
  with FCancelBtn do
  begin
    Parent := FForm;
    Left := 150;
    Top := 98;
    Width := 75;
    Height := 25;
    Cancel := True;
    Caption := '取消';
    ModalResult := 2;
    TabOrder := 3;
  end;
  with FEditUserName do
  begin
    Parent := FForm;
    Left := 24;
    Top := 22;
    Width := 201;
    Height := 21;
    TabOrder := 0;
  end;
end;

destructor TPasswordDialogForm.Destroy;
begin
  FLabelPassword.Free;
  FEditPassword.Free;
  FOKBtn.Free;
  FCancelBtn.Free;
  FLabelUserName.Free;
  FEditUserName.Free;
  FForm.Free;
  inherited;
end;

function TPasswordDialogForm.GetFormPassword: string;
begin
  Result := FEditPassword.Text;
end;

function TPasswordDialogForm.GetFormUserName: string;
begin
  Result := FEditUserName.Text;
end;

function TPasswordDialogForm.ShowModal: Integer;
begin
  Result := FForm.ShowModal;
end;

{ TSearchTextDialogForm }

procedure TSearchTextBar.ButtonCloseClick(Sender: TObject);
begin
  FCefBrowserHost.StopFinding(True);
  Hide;
  FCefBrowserHost.SetFocus(True);
end;

procedure TSearchTextBar.ButtonGoBackClick(Sender: TObject);
begin
  FCefBrowserHost.find(-1, FEditSearch.Text, False, False, True); // 不匹配大小写
  FCefBrowserHost.SetFocus(True);
end;

procedure TSearchTextBar.ButtonGoForwardClick(Sender: TObject);
begin
  FCefBrowserHost.find(-1, FEditSearch.Text, True, False, True); // 不匹配大小写
  FCefBrowserHost.SetFocus(True);
end;

procedure TSearchTextBar.Clear;
begin
  FEditSearch.Text := '';
end;

constructor TSearchTextBar.Create(AOwner: TComponent;
  CefBrowserHost: ICefBrowserHost);
begin
  inherited Create(AOwner);
  FCefBrowserHost := CefBrowserHost;

  Top := 0;
  Width := 372;
  Height := 35;
  BevelInner := bvNone;
  BevelKind := bksoft;
  BevelOuter := bvNone;
  ShowCaption := False;

  FEditSearch := TEdit.Create(nil);
  FButtonGoBack := TButton.Create(nil);
  FButtonGoForward := TButton.Create(nil);
  FButtonClose := TButton.Create(nil);
  with FEditSearch do
  begin
    Parent := Self;
    Left := 3;
    Top := 3;
    Width := 268;
    Height := 25;
    TabOrder := 0;
  end;
  with FButtonGoBack do
  begin
    Parent := Self;
    Left := 279;
    Top := 3;
    Width := 25;
    Height := 25;
    Caption := #9650;
    TabOrder := 1;
    OnClick := ButtonGoBackClick;
  end;
  with FButtonGoForward do
  begin
    Parent := Self;
    Left := 310;
    Top := 3;
    Width := 25;
    Height := 25;
    Caption := #9660;
    TabOrder := 2;
    OnClick := ButtonGoForwardClick;
  end;
  with FButtonClose do
  begin
    Parent := Self;
    Left := 341;
    Top := 3;
    Width := 25;
    Height := 25;
    Caption := #10006;
    TabOrder := 3;
    OnClick := ButtonCloseClick;
  end;
end;

destructor TSearchTextBar.Destroy;
begin
  FCefBrowserHost := nil;
  FEditSearch.Free;
  FButtonGoBack.Free;
  FButtonGoForward.Free;
  FButtonClose.Free;
  inherited;
end;

procedure TSearchTextBar.EditSetFocus;
begin
  FEditSearch.SetFocus;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
{$IF DEFINED(CLR)}
var
  I: Integer;
  Buffer: string;
  Size: TSize;
begin
  SetLength(Buffer, 52);
  for I := 0 to 25 do
    Buffer[I + 1] := Chr(I + Ord('A'));
  for I := 0 to 25 do
    Buffer[I + 27] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, Size);
  Result.X := Size.cx div 52;
  Result.Y := Size.cy;
end;
{$ELSE}

var
  I: Integer;
  Buffer: array [0 .. 51] of Char;
begin
  for I := 0 to 25 do
    Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do
    Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;
{$ENDIF}

function GetTextBaseline(AControl: TControl; ACanvas: TCanvas): Integer;
var
  tm: TTextMetric;
  ClientRect: TRect;
  Ascent: Integer;
begin
  ClientRect := AControl.ClientRect;
  GetTextMetrics(ACanvas.Handle, tm);
  Ascent := tm.tmAscent + 1;
  Result := ClientRect.Top + Ascent;
  Result := AControl.Parent.ScreenToClient
    (AControl.ClientToScreen(TPoint.Create(0, Result))).Y - AControl.Top;
end;

procedure MyConfirm(const ACaption: string; const AText: string;
  var BoolValue: Boolean);
var
  Form: TForm;
  Prompt: TLabel;
  DialogUnits: TPoint;
  CurPrompt: Integer;
  MaxPromptWidth: Integer;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;

  function GetPromptCaption(const ACaption: string): string;
  begin
    if (Length(ACaption) > 1) and (ACaption[1] < #32) then
      Result := Copy(ACaption, 2, MaxInt)
    else
      Result := ACaption;
  end;

  function GetMaxPromptWidth(Canvas: TCanvas): Integer;
  var
    LLabel: TLabel;
  begin
    Result := 0;
    // Use a TLabel rather than an API such as GetTextExtentPoint32 to
    // avoid differences in handling characters such as line breaks.
    LLabel := TLabel.Create(nil);
    try
      LLabel.Caption := GetPromptCaption(AText);
      Result := Max(Result, LLabel.Width + DialogUnits.X);
    finally
      LLabel.Free;
    end;
  end;

  function GetPasswordChar(const ACaption: string): Char;
  begin
    if (Length(ACaption) > 1) and (ACaption[1] < #32) then
      Result := '*'
    else
      Result := #0;
  end;

begin
  Form := TForm.CreateNew(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      MaxPromptWidth := GetMaxPromptWidth(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180 + MaxPromptWidth, DialogUnits.X, 4);
      PopupMode := pmAuto;
      Position := poScreenCenter;
      CurPrompt := MulDiv(8, DialogUnits.Y, 8);

      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := GetPromptCaption(AText);
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := CurPrompt;
        Constraints.MaxWidth := MaxPromptWidth;
        WordWrap := True;
      end;

      ButtonTop := Prompt.Top + Prompt.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(Form.ClientWidth - (ButtonWidth + MulDiv(8, DialogUnits.X, 4))
          * 2, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(Form.ClientWidth - (ButtonWidth + MulDiv(8, DialogUnits.X, 4)
          ), ButtonTop, ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;

      BoolValue := ShowModal = mrOk;
    finally
      Form.Free;
    end;
end;

end.
