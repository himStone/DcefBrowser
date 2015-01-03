unit dcefb_BorderLessPC;

interface

uses
  Winapi.Windows, System.Classes, Vcl.ComCtrls;

type
  TBorderLessPageControl = class(TPageControl)
  private
    fTabHeight: Integer;
    procedure SetTabHeight(NewTabHeight: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AdjustClientRect(var Rect: TRect); override;
  published
    property TabHeight: Integer read fTabHeight write SetTabHeight;
  end;

  // 去除了边框,所有TTabsheet.Tabvisible := False

  // procedure Register;

implementation

{
  procedure Register;
  begin
  RegisterComponents('MyComponents', [TBorderLessPageControl]);
  end; }

constructor TBorderLessPageControl.Create(AOwner: TComponent);
begin
  Inherited;
  fTabHeight := 0;
end;

procedure TBorderLessPageControl.AdjustClientRect(var Rect: TRect);
begin
  if (csDesigning in ComponentState) Then
  begin
    Inherited AdjustClientRect(Rect);
  end
  else
    Rect.Top := Rect.Top + fTabHeight;
end;

procedure TBorderLessPageControl.SetTabHeight(NewTabHeight: Integer);
begin
  if (NewTabHeight <> fTabHeight) Then
  begin
    fTabHeight := NewTabHeight;
    Invalidate;
  end;
end;

end.
