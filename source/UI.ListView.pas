{*******************************************************}
{                                                       }
{       FMX UI ListView 扩展单元                        }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.ListView;

interface

{$SCOPEDENUMS ON}

uses
  UI.Debug, UI.Base,
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  FMX.Utils, FMX.ImgList, FMX.MultiResBitmap, FMX.ActnList, System.Rtti, FMX.Consts,
  FMX.TextLayout, FMX.Objects, System.ImageList, System.RTLConsts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, System.Math,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement,
  FMX.ListView, FMX.ListView.Appearances, FMX.ListView.Types, FMX.Styles.Objects;

type
  TListExView = class;

  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TListExView = class(TListView, IView)
  private
    procedure SetLayout(const Value: TViewLayout);
    procedure SetBackground(const Value: TDrawable);
    procedure SetWeight(const Value: Single);
    procedure SetGravity(const Value: TLayoutGravity);
    procedure SetOrientation(const Value: TOrientation);
    procedure SetMaxHeight(const Value: Single);
    procedure SetMaxWidth(const Value: Single);
    procedure SetMinHeight(const Value: Single);
    procedure SetMinWidth(const Value: Single);
    procedure SetAdjustViewBounds(const Value: Boolean);
    procedure SetHeightSize(const Value: TViewSize);
    procedure SetWidthSize(const Value: TViewSize);
    function GetInVisible: Boolean;
    procedure SetInVisible(const Value: Boolean);
    function GetToastHeight: Single;
    procedure UpdateScrollBarEx(AW, AH: Single);
    {$IF CompilerVersion < 31}
    function GetOpacity: Single;
    {$ENDIF}
  protected
    FAdjustViewBounds: Boolean;
    FLayout: TViewLayout;
    FMinWidth: Single;
    FMinHeight: Single;
    FMaxWidth: Single;
    FMaxHeight: Single;
    FWeight: Single;
    FInVisible: Boolean;

    [Weak] FScrollBar: TScrollBar;
    FMaxKnownHeight: Single;

    function GetBackground: TDrawable;
    function GetLayout: TViewLayout;
    function GetParentControl: TControl;
    function GetParentView: IViewGroup;
    function GetAdjustViewBounds: Boolean;
    function GetGravity: TLayoutGravity;
    function GetMaxHeight: Single;
    function GetMaxWidth: Single;
    function GetMinHeight: Single;
    function GetMinWidth: Single;
    function GetWeight: Single;
    function GetViewStates: TViewStates;
    function GetDrawState: TViewState;
    function GetHeightSize: TViewSize;
    function GetWidthSize: TViewSize;
    function GetOrientation: TOrientation;
    function GetComponent: TComponent;
    function GetComponentState: TComponentState;
    function GetPosition: TPosition;

    function IsAutoSize: Boolean;

    procedure IncViewState(const State: TViewState);
    procedure DecViewState(const State: TViewState);

    procedure DoLayoutChanged(Sender: TObject); virtual;
    function AllowUseLayout(): Boolean; virtual;

    procedure DoAdjustViewBounds(var ANewWidth, ANewHeight: Single); virtual;
    procedure DoRecalcSize(var AWidth, AHeight: Single); virtual;
    procedure DoChangeSize(var ANewWidth, ANewHeight: Single); virtual;
    function DoSetSize(const ASize: TControlSize; const NewPlatformDefault: Boolean; ANewWidth, ANewHeight: Single;
      var ALastWidth, ALastHeight: Single): Boolean; override;
    procedure DoChange; override;
    function GetLocalRect: TRectF; override;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ToastHeight: Single read GetToastHeight;
    property Background: TDrawable read GetBackground write SetBackground;
    property Gravity: TLayoutGravity read GetGravity write SetGravity;
    property Orientation: TOrientation read GetOrientation write SetOrientation;
    property ParentView: IViewGroup read GetParentView;
  published
    property AdjustViewBounds: Boolean read GetAdjustViewBounds write SetAdjustViewBounds;
    property InVisible: Boolean read GetInVisible write SetInVisible default False;
    property HeightSize: TViewSize read GetHeightSize write SetHeightSize;
    property WidthSize: TViewSize read GetWidthSize write SetWidthSize;
    property MaxHeight: Single read GetMaxHeight write SetMaxHeight;
    property MaxWidth: Single read GetMaxWidth write SetMaxWidth;
    property MinHeight: Single read GetMinHeight write SetMinHeight;
    property MinWidth: Single read GetMinWidth write SetMinWidth;
    property Weight: Single read GetWeight write SetWeight;
    property Layout: TViewLayout read GetLayout write SetLayout;
    property Transparent default True;
  end;

implementation

resourcestring
  SNotSupports = '不支持的接口';

{ TListExView }

function TListExView.AllowUseLayout: Boolean;
begin
  Result := (not (csDesigning in ComponentState)) or
    (Assigned(ParentControl)) and (ParentControl is TRelativeLayout);
end;

constructor TListExView.Create(AOwner: TComponent);

  function FindScrollbar(): TScrollBar;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to ChildrenCount - 1 do
      if Children[I] is TScrollBar then begin
        Result := Children[I] as TScrollBar;
        Break;
      end;
  end;

begin
  inherited Create(AOwner);
  FAdjustViewBounds := True;
  if csDesigning in ComponentState then begin
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
  WidthSize := TViewSize.CustomSize;
  Transparent := True;
  FScrollBar := FindScrollbar;
  if Assigned(FScrollBar) then
    FScrollBar.Visible := False;
end;

procedure TListExView.DecViewState(const State: TViewState);
begin
end;

destructor TListExView.Destroy;
begin
  FreeAndNil(FLayout);
  inherited;
end;

procedure TListExView.DoAdjustViewBounds(var ANewWidth, ANewHeight: Single);
var
  AMaxW, AMaxH: Single;
begin
  if FAdjustViewBounds then begin
    AMaxW := FMaxWidth;
    AMaxH := FMaxHeight;

    if Assigned(ParentView) then begin
      if (AMaxW <= 0) and (WidthSize = TViewSize.WrapContent) then
        AMaxW := ParentView.MaxWidth;
      if (AMaxH <= 0) and (HeightSize = TViewSize.WrapContent) then
        AMaxH := ParentView.MaxHeight;
    end;

    if (AMaxW > 0) and (ANewWidth > AMaxW) then
      ANewWidth := AMaxW;
    if (AMaxH > 0) and (ANewHeight > AMaxH) then
      ANewHeight := AMaxH;
    if (FMinWidth > 0) and (ANewWidth < FMinWidth) then
      ANewWidth := FMinWidth;
    if (FMinHeight > 0) and (ANewHeight < FMinHeight) then
      ANewHeight := FMinHeight;
  end;
end;

procedure TListExView.DoChange;
begin
  if csDestroying in ComponentState then
    Exit;
  inherited DoChange;
end;

procedure TListExView.DoChangeSize(var ANewWidth, ANewHeight: Single);
begin
  DoRecalcSize(ANewWidth, ANewHeight);
  FMaxKnownHeight := ANewHeight;
  DoAdjustViewBounds(ANewWidth, ANewHeight);
  if Assigned(FScrollBar) and (HeightSize = TViewSize.WrapContent) then begin
    if FMaxKnownHeight <> ANewHeight then begin
      FScrollBar.Visible := FMaxKnownHeight > ANewHeight;
    end;
    FScrollBar.SetBounds(ANewWidth - FScrollBar.Width, 0, FScrollBar.Width, ANewHeight);
    UpdateScrollBarEx(ANewWidth, ANewHeight);
  end;
end;

procedure TListExView.DoLayoutChanged(Sender: TObject);
begin
  HandleSizeChanged;
end;

procedure TListExView.DoRecalcSize(var AWidth, AHeight: Single);
begin
  if HeightSize = TViewSize.WrapContent then
    AHeight := GetToastHeight;
end;

function TListExView.DoSetSize(const ASize: TControlSize;
  const NewPlatformDefault: Boolean; ANewWidth, ANewHeight: Single;
  var ALastWidth, ALastHeight: Single): Boolean;
begin
  DoChangeSize(ANewWidth, ANewHeight);
  Result := inherited DoSetSize(ASize, NewPlatformDefault, ANewWidth, ANewHeight,
    ALastWidth, ALastHeight);
end;

function TListExView.GetAdjustViewBounds: Boolean;
begin
  Result := FAdjustViewBounds;
end;

function TListExView.GetBackground: TDrawable;
begin
  Result := nil;
end;

function TListExView.GetComponent: TComponent;
begin
  Result := Self;
end;

function TListExView.GetComponentState: TComponentState;
begin
  Result := ComponentState;
end;

function TListExView.GetDrawState: TViewState;
begin
  Result := TViewState.None;
end;

function TListExView.GetGravity: TLayoutGravity;
begin
  Result := TLayoutGravity.None;
end;

function TListExView.GetHeightSize: TViewSize;
begin
  if Assigned(FLayout) then
    Result := FLayout.HeightSize
  else Result := TViewSize.CustomSize;
end;

function TListExView.GetInVisible: Boolean;
begin
  Result := FInVisible;
end;

function TListExView.GetLayout: TViewLayout;
begin
  if not AllowUseLayout then
    Result := nil
  else begin
    if not Assigned(FLayout) then begin
      FLayout := TViewLayout.Create(Self);
      FLayout.OnChanged := DoLayoutChanged;
    end;
    Result := FLayout;
  end;
end;

function TListExView.GetLocalRect: TRectF;
begin
  if csDestroying in ComponentState then
    Result := TRectF.Empty
  else
    Result := inherited GetLocalRect;
end;

function TListExView.GetMaxHeight: Single;
begin
  Result := FMaxHeight;
end;

function TListExView.GetMaxWidth: Single;
begin
  Result := FMinWidth;
end;

function TListExView.GetMinHeight: Single;
begin
  Result := FMinHeight;
end;

function TListExView.GetMinWidth: Single;
begin
  Result := FMinWidth;
end;

{$IF CompilerVersion < 31}
function TListExView.GetOpacity: Single;
begin
  Result := Opacity;
end;
{$ENDIF}

function TListExView.GetOrientation: TOrientation;
begin
  Result := TOrientation.Vertical;
end;

function TListExView.GetParentControl: TControl;
begin
  Result := ParentControl;
end;

function TListExView.GetParentView: IViewGroup;
begin
  Supports(Parent, IViewGroup, Result);
end;

function TListExView.GetPosition: TPosition;
begin
  Result := Position;
end;

function TListExView.GetToastHeight: Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
    Result := Result + GetItemHeight(I);
end;

function TListExView.GetViewStates: TViewStates;
begin
  Result := [];
end;

function TListExView.GetWeight: Single;
begin
  Result := FWeight;
end;

function TListExView.GetWidthSize: TViewSize;
begin
  if Assigned(FLayout) then
    Result := FLayout.WidthSize
  else Result := TViewSize.CustomSize;
end;

procedure TListExView.IncViewState(const State: TViewState);
begin
end;

function TListExView.IsAutoSize: Boolean;
begin
  Result := False;
end;

procedure TListExView.SetAdjustViewBounds(const Value: Boolean);
begin
  if FAdjustViewBounds <> Value then begin
    FAdjustViewBounds := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetBackground(const Value: TDrawable);
begin
  raise EViewError.Create(SNotSupports);
end;

procedure TListExView.SetGravity(const Value: TLayoutGravity);
begin
  raise EViewError.Create(SNotSupports);
end;

procedure TListExView.SetHeightSize(const Value: TViewSize);
begin
  if (not Assigned(FLayout)) and (Value <> TViewSize.CustomSize) then begin
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
  if Assigned(FLayout) then
    FLayout.HeightSize := Value;
end;

procedure TListExView.SetInVisible(const Value: Boolean);
begin
  if FInVisible <> Value then begin
    FInVisible := Value;
    if Visible then
      Repaint;
  end;
end;

procedure TListExView.SetLayout(const Value: TViewLayout);
begin
  if not AllowUseLayout then
    Exit;
  if (not Assigned(FLayout)) and (Assigned(Value)) then begin
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
  if Assigned(FLayout) then
    FLayout.Assign(Value);
end;

procedure TListExView.SetMaxHeight(const Value: Single);
begin
  if FMaxHeight <> Value then begin
    FMaxHeight := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetMaxWidth(const Value: Single);
begin
  if FMaxWidth <> Value then begin
    FMaxWidth := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetMinHeight(const Value: Single);
begin
  if FMinHeight <> Value then begin
    FMinHeight := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetMinWidth(const Value: Single);
begin
  if FMinWidth <> Value then begin
    FMinWidth := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetOrientation(const Value: TOrientation);
begin
  raise EViewError.Create(SNotSupports);
end;

procedure TListExView.SetWeight(const Value: Single);
begin
  if FWeight <> Value then begin
    FWeight := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetWidthSize(const Value: TViewSize);
begin
  if (not Assigned(FLayout)) and (Value <> TViewSize.CustomSize) then begin
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
  if Assigned(FLayout) then
    FLayout.WidthSize := Value;
end;

procedure TListExView.UpdateScrollBarEx(AW, AH: Single);
var
  LocalHeight, ViewSize: Single;
begin
  LocalHeight := AH;

  if ScrollViewPos < 0 then
    ViewSize := LocalHeight + ScrollViewPos
  else if ScrollViewPos > FMaxKnownHeight - LocalHeight then
    ViewSize := LocalHeight - (ScrollViewPos - (FMaxKnownHeight - LocalHeight))
  else
    ViewSize := LocalHeight;

  FScrollBar.BeginUpdate;
  try
    FScrollBar.Max := SideSpace * 2 + FMaxKnownHeight;
    FScrollBar.SmallChange := Adapter.GetDefaultViewHeight * 0.5;
    if (csDesigning in ComponentState) then  // Don't show at design time
      FScrollBar.Visible := False;
    FScrollBar.Value := ScrollViewPos;
    FScrollBar.ViewportSize := ViewSize;
  finally
    FScrollBar.EndUpdate;
  end;

end;

initialization

end.
