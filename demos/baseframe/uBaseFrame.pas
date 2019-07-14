unit uBaseFrame;

interface

uses
  System.Classes, System.SysUtils, FMX.Types,
  UI.Base, UI.Frame;

type
  TFrameView = class;
  TBaseFrameEvent = reference to procedure (View: TFrameView);

  TFrameViewClass = class of TFrameView;
  TFrameView = class(UI.Frame.TFrameView)
  protected
    procedure DoShow; override;
    procedure DoReStart; override;
    procedure DoFinish; override;
    function DoCanFinish(): Boolean; override;
    function DoCanFree(): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Finish(Ani: TFrameAniType); override;
    class function ShowMainFrame(const Title: string = ''; Ani: TFrameAniType = TFrameAniType.None;
      SwitchFlag: Boolean = False): TFrameView;
    class function ShowWithInit(AOnInit: TBaseFrameEvent; const Title: string = ''; Ani: TFrameAniType = TFrameAniType.None;
      SwitchFlag: Boolean = False): TFrameView;
    function StartFrame(FrameClass: TFrameViewClass; Ani: TFrameAniType = TFrameAniType.DefaultAni): TFrameView; overload;
    function StartFrame(FrameClass: TFrameViewClass; const Title: string; Ani: TFrameAniType = TFrameAniType.DefaultAni): TFrameView; overload;
  end;

  TFrame = class(TFrameView);
  TFrameClass = type of TFrame;

implementation

uses
  FMX.Forms,
  UI.Dialog;

{ TFrameView }

constructor TFrameView.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TFrameView.Destroy;
begin

  inherited;
end;

function TFrameView.DoCanFinish: Boolean;
begin
  Result := inherited;
end;

function TFrameView.DoCanFree: Boolean;
begin
  Result := inherited;
end;

procedure TFrameView.DoFinish;
begin
  inherited;
end;

procedure TFrameView.DoReStart;
begin
  inherited;
end;

procedure TFrameView.DoShow;
begin
  inherited;
end;

procedure TFrameView.Finish(Ani: TFrameAniType);
begin
  inherited Finish(Ani);
end;

class function TFrameView.ShowWithInit(AOnInit: TBaseFrameEvent;
  const Title: string; Ani: TFrameAniType; SwitchFlag: Boolean): TFrameView;
begin
  Result := CreateFrame(Application.MainForm, Title) as TFrameView;
  if Result <> nil then begin
    Result.Show(Ani, nil, SwitchFlag);
    if Assigned(AOnInit) then begin
      Result.Hint('Callback from ShowWithInit');
      AOnInit(Result);
    end;
  end;
end;

class function TFrameView.ShowMainFrame(const Title: string;
  Ani: TFrameAniType; SwitchFlag: Boolean): TFrameView;
begin
  Result := CreateFrame(Application.MainForm, Title) as TFrameView;
  if Result <> nil then begin
    Result.Show(Ani, nil, SwitchFlag);
  end;
end;

function TFrameView.StartFrame(FrameClass: TFrameViewClass; const Title: string;
  Ani: TFrameAniType): TFrameView;
begin
  Result := MakeFrame(FrameClass) as TFrameView;
  if Assigned(Result) then begin
    Result.Title := Title;
    Hide(Ani);
    Result.Show(Ani, nil);
  end;
end;

function TFrameView.StartFrame(FrameClass: TFrameViewClass; Ani: TFrameAniType): TFrameView;
begin
  Result := MakeFrame(FrameClass) as TFrameView;
  if Assigned(Result) then begin
    Hide(Ani);
    Result.Show(Ani, nil);
  end;
end;

end.
