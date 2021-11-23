{*******************************************************}
{                                                       }
{       FMX UI EditView 编辑框                          }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Edit;

interface

uses
  UI.Base, UI.Standard,
  {$IFDEF MSWINDOWS}UI.Debug, {$ENDIF}
  FMX.KeyMapping,
  FMX.VirtualKeyboard,
  {$IFDEF ANDROID}
  FMX.VirtualKeyboard.Android,
  {$ENDIF}
  {$IFDEF IOS}
  Macapi.Helpers, FMX.Platform.iOS, FMX.VirtualKeyboard.iOS,
  {$ENDIF}
  FMX.BehaviorManager, FMX.Forms, System.Messaging,
  FMX.Menus, FMX.Presentation.Messages, FMX.Controls.Presentation,
  FMX.Text, FMX.Edit, FMX.Edit.Style, FMX.Controls.Model, FMX.MagnifierGlass,
  FMX.SpellChecker, FMX.ActnList, FMX.Objects,
  System.Math, System.Actions, System.Rtti, FMX.Consts,
  System.TypInfo, System.SysUtils, System.Character, System.RTLConsts,
  FMX.Graphics, System.Generics.Collections, FMX.TextLayout,
  System.Classes, System.Types, System.UITypes, System.Math.Vectors, System.Analytics,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls;

type
  TEditDrawableBorder = class(TDrawableBorder);

type
  TEditViewBase = class(TView, ICaption)
  private
    FText: UI.Base.TTextSettings;
    FTextHint: string;
    FDrawable: TDrawableIcon;
    FIsChanging: Boolean;

    FOnTextChange: TNotifyEvent;
    FOnDrawViewBackgroud: TOnDrawViewBackgroud;

    function GetDrawable: TDrawableIcon;
    procedure SetDrawable(const Value: TDrawableIcon);
    procedure SetTextSettings(const Value: UI.Base.TTextSettings);
    procedure SetTextHint(const Value: string);
  protected
    procedure Loaded; override;
    procedure ImagesChanged; override;
    procedure PaintBackground; override;
    procedure DoPaintBackground(var R: TRectF); virtual;
    procedure DoPaintText(var R: TRectF); virtual;
    procedure SetGravity(const Value: TLayoutGravity); override;
    procedure DoLayoutChanged(Sender: TObject); override;
    function CreateBackground: TDrawable; override;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
    function GetDefaultSize: TSizeF; override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure SetName(const Value: TComponentName); override;
  protected
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
    function TextStored: Boolean;
    procedure DoChanged(Sender: TObject); virtual;
    procedure DoDrawableChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ToString: string; override;
    procedure AfterConstruction; override;
    procedure Change;

    property Text: string read GetText write SetText stored TextStored;
    property TextHint: string read FTextHint write SetTextHint;
    property TextSettings: UI.Base.TTextSettings read FText write SetTextSettings;
    property Drawable: TDrawableIcon read GetDrawable write SetDrawable;

    property OnTextChange: TNotifyEvent read FOnTextChange write FOnTextChange;
    property OnDrawBackgroud: TOnDrawViewBackgroud read FOnDrawViewBackgroud
      write FOnDrawViewBackgroud;
  end;

type
  TEditDataModel = class(TDataModel)
  public const
    DefaultSelectionColor = $802A8ADF;
    DefaultInputSupport = True;
  private
    FChanged: Boolean;
    FSelStart: Integer;
    FSelLength: Integer;
    FReadOnly: Boolean;
    FMaxLength: Integer;
    FPassword: Boolean;
    FKeyboardType : TVirtualkeyboardType;
    FReturnKeyType: TReturnKeyType;
    FImeMode: TImeMode;
    FKillFocusByReturn: Boolean;
    FCheckSpelling: Boolean;
    FCaretPosition: Integer;
    FCaret: TCaret;
    FTyping: Boolean;
    FFilterChar: string;
    FInputSupport: Boolean;
    FTextSettingsInfo: UI.Base.TTextSettings;
    FOnChange: TNotifyEvent;
    FOnChangeTracking: TNotifyEvent;
    FOnTyping: TNotifyEvent;
    FOnValidating: TValidateTextEvent;
    FOnValidate: TValidateTextEvent;
    FValidating: Boolean;
    procedure SetSelStart(const Value: Integer);
    procedure SetSelLength(const Value: Integer);
    procedure SetMaxLength(const Value: Integer);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetPassword(const Value: Boolean);
    procedure SetImeMode(const Value: TImeMode);
    procedure SetKeyboardType(const Value: TVirtualKeyboardType);
    procedure SetReturnKeyType(const Value: TReturnKeyType);
    procedure SetKillFocusByReturn(const Value: Boolean);
    procedure SetCheckSpelling(const Value: Boolean);
    procedure SetCaretPosition(const Value: Integer);
    procedure SetCaret(const Value: TCaret);
    procedure SetTyping(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetFilterChar(const Value: string);
    function GetText: string;
  protected
    ///<summary>Initial text filtering before calling <c>DoTruncating</c></summary>
    function DoFiltering(const Value: string): string; virtual;
    ///<summary>Maximum available text length filtering before calling <c>DoValidating</c></summary>
    function DoTruncating(const Value: string): string; virtual;
    ///<summary>Validate inputing text. Calling before OnChangeTracking</summary>
    function DoValidating(const Value: string): string; virtual;
    function DoValidate(const Value: string): string; virtual;
    procedure DoChangeTracking; virtual;
    procedure DoChange; virtual;
    procedure ResultTextSettingsChanged; virtual;
    /// <summary>
    ///   This property indicates that the control is in validate value mode. See DoValidate, Change
    /// </summary>
    property Validating: Boolean read FValidating;
  public
    constructor Create; override;
    destructor Destroy; override;
    function HasSelection: Boolean;
    function SelectedText: string;
    procedure Change;
    ///<summary>Set text in model without text validation and sending notification to presenter</summary>
    procedure SetTextWithoutValidation(const Value: string);
  public
    property CaretPosition: Integer read FCaretPosition write SetCaretPosition;
    property Caret: TCaret read FCaret write SetCaret;
    property CheckSpelling: Boolean read FCheckSpelling write SetCheckSpelling;
    property FilterChar: string read FFilterChar write SetFilterChar;
    ///<summary>Text control is in read-only mode</summary>
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property ImeMode: TImeMode read FImeMode write SetImeMode;
    property InputSupport: Boolean read FInputSupport write FInputSupport;
    property KeyboardType : TVirtualkeyboardType read FKeyboardType write SetKeyboardType;
    property KillFocusByReturn: Boolean read FKillFocusByReturn write SetKillFocusByReturn;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property Password: Boolean read FPassword write SetPassword;
    property ReturnKeyType: TReturnKeyType read FReturnKeyType write SetReturnKeyType;
    property SelStart: Integer read FSelStart write SetSelStart;
    property SelLength: Integer read FSelLength write SetSelLength;
    property Text: string read GetText write SetText;
    property TextSettingsInfo: UI.Base.TTextSettings read FTextSettingsInfo;
    property Typing: Boolean read FTyping write SetTyping;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeTracking: TNotifyEvent read FOnChangeTracking write FOnChangeTracking;
    property OnTyping: TNotifyEvent read FOnTyping write FOnTyping;
    property OnValidating: TValidateTextEvent read FOnValidating write FOnValidating;
    property OnValidate: TValidateTextEvent read FOnValidate write FOnValidate;
  end;

type
  TCustomEditView = class(TEditViewBase, ITextInput, ICaret, ITextActions, IVirtualKeyboardControl
    {$IF CompilerVersion > 30.0}, IReadOnly{$ENDIF})
  private
    FCursorFill: TBrush;
    FSelectionFill: TBrush;
    FModel: TEditDataModel;
    FTextHeight: Single;
    FLineHeight: Single;
    FLineTop: Single;
    {$IF (not Defined(ANDROID)) or (CompilerVersion < 33)}
    FCharsBuffer: string;
    {$ENDIF}
    FTextLayout: TTextLayout;
    FTextService: TTextService;
    FFirstVisibleChar: Integer;
    FInvisibleTextWidth: Single;
    FContentRect: TRectF;
    {$IFDEF ANDROID}
    FVKState: PByte;
    {$ENDIF}
    { Selection }
    FLeftSelPt: TSelectionPoint;
    FRightSelPt: TSelectionPoint;
    { Loupe }
    FLoupeService: ILoupeService;
    { Spelling }
    FSpellService: IFMXSpellCheckerService;
    FUpdateSpelling: Boolean;
    FSpellingRegions: TRegion;
    FSpellMenuItems: TList<TMenuItem>;
    FSpellHightlightRect: TRectF;
    FSpellFill: TBrush;
    FSpellUnderlineBrush: TStrokeBrush;
    FEditPopupMenu: TPopupMenu;
    FSelectionMode: TSelectionMode;
    FOnModelChange: TNotifyEvent;
    function GetCaretPosition: Integer; overload;
    function GetCaretPosition(const Value: Single): Integer; overload;
    function GetOriginCaretPosition: Integer;
    procedure SetCaretPosition(const Value: Integer);
    procedure SetSelectionMode(const Value: TSelectionMode);
    procedure UpdateSpelling;
    procedure InsertText(const AText: string);
    procedure DoTextChange(Sender: TObject);
    { Selections }
    procedure BeginSelection;
    procedure EndSelection;
    function HaveSelectionPickers: Boolean;
    procedure UpdateSelectionPointPositions;
    procedure DoSelPtMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoLeftSelPtChangePosition(Sender: TObject; var X, Y: Single);
    procedure DoLeftSelPtMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoRightSelPtChangePosition(Sender: TObject; var X, Y: Single);
    procedure DoRightSelPtMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    { Loupe }
    procedure HideLoupe;
    procedure ShowLoupe;
    procedure SetLoupePosition(const ASelectionPointType: TSelectionPointType); overload;
    procedure SetLoupePosition(const X, Y: Single); overload;
    procedure UpdateTextHeight;
    { ITextInput }
    function GetTextService: TTextService;
    procedure IMEStateUpdated;
    function GetTargetClausePointF: TPointF;
    procedure StartIMEInput;
    procedure EndIMEInput;
    function ITextInput.GetSelection = GetSelText;
    function ITextInput.GetSelectionRect = GetSelRect;
    function GetSelectionBounds: TRect;
    {$IF CompilerVersion >= 33}
    function GetSelectionPointSize: TSizeF;
    {$ENDIF}
    function HasText: Boolean;
    {$IFDEF ANDROID}
    procedure UpdateAndroidKeyboardServiceState;
    {$ENDIF}
    function GetMaxLength: Integer;
    function GetPassword: Boolean;
    function GetReadOnly: Boolean;
    procedure SetMaxLength(const Value: Integer);
    procedure SetPassword(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetImeMode(const Value: TImeMode);
    procedure SetKillFocusByReturn(const Value: Boolean);
    function GetFilterChar: string;
    function GetImeMode: TImeMode;
    function GetKillFocusByReturn: Boolean;
    procedure SetFilterChar(const Value: string);
    function GetInputSupport: Boolean;
    procedure SetInputSupport(const Value: Boolean);
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    procedure SetCaret(const Value: TCaret);
    function GetCheckSpelling: Boolean;
    procedure SetCheckSpelling(const Value: Boolean);
    function GetOnChange: TNotifyEvent;
    function GetOnChangeTracking: TNotifyEvent;
    function GetOnTyping: TNotifyEvent;
    function GetOnValidate: TValidateTextEvent;
    function GetOnValidating: TValidateTextEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOnChangeTracking(const Value: TNotifyEvent);
    procedure SetOnTyping(const Value: TNotifyEvent);
    procedure SetOnValidate(const Value: TValidateTextEvent);
    procedure SetOnValidating(const Value: TValidateTextEvent);
    function GetSelfCaret: TCaret;
    procedure SetSelectionFill(const Value: TBrush);
    function GetLength: Integer;
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    function ParentFrame: TFrame;
    { Messages From Model}
    procedure MMSelLengthChanged(var AMessage: TDispatchMessageWithValue<Integer>); message MM_EDIT_SELLENGTH_CHANGED;
    procedure MMSelStartChanged(var AMessage: TDispatchMessageWithValue<Integer>); message MM_EDIT_SELSTART_CHANGED;
    procedure MMCheckSpellingChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message MM_EDIT_CHECKSPELLING_CHANGED;
    procedure MMPasswordChanged(var AMessage: TDispatchMessage); message MM_EDIT_ISPASSWORD_CHANGED;
    procedure MMImeModeChanged(var AMessage: TDispatchMessage); message MM_EDIT_IMEMODE_CHANGED;
    procedure MMTextSettingsChanged(var AMessage: TDispatchMessage); message MM_EDIT_TEXT_SETTINGS_CHANGED;
    procedure MMTextChanged(var AMessage: TDispatchMessageWithValue<string>); message MM_EDIT_TEXT_CHANGED;
    procedure MMTextChanging(var AMessage: TDispatchMessageWithValue<string>); message MM_EDIT_TEXT_CHANGING;
    procedure MMEditButtonsChanged(var Message: TDispatchMessage); message MM_EDIT_EDITBUTTONS_CHANGED;
    /// <summary>Notification about changing of <c>MaxLength</c> property value</summary>
    procedure MMMaxLengthChanged(var Message: TDispatchMessage); message MM_EDIT_MAXLENGTH_CHANGED;
    /// <summary>Notification about changing a <c>TextPrompt</c> property</summary>
    procedure MMPromptTextChanged(var Message: TDispatchMessage); message MM_EDIT_PROMPTTEXT_CHANGED;
    /// <summary>Notification about changing of <c>CaretPosition</c> property value</summary>
    procedure MMCaretPositionChanged(var Message: TDispatchMessageWithValue<Integer>); message MM_EDIT_CARETPOSITION_CHANGED;
    {$IF CompilerVersion >= 32}
    /// <summary>Notification about changing of <c>FilterChar</c> property value</summary>
    procedure MMFilterCharChanged(var Message: TDispatchMessage); message MM_EDIT_FILTERCHAR_CHANGED;
    {$ENDIF}
    {$IF CompilerVersion >= 34}
    procedure MMGetCaretPositionByPoint(var Message: TDispatchMessageWithValue<TCustomEditModel.TGetCaretPositionInfo>); message MM_EDIT_GET_CARET_POSITION_BY_POINT;
    {$ENDIF}
    { Messages from PresentationProxy }
    {$IF CompilerVersion >= 34}
    /// <summary>Notification about lost focus. It's sent directly before the loss of focus.</summary>
    procedure PMDoBeforeExit(var AMessage: TDispatchMessage); message PM_DO_BEFORE_EXIT;
    {$ENDIF}
    procedure PMInit(var Message: TDispatchMessage); message PM_INIT;
    procedure PMGetTextContentRect(var Message: TDispatchMessageWithValue<TRectF>); message PM_EDIT_GET_TEXT_CONTENT_RECT;
    { Base Mouse, Touches and Keyboard Events }
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    procedure LongTap(const X, Y: Single);
    procedure DblTap;
    { ICaret }
    function ICaret.GetObject = GetCaret;
    procedure ShowCaret;
    procedure HideCaret;
    function GetCaret: TCustomCaret;
    {$IF CompilerVersion = 30}
    function ITextInput.ReadOnly = GetReadOnly;
    {$ENDIF}
    { Context menu }
    function CreatePopupMenu: TPopupMenu; virtual;
    function FindContextMenuItem(const AItemName: string): TMenuItem;
    function ShowContextMenu(const ScreenPosition: TPointF): Boolean; override;
    procedure UpdatePopupMenuItems; virtual;
    function GetEditPopupMenu: TPopupMenu;
    property EditPopupMenu: TPopupMenu read GetEditPopupMenu;
    { Standart Text Actions: Cut, Copy, Paste, Delete, Select All }
    procedure DoCopy(Sender: TObject);
    procedure DoCut(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoPaste(Sender: TObject);
    procedure DoSelectAll(Sender: TObject);
    { Spelling }
    procedure UpdateSpellPopupMenu(const APoint: TPointF);
    procedure SpellFixContextMenuHandler(Sender: TObject);
    procedure UpdateTextLayout;
    { IVirtualKeyboardControl }
    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure SetReturnKeyType(Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    function IVirtualKeyboardControl.IsPassword = GetPassword;
    property InputSupport: Boolean read GetInputSupport write SetInputSupport;
  protected
    FLastKey: Word;
    FLastChar: System.WideChar;
    FClipboardSvc: IFMXClipboardService;
    procedure RepaintEdit;
    procedure SetTextInternal(const Value: string); virtual;
    function GetPasswordCharWidth: Single;
    function TextWidth(const AStart, ALength: Integer): Single;
    procedure UpdateFirstVisibleChar;
    procedure UpdateCaretPosition;
    function GetSelText: string;
    function GetSelRect: TRectF;
    function CheckGravity(const SrcX, EditRectWidth, WholeTextWidth: Single): Single;

    { Content alignment }
    procedure RealignContent; virtual;
    procedure UpdateLayoutSize;
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure DoPaintText(var ARect: TRectF); override;
    procedure DoChangeTracking; virtual;
    procedure DoRealign; override;
    procedure DoTyping; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoInitStyle; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetCharX(a: Integer): Single;

    procedure PlayClickEffect(); override;

    { ITextActions }
    procedure DeleteSelection;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    procedure SelectWord;
    procedure ResetSelection;
    procedure GoToTextEnd;
    procedure GoToTextBegin;
    procedure Replace(const AStartPos: Integer; const ALength: Integer; const AStr: string);
    function HasSelection: Boolean;

    procedure HideInputMethod();

    property Caret: TCaret read GetSelfCaret write SetCaret;
    property CaretPosition: Integer read GetCaretPosition write SetCaretPosition;
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling default False;
    property ContentRect: TRectF read FContentRect;
    property SelText: string read GetSelText;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelectionFill: TBrush read FSelectionFill write SetSelectionFill;
    property TextHeight: Single read FTextHeight;
    property LineHeight: Single read FLineHeight;
    property LineTop: Single read FLineTop;
    property Length: Integer read GetLength;
    property SelectionMode: TSelectionMode read FSelectionMode write SetSelectionMode;
    property Model: TEditDataModel read FModel;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
    property Password: Boolean read GetPassword write SetPassword default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property FilterChar: string read GetFilterChar write SetFilterChar;
    property ImeMode: TImeMode read GetImeMode write SetImeMode default TImeMode.imDontCare;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType default TVirtualKeyboardType.Default;
    property KillFocusByReturn: Boolean read GetKillFocusByReturn write SetKillFocusByReturn default False;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType default TReturnKeyType.Default;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnChangeTracking: TNotifyEvent read GetOnChangeTracking write SetOnChangeTracking;
    property OnTyping: TNotifyEvent read GetOnTyping write SetOnTyping;
    property OnValidating: TValidateTextEvent read GetOnValidating write SetOnValidating;
    property OnValidate: TValidateTextEvent read GetOnValidate write SetOnValidate;
  published
    property Align;
    property Anchors;
    property TabOrder;
    property TabStop;
  end;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TEditView = class(TCustomEditView)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Action;
  published
    property Cursor default crIBeam;
    property CanFocus default True;
    property CanParentFocus;
    property DragMode default TDragMode.dmManual;
    property HitTest default True;
    property Clickable default True;


    property Text;
    property TextHint;
    property TextSettings;
    property Drawable;
    property OnTextChange;
    property OnDrawBackgroud;
    property Gravity default TLayoutGravity.CenterVertical;

     { inherited }
    property DisableFocusEffect;
    property KeyboardType;
    property ReturnKeyType;
    property Password;
    property ReadOnly;
    property MaxLength;
    property FilterChar;
    property ImeMode;
    property Position;
    property Width;
    property Height;
    property ClipChildren default False;
    property ClipParent default False;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible default True;
    property Caret;
    property KillFocusByReturn;
    property CheckSpelling;
    property ParentShowHint;
    property ShowHint;
    property SelectionFill;
    { events }
    property OnChange;
    property OnChangeTracking;
    property OnTyping;
    property OnApplyStyleLookup;
    property OnValidating;
    property OnValidate;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnKeyDown;
    property OnKeyUp;
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

implementation

const
  LOUPE_OFFSET = 10;
  IMEWindowGap = 2; // 2 is small space between conrol and IME window

  CutStyleName = 'cut'; //Do not localize
  CopyStyleName = 'copy'; //Do not localize
  PasteStyleName = 'paste'; //Do not localize
  DeleteStyleName = 'delete'; //Do not localize
  SelectAllStyleName = 'selectall'; //Do not localize

  CaretColorStyleResouceName = 'caretcolor';
  LeftSelectionPointStyleResourceName = 'leftselectionpoint';
  RightSelectionPointStyleResourceName = 'rightselectionpoint';

function LinkObserversValueModified(const AObservers: TObservers): Boolean;
begin
  Result := True;
  if AObservers.IsObserving(TObserverMapping.EditLinkID) then
  begin
    Result := TLinkObservers.EditLinkEdit(AObservers);
    if Result then
      TLinkObservers.EditLinkModified(AObservers);
  end;
  if Result and AObservers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueModified(AObservers);
end;

{$IF Defined(ANDROID) and (CompilerVersion <= 32)}
function PackText(const AText: string): string;
begin
  if not AText.IsEmpty then
    Result := Format('[%d]%s', [AText.Length, AText])
  else
    Result := string.Empty;
end;
{$ENDIF}

{ TEditViewBase }

procedure TEditViewBase.AfterConstruction;
begin
  inherited AfterConstruction;
  FText.OnChanged := DoChanged;
  FText.OnTextChanged := FOnTextChange;
  FIsChanging := False;
end;

function TEditViewBase.CanRePaintBk(const View: IView;
  State: TViewState): Boolean;
var
  Border: TViewBorder;
begin
  if Assigned(FOnDrawViewBackgroud) then
    Result := True
  else begin
    Result := inherited CanRePaintBk(View, State);
    if (not Result) and (Assigned(FBackground)) then begin
      Border := TDrawableBorder(FBackground).Border;
      Result := Assigned(Border) and (Border.Style <> TViewBorderStyle.None) and
        (Border.Width > 0) and (Border.Color.GetColor(State) <> TAlphaColorRec.Null);
    end;
  end;
end;

procedure TEditViewBase.Change;
begin
  if not FIsChanging and ([csLoading, csDestroying] * ComponentState = []){$IF CompilerVersion < 32} and not Released{$ENDIF} then
  begin
    FIsChanging := True;
    try
      DoChanged(FText);
    finally
      FIsChanging := False;
    end;
  end;
end;

constructor TEditViewBase.Create(AOwner: TComponent);
var
  SaveChange: TNotifyEvent;
begin
  inherited Create(AOwner);

  FIsChanging := True;
  FText := UI.Base.TTextSettings.Create(Self);

  if csDesigning in ComponentState then begin
    FDrawable := TDrawableIcon.Create(Self);
    FDrawable.SizeWidth := 16;
    FDrawable.SizeHeight := 16;
    FDrawable.OnChanged := DoDrawableChanged;

    if Assigned(Padding) then begin
      SaveChange := Padding.OnChange;
      Padding.OnChange := nil;
      Padding.Rect := RectF(6, 2, 6, 2);
      Padding.OnChange := SaveChange;
    end;
  end;

  SetAcceptsControls(False);
end;

function TEditViewBase.CreateBackground: TDrawable;
begin
  Result := TEditDrawableBorder.Create(Self, TViewBrushKind.Solid, $FFFFFFFF);
  with TDrawableBorder(Result).Border do begin
    Width := 1;
    DefaultStyle := TViewBorderStyle.RectBorder;
    Style := DefaultStyle;
    Color.Default := $BFC0C0C0;
    Color.Focused := $FF0066cc;
    Color.Hovered := $FFC0C0C0;
  end;
  Result.OnChanged := DoBackgroundChanged;
end;

destructor TEditViewBase.Destroy;
begin
  FreeAndNil(FText);
  FreeAndNil(FDrawable);
  inherited Destroy;
end;

procedure TEditViewBase.DoChanged(Sender: TObject);
begin
  FGravity := FText.Gravity;
  if FText.IsSizeChange then begin
    RecalcSize;
  end else
    Repaint;
  if FText.IsEffectsChange then
    UpdateEffects;
end;

procedure TEditViewBase.DoDrawableChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TEditViewBase.DoLayoutChanged(Sender: TObject);
begin
  inherited DoLayoutChanged(Sender);
end;

procedure TEditViewBase.DoPaintBackground(var R: TRectF);
begin
  R := RectF(R.Left + Padding.Left, R.Top + Padding.Top,
    R.Right - Padding.Right, R.Bottom - Padding.Bottom);
  if Assigned(FDrawable) and (not FDrawable.IsEmpty) then
    FDrawable.AdjustDraw(Canvas, R, True, DrawState);
  if (Assigned(FText)) then
    DoPaintText(R);
end;

procedure TEditViewBase.DoPaintText(var R: TRectF);
begin
  if FText.Text = '' then
    FText.Draw(Canvas, FTextHint, R, GetAbsoluteOpacity, TViewState(8))
  else
    FText.Draw(Canvas, R, GetAbsoluteOpacity, DrawState);
end;

function TEditViewBase.GetData: TValue;
begin
  Result := Text;
end;

function TEditViewBase.GetDefaultSize: TSizeF;
var
  MetricsService: IFMXDefaultMetricsService;
begin
  if (TBehaviorServices.Current.SupportsBehaviorService(IFMXDefaultMetricsService, MetricsService, Self)
    or SupportsPlatformService(IFMXDefaultMetricsService, MetricsService))
    and MetricsService.SupportsDefaultSize(TComponentKind.Edit) then
    Result := TSizeF.Create(MetricsService.GetDefaultSize(TComponentKind.Edit))
  else
    Result := TSizeF.Create(100, 22);
end;

function TEditViewBase.GetDrawable: TDrawableIcon;
begin
  if not Assigned(FDrawable) then begin
    FDrawable := TDrawableIcon.Create(Self);
    FDrawable.SizeWidth := 16;
    FDrawable.SizeHeight := 16;
    FDrawable.OnChanged := DoDrawableChanged;
  end;
  Result := FDrawable;
end;

function TEditViewBase.GetText: string;
begin
  Result := FText.Text;
end;

procedure TEditViewBase.ImagesChanged;
begin
  if Assigned(FDrawable) then
    FDrawable.Change;
  inherited ImagesChanged;
end;

procedure TEditViewBase.Loaded;
begin
  inherited Loaded;
  FText.OnChanged := DoChanged;
  Change;
end;

procedure TEditViewBase.PaintBackground;
var
  R: TRectF;
begin
  if AbsoluteInVisible then
    Exit;
  R := RectF(0, 0, Width, Height);
  if Assigned(FOnDrawViewBackgroud) then
    FOnDrawViewBackgroud(Self, Canvas, R, DrawState)
  else
    inherited PaintBackground;
  DoPaintBackground(R);
end;

procedure TEditViewBase.SetData(const Value: TValue);
begin
  if Value.IsEmpty then
    Text := string.Empty
  else
    if Value.IsType<TNotifyEvent> then
      FOnTextChange := Value.AsType<TNotifyEvent>()
    else
      Text := Value.ToString;
end;

procedure TEditViewBase.SetDrawable(const Value: TDrawableIcon);
begin
  Drawable.Assign(Value);
end;

procedure TEditViewBase.SetGravity(const Value: TLayoutGravity);
begin
  FGravity := Value;
  FText.Gravity := Value;
end;

procedure TEditViewBase.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (csLoading in TComponent(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then
    Text := Value;
end;

procedure TEditViewBase.SetText(const Value: string);
begin
  FText.Text := Value;
end;

procedure TEditViewBase.SetTextHint(const Value: string);
begin
  if FTextHint <> Value then begin
    FTextHint := Value;
    if FText.Text = '' then
      DoChanged(FText);
  end;
end;

procedure TEditViewBase.SetTextSettings(const Value: UI.Base.TTextSettings);
begin
  FText := Value;
end;

function TEditViewBase.TextStored: Boolean;
begin
  Result := (not Text.IsEmpty and not ActionClient) or (not (ActionClient and (ActionLink <> nil) and
    ActionLink.CaptionLinked and (Action is TContainedAction)));
end;

function TEditViewBase.ToString: string;
begin
  Result := Format('%s ''%s''', [inherited ToString, FText]);
end;

{ TEditDataModel }

procedure TEditDataModel.Change;
begin
  if FChanged then
  begin
    if not FValidating then
    begin
      FValidating := True;
      try
        Text := DoValidate(Text);
      finally
        FValidating := False;
      end;
    end;
    FChanged := False;
    try
      DoChange;
    finally
    end;
  end;
end;

constructor TEditDataModel.Create;
begin
  inherited Create;
  FInputSupport := DefaultInputSupport;
  FCaret := TCaret.Create(Owner as TFmxObject);
  FCaret.Visible := InputSupport;
  FCaret.Color := TAlphaColorRec.Null;
  FCaret.ReadOnly := ReadOnly;
end;

destructor TEditDataModel.Destroy;
begin
  FreeAndNil(FCaret);
  inherited;
end;

procedure TEditDataModel.DoChange;
begin
  if (Owner <> nil) and Assigned(FOnChange) and not (csLoading in Owner.ComponentState) then
    FOnChange(Owner);
end;

procedure TEditDataModel.DoChangeTracking;
begin
  FChanged := True;
  SendMessage(MM_EDIT_TEXT_CHANGING);
  if Assigned(FOnChangeTracking) and not (csLoading in Owner.ComponentState) then
    FOnChangeTracking(Owner);
end;

function TEditDataModel.DoFiltering(const Value: string): string;
begin
  Result := FilterText(Value, FilterChar);
end;

function TEditDataModel.DoTruncating(const Value: string): string;
begin
  Result := TruncateText(Value, MaxLength);
end;

function TEditDataModel.DoValidate(const Value: string): string;
begin
  Result := DoTruncating(DoFiltering(Value));
  if (Owner <> nil) and (Owner is TCustomEditView) and Assigned(FOnValidate) and not (csLoading in Owner.ComponentState) then
    FOnValidate(Owner, Result);
end;

function TEditDataModel.DoValidating(const Value: string): string;
begin
  Result := Value;
  if (Owner <> nil) and (Owner is TCustomEditView) and Assigned(FOnValidating) and not (csLoading in Owner.ComponentState) then
    FOnValidating(Owner, Result);
end;

function TEditDataModel.GetText: string;
begin
  Result := FTextSettingsInfo.Text;
end;

function TEditDataModel.HasSelection: Boolean;
begin
  Result := Abs(SelLength) > 0;
end;

function TEditDataModel.SelectedText: string;
begin
  if SelLength < 0 then
    Result := Text.Substring(SelStart - Abs(SelLength), Abs(SelLength))
  else if SelLength > 0 then
    Result := Text.Substring(SelStart, SelLength)
  else
    Result := string.Empty;
end;

procedure TEditDataModel.SetCaret(const Value: TCaret);
begin
  FCaret.Assign(Value);
  SendMessage(MM_EDIT_CARETCHANGED);
end;

procedure TEditDataModel.SetCaretPosition(const Value: Integer);
begin
  FCaretPosition := EnsureRange(Value, 0, FTextSettingsInfo.Text.Length);
  SendMessage<Integer>(MM_EDIT_CARETPOSITION_CHANGED, Value);
end;

procedure TEditDataModel.SetCheckSpelling(const Value: Boolean);
begin
  if FCheckSpelling <> Value then
  begin
    FCheckSpelling := Value;
    SendMessage<Boolean>(MM_EDIT_CHECKSPELLING_CHANGED, Value);
  end;
end;

procedure TEditDataModel.SetFilterChar(const Value: string);
var
  OldText: string;
begin
  if FFilterChar <> Value then
  begin
    FFilterChar := Value;
    {$IF CompilerVersion >= 32}
    SendMessage<string>(MM_EDIT_FILTERCHAR_CHANGED, Value);
    {$ENDIF}
    OldText := FTextSettingsInfo.Text;
    FTextSettingsInfo.Text := DoValidating(DoTruncating(DoFiltering(FTextSettingsInfo.Text)));
    if FTextSettingsInfo.Text <> OldText then
    begin
      DoChangeTracking;
      SendMessage<string>(MM_EDIT_TEXT_CHANGED, FTextSettingsInfo.Text);
      Change;
    end;
  end;
end;

procedure TEditDataModel.SetImeMode(const Value: TImeMode);
begin
  if FImeMode <> Value then
  begin
    FImeMode := Value;
    SendMessage<TImeMode>(MM_EDIT_IMEMODE_CHANGED, Value);
  end;
end;

procedure TEditDataModel.SetKeyboardType(const Value: TVirtualKeyboardType);
begin
  if FKeyboardType <> Value then
  begin
    FKeyboardType := Value;
    SendMessage<TVirtualKeyboardType>(MM_EDIT_KEYBOARDTYPE_CHANGED, Value);
  end;
end;

procedure TEditDataModel.SetKillFocusByReturn(const Value: Boolean);
begin
  if FKillFocusByReturn <> Value then
  begin
    FKillFocusByReturn := Value;
    SendMessage<Boolean>(MM_EDIT_KILLFOCUSBYRETURN_CHANGED, Value);
  end;
end;

procedure TEditDataModel.SetMaxLength(const Value: Integer);
begin
  if MaxLength <> Value then
  begin
    FMaxLength := Max(0, Value);
    Text := DoTruncating(Text);
    Change;
    SendMessage<Integer>(MM_EDIT_MAXLENGTH_CHANGED, Value);
  end;
end;

procedure TEditDataModel.SetPassword(const Value: Boolean);
begin
  if FPassword <> Value then
  begin
    FPassword := Value;
    SendMessage<Boolean>(MM_EDIT_ISPASSWORD_CHANGED, Value);
  end;
end;

procedure TEditDataModel.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    FCaret.ReadOnly := Value;
    SendMessage<Boolean>(MM_EDIT_READONLY_CHANGED, Value);
  end;
end;

procedure TEditDataModel.SetReturnKeyType(const Value: TReturnKeyType);
begin
  if FReturnKeyType <> Value then
  begin
    FReturnKeyType := Value;
    SendMessage<TReturnKeyType>(MM_EDIT_RETURNKEYTYPE_CHANGED, Value);
  end;
end;

procedure TEditDataModel.SetSelLength(const Value: Integer);
begin
  if FSelLength <> Value then
  begin
    FSelLength := Value;
    SendMessage<Integer>(MM_EDIT_SELLENGTH_CHANGED, Value);
  end;
end;

procedure TEditDataModel.SetSelStart(const Value: Integer);
begin
  if FSelStart <> Value then
  begin
    FSelStart := EnsureRange(Value, 0, Text.Length);
    SendMessage<Integer>(MM_EDIT_SELSTART_CHANGED, Value);
  end;
end;

procedure TEditDataModel.SetText(const Value: string);
var
  OldText: string;
begin
  if Text <> Value then
  begin
    OldText := FTextSettingsInfo.Text;
    FTextSettingsInfo.Text := DoValidating(DoTruncating(DoFiltering(Value)));
    if FTextSettingsInfo.Text <> OldText then
    begin
      DoChangeTracking;
      SendMessage<string>(MM_EDIT_TEXT_CHANGED, FTextSettingsInfo.Text);
      Change;
    end;
  end;
end;

procedure TEditDataModel.SetTextWithoutValidation(const Value: string);
begin
  if Text <> Value then
  begin
    FTextSettingsInfo.Text := Value;
    DoChangeTracking;
  end;
end;

procedure TEditDataModel.SetTyping(const Value: Boolean);
begin
  if FTyping <> Value then
  begin
    FTyping := Value;
    SendMessage(MM_EDIT_TYPING_CHANGED);
  end;
end;

procedure TEditDataModel.ResultTextSettingsChanged;
begin
  SendMessage(MM_EDIT_TEXT_SETTINGS_CHANGED);
end;

{ TCustomEditView }

procedure TCustomEditView.BeginSelection;
begin
  SelectionMode := TSelectionMode.TextSelection;
  FTextService.BeginSelection;
end;

function TCustomEditView.CheckGravity(const SrcX, EditRectWidth, WholeTextWidth: Single): Single;
begin
  case FGravity of
    // 中间
    TLayoutGravity.CenterHorizontal, TLayoutGravity.Center, TLayoutGravity.CenterHBottom:
      Result := SrcX + ((EditRectWidth - WholeTextWidth) / 2);
    // 右边
    TLayoutGravity.RightTop, TLayoutGravity.RightBottom, TLayoutGravity.CenterVRight:
      Result := SrcX + (EditRectWidth - WholeTextWidth);
  else
    Result := SrcX;
  end;
end;

procedure TCustomEditView.CMGesture(var EventInfo: TGestureEventInfo);
var
  LocalPoint: TPointF;
begin
  if EventInfo.GestureID = igiLongTap then
  begin
    LocalPoint := AbsoluteToLocal(EventInfo.Location);
    LongTap(LocalPoint.X, LocalPoint.Y);
  end
  else if EventInfo.GestureID = igiDoubleTap then
    DblTap
  else
    inherited;
end;

procedure TCustomEditView.CopyToClipboard;
begin
  if (FClipboardSvc = nil) or Password then
    Exit;

  if InputSupport and not SelText.IsEmpty then
    FClipboardSvc.SetClipboard(SelText);

  if not InputSupport and not Text.IsEmpty then
    FClipboardSvc.SetClipboard(Text);
end;

constructor TCustomEditView.Create(AOwner: TComponent);
var
  PlatformTextService: IFMXTextService;
begin
  inherited Create(AOwner);
  EnableExecuteAction := False;
  FModel := TEditDataModel.Create(Self);
  FModel.FTextSettingsInfo := FText;
  FModel.OnChange := DoTextChange;
  FContentRect.Left := 0;
  FContentRect.Top := 0;
  FContentRect.Right := 0;
  FContentRect.Bottom := 0;

  FTextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  if not TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(FClipboardSvc)) then
    FClipboardSvc := nil;
  if TPlatformServices.Current.SupportsPlatformService(IFMXTextService, IInterface(PlatformTextService)) then
    FTextService := PlatformTextService.GetTextServiceClass.Create(Self, False);
  TPlatformServices.Current.SupportsPlatformService(ILoupeService, IInterface(FLoupeService));

  FCursorFill := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Black);
  FSelectionFill := TBrush.Create(TBrushKind.Solid, $7F3399FF);
  AutoCapture := True;

  FFirstVisibleChar := 1;
  FInvisibleTextWidth := 0;
  SetAcceptsControls(False);
  Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.DoubleTap, TInteractiveGesture.LongTap];

  FSpellMenuItems := TList<TMenuItem>.Create;
  FSpellFill := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Red);
  FSpellUnderlineBrush := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Red);
  FSpellUnderlineBrush.Dash := TStrokeDash.Dot;
  FSpellUnderlineBrush.Thickness := 1;
  FModel.Receiver := Self;
end;

function TCustomEditView.CreatePopupMenu: TPopupMenu;
var
  TmpItem: TMenuItem;
begin
  Result := TPopupMenu.Create(Self);
  Result.Stored := False;

  TmpItem := TMenuItem.Create(Result);
  TmpItem.Parent := Result;
  TmpItem.Text := SEditCut;
  TmpItem.StyleName := CutStyleName;
  TmpItem.OnClick := DoCut;

  TmpItem := TMenuItem.Create(Result);
  TmpItem.Parent := Result;
  TmpItem.Text := SEditCopy;
  TmpItem.StyleName := CopyStyleName;
  TmpItem.OnClick := DoCopy;

  TmpItem := TMenuItem.Create(Result);
  TmpItem.Parent := Result;
  TmpItem.Text := SEditPaste;
  TmpItem.StyleName := PasteStyleName;
  TmpItem.OnClick := DoPaste;

  TmpItem := TMenuItem.Create(Result);
  TmpItem.Parent := Result;
  TmpItem.Text := SEditDelete;
  TmpItem.StyleName := DeleteStyleName;
  TmpItem.OnClick := DoDelete;

  TmpItem := TMenuItem.Create(Result);
  TmpItem.Parent := Result;
  TmpItem.Text := SMenuSeparator;

  TmpItem := TMenuItem.Create(Result);
  TmpItem.Parent := Result;
  TmpItem.Text := SEditSelectAll;
  TmpItem.StyleName := SelectAllStyleName;
  TmpItem.OnClick := DoSelectAll;
end;

procedure TCustomEditView.CutToClipboard;
begin
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if not TLinkObservers.EditLinkEdit(Observers) then
    begin
      TLinkObservers.EditLinkReset(Observers);
      Exit;
    end
    else
      TLinkObservers.EditLinkModified(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueModified(Observers);

  CopyToClipboard;
  DeleteSelection;
end;

procedure TCustomEditView.DblTap;
begin
  SelectWord;
end;

procedure TCustomEditView.DeleteSelection;
begin
  if ReadOnly or not InputSupport or (SelLength = 0) then
    Exit;

  Model.Text := Text.Remove(SelStart, SelLength);
  CaretPosition := SelStart;
  SelLength := 0;
end;

destructor TCustomEditView.Destroy;
begin
  FModel.Receiver := nil;
  FModel.Free;
  FLoupeService := nil;
  FreeAndNil(FCursorFill);
  FreeAndNil(FSelectionFill);
  FreeAndNil(FEditPopupMenu);
  FreeAndNil(FTextService);
  FClipboardSvc := nil;
  FSpellService := nil;
  Finalize(FSpellingRegions);
  FreeAndNil(FSpellMenuItems);
  FreeAndNil(FSpellFill);
  FreeAndNil(FSpellUnderlineBrush);
  FreeAndNil(FTextLayout);
  inherited Destroy;
end;

procedure TCustomEditView.DoChangeTracking;
begin
  UpdateSpelling;
end;

procedure TCustomEditView.DoCopy(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TCustomEditView.DoCut(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TCustomEditView.DoDelete(Sender: TObject);
begin
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if not TLinkObservers.EditLinkEdit(Observers) then
    begin
      TLinkObservers.EditLinkReset(Observers);
      Exit;
    end
    else
      TLinkObservers.EditLinkModified(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueModified(Observers);

  DeleteSelection;
end;

procedure TCustomEditView.DoEnter;
var
  Form: TCommonCustomForm;
begin
  inherited DoEnter;
  Form := TCommonCustomForm(Root);
  if not Model.ReadOnly and Model.InputSupport and not FTextService.HasMarkedText and
    ((Form = nil) or (Form.FormState - [TFmxFormState.Showing, TFmxFormState.Engaged, TFmxFormState.Modal] = [])) then
    SelectAll
  else begin
    UpdateSelectionPointPositions;
    UpdateCaretPosition;
  end;
end;

procedure TCustomEditView.DoExit;
begin
  {$IF CompilerVersion < 34}
  if FScene <> nil then begin
    Model.Change;
    if Observers.IsObserving(TObserverMapping.EditLinkID) then
      TLinkObservers.EditLinkUpdate(Observers);
    if Observers.IsObserving(TObserverMapping.ControlValueID) then
      TLinkObservers.ControlValueUpdate(Observers);
    inherited DoExit;
    UpdateSelectionPointPositions;
    {$IFDEF ANDROID}
    UpdateAndroidKeyboardServiceState;
    Self.FocusToNext;
    {$ENDIF}
  end else
  {$ENDIF}
    inherited DoExit;
end;

procedure TCustomEditView.DoInitStyle;
var
  ColorObject: TColorObject;
begin
  RealignContent;
  { Caret color}
  if FindStyleResource<TColorObject>(CaretColorStyleResouceName, ColorObject) then
    Model.Caret.DefaultColor := ColorObject.Color
  else
    Model.Caret.DefaultColor := TAlphaColorRec.Null;
  if not (csDesigning in ComponentState) then begin
    {$IFDEF MSWINDOWS}
    Model.Caret.Color := TAlphaColorRec.Black;
    {$ELSE}
    {$IFDEF IOS}
    Model.Caret.Color := $ff0066cc;
    {$ELSE}
    Model.Caret.Color := TAlphaColorRec.Black;
    {$ENDIF}
    {$ENDIF}
  end;
  { Selection points }
  if FindStyleResource<TSelectionPoint>(LeftSelectionPointStyleResourceName, FLeftSelPt) then
  begin
    FLeftSelPt.OnTrack := DoLeftSelPtChangePosition;
    FLeftSelPt.OnMouseDown := DoLeftSelPtMouseDown;
    FLeftSelPt.OnMouseUp := DoSelPtMouseUp;
    FLeftSelPt.Visible := False;
  end;
  if FindStyleResource<TSelectionPoint>(RightSelectionPointStyleResourceName, FRightSelPt) then
  begin
    FRightSelPt.OnTrack := DoRightSelPtChangePosition;
    FRightSelPt.OnMouseDown := DoRightSelPtMouseDown;
    FRightSelPt.OnMouseUp := DoSelPtMouseUp;
    FRightSelPt.Visible := False;
  end;
  TextSettings.Change;
  UpdateTextLayout;
end;

procedure TCustomEditView.DoLeftSelPtChangePosition(Sender: TObject; var X,
  Y: Single);
var
  CurrentPoint: TPointF;
  NewSelStart: Integer;
  OldSelStart: Integer;
  OldSelLength: Integer;
  NewSelLength: Integer;
  OldSelEnd: Integer;
begin
  if FLeftSelPt = nil then
    Exit;
  CurrentPoint := FLeftSelPt.Position.Point;

  OldSelStart := Model.SelStart;
  OldSelLength := Model.SelLength;
  OldSelEnd := Model.SelStart + Model.SelLength;
  NewSelStart := GetCaretPosition(X);
  NewSelLength := OldSelLength + OldSelStart - NewSelStart;

  Model.DisableNotify;
  try
    if NewSelStart < OldSelEnd - 1 then
    begin
      X := GetCharX(NewSelStart);
      Model.SelStart := NewSelStart;
      Model.SelLength := NewSelLength;
    end
    else
    begin
      X := GetCharX(OldSelEnd - 1);
      Model.SelStart := OldSelEnd - 1;
      Model.SelLength := 1;
    end;
  finally
    Model.EnableNotify;
  end;
  Y := CurrentPoint.Y;
  SetLoupePosition(TSelectionPointType.Left);
  UpdateSelectionPointPositions;
end;

procedure TCustomEditView.DoLeftSelPtMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  BeginSelection;
  SetLoupePosition(TSelectionPointType.Left);
  ShowLoupe;
end;

procedure TCustomEditView.DoPaintText(var ARect: TRectF);
var
  i: Integer;
  State: TCanvasSaveState;
  WholeTextWidth: Single;
  EditRectWidth: Single;
  T: string;

  procedure DrawSelection;
  var
    SelectionRect: TRectF;
  begin
    SelectionRect := GetSelRect;
    Canvas.FillRect(SelectionRect, 0, 0, AllCorners, AbsoluteOpacity, FSelectionFill);
  end;

  procedure DrawLeftAndRightSelectionSide;
  var
    SelectionRect: TRectF;
    HalfCaretWidth: Single;
    SideRect: TRectF;
  begin
    SelectionRect := GetSelRect;
    HalfCaretWidth := Model.Caret.Flasher.Size.Width / 2;
    FCursorFill.Color := Model.Caret.Flasher.Color;
    // Draw Left selection side
    SideRect := RectF(SelectionRect.Left - HalfCaretWidth, SelectionRect.Top,
                      SelectionRect.Left + HalfCaretWidth, SelectionRect.Bottom);
    Canvas.FillRect(SideRect, 0, 0, AllCorners, AbsoluteOpacity, FCursorFill);
    // Draw Right selection side
    SideRect := RectF(SelectionRect.Right - HalfCaretWidth, SelectionRect.Top,
                      SelectionRect.Right + HalfCaretWidth, SelectionRect.Bottom);
    Canvas.FillRect(SideRect, 0, 0, AllCorners, AbsoluteOpacity, FCursorFill);
  end;

var
  Shift, BP, EP, J: Integer;
  Rgn: TRegion;
  LText: string;
  VisibleCharPos: Single;
  R: TRectF;
begin
  if Text = '' then begin
    FText.Draw(Canvas, FTextHint, ARect, GetAbsoluteOpacity, TViewState(8));
    Exit;
  end;

  if ((FTextService = nil)) or (FTextService.Text.IsEmpty and (not FTextService.HasMarkedText)) then
    Exit;
  State := Canvas.SaveState;
  try
    { Draw selection }
    if IsFocused and Model.HasSelection then
    begin
      DrawSelection;
      { left picker -> | selected text | <- right picker }
      if HaveSelectionPickers then
        DrawLeftAndRightSelectionSide;
    end;
    { draw text }
    Canvas.IntersectClipRect(ARect);
    Canvas.Fill.Color := FText.Color.GetStateColor(DrawState);
    R := ARect;
    if Model.Password then
    begin
      R.Right := R.Left + GetPasswordCharWidth - 1;
      R.Top := LineTop - ContentRect.Top + ((LineHeight - R.Width) / 2);
      R.Bottom := R.Top + RectWidth(R);
      T := FTextService.CombinedText;
      WholeTextWidth := T.Length * GetPasswordCharWidth;
      EditRectWidth := ContentRect.Width;
      if WholeTextWidth < EditRectWidth then
        case FText.HorzAlign of
          TTextAlign.Trailing:
            OffsetRect(R, (EditRectWidth - WholeTextWidth), 0);
          TTextAlign.Center:
            OffsetRect(R, ((EditRectWidth - WholeTextWidth) / 2), 0);
        end;
      for i := FFirstVisibleChar to T.Length do
      begin
        Canvas.FillEllipse(R, AbsoluteOpacity, Canvas.Fill);
        OffsetRect(R, R.Width + 1, 0);
      end;
    end
    else
    begin
      FTextService.DrawSingleLine(Canvas,
        R, FFirstVisibleChar, FText.Font,
        AbsoluteOpacity, FillTextFlags, FText.HorzAlign, FText.VertAlign);
    end;
    //Spell highlighting
    if Model.CheckSpelling and (FSpellService <> nil) and not FTextService.HasMarkedText and not Text.IsEmpty then
    begin
      if FUpdateSpelling then
      begin
        LText := Text;
        Shift := 0;
        while (LText.Length > 0) and FMX.Text.FindWordBound(LText, 0, BP, EP) do
        begin
          if System.Length(FSpellService.CheckSpelling(LText.Substring(BP, EP - BP + 1))) > 0 then
          begin
            Rgn := FTextLayout.RegionForRange(TTextRange.Create(Shift + BP, EP - BP + 1));
            for J := Low(Rgn) to High(Rgn) do
            begin
              SetLength(FSpellingRegions, System.Length(FSpellingRegions) + 1);
              FSpellingRegions[High(FSpellingRegions)] := Rgn[J];
              R := ContentRect;
              FSpellingRegions[High(FSpellingRegions)].Offset(-R.Left, -R.Top);
            end;
          end;
          LText := LText.Remove(0, EP + 1);
          Inc(Shift, EP + 1);
        end;
        FUpdateSpelling := False;
      end;
      if System.Length(FSpellingRegions) > 0 then
      begin
        if FFirstVisibleChar > 1 then
        begin
          Rgn := FTextLayout.RegionForRange(TTextRange.Create(FFirstVisibleChar - 1, 1));
          if System.Length(Rgn) > 0 then
            VisibleCharPos := Rgn[0].Left
          else
            VisibleCharPos := 0;
        end
        else
          VisibleCharPos := 0;
        for I := Low(FSpellingRegions) to High(FSpellingRegions) do
          Canvas.DrawLine(TPointF.Create(FSpellingRegions[I].Left - VisibleCharPos, FSpellingRegions[I].Bottom),
            TPointF.Create(FSpellingRegions[I].Right - VisibleCharPos, FSpellingRegions[I].Bottom), AbsoluteOpacity,
            FSpellUnderlineBrush);
      end;
      if not FSpellHightlightRect.IsEmpty then
        Canvas.FillRect(FSpellHightlightRect, 0, 0, [], 0.2, FSpellFill);
    end;
  finally
    Canvas.RestoreState(State);
  end;
end;

procedure TCustomEditView.DoPaste(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TCustomEditView.DoRealign;
begin
  inherited DoRealign;
  RealignContent;
  CaretPosition := GetOriginCaretPosition;
end;

procedure TCustomEditView.DoRightSelPtChangePosition(Sender: TObject; var X,
  Y: Single);
var
  CurrentPoint: TPointF;
  NewSelEnd: Integer;
  OldSelStart: Integer;
  OldSelLength: Integer;
  NewSelLength: Integer;
  OldSelEnd: Integer;
  MinSelEnd: Integer;
begin
  if FRightSelPt = nil then
    Exit;
  CurrentPoint := FRightSelPt.Position.Point;
  Y := CurrentPoint.Y;

  OldSelStart := Model.SelStart;
  OldSelLength := Model.SelLength;
  OldSelEnd := Model.SelStart + Model.SelLength;
  MinSelEnd := Model.SelStart + 1;
  NewSelEnd := GetCaretPosition(X);
  NewSelLength := OldSelLength + NewSelEnd - OldSelEnd;

  Model.DisableNotify;
  try
    if NewSelEnd > MinSelEnd then
    begin
      X := GetCharX(NewSelEnd);
      Model.SelStart := NewSelEnd - NewSelLength;
      Model.SelLength := NewSelLength;
    end
    else
    begin
      X := GetCharX(MinSelEnd);
      Model.SelStart := OldSelStart;
      Model.SelLength := 1;
    end;
  finally
    Model.EnableNotify;
  end;

  SetLoupePosition(TSelectionPointType.Right);
  UpdateSelectionPointPositions;
end;

procedure TCustomEditView.DoRightSelPtMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  BeginSelection;
  SetLoupePosition(TSelectionPointType.Right);
  ShowLoupe;
end;

procedure TCustomEditView.DoSelectAll(Sender: TObject);
begin
  SelectAll;
end;

procedure TCustomEditView.DoSelPtMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  EndSelection;
end;

procedure TCustomEditView.DoTextChange(Sender: TObject);
begin
  {$IFDEF ANDROID}
  if (MaxLength > 0) and (System.Length(Text) > MaxLength) then
    Text := Text.SubString(0, MaxLength);
  {$ENDIF}
  if Assigned(FOnModelChange) then
    FOnModelChange(Sender);
end;

procedure TCustomEditView.DoTyping;
begin
  if Assigned(Model.OnTyping) then
    Model.OnTyping(Self);
end;

procedure TCustomEditView.EndIMEInput;
{$IFDEF ANDROID}
var
  LText: string;
{$ENDIF}
begin
  // Windows TextService controls CaretPosition and Text itself.
  // We have to update Text and CaretPosition only, if Edit initiate it.
  {$IFNDEF MSWINDOWS}
  Model.DisableNotify;
  try
    {$IFDEF ANDROID}
    LText := FTextService.CombinedText;
    if (Model.MaxLength > 0) and (System.Length(LText) > Model.MaxLength) then
      LText := LText.Substring(0, Model.MaxLength);
    Model.Text := LText;
    {$ELSE}
    Model.Text := FTextService.CombinedText;
    {$ENDIF}
  finally
    Model.EnableNotify;
  end;
  FTextService.Text := Model.Text; // FTextService.CombinedText;
  FTextService.CaretPosition := Point(GetOriginCaretPosition + FTextService.CombinedText.Length - FTextService.Text.Length, 0);
  {$ENDIF}
  RepaintEdit;
end;

procedure TCustomEditView.EndSelection;
begin
  HideLoupe;
  SelectionMode := TSelectionMode.None;
  FTextService.EndSelection;
end;

function TCustomEditView.FindContextMenuItem(
  const AItemName: string): TMenuItem;
var
  MenuObject: TFmxObject;
begin
  Result := nil;
  if FEditPopupMenu <> nil then
  begin
    MenuObject := FEditPopupMenu.FindStyleResource(AItemName);
    if MenuObject is TMenuItem then
      Result := TMenuItem(MenuObject);
  end;
end;

function TCustomEditView.GetCaret: TCustomCaret;
begin
  Result := Model.Caret;
end;

function TCustomEditView.GetCaretPosition: Integer;
begin
  if FTextService <> nil then
    Result := FTextService.TargetClausePosition.X
  else
    Result := -1;
end;

function TCustomEditView.GetCharX(a: Integer): Single;
var
  WholeTextWidth: Single;
  EditRectWidth: Single;
  Rgn: TRegion;
  T: string;
begin
  if Model.Password then begin
    T := FTextService.CombinedText;
    EditRectWidth := GetPasswordCharWidth;
    WholeTextWidth := T.Length * EditRectWidth + Padding.Left;
    Result := ContentRect.Left;
    if a > 0 then begin
      if a <= T.Length then
        Result := Result + (a - FFirstVisibleChar + 1) * EditRectWidth
      else
        Result := Result + (T.Length - FFirstVisibleChar + 1) * EditRectWidth;
    end;
    EditRectWidth := ViewRect.Width;
    if WholeTextWidth < EditRectWidth then
      Result := CheckGravity(Result, EditRectWidth, WholeTextWidth);
  end else begin
    Rgn := FTextLayout.RegionForRange(TTextRange.Create(0, 1));
    if System.Length(Rgn) > 0 then
      Result := Rgn[0].Left
    else
      Result := 0;
    if (FFirstVisibleChar - 1) < a then begin
      Rgn := FTextLayout.RegionForRange(TTextRange.Create(FFirstVisibleChar - 1, a - FFirstVisibleChar + 1));
      if System.Length(Rgn) > 0 then
        Result := Result + Rgn[High(Rgn)].Width;
    end;

    EditRectWidth := ViewRect.Width;
    WholeTextWidth := ContentRect.Width + Padding.Left;
    if WholeTextWidth < EditRectWidth then
      Result := CheckGravity(Result, EditRectWidth, WholeTextWidth);
  end;
end;

function TCustomEditView.GetCheckSpelling: Boolean;
begin
  Result := Model.CheckSpelling;
end;

function TCustomEditView.GetCaretPosition(const Value: Single): Integer;
var
  Tmp, WholeTextWidth, EditRectWidth, PwdW: Single;
  CombinedText: string;
begin
  Result := FFirstVisibleChar - 1;
  CombinedText := FTextService.CombinedText;

  if not CombinedText.IsEmpty then begin
    if Model.Password then begin
      PwdW := GetPasswordCharWidth;
      WholeTextWidth := CombinedText.Length * PwdW;

      EditRectWidth := ViewRect.Width;
      Tmp := Value;
      if WholeTextWidth < EditRectWidth then
        Tmp := CheckGravity(Value, EditRectWidth, WholeTextWidth);

      Result := Result + Trunc((Tmp - ContentRect.Left) / PwdW);
      if Result < 0 then
        Result := 0
      else if Result > CombinedText.Length then
        Result := CombinedText.Length;
    end
    else
      Result := FTextLayout.PositionAtPoint(TPointF.Create(Value + FInvisibleTextWidth, FTextLayout.TextRect.Top + FTextLayout.TextHeight / 2));
  end;
end;

function TCustomEditView.GetEditPopupMenu: TPopupMenu;
begin
  if FEditPopupMenu = nil then
    FEditPopupMenu := CreatePopupMenu;
  Result := FEditPopupMenu;
end;

function TCustomEditView.GetFilterChar: string;
begin
  Result := Model.FilterChar;
end;

function TCustomEditView.GetImeMode: TImeMode;
begin
  Result := Model.ImeMode;
end;

function TCustomEditView.GetInputSupport: Boolean;
begin
  Result := Model.InputSupport;
end;

function TCustomEditView.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := Model.KeyboardType;
end;

function TCustomEditView.GetKillFocusByReturn: Boolean;
begin
  Result := Model.KillFocusByReturn;
end;

function TCustomEditView.GetLength: Integer;
begin
  Result := Model.FTextSettingsInfo.Text.Length;
end;

function TCustomEditView.GetMaxLength: Integer;
begin
  Result := Model.MaxLength;
end;

function TCustomEditView.GetOnChange: TNotifyEvent;
begin
  Result := FOnModelChange;
end;

function TCustomEditView.GetOnChangeTracking: TNotifyEvent;
begin
  Result := Model.OnChangeTracking;
end;

function TCustomEditView.GetOnTyping: TNotifyEvent;
begin
  Result := Model.OnTyping;
end;

function TCustomEditView.GetOnValidate: TValidateTextEvent;
begin
  Result := Model.OnValidate;
end;

function TCustomEditView.GetOnValidating: TValidateTextEvent;
begin
  Result := Model.OnValidating;
end;

function TCustomEditView.GetOriginCaretPosition: Integer;
begin
  if FTextService <> nil then
    Result := FTextService.CaretPosition.X
  else
    Result := -1;
end;

function TCustomEditView.GetPassword: Boolean;
begin
  Result := Model.Password;
end;

function TCustomEditView.GetPasswordCharWidth: Single;
begin
  Result := FText.Font.Size / 2;
end;

function TCustomEditView.GetReadOnly: Boolean;
begin
  Result := Model.ReadOnly;
end;

function TCustomEditView.GetReturnKeyType: TReturnKeyType;
begin
  Result := Model.ReturnKeyType;
end;

function TCustomEditView.GetSelectionBounds: TRect;
begin
  Result := Rect(Model.SelStart, 0, Model.SelStart + Model.SelLength, 0);
end;

{$IF CompilerVersion >= 33}
function TCustomEditView.GetSelectionPointSize: TSizeF;
begin
  Result.Width := Model.SelLength;
  Result.Height := 0;
end;
{$ENDIF}

function TCustomEditView.GetSelfCaret: TCaret;
begin
  Result := Model.Caret;
end;

function TCustomEditView.GetSelLength: Integer;
begin
  Result := Abs(Model.SelLength);
end;

{$IFDEF ANDROID}
procedure TCustomEditView.UpdateAndroidKeyboardServiceState;
var
  ASvc: IFMXVirtualKeyboardService;
  AContext: TRttiContext;
  AType: TRttiType;
  AField: TRttiField;
  AInst: TVirtualKeyboardAndroid;
begin
  if not Assigned(FVKState) then begin
    if (not Assigned(Screen.FocusControl)) and
      TPlatformServices.Current.SupportsPlatformService
      (IFMXVirtualKeyboardService, ASvc) then
    begin
      AInst := ASvc as TVirtualKeyboardAndroid;
      AContext := TRttiContext.Create;
      AType := AContext.GetType(TVirtualKeyboardAndroid);
      AField := AType.GetField('FState');
      if AField.GetValue(AInst).AsOrdinal <> 0 then
      begin
        FVKState := PByte(AInst);
        Inc(FVKState, AField.Offset);
      end;
    end;
  end;
  if Assigned(FVKState) and (FVKState^ <> 0) then
    FVKState^ := 0;
end;
{$ENDIF}

function TCustomEditView.GetSelRect: TRectF;
var
  Offset, StartPosition, EndPosition: Integer;
begin
  Result := ContentRect;
  Result.Top := Trunc(LineTop);
  Result.Bottom := Result.Top + LineHeight;
  {$IFNDEF ANDROID}
  if GetOriginCaretPosition <= Min(Model.SelStart, Model.SelStart + Model.SelLength) then
    Offset := FTextService.CombinedText.Length - FTextService.Text.Length
  else
    Offset := 0;
  {$ELSE}
  Offset := 0;
  {$ENDIF}
  StartPosition := Model.SelStart + Offset;
  EndPosition := Model.SelStart + Model.SelLength + Offset;
  Result.Left := GetCharX(Min(StartPosition, EndPosition));
  Result.Right := GetCharX(Max(StartPosition, EndPosition));
end;

function TCustomEditView.GetSelStart: Integer;
begin
  if Model.SelLength > 0 then
    Result := Model.SelStart
  else
    if Model.SelLength < 0 then
      Result := Model.SelStart + Model.SelLength
    else
      Result := GetOriginCaretPosition;
end;

function TCustomEditView.GetSelText: string;
begin
  Result := Model.SelectedText;
end;

function TCustomEditView.GetTargetClausePointF: TPointF;
var
  Str: String;
begin
  Str := FTextService.CombinedText.Substring(0, Round(FTextService.TargetClausePosition.X) );
  if FFirstVisibleChar > 1 then
    Str := Str.Substring(FFirstVisibleChar - 1, MaxInt);
  Result.X := TextWidth(0, Str.Length);
  Result.Y := (ContentRect.Height / 2) + FText.Font.Size / 2 + IMEWindowGap;
  Result.Offset(ContentRect.TopLeft);
  Result := LocalToAbsolute(Result);
end;

function TCustomEditView.GetText: string;
begin
  Result := FTextService.CombinedText;
end;

function TCustomEditView.GetTextService: TTextService;
begin
  Result := FTextService;
end;

procedure TCustomEditView.GoToTextBegin;
begin
  CaretPosition := 0;
end;

procedure TCustomEditView.GoToTextEnd;
begin
  CaretPosition := Text.Length;
end;

function TCustomEditView.HasSelection: Boolean;
begin
  Result := Model.HasSelection;
end;

function TCustomEditView.HasText: Boolean;
begin
  Result := Text.Length > 0;
end;

function TCustomEditView.HaveSelectionPickers: Boolean;
begin
  Result := (FLeftSelPt <> nil) and (FRightSelPt <> nil);
end;

procedure TCustomEditView.HideCaret;
begin
  Model.Caret.Hide;
end;

procedure TCustomEditView.HideInputMethod;
{$IF Defined(ANDROID) or Defined(IOS)}
var
  AService: IFMXVirtualKeyboardService;
{$ENDIF}
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  try
    if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, AService) then
    begin
      AService.HideVirtualKeyboard();
    end;
  except
  end;
{$ENDIF}
end;

procedure TCustomEditView.HideLoupe;
begin
  if FLoupeService <> nil then
    FLoupeService.Hide;
end;

procedure TCustomEditView.IMEStateUpdated;
var
  CombinedText: string;
begin
  CombinedText := FTextService.CombinedText;
  FTextLayout.Text := CombinedText;
  SetCaretPosition(GetOriginCaretPosition);
  Model.SetTextWithoutValidation(CombinedText);
  // Windows TextService controls CaretPosition and Text itself.
  // We have to update Text and CaretPosition only, if Edit initiate it.
  {$IFNDEF MSWINDOWS}
  if Model.SelLength > 0 then
  begin
    Model.DisableNotify;
    try
      Model.SelLength := 0;
    finally
      Model.EnableNotify;
    end;
    UpdateSelectionPointPositions;
  end;
  {$ENDIF}
  LinkObserversValueModified(Self.Observers);
  DoChangeTracking;
  DoTyping;
  {$IFDEF ANDROID}
  FModel.DoChange;
  {$ENDIF}
end;

procedure TCustomEditView.InsertText(const AText: string);
var
  OldText: string;
  SelStart, SelLength: Integer;
begin
  if Model.ReadOnly and not Model.InputSupport then
    Exit;

  OldText := Text;
  SelStart := Min(Model.SelStart, Model.SelStart + Model.SelLength);
  SelLength := Abs(Model.SelLength);

  OldText := OldText.Remove(SelStart, SelLength);

  SelStart := CaretPosition;
  if Model.SelLength < 0 then
    SelStart := SelStart + Model.SelLength;
  OldText := OldText.Insert(SelStart, AText);

  Model.DisableNotify;
  try
    Model.SelLength := 0;
  finally
    Model.EnableNotify;
  end;

  if (Model.MaxLength <= 0) or (OldText.Length <= Model.MaxLength) then
  begin
    SetTextInternal(OldText);
    CaretPosition := SelStart + AText.Length;
  end;
end;

procedure TCustomEditView.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
var
  TmpS: string;
  OldCaretPosition: Integer;
  LCaret: Integer;
  IsCtrlOrCmd, KeyHandled: Boolean;
begin
  if not Model.InputSupport then
    Exit;
  KeyHandled := False;
  try
    if Observers.IsObserving(TObserverMapping.EditLinkID) then
    begin
      if (Key = vkBack) or (Key = vkDelete) or ((Key = vkInsert) and (ssShift in Shift)) then
        if not TLinkObservers.EditLinkEdit(Observers) then
        begin
          TLinkObservers.EditLinkReset(Observers);
          KeyHandled := True;
          Exit;
        end;

      if (KeyChar >= #32) and not TLinkObservers.EditLinkIsValidChar(Observers, KeyChar) then
      begin
        KeyHandled := True;
        Exit;
      end;
      case KeyChar of
        ^H, ^V, ^X, #32..High(Char):
          if not TLinkObservers.EditLinkEdit(Observers) then
          begin
            TLinkObservers.EditLinkReset(Observers);
            KeyHandled := True;
            Exit;
          end;
        #27:
          begin
            TLinkObservers.EditLinkReset(Observers);
            SelectAll;
            KeyHandled := True;
            Exit;
          end;
      end;

      if TLinkObservers.EditLinkIsEditing(Observers) then
        TLinkObservers.EditLinkModified(Observers);
    end;
    if Observers.IsObserving(TObserverMapping.ControlValueID) then
      TLinkObservers.ControlValueModified(Observers);

    inherited;
    OldCaretPosition := GetOriginCaretPosition;
    FLastChar := KeyChar;
    FLastKey := Key;
    IsCtrlOrCmd := Shift * [ssCtrl, ssCommand] <> [];
    case Key of
      vkA:
        if IsCtrlOrCmd and (Shift * [ssAlt, ssShift] = []) then
        begin
          SelectAll;
          KeyHandled := True;
        end;
      vkC:
        if IsCtrlOrCmd then
        begin
          CopyToClipboard;
          KeyHandled := True;
        end;
      vkV:
        if IsCtrlOrCmd then
        begin
          PasteFromClipboard;
          DoTyping;
          KeyHandled := True;
        end;
      vkX:
        if IsCtrlOrCmd and not Model.ReadOnly then
        begin
          CutToClipboard;
          DoTyping;
          KeyHandled := True;
        end;
      vkZ:
        if IsCtrlOrCmd then
        begin
          if Observers.IsObserving(TObserverMapping.EditLinkID) then
            TLinkObservers.EditLinkReset(Observers);
          if Observers.IsObserving(TObserverMapping.ControlValueID) then
            TLinkObservers.ControlValueUpdate(Observers);
          KeyHandled := True;
        end;
      vkReturn:
        begin
          Model.DisableNotify;
          try
            Model.Typing := False;
          finally
            Model.EnableNotify;
          end;
          Model.Change;
          if Observers.IsObserving(TObserverMapping.EditLinkID) then
            TLinkObservers.EditLinkUpdate(Observers);
          if Observers.IsObserving(TObserverMapping.ControlValueID) then
            TLinkObservers.ControlValueUpdate(Observers);
          if Model.KillFocusByReturn and (Root <> nil) then
            Root.SetFocused(nil);
          // not need to perform KeyHandled := True;
        end;
      vkEnd:
      begin
        CaretPosition := Text.Length;
        KeyHandled := True;
      end;
      vkHome:
      begin
        CaretPosition := 0;
        KeyHandled := True;
      end;
      vkLeft:
      begin
        if IsCtrlOrCmd then
        begin
          CaretPosition := GetPrevLexemeBegin(Text, GetOriginCaretPosition);
          KeyHandled := True;
        end
        else
          if (GetOriginCaretPosition > 0) and not Text.IsEmpty then
          begin
            if Text.Chars[GetOriginCaretPosition - 1].IsLowSurrogate then
              CaretPosition := GetOriginCaretPosition - 2
            else
              CaretPosition := GetOriginCaretPosition - 1;
            KeyHandled := True;
          end;
      end;
      vkRight:
      begin
        if IsCtrlOrCmd then
        begin
          CaretPosition := GetNextLexemeBegin(Text, GetOriginCaretPosition);
          KeyHandled := True;
        end
        else
          if (Text.Length > GetOriginCaretPosition) then
          begin
            if Text.Chars[GetOriginCaretPosition].IsHighSurrogate then
              CaretPosition := GetOriginCaretPosition + 2
            else
              CaretPosition := GetOriginCaretPosition + 1;
            KeyHandled := True;
          end;
      end;
      vkDelete:
      begin
        if not Model.ReadOnly then
        begin
          if Model.HasSelection then
          begin
            if Shift = [ssShift] then
              CutToClipboard
            else
              DeleteSelection;
            DoTyping;
            KeyHandled := True;
          end
          else
          begin
            TmpS := Text;
            if not TmpS.IsEmpty then
            begin
              if IsCtrlOrCmd then
              begin
                //Delete whole word
                LCaret := GetNextLexemeBegin(Text, GetOriginCaretPosition);
                if LCaret < 0 then
                  Exit;
                TmpS := TmpS.Remove(LCaret, GetOriginCaretPosition - LCaret);
              end
              else
              begin
                LCaret := GetOriginCaretPosition;
                //Delete single character
                if (Text.Length > 1) and (GetOriginCaretPosition < Text.Length) and Text.Chars[GetOriginCaretPosition].IsHighSurrogate then
                  TmpS := TmpS.Remove(GetOriginCaretPosition, 2)
                else
                  TmpS := TmpS.Remove(GetOriginCaretPosition, 1);
              end;
              SetTextInternal(TmpS);
              CaretPosition := LCaret;
              DoTyping;
              KeyHandled := True;
            end;
          end;
        end;
      end;
      vkBack:
        if not Model.ReadOnly then
        begin
          if Model.HasSelection then
          begin
            DeleteSelection;
            DoTyping;
            KeyHandled := True;
          end
          else
          begin
            TmpS := Text;
            if not TmpS.IsEmpty then
            begin
              if IsCtrlOrCmd then
              begin
                //Delete whole word
                LCaret := GetPrevLexemeBegin(Text, GetOriginCaretPosition);
                if LCaret < 0 then
                  Exit;
                TmpS := TmpS.Remove(LCaret, GetOriginCaretPosition - LCaret);
              end
              else
              begin
                LCaret := GetOriginCaretPosition - 1;
                if TmpS.Chars[LCaret].IsLowSurrogate then
                begin
                  Dec(LCaret);
                  TmpS := TmpS.Remove(LCaret, 2)
                end
                else
                  TmpS := TmpS.Remove(LCaret, 1);
              end;
              SetTextInternal(TmpS);
              CaretPosition := LCaret;
              DoTyping;
              KeyHandled := True;
            end;
          end;
        end;
      vkInsert:
        if Shift = [ssShift] then
        begin
          PasteFromClipboard;
          DoTyping;
          KeyHandled := True;
        end
        else if IsCtrlOrCmd then
        begin
          CopyToClipboard;
          KeyHandled := True;
        end;
    end;

    if (KeyChar <> #0) and not Model.FilterChar.IsEmpty and not Model.FilterChar.Contains(KeyChar) then
      KeyChar := #0;

    if Key in [vkEnd, vkHome, vkLeft, vkRight] then
    begin
      Model.DisableNotify;
      try
        if ssShift in Shift then
        begin
          Model.SelStart := GetOriginCaretPosition;
          Model.SelLength := Model.SelLength - (GetOriginCaretPosition - OldCaretPosition);
        end
        else
          Model.SelLength := 0;
        RepaintEdit;
        UpdateSelectionPointPositions;
        KeyHandled := True;
      finally
        Model.EnableNotify;
      end;
    end;

    if (Ord(KeyChar) >= 32) and not Model.ReadOnly then
    begin
    {$IF (not Defined(ANDROID)) or (CompilerVersion < 33)}
      FCharsBuffer := FCharsBuffer + KeyChar;
      if not KeyChar.IsHighSurrogate then
      begin
        Model.DisableNotify;
        try
          Model.Typing:= True;
        finally
          Model.EnableNotify;
        end;
        InsertText(FCharsBuffer);
        FCharsBuffer := string.Empty;
        DoTyping;
      end;
      KeyHandled := True;
    {$ELSE}
      // On the Android we use proxy native EditText for inputting any kind of text. So implementation TTextService takes
      // care on any kind of text inputting. Therefore, we don't need to intercept inputting latin chars also.
    {$ENDIF}
    end;
    //if ResourceControl <> nil then
    //  ResourceControl.UpdateEffects;
  finally
    if KeyHandled then
    begin
      Key := 0;
      KeyChar := #0;
    end;
  end;
end;

procedure TCustomEditView.Loaded;
begin
  inherited Loaded;
  DoInitStyle;
end;

procedure TCustomEditView.LongTap(const X, Y: Single);
begin
  if SelectionMode <> TSelectionMode.TextSelection then
  begin
    SelectionMode := TSelectionMode.CursorPosChanging;
    if FLoupeService <> nil then
    begin
      FLoupeService.SetLoupeMode(TLoupeMode.Circle);
      ShowLoupe;
      SetLoupePosition(X, Y);
    end;
  end;
end;

procedure TCustomEditView.MMCaretPositionChanged(
  var Message: TDispatchMessageWithValue<Integer>);
begin
  SetCaretPosition(Message.Value);
end;

procedure TCustomEditView.MMCheckSpellingChanged(
  var AMessage: TDispatchMessageWithValue<Boolean>);
var
  I: Integer;
begin
  if Model.CheckSpelling then
  begin
    if not TPlatformServices.Current.SupportsPlatformService(IFMXSpellCheckerService, IInterface(FSpellService)) then
      FSpellService := nil;
    FUpdateSpelling := not Text.IsEmpty;
  end
  else
  begin
    for I := 0 to FSpellMenuItems.Count - 1 do
      FSpellMenuItems[I].Parent := nil;
    FSpellMenuItems.Clear;
    FSpellService := nil;
    SetLength(FSpellingRegions, 0);
    FUpdateSpelling := False;
    FSpellHightlightRect := TRectF.Empty;
  end;
end;

procedure TCustomEditView.MMEditButtonsChanged(var Message: TDispatchMessage);
begin
end;

{$IF CompilerVersion >= 34}
procedure TCustomEditView.MMGetCaretPositionByPoint(
  var Message: TDispatchMessageWithValue<TCustomEditModel.TGetCaretPositionInfo>);
begin
  Message.Value.CaretPosition := GetCaretPosition(Message.Value.HitPoint.X);
end;
{$ENDIF}

{$IF CompilerVersion >= 32}
procedure TCustomEditView.MMFilterCharChanged(var Message: TDispatchMessage);
begin
  if FTextService <> nil then
    FTextService.FilterChar := Model.FilterChar;
end;
{$ENDIF}

procedure TCustomEditView.MMImeModeChanged(var AMessage: TDispatchMessage);
begin
  if Model.Password then
    FTextService.SetImeMode(TImeMode.imDisable)
  else
    FTextService.SetImeMode(Model.ImeMode);
end;

procedure TCustomEditView.MMMaxLengthChanged(var Message: TDispatchMessage);
begin
  if FTextService <> nil then
    FTextService.MaxLength := Model.MaxLength;
end;

procedure TCustomEditView.MMPasswordChanged(var AMessage: TDispatchMessage);
begin
  if Model.Password then
    FTextService.SetImeMode(TImeMode.imDisable)
  else
    FTextService.SetImeMode(Model.ImeMode);
  RepaintEdit;
end;

procedure TCustomEditView.MMPromptTextChanged(var Message: TDispatchMessage);
begin
  Repaint;
end;

procedure TCustomEditView.MMSelLengthChanged(
  var AMessage: TDispatchMessageWithValue<Integer>);
begin
  UpdateSelectionPointPositions;
  RepaintEdit;
end;

procedure TCustomEditView.MMSelStartChanged(
  var AMessage: TDispatchMessageWithValue<Integer>);
begin
  UpdateSelectionPointPositions;
  RepaintEdit;
end;

procedure TCustomEditView.MMTextChanged(var AMessage: TDispatchMessageWithValue<string>);
var
  LText: string;
begin
  LText := AMessage.Value;
  if FTextService.CombinedText <> LText then
  begin
    FTextLayout.Text := LText;
    {$IF (not Defined(ANDROID)) or (CompilerVersion > 32)}
    FTextService.Text := LText;
    {$ELSE}
    FTextService.Text := PackText(LText);
    {$ENDIF}
    UpdateFirstVisibleChar;
    if FTextService.CaretPosition.X > LText.Length then
      SetCaretPosition(LText.Length)
    else
      UpdateCaretPosition;
    RepaintEdit;
  end;
end;

procedure TCustomEditView.MMTextChanging(
  var AMessage: TDispatchMessageWithValue<string>);
begin
  DoChangeTracking;
end;

procedure TCustomEditView.MMTextSettingsChanged(var AMessage: TDispatchMessage);
begin
  if ([csLoading, csDesigning] * ComponentState = []){$IF CompilerVersion < 32} and not Released{$ENDIF} then
  begin
    UpdateTextHeight;
    if not FDisableAlign then
      RealignContent;
    RepaintEdit;
  end;
  UpdateTextLayout;
end;

procedure TCustomEditView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  NewPosition: Integer;
begin
  inherited;
  Model.DisableNotify;
  try
    if (Button = TMouseButton.mbLeft) and Model.InputSupport then
    begin
      NewPosition := GetCaretPosition(X);
      if ssShift in Shift then
        Model.SelLength := NewPosition - Model.SelStart
      else
        Model.SelLength := 0;
      CaretPosition := NewPosition;
      Model.SelStart := NewPosition;
      if ssDouble in Shift then
      begin
        SelectionMode := TSelectionMode.None;
        SelectWord;
      end
      else
      {$IF not Defined(IOS) and not Defined(ANDROID)}
        BeginSelection;
      {$ENDIF}
    end
    else
      UpdateCaretPosition;
  finally
    Model.EnableNotify;
  end;
end;

procedure TCustomEditView.MouseMove(Shift: TShiftState; X, Y: Single);

  function DefineNewCarretPosition(const AX: Single): Integer;
  begin
    Result := GetCaretPosition(AX);
    if AX > ContentRect.Right then
      Inc(Result);
  end;

var
  OldCaretPosition: Integer;
begin
  inherited;
  { Changing cursor position }
  if SelectionMode = TSelectionMode.CursorPosChanging then
  begin
    if FLoupeService <> nil then
    begin
      FLoupeService.SetLoupeMode(TLoupeMode.Circle);
      SetLoupePosition(X, Y);
      ShowLoupe;
    end;
    CaretPosition := DefineNewCarretPosition(X);
  end;
  { Changing selection bounds }
  if SelectionMode = TSelectionMode.TextSelection then
  begin
    OldCaretPosition := GetOriginCaretPosition;
    Model.DisableNotify;
    try
      {$IFNDEF ANDROID}
      CaretPosition := DefineNewCarretPosition(X);
      Model.SelStart := GetOriginCaretPosition;
      {$ELSE}
      if Model.SelLength = 0 then
        Model.SelStart := OldCaretPosition;
      Model.SelStart := DefineNewCarretPosition(X);
      {$ENDIF}
      Model.SelLength := Model.SelLength - (Model.SelStart - OldCaretPosition);
    finally
      Model.EnableNotify;
    end;
  end;
end;

procedure TCustomEditView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  HideLoupe;
  if SelectionMode = TSelectionMode.CursorPosChanging then
    FTextService.EndSelection;
  SelectionMode := TSelectionMode.None;
  UpdateSelectionPointPositions;
end;

function TCustomEditView.ParentFrame: TFrame;
var
  P: TFmxObject;
begin
  Result := nil;
  P := Self;
  while P <> nil do begin
    if P is TFrame then begin
      Result := P as TFrame;
      Break;
    end else
      P := P.Parent;
  end;
end;

procedure TCustomEditView.PasteFromClipboard;
var
  OldText, Value: string;
  LCaretPosition: Integer;
begin
  if ReadOnly or not InputSupport then
    Exit;
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if not TLinkObservers.EditLinkEdit(Observers) then
    begin
      TLinkObservers.EditLinkReset(Observers);
      Exit;
    end
    else
      TLinkObservers.EditLinkModified(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueModified(Observers);

  if (FClipboardSvc <> nil) and not FClipboardSvc.GetClipboard.IsEmpty then
  begin
    OldText := Model.Text;
    OldText := OldText.Remove(SelStart, SelLength);

    Value := FClipboardSvc.GetClipboard.ToString.Split([sLineBreak], 1, TStringSplitOptions.None)[0];
    OldText := OldText.Insert(SelStart, Value);
    if MaxLength > 0 then
      OldText := OldText.Substring(0, MaxLength);
    LCaretPosition := SelStart + Value.Length;
    Model.Text := OldText;
    Model.CaretPosition := LCaretPosition;
    Model.SelStart := LCaretPosition;
    Model.SelLength := 0;
  end;
end;

procedure TCustomEditView.PlayClickEffect;
begin
end;

{$IF CompilerVersion >= 34}
procedure TCustomEditView.PMDoBeforeExit(var AMessage: TDispatchMessage);
begin
  if FScene <> nil then
  begin
    Model.Change;
    if Observers.IsObserving(TObserverMapping.EditLinkID) then
      TLinkObservers.EditLinkUpdate(Observers);
    if Observers.IsObserving(TObserverMapping.ControlValueID) then
      TLinkObservers.ControlValueUpdate(Observers);
    inherited;
    UpdateSelectionPointPositions;
  end
  else
    inherited;
end;
{$ENDIF}

procedure TCustomEditView.PMGetTextContentRect(
  var Message: TDispatchMessageWithValue<TRectF>);
begin
  Message.Value := ContentRect;
end;

procedure TCustomEditView.PMInit(var Message: TDispatchMessage);
begin
  if FTextService <> nil then
  begin
    FTextService.MaxLength := Model.MaxLength;
    FTextLayout.Text := Model.Text;
    {$IF (not Defined(ANDROID)) or (CompilerVersion > 32)}
    FTextService.Text := Model.Text;
    {$ELSE}
    FTextService.Text := PackText(Model.Text);
    {$ENDIF}
    if FTextService.CaretPosition.X > Model.Text.Length then
      SetCaretPosition(Text.Length);
  end;
  RepaintEdit;
  UpdateTextLayout;
end;

procedure TCustomEditView.RealignContent;
var
  Size: TSizeF;
  Pos: TPointF;
  OldDisableAlign: Boolean;
begin
  OldDisableAlign := FDisableAlign;
  try
    FDisableAlign := True;

    if FTextHeight <= 0 then
      UpdateTextHeight;

    Pos.X := 0;
    Pos.Y := 0;
    Size.cx := Max(Width - Pos.X, 0);
    Size.cy := Max(Height - Pos.Y, 0);

    // 计算行高
    FLineHeight := Max(Min(Size.cy, FTextHeight + Max(1, Round(FTextHeight / 10))), 0);

    // 计算出内容区矩形
    FContentRect := RectF(Pos.X + Padding.Left, Pos.Y + Padding.Top,
      Size.cx - Padding.Right, Size.cy - Padding.Bottom);
    if Assigned(FDrawable) and (not FDrawable.IsEmpty) then
      FDrawable.AdjustDraw(Canvas, FContentRect, False, DrawState);

    // 计算出文本顶部位置
    case FText.VertAlign of
      TTextAlign.Center:
        FLineTop := FContentRect.Top + Max((FContentRect.Height - FLineHeight) / 2, 0);
      TTextAlign.Leading:
        FLineTop := FContentRect.Top;
      TTextAlign.Trailing:
        FLineTop := Max(FContentRect.Height - FLineHeight, 0) + FContentRect.Top;
    end;

    FTextLayout.TopLeft := ContentRect.TopLeft;
    UpdateLayoutSize;
  finally
    FDisableAlign := OldDisableAlign;
  end;
end;

procedure TCustomEditView.RepaintEdit;
begin
  Repaint;
end;

procedure TCustomEditView.Replace(const AStartPos, ALength: Integer; const AStr: string);
begin
end;

procedure TCustomEditView.ResetSelection;
var
  LCaretPosition: Integer;
begin
  LCaretPosition := GetSelStart + GetSelLength;
  SelLength := 0;
  CaretPosition := LCaretPosition;
end;

procedure TCustomEditView.Resize;
begin
  inherited Resize;
  RealignContent;
  UpdateSpelling;
end;

procedure TCustomEditView.SelectAll;
var
  CaretPos: Integer;
begin
  if InputSupport then
  begin
    CaretPos := CaretPosition;
    try
      SelStart := 0;
      SelLength := Text.Length;
    finally
      CaretPosition := CaretPos;
    end;
  end;
end;

procedure TCustomEditView.SelectWord;
var
  WordBeginIndex, WordEndIndex: Integer;
  CaretPos: Integer;
begin
  if Text.Length = 0 then
    Exit;

  CaretPos := GetOriginCaretPosition;
  if FindWordBound(Text, CaretPos, WordBeginIndex, WordEndIndex) and InRange(CaretPos, WordBeginIndex, WordEndIndex + 1) then
  begin
    Model.SelStart := WordBeginIndex;
    Model.SelLength := Max(WordEndIndex - Model.SelStart + 1, 0);
  end
  else
    Model.SelLength := 0;
end;

procedure TCustomEditView.SetCaret(const Value: TCaret);
begin
  Model.Caret := Value;
end;

procedure TCustomEditView.SetCaretPosition(const Value: Integer);
var
  P: TPoint;
begin
  if FTextService <> nil then
  begin
    P.X := 0; P.Y := 0;
    if Value < 0 then
      P.X := 0
    else
      if Value > Text.Length then
        P.X := Text.Length
      else
        P.X := Value;
    FTextService.CaretPosition := P;

    UpdateFirstVisibleChar;

    Model.DisableNotify;
    try
      Model.CaretPosition := P.X;
      if Model.SelLength <= 0 then
        Model.SelStart := Value;
    finally
      Model.EnableNotify;
    end;

    UpdateSelectionPointPositions;
    RepaintEdit;

    UpdateCaretPosition;
  end;
end;

procedure TCustomEditView.SetCheckSpelling(const Value: Boolean);
begin
  Model.CheckSpelling := Value;
end;

procedure TCustomEditView.SetFilterChar(const Value: string);
begin
  Model.FilterChar := Value;
end;

procedure TCustomEditView.SetImeMode(const Value: TImeMode);
begin
  Model.ImeMode := Value;
end;

procedure TCustomEditView.SetInputSupport(const Value: Boolean);
begin
  Model.InputSupport := Value;
end;

procedure TCustomEditView.SetKeyboardType(Value: TVirtualkeyboardType);
begin
  Model.KeyboardType := Value;
end;

procedure TCustomEditView.SetKillFocusByReturn(const Value: Boolean);
begin
  Model.KillFocusByReturn := Value;
end;

procedure TCustomEditView.SetLoupePosition(const X, Y: Single);
var
  LoupePos: TPointF;
  ZoomPos: TPointF;
begin
  if FLoupeService <> nil then
  begin
    LoupePos := TPointF.Create(X - FLoupeService.GetWidth / 2, Y - FLoupeService.GetHeight);
    LoupePos := LocalToAbsolute(LoupePos);
    ZoomPos := LocalToAbsolute(TPointF.Create(X, Y));
    FLoupeService.SetPosition(LoupePos);
    FLoupeService.SetZoomRegionCenter(ZoomPos);
    ShowLoupe;
  end;
end;

procedure TCustomEditView.SetMaxLength(const Value: Integer);
begin
  Model.MaxLength := Value;
end;

procedure TCustomEditView.SetOnChange(const Value: TNotifyEvent);
begin
  FOnModelChange := Value;
end;

procedure TCustomEditView.SetOnChangeTracking(const Value: TNotifyEvent);
begin
  Model.OnChangeTracking := Value;
end;

procedure TCustomEditView.SetOnTyping(const Value: TNotifyEvent);
begin
  Model.OnTyping := Value;
end;

procedure TCustomEditView.SetOnValidate(const Value: TValidateTextEvent);
begin
  Model.OnValidate := Value;
end;

procedure TCustomEditView.SetOnValidating(const Value: TValidateTextEvent);
begin
  Model.OnValidating := Value;
end;

procedure TCustomEditView.SetPassword(const Value: Boolean);
begin
  Model.Password := Value;
end;

procedure TCustomEditView.SetReadOnly(const Value: Boolean);
begin
  Model.ReadOnly := Value;
end;

procedure TCustomEditView.SetReturnKeyType(Value: TReturnKeyType);
begin
  Model.ReturnKeyType := Value;
end;

procedure TCustomEditView.SetLoupePosition(
  const ASelectionPointType: TSelectionPointType);
var
  SelectionRect: TRectF;
  ZoomCenter: TPointF;
  LoupePos: TPointF;
begin
  SelectionRect := GetSelRect;
  if FLoupeService <> nil then
  begin
    case ASelectionPointType of
      TSelectionPointType.Left:
      begin
        ZoomCenter := TPointF.Create(SelectionRect.Left, SelectionRect.Top + SelectionRect.Height / 2);
        LoupePos := SelectionRect.TopLeft + TPointF.Create(-FLoupeService.GetWidth / 2, -FLoupeService.GetHeight) + TPointF.Create(0, -LOUPE_OFFSET);
      end;
      TSelectionPointType.Right:
      begin
        ZoomCenter := TPointF.Create(SelectionRect.Right, SelectionRect.Top + SelectionRect.Height / 2);
        LoupePos := TPointF.Create(SelectionRect.Right, SelectionRect.Top) + TPointF.Create(-FLoupeService.GetWidth / 2, -FLoupeService.GetHeight) + TPointF.Create(0, -LOUPE_OFFSET);
      end;
    end;
    ZoomCenter := LocalToAbsolute(ZoomCenter);
    LoupePos := LocalToAbsolute(LoupePos);

    FLoupeService.SetPosition(LoupePos);
    FLoupeService.SetZoomRegionCenter(ZoomCenter);
  end;
end;

procedure TCustomEditView.SetSelectionFill(const Value: TBrush);
begin
  FSelectionFill.Assign(Value);
end;

procedure TCustomEditView.SetSelectionMode(const Value: TSelectionMode);
begin
  FSelectionMode := Value;
  if FLoupeService <> nil then
    case Value of
      TSelectionMode.None: ;
      TSelectionMode.TextSelection:
        FLoupeService.SetLoupeMode(TLoupeMode.Rectangle);
      TSelectionMode.CursorPosChanging:
        FLoupeService.SetLoupeMode(TLoupeMode.Circle);
    end;
end;

procedure TCustomEditView.SetSelLength(const Value: Integer);
begin
  Model.SelLength := Value;
end;

procedure TCustomEditView.SetSelStart(const Value: Integer);
begin
  Model.SelLength := 0;
  Model.SelStart := Value;
  Model.CaretPosition := Value;
end;

procedure TCustomEditView.SetText(const Value: string);
begin
  if FTextService.CombinedText <> Value then
  begin
    SetTextInternal(Value);
    SetCaretPosition(Min(Value.Length, FTextService.CaretPosition.X));
    Model.DisableNotify;
    try
      Model.SelStart := 0;
      Model.SelLength := 0;
    finally
      Model.EnableNotify;
    end;
    Model.Change;
    RepaintEdit;
  end;
end;

procedure TCustomEditView.SetTextInternal(const Value: string);
begin
{$IF CompilerVersion > 32}
  {$IFDEF ANDROID}
  FTextService.Text := Value;
  Model.Text := Value;
  {$ELSE}
  Model.Text := Value;
  FTextService.Text := Model.Text;
  {$ENDIF}
  FTextLayout.Text := Model.Text;
{$ELSE}
  Model.Text := Value;
  FTextLayout.Text := Model.Text;
  {$IFNDEF ANDROID}
  FTextService.Text := Model.Text;
  {$ELSE}
  FTextService.Text := PackText(Model.Text);
  {$ENDIF}
{$ENDIF}
  UpdateCaretPosition;
end;

procedure TCustomEditView.ShowCaret;
begin
  Model.Caret.Show;
end;

function TCustomEditView.ShowContextMenu(
  const ScreenPosition: TPointF): Boolean;

  function ShowDefaultMenu: Boolean;
  begin
    Result := False;
    if EditPopupMenu <> nil then
      try
        if Root <> nil then
          EditPopupMenu.Parent := Root.GetObject;
        Result := True;
        UpdatePopupMenuItems;
        if Model.CheckSpelling and (FSpellService <> nil) and (System.Length(FSpellingRegions) > 0) then
          UpdateSpellPopupMenu(ScreenToLocal(ScreenPosition));
        EditPopupMenu.PopupComponent := Self;
        EditPopupMenu.Popup(Round(ScreenPosition.X), Round(ScreenPosition.Y));
      finally
        EditPopupMenu.Parent := nil;
      end;
  end;

  function ShowUsersPopupMenu: Boolean;
  begin
    Result := ShowContextMenu(ScreenPosition)
  end;

begin
  Result := False;
  if not (csDesigning in ComponentState) then
    if (PopupMenu <> nil) then
      Result := ShowUsersPopupMenu
    else
      Result := ShowDefaultMenu;
end;

procedure TCustomEditView.ShowLoupe;
begin
  if FLoupeService <> nil then
  begin
    FLoupeService.SetLoupeScale(TCustomMagnifierGlass.DefaultLoupeScale);
    FLoupeService.ShowFor(Self);
  end;
end;

procedure TCustomEditView.SpellFixContextMenuHandler(Sender: TObject);
var
  LPos: Integer;
  BP, EP: Integer;
begin
  if Sender is TMenuItem then
  begin
    LPos := TMenuItem(Sender).Tag;
    if (LPos > -1) and FMX.Text.FindWordBound(Text, LPos, BP, EP) then
      Text := Text.Substring(0, BP) + TMenuItem(Sender).Text + Text.Substring(EP + 1);
  end;
end;

procedure TCustomEditView.StartIMEInput;
begin
  FTextService.CaretPosition := Point(GetOriginCaretPosition, 0);
end;

function TCustomEditView.TextWidth(const AStart, ALength: Integer): Single;
var
  Rgn: TRegion;
  S, L, I: Integer;
begin
  if Model.Password then
    Result := GetPasswordCharWidth * ALength
  else begin
    if AStart < FTextLayout.Text.Length then begin
      S := AStart;
      L := ALength;
      if FTextLayout.Text.Chars[S].IsLowSurrogate then begin
        Inc(S);
        Dec(L);
      end;
      Rgn := FTextLayout.RegionForRange(TTextRange.Create(S, L));
      Result := 0;
      for I := Low(Rgn) to High(Rgn) do
        Result := Result + Rgn[I].Width;
    end else
      if AStart = FTextLayout.Text.Length then
        Result := FTextLayout.TextWidth
      else
        Result := 0;
  end;
end;

procedure TCustomEditView.UpdateCaretPosition;
var
  CaretHeight: Single;
  Pos: TPointF;
begin
  if IsFocused then begin
    CaretHeight := Trunc(LineHeight);
    Pos.Y := Trunc(LineTop);
//    {$IFNDEF IOS}
//    CaretHeight := Trunc(LineHeight);
//    Pos.Y := Trunc(LineTop);
//    {$ELSE}
//    CaretHeight := Trunc(ContentRect.Height);
//    Pos.Y := Trunc(ContentRect.Top);
//    {$ENDIF}
    if FTextService.HasMarkedText then
      Pos.X := GetCharX(FTextService.TargetClausePosition.X)
    else
      Pos.X := GetCharX(FTextService.CaretPosition.X);
    Pos.X := Max(0, Min(Pos.X, ContentRect.Right - Model.Caret.Size.cx + 1));
    Model.Caret.BeginUpdate;
    try
      Model.Caret.Pos := Pos;
      Model.Caret.Size := TPointF.Create(Min(Model.Caret.Size.cx, ContentRect.Width), CaretHeight);
    finally
      Model.Caret.EndUpdate;
    end;
  end;
end;

procedure TCustomEditView.UpdateFirstVisibleChar;
var
  MarkedPosition: Integer;
  LEditRect: TRectF;
  TempStr: string;
begin
  FTextLayout.Text := FTextService.CombinedText;
  MarkedPosition := FTextService.TargetClausePosition.X;
  if FFirstVisibleChar >= (MarkedPosition + 1) then
  begin
    FFirstVisibleChar := MarkedPosition;
    if FFirstVisibleChar < 1 then
      FFirstVisibleChar := 1;
  end
  else
  begin
    LEditRect := ContentRect;
    if FTextLayout.TextWidth > LEditRect.Width then
    begin
      //Text is longer than content width
      TempStr := FTextService.CombinedText;
      if MarkedPosition < (FFirstVisibleChar - 1) then
        //New position is lefter than left visual character
        FFirstVisibleChar := MarkedPosition
      else
        //Looking for the shift when caret position will be visible
        while (TextWidth(FFirstVisibleChar - 1, MarkedPosition - FFirstVisibleChar + 1) > LEditRect.Width)
          and (FFirstVisibleChar < TempStr.Length) do
          Inc(FFirstVisibleChar);
    end
    else
      //Text fits content
      FFirstVisibleChar := 1;
  end;
  if (FFirstVisibleChar > 0) and (FTextLayout.Text.Length > 0) then
  begin
    if FTextLayout.Text.Chars[FFirstVisibleChar - 1].IsLowSurrogate then
      Inc(FFirstVisibleChar);
    FInvisibleTextWidth := TextWidth(0, FFirstVisibleChar - 1);
  end;
end;

procedure TCustomEditView.UpdateLayoutSize;
var
  LSize: TPointF;
begin
  LSize := TTextLayout.MaxLayoutSize;
  if FText.HorzAlign <> TTextAlign.Leading then
    LSize.X := ContentRect.Width;
  if FText.VertAlign <> TTextAlign.Leading then
    LSize.Y := ContentRect.Height;
  // fixed by 凌风
  if LSize.X < 0 then
    LSize.X := 0;
  if LSize.Y < 0 then
    LSize.Y := 0;
  // fix end
  FTextLayout.MaxSize := LSize;
end;

procedure TCustomEditView.UpdatePopupMenuItems;
var
  SelTextIsValid: Boolean;

  procedure SetParam(AParamName : string; AValue : Boolean) ;
  var
    LMenuItem : TMenuItem;
  begin
    LMenuITem := FindContextMenuItem(AParamName);
    if LMenuItem <> nil then
      LMenuItem.Enabled := AValue;
  end;

begin
  SelTextIsValid := not SelText.IsEmpty;
  SetParam(CutStyleName, SelTextIsValid and not Model.ReadOnly and Model.InputSupport and not Model.Password);
  SetParam(CopyStyleName, SelTextIsValid and not Model.Password);
  if FClipboardSvc <> nil then
    SetParam(PasteStyleName, (not FClipBoardSvc.GetClipboard.IsEmpty) and (not Model.ReadOnly) and Model.InputSupport)
  else
    SetParam(PasteStyleName, False);
  SetParam(DeleteStyleName, SelTextIsValid and not Model.ReadOnly and Model.InputSupport);
  SetParam(SelectAllStyleName, SelText <> Text);
end;

procedure TCustomEditView.UpdateSelectionPointPositions;
var
  R: TRectF;
  IsParentFocused: Boolean;
begin
  IsParentFocused := (ParentControl <> nil) and ParentControl.IsFocused;
  Model.Caret.TemporarilyHidden := Model.HasSelection and IsParentFocused;
  if HaveSelectionPickers then
  begin
    FLeftSelPt.Visible := (Model.SelLength > 0) and IsParentFocused and (Model.SelStart + 1 >= FFirstVisibleChar);
    FRightSelPt.Visible := (Model.SelLength > 0) and IsParentFocused and (GetCharX(Model.SelStart + Model.SelLength) < ContentRect.Right);

    R := GetSelRect;

    FLeftSelPt.Position.X := R.Left;
    {$IF Defined(ANDROID) and (CompilerVersion > 32)}
      FLeftSelPt.Position.Y := R.Bottom + 2 * FLeftSelPt.GripSize;
    {$ELSE}
      FLeftSelPt.Position.Y := R.Top - 2 * FLeftSelPt.GripSize;
    {$ENDIF}

    FRightSelPt.Position.X := R.Right;
    FRightSelPt.Position.Y := R.Bottom + 2 * FLeftSelPt.GripSize;
  end;
end;

procedure TCustomEditView.UpdateSpelling;
begin
  if Model.CheckSpelling then begin
    FUpdateSpelling := True;
    SetLength(FSpellingRegions, 0);
  end;
end;

procedure TCustomEditView.UpdateSpellPopupMenu(const APoint: TPointF);
var
  I, J, BP, EP: Integer;
  LPos: Integer;
  Spells: TArray<string>;
  LMenuItem: TMenuItem;
begin
  for I := 0 to FSpellMenuItems.Count - 1 do
    FSpellMenuItems[I].Parent := nil;
  FSpellMenuItems.Clear;

  for I := Low(FSpellingRegions) to High(FSpellingRegions) do
    if FSpellingRegions[I].Contains(APoint) then
    begin
      LPos := FTextLayout.PositionAtPoint(APoint);
      if (LPos > -1) and FMX.Text.FindWordBound(Text, LPos, BP, EP) then
      begin
        Spells := FSpellService.CheckSpelling(Text.Substring(BP, EP - BP + 1));
        if System.Length(Spells) > 0 then
        begin
          for J := Low(Spells) to High(Spells) do
          begin
            LMenuItem := TMenuItem.Create(EditPopupMenu);
            LMenuItem.Text := Spells[J];
            LMenuItem.Font.Style := LMenuItem.Font.Style + [TFontStyle.fsBold];
            LMenuItem.Tag := LPos;
            LMenuItem.OnClick := SpellFixContextMenuHandler;
            EditPopupMenu.InsertObject(FSpellMenuItems.Count, LMenuItem);
            FSpellMenuItems.Add(LMenuItem);
          end;
          LMenuItem := TMenuItem.Create(EditPopupMenu);
          LMenuItem.Text := SMenuSeparator;
          EditPopupMenu.InsertObject(FSpellMenuItems.Count, LMenuItem);
          FSpellMenuItems.Add(LMenuItem);
        end;
      end;
      Break;
    end;
end;

procedure TCustomEditView.UpdateTextHeight;
begin
  FTextHeight := 0;
  if Assigned(FText) then begin
    TCanvasManager.MeasureCanvas.Font.Assign(FText.Font);
    FTextHeight := TCanvasManager.MeasureCanvas.TextHeight('Lb|y'); // do not localize
  end;
end;

procedure TCustomEditView.UpdateTextLayout;
begin
  FTextLayout.BeginUpdate;
  try
    FTextLayout.HorizontalAlign := FText.HorzAlign;
    FTextLayout.VerticalAlign := FText.VertAlign;
    FTextLayout.Font := FText.Font;
    FTextLayout.TopLeft := ContentRect.TopLeft;
    UpdateLayoutSize;
  finally
    FTextLayout.EndUpdate;
  end;
  UpdateTextHeight;
end;

{ TEditView }

constructor TEditView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Gravity := TLayoutGravity.CenterVertical;
  Cursor := crIBeam;
  CanFocus := True;
  DragMode := TDragMode.dmManual;
  HitTest := True;
end;

destructor TEditView.Destroy;
begin
  inherited Destroy;
end;

initialization
  {$IFDEF ANDROID}
  // 解决 Android 下键盘事件不响应的问题
  //RegisterKeyMapping(23, 23, TKeyKind.Functional);
  {$ENDIF}

end.
