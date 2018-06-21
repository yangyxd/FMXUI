{*******************************************************}
{                                                       }
{       FMX UI 组件注册单元                             }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Reg;

interface

const
  PageName = 'FMX UI';

procedure Register;

implementation

uses
  UI.Debug,
  System.SysUtils, System.Actions,
  UI.Base, UI.Standard, UI.Edit, UI.Dialog, UI.Grid, UI.Calendar,
  UI.ListView,
  // UI.ListViewEx,
  UI.Toast,

  UI.Design.Bounds,
  UI.Design.GridColumns,
  UI.Design.Accessory,

  UI.Utils.SVGImage,
  UI.Design.SVGImage,

  UI.Frame,
  {$IFDEF MSWINDOWS}
  Windows, Registry,
  {$ENDIF}
  ComponentDesigner, DesignIntf, DesignEditors,
  DesignerTypes, PropertyCategories, VCLEditors,
  System.Classes, System.Types, System.TypInfo, System.UITypes,
  System.Generics.Collections, System.RTLConsts,

  ActionEditors, FMX.ActnList, FMX.ImgList,
  Vcl.ComCtrls, Vcl.Graphics, FMX.Graphics,

  FMX.Ani, FMX.Types, FMX.Styles, FMX.Controls, FMX.StdCtrls, FMX.Edit;

resourcestring
  sFMXUICategoryName = 'FMXUI';
  sTouchCategoryName = 'Touch';

type
  TViewControlEditor = class(TDefaultEditor)
  private
    FCmdIndex: TArray<Integer>;
  protected
    procedure DesignerModified;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TPatchBoundsProperty = class(TClassProperty)
  private
  protected
    procedure Edit; override;
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TViewAccessoryProperty = class(TClassProperty)
  private
  protected
    procedure Edit; override;
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TGridColumnsSettingsProperty = class(TClassProperty)
  private
  protected
    procedure Edit; override;
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TSVGImageProperty = class(TClassProperty)
  private
  protected
    procedure Edit; override;
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

//  TImageIndexExProperty = class(TIntegerProperty, ICustomPropertyDrawing,
//    ICustomPropertyListDrawing, ICustomPropertyDrawing80)
//  private
//    LI: IGlyph;
//  public
//    function GetAttributes: TPropertyAttributes; override;
//    procedure GetProperties(Proc: TGetPropProc); override;
//    function GetValue: string; override;
//    procedure GetValues(Proc: TGetStrProc); override;
//    procedure SetValue(const Value: string); override;
//    { ICustomPropertyListDrawing }
//    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
//      var AHeight: Integer);
//    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
//      var AWidth: Integer);
//    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
//      const ARect: TRect; ASelected: Boolean);
//    { ICustomPropertyDrawing }
//    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
//      ASelected: Boolean);
//    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
//      ASelected: Boolean);
//    { ICustomPropertyDrawing80 }
//    function PropDrawNameRect(const ARect: TRect): TRect;
//    function PropDrawValueRect(const ARect: TRect): TRect;
//    { IProperty160 }
//    procedure SetPropertyPath(const Value: string);
//  end;

  TLayoutComponentProperty = class(TComponentProperty)
  protected
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{$IFDEF MSWINDOWS}
var
  [Weak] FCopyBackground: TObject = nil;
  [Weak] FCopyDrawable: TObject = nil;
{$ENDIF}

{$IFDEF MSWINDOWS}
// 设置环境变量
procedure SetEnvPath(const sName, sValue: string);
var
  reg : TRegistry;
  sLMKey : string;
begin
  sLMKey := 'System/CurrentControlSet/Control/Session Manager/Environment';
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey(sLMKey,False) then begin
      reg.WriteString(sName, sValue);
      reg.CloseKey;
      SetEnvironmentVariable(PChar(sName), PChar(sValue));//更新当前进程的环境变量
    end;
  except
  end;
  reg.Free;
end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents(PageName, [TView, TViewGroup, TLinearLayout, TRelativeLayout, TGridsLayout]);

  RegisterComponents(PageName, [TImageView]);
  RegisterComponents(PageName, [TImageViewerEx]);
  RegisterComponents(PageName, [TTextView]);
  RegisterComponents(PageName, [TEditView]);
  RegisterComponents(PageName, [TButtonView]);
  RegisterComponents(PageName, [TBadgeView]);
  RegisterComponents(PageName, [TProgressView]);
  RegisterComponents(PageName, [TRingView]);
  RegisterComponents(PageName, [TMultiPathView]);
  RegisterComponents(PageName, [TCameraViewer]);

  RegisterComponents(PageName, [THorzScrollView]);
  RegisterComponents(PageName, [TVertScrollView]);
  RegisterComponents(PageName, [TListViewEx]);

  RegisterComponents(PageName, [TGridView]);
  RegisterComponents(PageName, [TDBGridView]);
  RegisterComponents(PageName, [TStringGridView]);

  RegisterComponents(PageName, [TCalendarView]);
  RegisterComponents(PageName, [TDateView]);
  RegisterComponents(PageName, [TTimeView]);
  RegisterComponents(PageName, [TCalendarLanguage_CN]);

  RegisterComponents(PageName, [TDialogStyleManager]);
  RegisterComponents(PageName, [TToastManager]);

  RegisterComponents(PageName, [TDrawableBrush]);

  RegisterComponentEditor(TView, TViewControlEditor);

  RegisterPropertyEditor(TypeInfo(TPatchBounds), TPersistent, '', TPatchBoundsProperty);
  RegisterPropertyEditor(TypeInfo(TGridColumnsSetting), TGridBase, '', TGridColumnsSettingsProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TViewLayout, '', TLayoutComponentProperty);
  RegisterPropertyEditor(TypeInfo(TViewAccessory), TPersistent, '', TViewAccessoryProperty);

  RegisterPropertyEditor(TypeInfo(TSVGImage), TPersistent, '', TSVGImageProperty);

  //RegisterPropertyEditor(TypeInfo(TCustomImageList), TPersistent, '', TShareImageListProperty);

  //RegisterPropertyEditor(TypeInfo(TImageIndex), TViewImagesBrush, '', TAlphaColorProperty);
  //RegisterSelectionEditor(TView, TLayoutFilter);
  //RegisterComponentEditor(TCustomButton, TViewControlEditor);
  //RegisterComponentEditor(TCustomEdit, TViewControlEditor);
  //RegisterPropertyEditor(TypeInfo(WideString), TADOTable, 'TableName', TTableNameProperty);
  //RegisterPropertyEditor(TypeInfo(TBrushKind), TStrokeBrush, '', TBrushKindProperty);
  //RegisterComponentEditor(TCustomBindingsList, TBindCompListEditor);
  //RegisterSelectionEditor(TBaseLinkingBindSource, TBindCompFactorySelectionEditor);

  RegisterPropertiesInCategory(sFMXUICategoryName, [
      { TView }
      'AdjustViewBounds',
      'Brush',
      'Background',
      'Clickable',
      'Checked',
      'Enabled',
      'Layout',
      'Padding',
      'Paddings',
      'Margin',
      'Margins',
      'InVisible',
      'WidthSize',
      'HeightSize',
      'MinWidth',
      'MinHeight',
      'MaxWidth',
      'MaxHeight',
      'Gravity',
      'Weight',
      'CaptureDragForm',
      'Orientation',
      'OnClick',
      'OnPaint',
      'OnResize',
      { TScrollView }
      'ShowScrollBars',
      'ScrollBars',
      'DragScroll',
      'DragOneWay',
      'ScrollSmallChangeFraction',
      'ScrollStretchGlowColor',
      'ScrollbarWidth',
      { TGridsLayout }
      'ColumnCount',
      'ColumnWidth',
      'ColumnHeight',
      'Divider',
      'SpacingHorizontal',
      'SpacingVertical',
      'SpacingBorder',
      'StretchMode',
      'ForceColumnSize',
      { TImageView }
      'Image',
      'ScaleType',
      'Stretch',
      'Zoom',
      'OnZoom',
      { TListViewEx}
      'AllowItemClickEx',
      'DividerHeight',
      'EnablePullRefresh',
      'EnablePullLoad',
      'OnPullRefresh',
      'OnPullLoad',
      'OnInitFooter',
      'OnInitHeader',
      'OnItemClick',
      'OnItemClickEx',
      'OnScrollChange',
      'OnItemMeasureHeight',
      { TEditView}
      'KeyboardType',
      'ReturnKeyType',
      'Password',
      'ReadOnly',
      'MaxLength',
      'FilterChar',
      'ImeMode',
      'Caret',
      'KillFocusByReturn',
      'CheckSpelling',
      'SelectionFill',
      'OnValidating',
      'OnTyping',
      { TTextView }
      'HtmlText',
      'Drawable',
      'GroupIndex',
      'OnDrawBackgroud',
      { TProgressView }
      'Min',
      'Max',
      'Value',
      'ForeGround',
      'StartAngle',
      'Kind',
      'SolidForeGround',
      'PaddingBorder',
      'OnValueChange',
      { TRingView }
      'StyleOuter',
      'StyleInner',
      'Distance',
      'AngleStart',
      'AngleEnd',
      'ClickInPath',
      { TBadgeView }
      'AutoSize',
      'TargetView',
      'BadgeCount',
      'TextColor',
      'Style',
      'MaxValue',
      'Icon',
      'ValueOutTail',
      { TMultiPathView }
      'Paths',
      'ActiveIndex',
      { GridView }
      'RowHeight',
      'DrawableCells',
      'Options',
      'FixedSettings',
      'FooterStyle',
      'MinRowCount',
      'DataSource',
      'ShowCheck',
      'ColCount',
      'RowCount',
      'ShowColIndex',
      'ColumnsSettings',

      { CalendarView }
      'DateTime',
      'DaysOfWeekDisabled',
      'DaysOfWeekHighlighted',
      'StartDate',
      'EndDate',
      'Language',
      'RowHeight',
      'RowLunarHeight',
      'RowLunarPadding',
      'RowPadding',
      'TextSettingsOfLunar',
      'TextSettingsOfTitle',
      'TextSettingsOfWeeks',
      'ViewModeMax',
      'ViewModeMin',
      'ViewModeStart',
      'WeekStart',
      'WeeksWidth',
      'OnOwnerDrawCalendar',
      'OnOwnerLunarData',
      'OnClickView',

      'OnTitleClick',
      'OnColumnMoved',
      'OnFixedCellClick',
      'OnCellClick',
      'OnCellEnter',
      'OnCellLeave',
      'OnCellCheck',
      'OnCellEditDone',
      'OnItemIndexChange',
      'OnRowSelChange',
      'OnDrawFixedColText',
      'OnDrawFixedCellsText',
      'OnDrawCells',
      'OnDrawFooterCells',
      { Text }
      'Text',
      'TextHint',
      'TextSettings',
      'OnDrawText',
      'OnTextChange'
    ]);

  RegisterPropertiesInCategory(sTouchCategoryName, [
      'Touch', 'TouchTargetExpansion', 'OnGesture'
    ]);

  RegisterPropertiesInCategory(sLayoutCategoryName, [
      'Layout',
      'Padding',
      'Paddings',
      'Margin',
      'Margins',
      'WidthSize',
      'HeightSize',
      'MinWidth',
      'MinHeight',
      'MaxWidth',
      'MaxHeight',
      'Gravity',
      'Weight',
      'Orientation',
      'ColumnCount',
      'ColumnWidth',
      'ColumnHeight',
      'Divider',
      'SpacingHorizontal',
      'SpacingVertical',
      'SpacingBorder',
      'StretchMode',
      'ForceColumnSize'
    ]);
end;

procedure RegisterAliases;
begin
  AddEnumElementAliases(TypeInfo(TLayoutGravity),
    ['None', 'LeftTop', 'LeftBottom', 'RightTop', 'RightBottom',
    'CenterVertical', 'CenterHorizontal', 'CenterHBottom', 'CenterVRight', 'Center']);
  AddEnumElementAliases(TypeInfo(TViewSize),
    ['CustomSize', 'WrapContent', 'FillParent']);
  AddEnumElementAliases(TypeInfo(TDrawablePosition),
    ['Left', 'Right', 'Top', 'Bottom', 'Center']);
  AddEnumElementAliases(TypeInfo(TDrawableKind),
    ['None', 'Circle', 'Ellipse']);
  AddEnumElementAliases(TypeInfo(TViewBorderStyle),
    ['None', 'RectBorder', 'RectBitmap', 'CircleBorder', 'EllipseBorder', 'LineEdit', 'LineTop', 'LineBottom', 'LineLeft', 'LineRight']);
  AddEnumElementAliases(TypeInfo(TViewBrushKind),
    ['None', 'Solid', 'Gradient', 'Bitmap', 'Resource', 'Patch9Bitmap', 'AccessoryBitmap', 'SVGImage']);
  AddEnumElementAliases(TypeInfo(TViewScroll),
    ['None', 'Horizontal', 'Vertical', 'Both']);
  AddEnumElementAliases(TypeInfo(TViewStretchMode),
    ['None', 'SpacingWidth', 'ColumnWidth', 'SpacingWidthUniform']);
  AddEnumElementAliases(TypeInfo(TProgressKind),
    ['Horizontal', 'Vertical', 'CircleRing']);
  AddEnumElementAliases(TypeInfo(TImageScaleType),
    ['None', 'Matrix', 'Center', 'CenterCrop', 'CenterInside', 'FitCenter', 'FitStart', 'FitEnd']);
  AddEnumElementAliases(TypeInfo(TBadgeStyle),
    ['EmptyText', 'NumberText', 'NewText', 'HotText', 'Icon']);
  AddEnumElementAliases(TypeInfo(TRingViewStyle),
    ['Rectangle', 'Circle', 'Ellipse']);
  AddEnumElementAliases(TypeInfo(TGridDataType),
    ['PlanText', 'CheckBox', 'RadioButton', 'Image', 'ProgressBar', 'CustomDraw']);
  AddEnumElementAliases(TypeInfo(TGridFooterStyle),
    ['None', 'DataTotal', 'DataAverage', 'DataMin', 'DataMax']);
  AddEnumElementAliases(TypeInfo(TViewAccessoryStyle),
    ['Accessory', 'Path']);
  AddEnumElementAliases(TypeInfo(TViewAccessoryType),
    ['None', 'More', 'Checkmark', 'Detail', 'Ellipses', 'Flag', 'Back', 'Refresh',
     'Action', 'Play','Rewind', 'Forwards', 'Pause', 'Stop', 'Add', 'Prior',
     'Next', 'BackWard', 'ForwardGo', 'ArrowUp', 'ArrowDown', 'ArrowLeft','ArrowRight', 'Reply',
     'Search', 'Bookmarks', 'Trash', 'Organize', 'Camera', 'Compose', 'Info',
     'Pagecurl', 'Details', 'RadioButton', 'RadioButtonChecked', 'CheckBox',
     'CheckBoxChecked', 'User', 'Password', 'Down', 'Exit', 'Finish', 'Calendar', 'Cross', 'Menu',
     'About', 'Share', 'UserMsg', 'Cart', 'Setting', 'Edit', 'Home', 'Heart',
     'Comment', 'Collection', 'Fabulous', 'Image', 'Help', 'VCode', 'Time', 'UserReg', 'Scan', 'Circle', 'Location',
     'UserDefined1', 'UserDefined2', 'UserDefined3'
    ]);
  AddEnumElementAliases(TypeInfo(TCalendarViewType),
    ['Days', 'Months', 'Years', 'Decades', 'Centuries']);
end;

procedure UnregisterAliases;
begin
  RemoveEnumElementAliases(TypeInfo(TLayoutGravity));
  RemoveEnumElementAliases(TypeInfo(TViewSize));
  RemoveEnumElementAliases(TypeInfo(TDrawablePosition));
  RemoveEnumElementAliases(TypeInfo(TDrawableKind));
  RemoveEnumElementAliases(TypeInfo(TViewBorderStyle));
  RemoveEnumElementAliases(TypeInfo(TViewBrushKind));
  RemoveEnumElementAliases(TypeInfo(TViewScroll));
  RemoveEnumElementAliases(TypeInfo(TViewStretchMode));
  RemoveEnumElementAliases(TypeInfo(TProgressKind));
  RemoveEnumElementAliases(TypeInfo(TImageScaleType));
  RemoveEnumElementAliases(TypeInfo(TBadgeStyle));
  RemoveEnumElementAliases(TypeInfo(TRingViewStyle));
  RemoveEnumElementAliases(TypeInfo(TGridDataType));
  RemoveEnumElementAliases(TypeInfo(TGridFooterStyle));
  RemoveEnumElementAliases(TypeInfo(TViewAccessoryStyle));
  RemoveEnumElementAliases(TypeInfo(TViewAccessoryType));
  RemoveEnumElementAliases(TypeInfo(TCalendarViewType));
end;

{ TViewControlEditor }

type
  TViewPri = class(TView);

procedure TViewControlEditor.DesignerModified;
begin
  if Designer <> nil then
    Designer.Modified;
end;

procedure TViewControlEditor.ExecuteVerb(Index: Integer);
var
  O: TDrawableBase;
begin
  if not (Component is TControl) then Exit;
  case FCmdIndex[Index] of
    0:
      begin
        if TControl(Component).Index > 0 then
          TControl(Component).Index := TControl(Component).Index - 1;
      end;
    1:
      begin
        if TControl(Component).Index < TControl(Component).Parent.ChildrenCount - 1 then
          TControl(Component).Index := TControl(Component).Index + 1;
      end;
    2:
      begin
        TControl(Component).Index := 0;
      end;
    3:
      begin
        TControl(Component).Index := TControl(Component).Parent.ChildrenCount - 1;
      end;
    4:
      begin
        FCopyBackground := TViewPri(Component).FBackground;
      end;
    5:
      begin
        try
          TView(Component).Background := TDrawable(FCopyBackground);
        except
          MessageBox(0, PChar(Exception(ExceptObject).Message), 'Error', 48);
        end;
      end;
    6:
      begin
        if TView.ExistRttiValue(Component, 'Drawable') then
          FCopyDrawable := TView.GetRttiObject(Component, 'Drawable')
        else
          FCopyDrawable := TView.GetRttiObject(Component, 'FDrawable');
      end;
    7:
      begin
        try
          if Component is TTextView then begin
            TTextView(Component).Drawable := TDrawableIcon(FCopyDrawable)
          end else if Component is TCustomEditView then begin
            TCustomEditView(Component).Drawable := TDrawableIcon(FCopyDrawable)
          end else
            TView.InvokeMethod(Component, 'SetDrawable', [TDrawableIcon(FCopyDrawable)]);
        except
          MessageBox(0, PChar(Exception(ExceptObject).Message), 'Error', 48);
        end;
      end;
  end;
  Designer.SelectComponent(Component);
  DesignerModified;
end;

function TViewControlEditor.GetVerb(Index: Integer): string;
const
  CmdNames: TArray<string> = ['前移', '后移', '移至最前', '移至最后',
    'Copy Background', 'Paste Background', 'Copy Drawable', 'Paste Drawable'];
begin
  Result := CmdNames[FCmdIndex[Index]];
end;

function TViewControlEditor.GetVerbCount: Integer;
var
  O: TObject;
begin
  SetLength(FCmdIndex, 20);
  if (Component is TControl) and ((TControl(Component).Parent is TLinearLayout) or
    (TControl(Component).Parent is TGridsLayout)) then
  begin
    Result := 4;
    FCmdIndex[0] := 0;
    FCmdIndex[1] := 1;
    FCmdIndex[2] := 2;
    FCmdIndex[3] := 3;
  end else
    Result := 0;
  if (Component is TView) then begin
    FCmdIndex[Result] := 4;
    Inc(Result);
    if Assigned(FCopyBackground) then begin
      FCmdIndex[Result] := 5;
      Inc(Result);
    end;
    O := TView.GetRttiObject(Component, 'Drawable');
    if not Assigned(O) then
      O := TView.GetRttiObject(Component, 'FDrawable');
    if Assigned(O) and (O is TDrawableBase) then begin
      FCmdIndex[Result] := 6;
      Inc(Result);
      if Assigned(FCopyDrawable) then begin
        FCmdIndex[Result] := 7;
        Inc(Result);
      end;
    end;
  end;
end;

{ TPatchBoundsProperty }

procedure TPatchBoundsProperty.Edit;
var
  Component: TObject;
  Dialog: TBoundsDesigner;
begin
  Component := GetComponent(0);
  if not (Component is TPatch9Bitmap) then
    Exit;
  Dialog := TBoundsDesigner.Create(nil);
  try
    Dialog.Caption := '9宫格绘图编辑器';
    Dialog.Bitmap := TPatch9Bitmap(Component).Bitmap;
    Dialog.Bounds := TPatch9Bitmap(Component).Bounds.Rect;
    if Dialog.ShowModal = mrOK then begin
      TPatch9Bitmap(Component).Bounds.Rect := Dialog.Bounds;
    end;
  finally
    Dialog.Free;
  end;
end;

function TPatchBoundsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly, paDialog];
end;

{ TLayoutComponentProperty }

procedure TLayoutComponentProperty.GetValues(Proc: TGetStrProc);
var
  I, J: Integer;
  P: TComponent;
  Item: TFmxObject;
  IsSel: Boolean;
  Selects: IDesignerSelections;
begin
  if Assigned(Designer.CurrentParent) then begin
    P := Designer.CurrentParent;
    Selects := GetSelections;
    for I := 0 to TControl(P).ChildrenCount - 1 do begin
      Item := TControl(P).Children.Items[I];
      if (Item is TControl) and (Item.Name <> '') then begin
        IsSel := False;
        if Assigned(Selects) and (Selects.Count > 0) then begin
          for j := 0 to Selects.Count - 1 do
            if Item = Selects.Items[J] then begin
              IsSel := True;
              Break;
            end;
        end;
        if not IsSel then
          Proc(Item.Name);
      end;
    end;
  end;
end;

//{ TShareImageListProperty }
//
//function TShareImageListProperty.GetValue: string;
//begin
//  Result := Designer.GetComponentName(GetComponentReference);
//end;
//
//procedure TShareImageListProperty.GetValues(Proc: TGetStrProc);
//var
//  I: Integer;
//  AList: TList<TShareImageList>;
//begin
//  Designer.GetComponentNames(GetTypeData(GetPropType), Proc);
//  AList := TShareImageList.GetShareImageList;
//  if Assigned(AList) then begin
//    for I := 0 to AList.Count - 1 do
//      if Assigned(AList[I].Owner) then
//        Proc(AList[I].Owner.ClassName + ':' + AList[I].Name);
//  end;
//end;
//
//procedure TShareImageListProperty.SetValue(const Value: string);
//var
//  Component: TComponent;
//  I: Integer;
//  AList: TList<TShareImageList>;
//  V: string;
//begin
//  AList := TShareImageList.GetShareImageList;
//  Component := nil;
//  if Value <> '' then begin
//    if Assigned(AList) and (Pos(':', Value) > 0) then begin
//      for I := 0 to AList.Count - 1 do begin
//        if Assigned(AList[I].Owner) then
//          V := LowerCase(AList[I].Owner.ClassName + ':' + AList[I].Name)
//        else
//          V := '';
//        if V = LowerCase(Value) then begin
//          Component := AList[I];
//          Break;
//        end;
//      end;
//    end;
//
//    if Component = nil then
//      Component := Designer.GetComponent(Value);
//    if not (Component is GetTypeData(GetPropType)^.ClassType) then
//      raise EDesignPropertyError.CreateRes(@SInvalidPropertyValue);
//  end;
//  SetOrdValue(LongInt(Component));
//end;

{ TGridColumnsSettingsProperty }

procedure TGridColumnsSettingsProperty.Edit;
var
  Component: TObject;
  Dialog: TGridColumnsDesigner;
begin
  Component := GetComponent(0);
  if not (Component is TGridView) then
    Exit;
  Dialog := TGridColumnsDesigner.Create(nil);
  try
    Dialog.Caption := 'GridView 列设计器 (隐藏的列请通过点击“上一项”或“下一项”切换)';
    Dialog.Columns := TGridView(Component).Columns;
    if Dialog.ShowModal = mrOK then
      TGridView(Component).Columns.Assign(Dialog.Columns);
  finally
    Dialog.Free;
  end;
end;

function TGridColumnsSettingsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly, paDialog];
end;

{ TSVGImageProperty }

procedure TSVGImageProperty.Edit;
var
  Component: TObject;
  Dialog: TFrmDesignSVGImage;
begin
  Component := GetComponent(0);
  if not (Component is TViewBrushBase) then
    Exit;
  Dialog := TFrmDesignSVGImage.Create(nil);
  try
    Dialog.Caption := 'SVG Image';
    Dialog.LoadImage(TViewBrushBase(Component).SVGImage);
    if Dialog.ShowModal = mrOK then
      TViewBrushBase(Component).SVGImage := Dialog.Bmp;
  finally
    Dialog.Free;
  end;
end;

function TSVGImageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly, paDialog];
end;

{ TViewAccessoryProperty }

procedure TViewAccessoryProperty.Edit;
var
  Component: TObject;
  Dialog: TAccessoryDesigner;
begin
  Component := GetComponent(0);
  if not (Component is TViewBrushBase) then
    Exit;
  Dialog := TAccessoryDesigner.Create(nil);
  try
    Dialog.Caption := 'Accessory Designer';
    Dialog.Accessory := TViewBrushBase(Component).Accessory;
    if Dialog.ShowModal = mrOK then begin
      TViewBrushBase(Component).Accessory := Dialog.Accessory;
    end;
  finally
    Dialog.Free;
  end;
end;

function TViewAccessoryProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly, paDialog];
end;

initialization
  RegisterAliases;
  RegisterFmxClasses([TView, TLinearLayout, TRelativeLayout, TGridView,
    TTextView, TButtonView, TEditView, TAlertDialog, TDialogStyleManager]);

finalization
  UnregisterAliases;
  {$IFDEF MSWINDOWS}
  FCopyBackground := nil;
  FCopyDrawable := nil;
  {$ENDIF}

end.
