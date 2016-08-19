unit UI.Reg;

interface

const
  PageName = 'YxdFMX';

procedure Register;

implementation

uses
  UI.Base, UI.Standard,
  {$IFDEF MSWINDOWS}
  Windows, Registry,
  {$ENDIF}
  ComponentDesigner, DesignIntf, DesignEditors,
  DesignerTypes, PropertyCategories,
  System.Classes, System.Types, System.TypInfo, System.UITypes,
  FMX.Types, FMX.Styles;

procedure Register;
begin
  RegisterComponents(PageName, [TView, TLinearLayout, TRelativeLayout]);

  RegisterComponents(PageName, [TTextView]);
  RegisterComponents(PageName, [TButtonView]);

  //RegisterPropertyEditor(TypeInfo(TImageIndex), TView, '', TImageIndexProperty);
end;

procedure RegisterAliases;
begin
  AddEnumElementAliases(TypeInfo(TLayoutGravity),
    ['None', 'LeftTop', 'LeftBottom', 'RightTop', 'RightBottom',
    'CenterVertical', 'CenterHorizontal', 'CenterHBottom', 'CenterVRight', 'Center']);
  AddEnumElementAliases(TypeInfo(TViewSize),
    ['WrapContent', 'FillParent']);
  AddEnumElementAliases(TypeInfo(TDrawablePosition),
    ['Left', 'Right', 'Top', 'Bottom']);
end;

procedure UnregisterAliases;
begin
  RemoveEnumElementAliases(TypeInfo(TLayoutGravity));
  RemoveEnumElementAliases(TypeInfo(TViewSize));
  RemoveEnumElementAliases(TypeInfo(TDrawablePosition));
end;

initialization
  RegisterAliases;
  RegisterFmxClasses([TView, TLinearLayout, TRelativeLayout]);

finalization
  UnregisterAliases;

end.
