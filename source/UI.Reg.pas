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
  UI.Base, UI.Standard, UI.Edit, UI.Dialog, UI.ListView, UI.Toast,
  {$IFDEF MSWINDOWS}
  Windows, Registry,
  {$ENDIF}
  ComponentDesigner, DesignIntf, DesignEditors,
  DesignerTypes, PropertyCategories,
  System.Classes, System.Types, System.TypInfo, System.UITypes,
  FMX.Types, FMX.Styles, FMX.Controls, FMX.StdCtrls, FMX.Edit;

type
  TViewControlEditor = class(TDefaultEditor)
  private
  protected
    procedure DesignerModified;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

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
  RegisterComponents(PageName, [TView, TLinearLayout, TRelativeLayout]);

  RegisterComponents(PageName, [TTextView]);
  RegisterComponents(PageName, [TButtonView]);
  RegisterComponents(PageName, [TEditView]);
  RegisterComponents(PageName, [TListExView]);

  RegisterComponents(PageName, [TDialogStyleManager]);
  RegisterComponents(PageName, [TToastManager]);
  //RegisterComponents(PageName, [TAlertDialog]);

  RegisterComponentEditor(TView, TViewControlEditor);
  //RegisterComponentEditor(TCustomButton, TViewControlEditor);
  //RegisterComponentEditor(TCustomEdit, TViewControlEditor);
  //RegisterPropertyEditor(TypeInfo(TImageIndex), TView, '', TImageIndexProperty);
end;

procedure RegisterAliases;
begin
  AddEnumElementAliases(TypeInfo(TLayoutGravity),
    ['None', 'LeftTop', 'LeftBottom', 'RightTop', 'RightBottom',
    'CenterVertical', 'CenterHorizontal', 'CenterHBottom', 'CenterVRight', 'Center']);
  AddEnumElementAliases(TypeInfo(TViewSize),
    ['CustomSize', 'WrapContent', 'FillParent']);
  AddEnumElementAliases(TypeInfo(TDrawablePosition),
    ['Left', 'Right', 'Top', 'Bottom']);
  AddEnumElementAliases(TypeInfo(TViewBorderStyle),
    ['None', 'RectBorder', 'LineBottom', 'LineSimple']);
  AddEnumElementAliases(TypeInfo(TViewBrushKind),
    ['None', 'Solid', 'Gradient', 'Bitmap', 'Resource', 'Patch9Bitmap']);
  AddEnumElementAliases(TypeInfo(TViewScroll),
    ['None', 'Horizontal', 'Vertical']);
end;

procedure UnregisterAliases;
begin
  RemoveEnumElementAliases(TypeInfo(TLayoutGravity));
  RemoveEnumElementAliases(TypeInfo(TViewSize));
  RemoveEnumElementAliases(TypeInfo(TDrawablePosition));
  RemoveEnumElementAliases(TypeInfo(TViewBorderStyle));
  RemoveEnumElementAliases(TypeInfo(TViewBrushKind));
  RemoveEnumElementAliases(TypeInfo(TViewScroll));
end;

{ TViewControlEditor }

procedure TViewControlEditor.DesignerModified;
begin
  if Designer <> nil then
    Designer.Modified;
end;

procedure TViewControlEditor.ExecuteVerb(Index: Integer);
begin
  if not (Component is TControl) then Exit;
  case Index of
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
  end;
  Designer.SelectComponent(Component);
  DesignerModified;
end;

function TViewControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := '前移';
    1: Result := '后移';
    2: Result := '移至最前';
    3: Result := '移至最后';
  end;
end;

function TViewControlEditor.GetVerbCount: Integer;
begin
  if (Component is TControl) and (TControl(Component).Parent is TLinearLayout) then
    Result := 4
  else
    Result := 0;
end;

initialization
  RegisterAliases;
  RegisterFmxClasses([TView, TLinearLayout, TRelativeLayout,
    TTextView, TButtonView, TEditView, TAlertDialog, TDialogStyleManager]);

finalization
  UnregisterAliases;

end.
