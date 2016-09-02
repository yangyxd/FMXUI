{*******************************************************}
{                                                       }
{       FMX UI Toast 自动消失提示组件                   }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Toast;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Objects,
  System.UITypes,
  FMX.Graphics,
  System.Actions,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  TToastLength = (LongToast, ShortToast);

procedure Toast(const Msg: string; Duration: TToastLength = ShortToast);

implementation

{$IFDEF ANDROID}
uses
  UI.Toast.Android;
{$ENDIF}
{$IFDEF MSWINDOWS}
uses
  FMX.Dialogs;
{$ENDIF}
{$IFDEF IOS}
uses
  UI.Toast.AndroidLike;
var
  LToast: TToast;
{$ENDIF}

{$IFDEF ANDROID}
procedure Toast(const Msg: string; Duration: TToastLength = ShortToast);
begin
  UI.Toast.Android.Toast(Msg, Duration);
end;
{$ENDIF}

{$IFDEF IOS}
procedure Toast(const Msg: string; Duration: TToastLength = ShortToast);
begin
  if Msg <> '' then
    LToast.Now(Msg);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure Toast(const Msg: string; Duration: TToastLength = ShortToast);
begin
  ShowMessage(Msg);
end;
{$ENDIF}

initialization
  {$IFDEF IOS}
  LToast := TToast.Create(nil);
  {$ENDIF}

finalization
  {$IFDEF IOS}
  FreeAndNil(LToast);
  {$ENDIF}

end.
