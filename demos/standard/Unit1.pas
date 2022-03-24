unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.DateTimeCtrls,
{$IF DEFINED(ANDROID) AND (RTLVersion >= 33)}
  Androidapi.Helpers, Androidapi.JNI.Os,
  System.Permissions,
{$ENDIF}
  UI.Base, UI.Standard, UI.Toast, UI.ListView, UI.Dialog, UI.VKhelper;

type
  TForm1 = class(TForm)
    ToastManager1: TToastManager;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    {$IF DEFINED(ANDROID) AND (RTLVersion >= 33)}
    procedure PermissionsCheck;
    procedure PermissionsResultHandler(Sender: TObject; const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);

    {$ENDIF}
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  {$IF DEFINED(ANDROID) AND (RTLVersion >= 33)}
  Androidapi.JNI.JavaTypes,
  {$ENDIF}
  UI.Async,
  UI.Frame,
  uFrame1,
  uFrame2;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // use android transparent statusbar new method
  TFrameView.SetStatusTransparentNewMethod(True);

  TFrameView.SetDefaultStatusLight(False);
  TFrameView.SetDefaultStatusTransparent(True);
  TFrameView.SetDefaultStatusColor($ff800080);
  //TFrameView.SetDefaultBackColor($fff1f2f3);

end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TFrame1.ShowFrame(Self, 'FMXUI Demo');

  {$IF DEFINED(ANDROID) AND (RTLVersion >= 33)}
  PermissionsCheck;
  {$ENDIF}
end;

{$IF DEFINED(ANDROID) AND (RTLVersion >= 33)}
procedure TForm1.PermissionsCheck;
begin
  if TJBuild_VERSION.JavaClass.SDK_INT >= 23 then
    PermissionsService.RequestPermissions([JStringToString(TJManifest_permission.JavaClass.CAMERA),
       JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE),
       JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE)], PermissionsResultHandler);
end;

procedure TForm1.PermissionsResultHandler(Sender: TObject; const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);
begin
  if PermissionsService.IsEveryPermissionGranted(
    [JStringToString(TJManifest_permission.JavaClass.CAMERA),
     JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE),
     JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE)]) then
    Toast('Permission granted')
  else
    Toast('Permission not granted');
end;
{$ENDIF}

end.
