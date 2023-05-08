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
    FPermissionCamera,
    FPermissionReadExternalStorage,
    FPermissionWriteExternalStorage: string;
    procedure PermissionsCheck;
    procedure PermissionsResultHandler(
      {$IF RTLVersion >= 35}
      Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray
      {$ELSE}
      const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>
      {$ENDIF}
      );
    {$IF RTLVersion >= 35}
    procedure PermissionsDisplayRationaleHandler(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
    {$ENDIF}
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
  if TJBuild_VERSION.JavaClass.SDK_INT >= 23 then begin
    FPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
    FPermissionReadExternalStorage := JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE);
    FPermissionWriteExternalStorage := JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE);

    PermissionsService.RequestPermissions([FPermissionCamera,
       FPermissionReadExternalStorage, FPermissionWriteExternalStorage],
       PermissionsResultHandler{$IF RTLVersion >= 35}, PermissionsDisplayRationaleHandler{$ENDIF});
  end;
end;

procedure TForm1.PermissionsResultHandler(
  {$IF RTLVersion >= 35}
  Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray
  {$ELSE}
  const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>
  {$ENDIF}
);
begin
  if PermissionsService.IsEveryPermissionGranted([FPermissionCamera,
    FPermissionReadExternalStorage, FPermissionWriteExternalStorage]) then
    Toast('Permission granted')
  else
    Toast('Permission not granted');
end;

{$IF RTLVersion >= 35}
procedure TForm1.PermissionsDisplayRationaleHandler(Sender: TObject;const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
var
  I: Integer;
  RationaleMsg: string;
begin
  RationaleMsg := '';
  for I := 0 to High(APermissions) do
  begin
    if APermissions[I] = FPermissionCamera then
      RationaleMsg := RationaleMsg + 'The app needs to access the camera to take a photo' + SLineBreak + SLineBreak
    else if APermissions[I] = FPermissionReadExternalStorage then
      RationaleMsg := RationaleMsg + 'The app needs to load files from your device' + SLineBreak + SLineBreak
    else if APermissions[I] = FPermissionWriteExternalStorage then
      RationaleMsg := RationaleMsg + 'The app needs to write files to your device' + SLineBreak + SLineBreak;
  end;

  TDialogBuilder.Create(Self)
    .SetTitle('Warnning')
    .SetMessage(RationaleMsg)
    .SetPositiveButton('OK',
      procedure (Dialog: IDialog; Which: Integer) begin
        APostRationaleProc;
      end
    )
    .SetNegativeButton('Exit',
      procedure (Dialog: IDialog; Which: Integer) begin
        Close;
      end
    )
    .SetCancelable(False)
    .Show;
end;
{$ENDIF}
{$ENDIF}

end.
