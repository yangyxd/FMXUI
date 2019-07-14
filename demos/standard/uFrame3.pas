unit uFrame3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Standard, UI.Base, UI.Frame, FMX.Controls.Presentation;

type
  TFrame3 = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    View1: TView;
    RelativeLayout1: TRelativeLayout;
    ButtonView1: TButtonView;
    ButtonView2: TButtonView;
    btnBack: TTextView;
    procedure ButtonView1Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation


{$R *.fmx}

{$IFDEF ANDROID}
uses
  FMX.Platform.Android,
  FMX.VirtualKeyboard.Android,
  Androidapi.Helpers,
  Androidapi.Jni,
  //Androidapi.JNI.Media,
  //Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Util,
  Androidapi.JNI.App,
  Androidapi.JNI.Os,
  FMX.Helpers.Android;
{$ENDIF}

procedure TFrame3.btnBackClick(Sender: TObject);
begin
  Finish();
end;

procedure TFrame3.ButtonView1Click(Sender: TObject);
begin
  StatusColor := $ff009900;
  Hint(Format('%.2f', [TView.GetStatusHeight]));
end;

procedure TFrame3.ButtonView2Click(Sender: TObject);
begin
  {$IFDEF ANDROID}
  Hint(Format('SDK: %d', [TJBuild_VERSION.JavaClass.SDK_INT]));
  {$ENDIF}
end;

end.
