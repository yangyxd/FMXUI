unit uFrameDateView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Standard, UI.Base, UI.Frame, UI.Calendar, FMX.Controls.Presentation,
  FMX.Gestures;

type
  TFrameDateView = class(TFrame)
    LinearLayout1: TLinearLayout;
    btnBack: TTextView;
    tvTitle: TTextView;
    CalendarView1: TCalendarView;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CalendarLanguage_CN1: TCalendarLanguage_CN;
    GestureManager1: TGestureManager;
    procedure btnBackClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameDateView.btnBackClick(Sender: TObject);
begin
  Finish;
end;

procedure TFrameDateView.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.IsChecked then
    CalendarView1.Language := CalendarLanguage_CN1
  else
    CalendarView1.Language := nil;
end;

procedure TFrameDateView.CheckBox2Change(Sender: TObject);
begin
  if CheckBox2.IsChecked then
    CalendarView1.Options := CalendarView1.Options + [coShowLunar]
  else
    CalendarView1.Options := CalendarView1.Options - [coShowLunar]
end;

end.
