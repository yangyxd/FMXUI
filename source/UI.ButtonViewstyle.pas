{*******************************************************}
{                                                       }
{       FMX UI 标准组件ButtonView、TextView的拓展       }
{                                                       }
{         版权所有 (C) 2023 dqi1999                     }
{                                                       }
{*******************************************************}

unit UI.ButtonViewstyle;

interface

uses
  FMX.Graphics, System.Math, System.Classes, System.SysUtils, System.UIConsts,
  System.UITypes, System.JSON, UI.Base, UI.Standard, UI.Json;

const
  BOOTSTRAP_FAMILY = 'Bootstrap';
  BOOTSTRAP_NORMAL = 'Normal';
  BOOTSTRAP_OUTLINE = 'Outline';
  BOOTSTRAP_BORDER_WIDTH = 2;
  btn_primary = 'Primary';
  btn_secondary = 'Secondary';
  btn_success = 'Success';
  btn_danger = 'Danger';
  btn_warning = 'Warning';
  btn_info = 'Info';
  btn_light = 'Light';
  btn_dark = 'Dark';

  // Button Family as Angular Themes
  ANGULAR_STROKED_WIDTH = 2;
  ANGULAR_RAISED_WIDTH = 3;
  ANGULAR_LIGHT_FAMILY = 'Angular-Light';
  ANGULAR_DARK_FAMILY = 'Angular-Dark';

  // Button Class as Angular Styling
  btn_Basic = 'Basic';
  btn_Warn = 'Warn';
  btn_Link = 'Link';

  // DeepPurple & Amber Light Theme
  btn_PrimaryDeepPurple = 'DeepPurple';
  btn_AccentAmber = 'Amber';
  // Indigo & Pink Light Theme
  btn_PrimaryIndigo = 'Indigo';
  btn_AccentPink = 'Pink';

  // Pink & BlueGray Dark Theme
  btn_PrimaryPink = 'Pink';
  btn_AccentBlueGray = 'Blue-gray';
  // Purple & Green Dark Theme
  btn_PrimaryPurple = 'Purple';
  btn_AccentGreen = 'Green';

  // Button Appearance as Angular Attributes
  FlatAttr = 'Flat';
  RaisedAttr = 'Raised';
  BasicAttr = 'Basic';
  StrokedAttr = 'Stroked';


type
  // TSTYLEFAMILY=('Bootstrap','Angular-Light','Angular-Dark');

  TTextviewstyle = record
    const
      BootstrapbtnKinds: array[0..7] of string = ('Primary', 'Secondary', 'Success', 'Danger', 'Warning', 'Info', 'Light', 'Dark');
      AngularbtnKinds: array[0..6] of string = ('Basic', 'DeepPurple', 'Amber', 'Indigo', 'Pink', 'Warn', 'Link');
      AngularDarkbtnKinds: array[0..6] of string = ('Basic', 'Pink', 'Blue-gray', 'Purple', 'Green', 'Warn', 'Link');
    class function GetFAMILYAllClass(AFamily: string): Tarray<string>; static;
    class function GetFAMILYAAppearance(AFamily: string): Tarray<string>; static;
    class function GetAAppearance(AFamily: string; idx: Integer): string; static;
    class function GetAllClassFromFAMILYidx(AFamilyidx: Integer): Tarray<string>; static;
    class function GetClass(AFamily: string; idx: Integer): string; static;
    class function GetFAMILYClassCount(AFamily: string): Integer; static;
    class function GetFAMILYs: Tarray<string>; static;
    class function GetFAMILYI(idx: Integer): string; static;
    class function GetFAMILYCount: Integer; static;
    class procedure BootstrapClassToColors(const AClass: string; const AAppearance: string; var AFontColor, AButtonColor: TAlphaColor; out AOutLine: Boolean); static;
    class procedure AngularClassToLightColors(const AClass: string; const AAppearance: string; var AFontColor, AButtonColor: TAlphaColor); static;
    class procedure AngularClassToDarkColors(const AClass: string; const AAppearance: string; var AFontColor, AButtonColor: TAlphaColor); static;
    class procedure setBootstrap(button: TTextView; AClass, AAppearance: string); static;
    class procedure setAngularlight(button: TTextView; AClass, AAppearance: string); static;
    class procedure setAngularDark(button: TTextView; AClass, AAppearance: string); static;
    class procedure SetBtnKindColor(button: TTextView; ViewState: TViewState; btncolor: TAlphaColor; FontColor: TAlphaColor; BorderColor: TAlphaColor = TAlphaColorRec.Null); static;
    class procedure SetButtonStyle(button: TTextView; const AFamily: string; const AClass: string; const AAppearance: string); overload; static;
    class procedure SetButtonStyle(button: TTextView; const AFamily: Integer; const AClass: Integer; const AAppearance: Integer); overload; static;
    class procedure SetTextViewStyle(TextView: TTextView; const AFamily: string; const AClass: string); overload; static;

  private

  end;

  TTextViewHelper = class helper for TTextView
  public
    /// AFamily,buttonview 风格大类 "Bootstrap"....
    /// AClass，按钮颜色类型
    /// AAppearance，按钮外观，"Normal"默认为填充类型，"Outline"为外框外观
    procedure SetButtonStyle(const AFamily: string; const AClass: string; const AAppearance: string);
    procedure SetTextViewStyle(const AFamily: string; const AClass: string);
  end;

const
  CNColorJsonTxt = '[{"name":"蔚蓝","bkcolor":"#70f3ff","fcolor":"#000000"},{"name":"蓝","bkcolor":"#44cef6","fcolor":"#' + '000000"},{"name":"碧蓝","bkcolor":"#3eede7","fcolor":"#000000"},{"name":"石青","bkcolor":"#1685a9","' + 'fcolor":"#ffffff"},{"name":"靛青","bkcolor":"#177cb0","fcolor":"#ffffff"},{"name":"靛蓝","bkcolor":"#' + '065279","fcolor":"#ffffff"},{"name":"花青","bkcolor":"#003472","fcolor":"#ffffff"},{"name":"宝蓝","' +
    'bkcolor":"#4b5cc4","fcolor":"#ffffff"},{"name":"蓝灰色","bkcolor":"#a1afc9","fcolor":"#ffffff"},{"' + 'name":"藏青","bkcolor":"#2e4e7e","fcolor":"#ffffff"},{"name":"藏蓝","bkcolor":"#3b2e7e","fcolor":"#' + 'ffffff"},{"name":"黛","bkcolor":"#4a4266","fcolor":"#ffffff"},{"name":"黛绿","bkcolor":"#426666","' + 'fcolor":"#ffffff"},{"name":"黛蓝","bkcolor":"#425066","fcolor":"#ffffff"},{"name":"黛紫","bkcolor":"#' + '574266","fcolor":"#ffffff"},{"name":"紫色","bkcolor":"#8d4bbb","fcolor":"#ffffff"},{"name":"紫酱","' +
    'bkcolor":"#815463","fcolor":"#ffffff"},{"name":"酱紫","bkcolor":"#815476","fcolor":"#ffffff"},{"name":' + '"紫檀","bkcolor":"#4c221b","fcolor":"#ffffff"},{"name":"绀青","bkcolor":"#003371","fcolor":"#ffffff"},{"' + 'name":"紫棠","bkcolor":"#56004f","fcolor":"#ffffff"},{"name":"青莲","bkcolor":"#801dae","fcolor":"#' + 'ffffff"},{"name":"群青","bkcolor":"#4c8dae","fcolor":"#ffffff"},{"name":"雪青","bkcolor":"#b0a4e3","' +
    'fcolor":"#000000"},{"name":"丁香色","bkcolor":"#cca4e3","fcolor":"#000000"},{"name":"藕色","bkcolor":"#' + 'edd1d8","fcolor":"#000000"},{"name":"藕荷色","bkcolor":"#e4c6d0","fcolor":"#000000"},{"name":"朱砂","' + 'bkcolor":"#ff461f","fcolor":"#ffffff"},{"name":"火红","bkcolor":"#ff2d51","fcolor":"#ffffff"},{"name":' + '"朱膘","bkcolor":"#f36838","fcolor":"#ffffff"},{"name":"洋红","bkcolor":"#ff4777","fcolor":"#ffffff"},{"' +
    'name":"品红","bkcolor":"#f00056","fcolor":"#ffffff"},{"name":"粉红","bkcolor":"#ffb3a7","fcolor":"#' + 'ffffff"},{"name":"桃红","bkcolor":"#f47983","fcolor":"#ffffff"},{"name":"海棠红","bkcolor":"#db5a6b","' + 'fcolor":"#ffffff"},{"name":"樱桃色","bkcolor":"#c93756","fcolor":"#ffffff"},{"name":"酡颜","bkcolor":"#' + 'f9906f","fcolor":"#ffffff"},{"name":"银红","bkcolor":"#f05654","fcolor":"#ffffff"},{"name":"大红","' + 'bkcolor":"#ff2121","fcolor":"#ffffff"},{"name":"石榴红","bkcolor":"#f20c00","fcolor":"#ffffff"},{"' +
    'name":"绛紫","bkcolor":"#8c4356","fcolor":"#ffffff"},{"name":"绯红","bkcolor":"#c83c23","fcolor":"#' + 'ffffff"},{"name":"胭脂","bkcolor":"#9d2933","fcolor":"#ffffff"},{"name":"朱红","bkcolor":"#ff4c00","' + 'fcolor":"#ffffff"},{"name":"丹","bkcolor":"#ff4e20","fcolor":"#ffffff"},{"name":"彤","bkcolor":"#' + 'f35336","fcolor":"#ffffff"},{"name":"酡红","bkcolor":"#dc3023","fcolor":"#ffffff"},{"name":"炎","' + 'bkcolor":"#ff3300","fcolor":"#ffffff"},{"name":"茜色","bkcolor":"#cb3a56","fcolor":"#ffffff"},{"name":' +
    '"绾","bkcolor":"#a98175","fcolor":"#ffffff"},{"name":"檀","bkcolor":"#b36d61","fcolor":"#ffffff"},{"' + 'name":"嫣红","bkcolor":"#ef7a82","fcolor":"#ffffff"},{"name":"洋红","bkcolor":"#ff0097","fcolor":"#' + 'ffffff"},{"name":"枣红","bkcolor":"#c32136","fcolor":"#ffffff"},{"name":"殷红","bkcolor":"#be002f","' + 'fcolor":"#ffffff"},{"name":"赫赤","bkcolor":"#c91f37","fcolor":"#ffffff"},{"name":"银朱","bkcolor":"#' + 'bf242a","fcolor":"#ffffff"},{"name":"赤","bkcolor":"#c3272b","fcolor":"#ffffff"},{"name":"胭脂","' +
    'bkcolor":"#9d2933","fcolor":"#ffffff"},{"name":"栗色","bkcolor":"#60281e","fcolor":"#ffffff"},{"name":' + '"玄色","bkcolor":"#622a1d","fcolor":"#ffffff"},{"name":"松花色","bkcolor":"#bce672","fcolor":"#000000"},{' + '"name":"柳黄","bkcolor":"#c9dd22","fcolor":"#000000"},{"name":"嫩绿","bkcolor":"#bddd22","fcolor":"#' + '000000"},{"name":"柳绿","bkcolor":"#afdd22","fcolor":"#000000"},{"name":"葱黄","bkcolor":"#a3d900","' +
    'fcolor":"#000000"},{"name":"葱绿","bkcolor":"#9ed900","fcolor":"#000000"},{"name":"豆绿","bkcolor":"#' + '9ed048","fcolor":"#000000"},{"name":"豆青","bkcolor":"#96ce54","fcolor":"#000000"},{"name":"油绿","' + 'bkcolor":"#00bc12","fcolor":"#000000"},{"name":"葱倩","bkcolor":"#0eb83a","fcolor":"#000000"},{"name":' + '"葱青","bkcolor":"#0eb83a","fcolor":"#000000"},{"name":"青葱","bkcolor":"#0aa344","fcolor":"#000000"},{"' +
    'name":"石绿","bkcolor":"#16a951","fcolor":"#ffffff"},{"name":"松柏绿","bkcolor":"#21a675","fcolor":"#' + 'ffffff"},{"name":"松花绿","bkcolor":"#057748","fcolor":"#ffffff"},{"name":"绿沈","bkcolor":"#0c8918","' + 'fcolor":"#ffffff"},{"name":"绿色","bkcolor":"#00e500","fcolor":"#000000"},{"name":"草绿","bkcolor":"#' + '40de5a","fcolor":"#000000"},{"name":"青翠","bkcolor":"#00e079","fcolor":"#000000"},{"name":"青色","' + 'bkcolor":"#00e09e","fcolor":"#000000"},{"name":"翡翠色","bkcolor":"#3de1ad","fcolor":"#000000"},{"' +
    'name":"碧绿","bkcolor":"#2add9c","fcolor":"#000000"},{"name":"玉色","bkcolor":"#2edfa3","fcolor":"#' + '000000"},{"name":"缥","bkcolor":"#7fecad","fcolor":"#000000"},{"name":"艾绿","bkcolor":"#a4e2c6","' + 'fcolor":"#000000"},{"name":"石青","bkcolor":"#7bcfa6","fcolor":"#000000"},{"name":"碧色","bkcolor":"#' + '1bd1a5","fcolor":"#000000"},{"name":"青碧","bkcolor":"#48c0a3","fcolor":"#000000"},{"name":"铜绿","' + 'bkcolor":"#549688","fcolor":"#000000"},{"name":"竹青","bkcolor":"#789262","fcolor":"#000000"},{"name":' +
    '"墨灰","bkcolor":"#758a99","fcolor":"#ffffff"},{"name":"墨色","bkcolor":"#50616d","fcolor":"#ffffff"},{"' + 'name":"鸦青","bkcolor":"#424c50","fcolor":"#ffffff"},{"name":"黯","bkcolor":"#41555d","fcolor":"#' + 'ffffff"},{"name":"樱草色","bkcolor":"#eaff56","fcolor":"#000000"},{"name":"鹅黄","bkcolor":"#fff143","' + 'fcolor":"#000000"},{"name":"鸭黄","bkcolor":"#faff72","fcolor":"#000000"},{"name":"杏黄","bkcolor":"#' +
    'ffa631","fcolor":"#000000"},{"name":"橙黄","bkcolor":"#ffa400","fcolor":"#ffffff"},{"name":"橙色","' + 'bkcolor":"#fa8c35","fcolor":"#ffffff"},{"name":"杏红","bkcolor":"#ff8c31","fcolor":"#000000"},{"name":' + '"橘黄","bkcolor":"#ff8936","fcolor":"#000000"},{"name":"橘红","bkcolor":"#ff7500","fcolor":"#000000"},{"' + 'name":"藤黄","bkcolor":"#ffb61e","fcolor":"#000000"},{"name":"姜黄","bkcolor":"#ffc773","fcolor":"#' +
    '000000"},{"name":"雌黄","bkcolor":"#ffc64b","fcolor":"#000000"},{"name":"赤金","bkcolor":"#f2be45","' + 'fcolor":"#000000"},{"name":"缃色","bkcolor":"#f0c239","fcolor":"#000000"},{"name":"雄黄","bkcolor":"#' + 'e9bb1d","fcolor":"#000000"},{"name":"秋香色","bkcolor":"#d9b611","fcolor":"#000000"},{"name":"金色","' + 'bkcolor":"#eacd76","fcolor":"#000000"},{"name":"牙色","bkcolor":"#eedeb0","fcolor":"#000000"},{"name":' +
    '"枯黄","bkcolor":"#d3b17d","fcolor":"#000000"},{"name":"黄栌","bkcolor":"#e29c45","fcolor":"#ffffff"},{"' + 'name":"乌金","bkcolor":"#a78e44","fcolor":"#000000"},{"name":"昏黄","bkcolor":"#c89b40","fcolor":"#' + 'ffffff"},{"name":"棕黄","bkcolor":"#ae7000","fcolor":"#ffffff"},{"name":"琥珀","bkcolor":"#ca6924","' + 'fcolor":"#ffffff"},{"name":"棕色","bkcolor":"#b25d25","fcolor":"#ffffff"},{"name":"茶色","bkcolor":"#' + 'b35c44","fcolor":"#ffffff"},{"name":"棕红","bkcolor":"#9b4400","fcolor":"#ffffff"},{"name":"赭","' +
    'bkcolor":"#9c5333","fcolor":"#ffffff"},{"name":"驼色","bkcolor":"#a88462","fcolor":"#ffffff"},{"name":' + '"秋色","bkcolor":"#896c39","fcolor":"#ffffff"},{"name":"棕绿","bkcolor":"#827100","fcolor":"#ffffff"},{"' + 'name":"褐色","bkcolor":"#6e511e","fcolor":"#ffffff"},{"name":"棕黑","bkcolor":"#7c4b00","fcolor":"#' + 'ffffff"},{"name":"赭色","bkcolor":"#955539","fcolor":"#ffffff"},{"name":"赭石","bkcolor":"#845a33","' +
    'fcolor":"#ffffff"},{"name":"精白","bkcolor":"#ffffff","fcolor":"#000000"},{"name":"铅白","bkcolor":"#' + 'f0f0f4","fcolor":"#000000"},{"name":"霜色","bkcolor":"#e9f1f6","fcolor":"#000000"},{"name":"雪白","' + 'bkcolor":"#f0fcff","fcolor":"#000000"},{"name":"莹白","bkcolor":"#e3f9fd","fcolor":"#000000"},{"name":' + '"月白","bkcolor":"#d6ecf0","fcolor":"#000000"},{"name":"象牙白","bkcolor":"#fffbf0","fcolor":"#000000"},{' +
    '"name":"缟","bkcolor":"#f2ecde","fcolor":"#000000"},{"name":"鱼肚白","bkcolor":"#fcefe8","fcolor":"#' + '000000"},{"name":"白粉","bkcolor":"#fff2df","fcolor":"#000000"},{"name":"荼白","bkcolor":"#f3f9f1","' + 'fcolor":"#000000"},{"name":"鸭卵青","bkcolor":"#e0eee8","fcolor":"#000000"},{"name":"素","bkcolor":"#' + 'e0f0e9","fcolor":"#000000"},{"name":"青白","bkcolor":"#c0ebd7","fcolor":"#000000"},{"name":"蟹壳青","' +
    'bkcolor":"#bbcdc5","fcolor":"#000000"},{"name":"花白","bkcolor":"#c2ccd0","fcolor":"#000000"},{"name":' + '"老银","bkcolor":"#bacac6","fcolor":"#000000"},{"name":"灰色","bkcolor":"#808080","fcolor":"#ffffff"},{"' + 'name":"苍色","bkcolor":"#75878a","fcolor":"#ffffff"},{"name":"水色","bkcolor":"#88ada6","fcolor":"#' + 'ffffff"},{"name":"黝","bkcolor":"#6b6882","fcolor":"#ffffff"},{"name":"乌色","bkcolor":"#725e82","' +
    'fcolor":"#ffffff"},{"name":"玄青","bkcolor":"#3d3b4f","fcolor":"#ffffff"},{"name":"乌黑","bkcolor":"#' + '392f41","fcolor":"#ffffff"},{"name":"黎","bkcolor":"#75664d","fcolor":"#ffffff"},{"name":"黧","' + 'bkcolor":"#5d513c","fcolor":"#ffffff"},{"name":"黝黑","bkcolor":"#665757","fcolor":"#ffffff"},{"name":' + '"缁色","bkcolor":"#493131","fcolor":"#ffffff"},{"name":"煤黑","bkcolor":"#312520","fcolor":"#ffffff"},{"' + 'name":"漆黑","bkcolor":"#161823","fcolor":"#ffffff"},{"name":"黑色","bkcolor":"#000000","fcolor":"#' + 'ffffff"}]';

  JPColorjsonTxt = '[{"name":"古代紫","bkcolor":"#895b8a ","fcolor":"#ffffff"},{"name":"茄子紺","bkcolor":"#824880 ","fcolor":' + '"#ffffff"},{"name":"二藍","bkcolor":"#915c8b ","fcolor":"#ffffff"},{"name":"京紫","bkcolor":"#9d5b8b ","' + 'fcolor":"#ffffff"},{"name":"蒲葡","bkcolor":"#7a4171 ","fcolor":"#ffffff"},{"name":"若紫","bkcolor":"#' + 'bc64a4 ","fcolor":"#ffffff"},{"name":"紅紫","bkcolor":"#b44c97 ","fcolor":"#ffffff"},{"name":"梅紫","' +
    'bkcolor":"#aa4c8f ","fcolor":"#ffffff"},{"name":"菖蒲色","bkcolor":"#cc7eb1 ","fcolor":"#ffffff"},{"' + 'name":"紅藤色","bkcolor":"#cca6bf ","fcolor":"#ffffff"},{"name":"浅紫","bkcolor":"#c4a3bf ","fcolor":"#' + 'ffffff"},{"name":"紫水晶","bkcolor":"#e7e7eb ","fcolor":"#000000"},{"name":"薄梅鼠","bkcolor":"#dcd6d9","' + 'fcolor":"#000000"},{"name":"暁鼠","bkcolor":"#d3cfd9","fcolor":"#000000"},{"name":"牡丹鼠","bkcolor":"#' +
    'd3ccd6","fcolor":"#000000"},{"name":"霞色","bkcolor":"#c8c2c6","fcolor":"#ffffff"},{"name":"藤鼠","' + 'bkcolor":"#a6a5c4 ","fcolor":"#ffffff"},{"name":"半色","bkcolor":"#a69abd ","fcolor":"#ffffff"},{"' + 'name":"薄色","bkcolor":"#a89dac ","fcolor":"#ffffff"},{"name":"薄鼠","bkcolor":"#9790a4 ","fcolor":"#' + 'ffffff"},{"name":"鳩羽鼠","bkcolor":"#9e8b8e","fcolor":"#ffffff"},{"name":"鳩羽色","bkcolor":"#95859c ","' +
    'fcolor":"#ffffff"},{"name":"桔梗鼠","bkcolor":"#95949a ","fcolor":"#ffffff"},{"name":"紫鼠","bkcolor":"#' + '71686c ","fcolor":"#ffffff"},{"name":"葡萄鼠","bkcolor":"#705b67 ","fcolor":"#ffffff"},{"name":"濃色","' + 'bkcolor":"#634950 ","fcolor":"#ffffff"},{"name":"紫鳶","bkcolor":"#5f414b ","fcolor":"#ffffff"},{"' + 'name":"濃鼠","bkcolor":"#4f455c ","fcolor":"#ffffff"},{"name":"藤煤竹","bkcolor":"#5a5359 ","fcolor":"#' +
    'ffffff"},{"name":"滅紫","bkcolor":"#594255 ","fcolor":"#ffffff"},{"name":"紅消鼠","bkcolor":"#524748 ","' + 'fcolor":"#ffffff"},{"name":"似せ紫","bkcolor":"#513743 ","fcolor":"#ffffff"},{"name":"灰黄緑","bkcolor":"#' + 'e6eae3","fcolor":"#000000"},{"name":"蕎麦切色","bkcolor":"#d4dcd6","fcolor":"#000000"},{"name":"薄雲鼠","' + 'bkcolor":"#d4dcda","fcolor":"#000000"},{"name":"枯野色","bkcolor":"#d3cbc6","fcolor":"#000000"},{"' +
    'name":"潤色","bkcolor":"#c8c2be ","fcolor":"#ffffff"},{"name":"利休白茶","bkcolor":"#b3ada0 ","fcolor":"#' + 'ffffff"},{"name":"茶鼠","bkcolor":"#a99e93 ","fcolor":"#ffffff"},{"name":"胡桃染","bkcolor":"#a58f86 ","' + 'fcolor":"#ffffff"},{"name":"江戸鼠","bkcolor":"#928178 ","fcolor":"#ffffff"},{"name":"煤色","bkcolor":"#' + '887f7a ","fcolor":"#ffffff"},{"name":"丁子茶","bkcolor":"#b4866b ","fcolor":"#ffffff"},{"name":"柴染","' +
    'bkcolor":"#b28c6e ","fcolor":"#ffffff"},{"name":"宗伝唐茶","bkcolor":"#a16d5d ","fcolor":"#ffffff"},{"' + 'name":"砺茶","bkcolor":"#9f6f55 ","fcolor":"#ffffff"},{"name":"煎茶色","bkcolor":"#8c6450 ","fcolor":"#' + 'ffffff"},{"name":"銀煤竹","bkcolor":"#856859 ","fcolor":"#ffffff"},{"name":"黄枯茶","bkcolor":"#765c47 ","' + 'fcolor":"#ffffff"},{"name":"煤竹色","bkcolor":"#6f514c ","fcolor":"#ffffff"},{"name":"焦茶","bkcolor":"#' +
    '6f4b3e ","fcolor":"#ffffff"},{"name":"黒橡","bkcolor":"#544a47 ","fcolor":"#ffffff"},{"name":"憲法色","' + 'bkcolor":"#543f32 ","fcolor":"#ffffff"},{"name":"涅色","bkcolor":"#554738 ","fcolor":"#ffffff"},{"' + 'name":"檳榔子染","bkcolor":"#433d3c ","fcolor":"#ffffff"},{"name":"黒鳶","bkcolor":"#432f2f ","fcolor":"#' + 'ffffff"},{"name":"赤墨","bkcolor":"#3f312b ","fcolor":"#ffffff"},{"name":"黒紅","bkcolor":"#302833 ","' +
    'fcolor":"#ffffff"},{"name":"白","bkcolor":"#ffffff ","fcolor":"#000000"},{"name":"胡粉色","bkcolor":"#' + 'fffffc ","fcolor":"#000000"},{"name":"卯の花色","bkcolor":"#f7fcfe ","fcolor":"#000000"},{"name":"白磁","' + 'bkcolor":"#f8fbf8 ","fcolor":"#000000"},{"name":"生成り色","bkcolor":"#fbfaf5 ","fcolor":"#000000"},{"' + 'name":"乳白色","bkcolor":"#f3f3f3 ","fcolor":"#000000"},{"name":"白練","bkcolor":"#f3f3f2 ","fcolor":"#' +
    '000000"},{"name":"素色","bkcolor":"#eae5e3 ","fcolor":"#000000"},{"name":"白梅鼠","bkcolor":"#e5e4e6 ","' + 'fcolor":"#000000"},{"name":"白鼠","bkcolor":"#dcdddd ","fcolor":"#000000"},{"name":"絹鼠","bkcolor":"#' + 'dddcd6 ","fcolor":"#000000"},{"name":"灰青","bkcolor":"#c0c6c9 ","fcolor":"#ffffff"},{"name":"銀鼠","' + 'bkcolor":"#afafb0 ","fcolor":"#ffffff"},{"name":"薄鈍","bkcolor":"#adadad ","fcolor":"#ffffff"},{"' +
    'name":"薄墨色","bkcolor":"#a3a3a2 ","fcolor":"#ffffff"},{"name":"錫色","bkcolor":"#9ea1a3 ","fcolor":"#' + 'ffffff"},{"name":"素鼠","bkcolor":"#9fa0a0 ","fcolor":"#ffffff"},{"name":"鼠色","bkcolor":"#949495 ","' + 'fcolor":"#ffffff"},{"name":"源氏鼠","bkcolor":"#888084 ","fcolor":"#ffffff"},{"name":"灰色","bkcolor":"#' + '7d7d7d ","fcolor":"#ffffff"},{"name":"鉛色","bkcolor":"#7b7c7d ","fcolor":"#ffffff"},{"name":"鈍色","' +
    'bkcolor":"#727171 ","fcolor":"#ffffff"},{"name":"墨","bkcolor":"#595857 ","fcolor":"#ffffff"},{"' + 'name":"丼鼠","bkcolor":"#595455 ","fcolor":"#ffffff"},{"name":"消炭色","bkcolor":"#524e4d ","fcolor":"#' + 'ffffff"},{"name":"藍墨茶","bkcolor":"#474a4d ","fcolor":"#ffffff"},{"name":"羊羹色","bkcolor":"#383c3c ","' + 'fcolor":"#ffffff"},{"name":"蝋色","bkcolor":"#2b2b2b ","fcolor":"#ffffff"},{"name":"黒","bkcolor":"#' +
    '2b2b2b ","fcolor":"#ffffff"},{"name":"烏羽色","bkcolor":"#180614 ","fcolor":"#ffffff"},{"name":"鉄黒","' + 'bkcolor":"#281a14 ","fcolor":"#ffffff"},{"name":"濡羽色","bkcolor":"#000b00 ","fcolor":"#ffffff"},{"' + 'name":"黒壇","bkcolor":"#250d00 ","fcolor":"#ffffff"},{"name":"憲法黒茶","bkcolor":"#241a08 ","fcolor":"#' + 'ffffff"},{"name":"暗黒色","bkcolor":"#16160e ","fcolor":"#ffffff"},{"name":"萌葱色","bkcolor":"#006e54 ","' +
    'fcolor":"#ffffff"},{"name":"花緑青","bkcolor":"#00a381 ","fcolor":"#ffffff"},{"name":"翡翠色","bkcolor":"#' + '38b48b ","fcolor":"#ffffff"},{"name":"青緑","bkcolor":"#00a497 ","fcolor":"#ffffff"},{"name":"水浅葱","' + 'bkcolor":"#80aba9 ","fcolor":"#ffffff"},{"name":"錆浅葱","bkcolor":"#5c9291 ","fcolor":"#ffffff"},{"' + 'name":"青碧","bkcolor":"#478384 ","fcolor":"#ffffff"},{"name":"御召茶","bkcolor":"#43676b ","fcolor":"#' +
    'ffffff"},{"name":"湊鼠","bkcolor":"#80989b ","fcolor":"#ffffff"},{"name":"高麗納戸","bkcolor":"#2c4f54 ","' + 'fcolor":"#ffffff"},{"name":"百入茶","bkcolor":"#1f3134 ","fcolor":"#ffffff"},{"name":"錆鼠","bkcolor":"#' + '47585c ","fcolor":"#ffffff"},{"name":"錆鉄御納戸","bkcolor":"#485859","fcolor":"#ffffff"},{"name":"藍鼠","' + 'bkcolor":"#6c848d","fcolor":"#ffffff"},{"name":"錆御納戸","bkcolor":"#53727d ","fcolor":"#ffffff"},{"' +
    'name":"舛花色","bkcolor":"#5b7e91 ","fcolor":"#ffffff"},{"name":"熨斗目花色","bkcolor":"#426579 ","fcolor":"' + '#ffffff"},{"name":"御召御納戸","bkcolor":"#4c6473 ","fcolor":"#ffffff"},{"name":"鉄御納戸","bkcolor":"#' + '455765 ","fcolor":"#ffffff"},{"name":"紺鼠","bkcolor":"#44617b","fcolor":"#ffffff"},{"name":"藍鉄","' + 'bkcolor":"#393f4c ","fcolor":"#ffffff"},{"name":"青褐","bkcolor":"#393e4f ","fcolor":"#ffffff"},{"' +
    'name":"褐返","bkcolor":"#203744 ","fcolor":"#ffffff"},{"name":"褐色","bkcolor":"#4d4c61 ","fcolor":"#' + 'ffffff"},{"name":"月白","bkcolor":"#eaf4fc ","fcolor":"#000000"},{"name":"白菫色","bkcolor":"#eaedf7 ","' + 'fcolor":"#000000"},{"name":"白花色","bkcolor":"#e8ecef ","fcolor":"#000000"},{"name":"藍白","bkcolor":"#' + 'ebf6f7 ","fcolor":"#000000"},{"name":"白藍","bkcolor":"#c1e4e9 ","fcolor":"#000000"},{"name":"水色","' +
    'bkcolor":"#bce2e8 ","fcolor":"#000000"},{"name":"瓶覗","bkcolor":"#a2d7dd ","fcolor":"#000000"},{"' + 'name":"秘色色","bkcolor":"#abced8 ","fcolor":"#000000"},{"name":"空色","bkcolor":"#a0d8ef ","fcolor":"#' + '000000"},{"name":"勿忘草色","bkcolor":"#89c3eb ","fcolor":"#000000"},{"name":"青藤色","bkcolor":"#84a2d4 ",' + '"fcolor":"#000000"},{"name":"白群","bkcolor":"#83ccd2 ","fcolor":"#000000"},{"name":"浅縹","bkcolor":"#' +
    '84b9cb ","fcolor":"#ffffff"},{"name":"薄花色","bkcolor":"#698aab ","fcolor":"#ffffff"},{"name":"納戸色","' + 'bkcolor":"#008899 ","fcolor":"#ffffff"},{"name":"浅葱色","bkcolor":"#00a3af ","fcolor":"#ffffff"},{"' + 'name":"花浅葱","bkcolor":"#2a83a2 ","fcolor":"#ffffff"},{"name":"新橋色","bkcolor":"#59b9c6 ","fcolor":"#' + 'ffffff"},{"name":"天色","bkcolor":"#2ca9e1 ","fcolor":"#000000"},{"name":"露草色","bkcolor":"#38a1db ","' +
    'fcolor":"#000000"},{"name":"青","bkcolor":"#0095d9 ","fcolor":"#000000"},{"name":"薄藍","bkcolor":"#' + '0094c8 ","fcolor":"#ffffff"},{"name":"縹色","bkcolor":"#2792c3 ","fcolor":"#ffffff"},{"name":"紺碧","' + 'bkcolor":"#007bbb ","fcolor":"#ffffff"},{"name":"薄群青","bkcolor":"#5383c3 ","fcolor":"#ffffff"},{"' + 'name":"薄花桜","bkcolor":"#5a79ba ","fcolor":"#ffffff"},{"name":"群青色","bkcolor":"#4c6cb3 ","fcolor":"#' +
    'ffffff"},{"name":"杜若色","bkcolor":"#3e62ad ","fcolor":"#ffffff"},{"name":"瑠璃色","bkcolor":"#1e50a2 ","' + 'fcolor":"#ffffff"},{"name":"薄縹","bkcolor":"#507ea4 ","fcolor":"#ffffff"},{"name":"瑠璃紺","bkcolor":"#' + '19448e ","fcolor":"#ffffff"},{"name":"紺瑠璃","bkcolor":"#164a84","fcolor":"#ffffff"},{"name":"藍色","' + 'bkcolor":"#165e83","fcolor":"#ffffff"},{"name":"青藍","bkcolor":"#274a78 ","fcolor":"#ffffff"},{"' +
    'name":"深縹","bkcolor":"#2a4073 ","fcolor":"#ffffff"},{"name":"紺色","bkcolor":"#223a70 ","fcolor":"#' + 'ffffff"},{"name":"紺青","bkcolor":"#192f60 ","fcolor":"#ffffff"},{"name":"留紺","bkcolor":"#1c305c ","' + 'fcolor":"#ffffff"},{"name":"濃藍","bkcolor":"#0f2350 ","fcolor":"#ffffff"},{"name":"鉄紺","bkcolor":"#' + '17184b ","fcolor":"#ffffff"},{"name":"漆黒","bkcolor":"#0d0015 ","fcolor":"#ffffff"},{"name":"淡藤色","' +
    'bkcolor":"#bbc8e6 ","fcolor":"#000000"},{"name":"藤色","bkcolor":"#bbbcde ","fcolor":"#000000"},{"' + 'name":"紅掛空色","bkcolor":"#8491c3 ","fcolor":"#ffffff"},{"name":"紅碧","bkcolor":"#8491c3 ","fcolor":"#' + 'ffffff"},{"name":"紺桔梗","bkcolor":"#4d5aaf ","fcolor":"#ffffff"},{"name":"花色","bkcolor":"#4d5aaf ","' + 'fcolor":"#ffffff"},{"name":"紺藍","bkcolor":"#4a488e ","fcolor":"#ffffff"},{"name":"紅桔梗","bkcolor":"#' +
    '4d4398 ","fcolor":"#ffffff"},{"name":"桔梗色","bkcolor":"#5654a2 ","fcolor":"#ffffff"},{"name":"藤納戸","' + 'bkcolor":"#706caa ","fcolor":"#ffffff"},{"name":"紅掛花色","bkcolor":"#68699b ","fcolor":"#ffffff"},{"' + 'name":"紫苑色","bkcolor":"#867ba9 ","fcolor":"#ffffff"},{"name":"白藤色","bkcolor":"#dbd0e6 ","fcolor":"#' + '000000"},{"name":"藤紫","bkcolor":"#a59aca ","fcolor":"#ffffff"},{"name":"菫色","bkcolor":"#7058a3 ","' +
    'fcolor":"#ffffff"},{"name":"青紫","bkcolor":"#674598 ","fcolor":"#ffffff"},{"name":"菖蒲色","bkcolor":"#' + '674196 ","fcolor":"#ffffff"},{"name":"竜胆色","bkcolor":"#9079ad ","fcolor":"#ffffff"},{"name":"江戸紫","' + 'bkcolor":"#745399 ","fcolor":"#ffffff"},{"name":"本紫","bkcolor":"#65318e ","fcolor":"#ffffff"},{"' + 'name":"葡萄色","bkcolor":"#522f60 ","fcolor":"#ffffff"},{"name":"深紫","bkcolor":"#493759 ","fcolor":"#' +
    'ffffff"},{"name":"紫黒","bkcolor":"#2e2930 ","fcolor":"#ffffff"},{"name":"紫","bkcolor":"#884898 ","' + 'fcolor":"#ffffff"},{"name":"薄葡萄","bkcolor":"#c0a2c7 ","fcolor":"#ffffff"},{"name":"紫紺","bkcolor":"#' + '460e44 ","fcolor":"#ffffff"},{"name":"暗紅色","bkcolor":"#74325c ","fcolor":"#ffffff"},{"name":"桑の実色","' + 'bkcolor":"#55295b ","fcolor":"#ffffff"},{"name":"黄金","bkcolor":"#e6b422","fcolor":"#000000"},{"' +
    'name":"櫨染","bkcolor":"#d9a62e","fcolor":"#000000"},{"name":"黄朽葉色","bkcolor":"#d3a243","fcolor":"#' + '000000"},{"name":"山吹茶","bkcolor":"#c89932 ","fcolor":"#ffffff"},{"name":"芥子色","bkcolor":"#d0af4c ","' + 'fcolor":"#000000"},{"name":"豆がら茶","bkcolor":"#8b968d ","fcolor":"#ffffff"},{"name":"麹塵","bkcolor":"#' + '6e7955 ","fcolor":"#ffffff"},{"name":"山鳩色","bkcolor":"#767c6b ","fcolor":"#ffffff"},{"name":"利休鼠","' +
    'bkcolor":"#888e7e ","fcolor":"#ffffff"},{"name":"海松茶","bkcolor":"#5a544b ","fcolor":"#ffffff"},{"' + 'name":"藍海松茶","bkcolor":"#56564b ","fcolor":"#ffffff"},{"name":"藍媚茶","bkcolor":"#56564b ","fcolor":"#' + 'ffffff"},{"name":"千歳茶","bkcolor":"#494a41 ","fcolor":"#ffffff"},{"name":"岩井茶","bkcolor":"#6b6f59 ","' + 'fcolor":"#ffffff"},{"name":"仙斎茶","bkcolor":"#474b42 ","fcolor":"#ffffff"},{"name":"黒緑","bkcolor":"#' +
    '333631 ","fcolor":"#ffffff"},{"name":"柳煤竹","bkcolor":"#5b6356 ","fcolor":"#ffffff"},{"name":"樺茶色","' + 'bkcolor":"#726250 ","fcolor":"#ffffff"},{"name":"空五倍子色","bkcolor":"#9d896c ","fcolor":"#ffffff"},{"' + 'name":"生壁色","bkcolor":"#94846a ","fcolor":"#ffffff"},{"name":"肥後煤竹","bkcolor":"#897858 ","fcolor":"#' + 'ffffff"},{"name":"媚茶","bkcolor":"#716246 ","fcolor":"#ffffff"},{"name":"白橡","bkcolor":"#cbb994 ","' +
    'fcolor":"#ffffff"},{"name":"亜麻色","bkcolor":"#d6c6af ","fcolor":"#000000"},{"name":"榛色","bkcolor":"#' + 'bfa46f ","fcolor":"#ffffff"},{"name":"灰汁色","bkcolor":"#9e9478 ","fcolor":"#ffffff"},{"name":"利休茶","' + 'bkcolor":"#a59564 ","fcolor":"#ffffff"},{"name":"鶯茶","bkcolor":"#715c1f ","fcolor":"#ffffff"},{"' + 'name":"木蘭色","bkcolor":"#c7b370 ","fcolor":"#ffffff"},{"name":"砂色","bkcolor":"#dcd3b2 ","fcolor":"#' +
    'ffffff"},{"name":"油色","bkcolor":"#a19361 ","fcolor":"#ffffff"},{"name":"利休色","bkcolor":"#8f8667 ","' + 'fcolor":"#ffffff"},{"name":"梅幸茶","bkcolor":"#887938 ","fcolor":"#ffffff"},{"name":"璃寛茶","bkcolor":"#' + '6a5d21 ","fcolor":"#ffffff"},{"name":"黄海松茶","bkcolor":"#918754 ","fcolor":"#ffffff"},{"name":"菜種油色",' + '"bkcolor":"#a69425 ","fcolor":"#ffffff"},{"name":"青朽葉","bkcolor":"#ada250 ","fcolor":"#ffffff"},{"' +
    'name":"根岸色","bkcolor":"#938b4b ","fcolor":"#ffffff"},{"name":"鶸茶","bkcolor":"#8c8861 ","fcolor":"#' + 'ffffff"},{"name":"柳茶","bkcolor":"#a1a46d ","fcolor":"#ffffff"},{"name":"海松色","bkcolor":"#726d40 ","' + 'fcolor":"#ffffff"},{"name":"鶯色","bkcolor":"#928c36 ","fcolor":"#ffffff"},{"name":"緑黄色","bkcolor":"#' + 'dccb18 ","fcolor":"#000000"},{"name":"鶸色","bkcolor":"#d7cf3a ","fcolor":"#000000"},{"name":"抹茶色","' +
    'bkcolor":"#c5c56a ","fcolor":"#ffffff"},{"name":"若草色","bkcolor":"#c3d825 ","fcolor":"#000000"},{"' + 'name":"黄緑","bkcolor":"#b8d200 ","fcolor":"#000000"},{"name":"若芽色","bkcolor":"#e0ebaf","fcolor":"#' + '000000"},{"name":"若菜色","bkcolor":"#d8e698","fcolor":"#000000"},{"name":"若苗色","bkcolor":"#c7dc68","' + 'fcolor":"#000000"},{"name":"青丹","bkcolor":"#99ab4e ","fcolor":"#ffffff"},{"name":"草色","bkcolor":"#' +
    '7b8d42 ","fcolor":"#ffffff"},{"name":"苔色","bkcolor":"#69821b ","fcolor":"#ffffff"},{"name":"萌黄","' + 'bkcolor":"#aacf53","fcolor":"#000000"},{"name":"苗色","bkcolor":"#b0ca71","fcolor":"#ffffff"},{"name":' + '"若葉色","bkcolor":"#b9d08b","fcolor":"#000000"},{"name":"松葉色","bkcolor":"#839b5c ","fcolor":"#ffffff"}' + ',{"name":"夏虫色","bkcolor":"#cee4ae ","fcolor":"#000000"},{"name":"鶸萌黄","bkcolor":"#82ae46 ","fcolor":' +
    '"#ffffff"},{"name":"柳色","bkcolor":"#a8c97f ","fcolor":"#ffffff"},{"name":"青白橡","bkcolor":"#9ba88d ",' + '"fcolor":"#ffffff"},{"name":"柳鼠","bkcolor":"#c8d5bb ","fcolor":"#000000"},{"name":"裏葉柳","bkcolor":"#' + 'c1d8ac ","fcolor":"#000000"},{"name":"山葵色","bkcolor":"#a8bf93 ","fcolor":"#ffffff"},{"name":"老竹色","' + 'bkcolor":"#769164","fcolor":"#ffffff"},{"name":"白緑","bkcolor":"#d6e9ca","fcolor":"#000000"},{"name":' +
    '"淡萌黄","bkcolor":"#93ca76","fcolor":"#ffffff"},{"name":"柳染","bkcolor":"#93b881 ","fcolor":"#ffffff"},' + '{"name":"薄萌葱","bkcolor":"#badcad ","fcolor":"#000000"},{"name":"深川鼠","bkcolor":"#97a791 ","fcolor":"' + '#ffffff"},{"name":"若緑","bkcolor":"#98d98e ","fcolor":"#000000"},{"name":"浅緑","bkcolor":"#88cb7f ","' + 'fcolor":"#ffffff"},{"name":"薄緑","bkcolor":"#69b076","fcolor":"#ffffff"},{"name":"青鈍","bkcolor":"#' +
    '6b7b6e","fcolor":"#ffffff"},{"name":"青磁鼠","bkcolor":"#bed2c3","fcolor":"#000000"},{"name":"薄青","' + 'bkcolor":"#93b69c","fcolor":"#ffffff"},{"name":"錆青磁","bkcolor":"#a6c8b2 ","fcolor":"#ffffff"},{"' + 'name":"緑青色","bkcolor":"#47885e ","fcolor":"#ffffff"},{"name":"千歳緑","bkcolor":"#316745 ","fcolor":"#' + 'ffffff"},{"name":"若竹色","bkcolor":"#68be8d ","fcolor":"#ffffff"},{"name":"緑","bkcolor":"#3eb370 ","' +
    'fcolor":"#ffffff"},{"name":"常磐色","bkcolor":"#007b43 ","fcolor":"#ffffff"},{"name":"千草鼠","bkcolor":"#' + 'bed3ca","fcolor":"#000000"},{"name":"千草色","bkcolor":"#92b5a9 ","fcolor":"#ffffff"},{"name":"青磁色","' + 'bkcolor":"#7ebea5 ","fcolor":"#ffffff"},{"name":"青竹色","bkcolor":"#7ebeab ","fcolor":"#ffffff"},{"' + 'name":"常磐緑","bkcolor":"#028760 ","fcolor":"#ffffff"},{"name":"木賊色","bkcolor":"#3b7960 ","fcolor":"#' +
    'ffffff"},{"name":"天鵞絨","bkcolor":"#2f5d50","fcolor":"#ffffff"},{"name":"虫襖","bkcolor":"#3a5b52 ","' + 'fcolor":"#ffffff"},{"name":"革色","bkcolor":"#475950 ","fcolor":"#ffffff"},{"name":"深緑","bkcolor":"#' + '00552e ","fcolor":"#ffffff"},{"name":"鉄色","bkcolor":"#005243 ","fcolor":"#ffffff"},{"name":"小豆色","' + 'bkcolor":"#96514d","fcolor":"#ffffff"},{"name":"枯茶","bkcolor":"#8d6449","fcolor":"#ffffff"},{"' +
    'name":"饴色","bkcolor":"#deb068","fcolor":"#000000"},{"name":"骆驼色","bkcolor":"#bf794e","fcolor":"#' + 'ffffff"},{"name":"土色","bkcolor":"#bc763c","fcolor":"#ffffff"},{"name":"黄唐色","bkcolor":"#b98c46","' + 'fcolor":"#ffffff"},{"name":"桑染","bkcolor":"#b79b5b","fcolor":"#ffffff"},{"name":"栌色","bkcolor":"#' + 'b77b57","fcolor":"#ffffff"},{"name":"黄橡","bkcolor":"#b68d4c","fcolor":"#ffffff"},{"name":"丁字染","' +
    'bkcolor":"#ad7d4c","fcolor":"#ffffff"},{"name":"香染","bkcolor":"#ad7d4c","fcolor":"#ffffff"},{"name":' + '"枇杷茶","bkcolor":"#ae7c4f","fcolor":"#ffffff"},{"name":"芝翫茶","bkcolor":"#ad7e4e","fcolor":"#ffffff"},' + '{"name":"焦香","bkcolor":"#ae7c58","fcolor":"#ffffff"},{"name":"胡桃色","bkcolor":"#a86f4c","fcolor":"#' + 'ffffff"},{"name":"渋纸色","bkcolor":"#946243","fcolor":"#ffffff"},{"name":"朽葉色","bkcolor":"#917347","' +
    'fcolor":"#ffffff"},{"name":"桑茶","bkcolor":"#956f29","fcolor":"#ffffff"},{"name":"路考茶","bkcolor":"#' + '8c7042","fcolor":"#ffffff"},{"name":"国防色","bkcolor":"#7b6c3e","fcolor":"#ffffff"},{"name":"伽羅色","' + 'bkcolor":"#d8a373","fcolor":"#000000"},{"name":"江戸茶","bkcolor":"#cd8c5c","fcolor":"#ffffff"},{"' + 'name":"樺色","bkcolor":"#cd5e3c","fcolor":"#ffffff"},{"name":"紅鬱金","bkcolor":"#cb8347","fcolor":"#' +
    'ffffff"},{"name":"土器色","bkcolor":"#c37854","fcolor":"#ffffff"},{"name":"狐色","bkcolor":"#c38743","' + 'fcolor":"#ffffff"},{"name":"黄土色","bkcolor":"#c39143","fcolor":"#ffffff"},{"name":"琥珀色","bkcolor":"#' + 'bf783a","fcolor":"#ffffff"},{"name":"赤茶","bkcolor":"#bb5535","fcolor":"#ffffff"},{"name":"代赭","' + 'bkcolor":"#bb5520","fcolor":"#ffffff"},{"name":"煉瓦色","bkcolor":"#b55233","fcolor":"#ffffff"},{"' +
    'name":"雀茶","bkcolor":"#aa4f37","fcolor":"#ffffff"},{"name":"団十郎茶","bkcolor":"#9f563a","fcolor":"#' + 'ffffff"},{"name":"柿渋色","bkcolor":"#9f563a","fcolor":"#ffffff"},{"name":"紅鳶","bkcolor":"#9a493f","' + 'fcolor":"#ffffff"},{"name":"灰茶","bkcolor":"#98623c","fcolor":"#ffffff"},{"name":"茶色","bkcolor":"#' + '965042","fcolor":"#ffffff"},{"name":"檜皮色","bkcolor":"#965036","fcolor":"#ffffff"},{"name":"鳶色","' +
    'bkcolor":"#95483f","fcolor":"#ffffff"},{"name":"柿茶","bkcolor":"#954e2a","fcolor":"#ffffff"},{"name":' + '"弁柄色","bkcolor":"#8f2e14","fcolor":"#ffffff"},{"name":"赤錆色","bkcolor":"#8a3319","fcolor":"#ffffff"},' + '{"name":"褐色","bkcolor":"#8a3b00","fcolor":"#ffffff"},{"name":"栗梅","bkcolor":"#852e19","fcolor":"#' + 'ffffff"},{"name":"紅檜皮","bkcolor":"#7b4741","fcolor":"#ffffff"},{"name":"海老茶","bkcolor":"#773c30","' +
    'fcolor":"#ffffff"},{"name":"唐茶","bkcolor":"#783c1d","fcolor":"#ffffff"},{"name":"栗色","bkcolor":"#' + '762f07","fcolor":"#ffffff"},{"name":"赤銅色","bkcolor":"#752100","fcolor":"#ffffff"},{"name":"錆色","' + 'bkcolor":"#6c3524","fcolor":"#ffffff"},{"name":"赤褐色","bkcolor":"#683f36","fcolor":"#ffffff"},{"' + 'name":"茶褐色","bkcolor":"#664032","fcolor":"#ffffff"},{"name":"栗皮茶","bkcolor":"#6d3c32","fcolor":"#' +
    'ffffff"},{"name":"黒茶","bkcolor":"#583822","fcolor":"#ffffff"},{"name":"葡萄茶","bkcolor":"#6c2c2f","' + 'fcolor":"#ffffff"},{"name":"葡萄色","bkcolor":"#640125","fcolor":"#ffffff"},{"name":"萱草色","bkcolor":"#' + 'f8b862","fcolor":"#000000"},{"name":"柑子色","bkcolor":"#f6ad49","fcolor":"#000000"},{"name":"金茶","' + 'bkcolor":"#f39800","fcolor":"#000000"},{"name":"蜜柑色","bkcolor":"#f08300","fcolor":"#000000"},{"' +
    'name":"鉛丹色","bkcolor":"#ec6d51","fcolor":"#000000"},{"name":"黄丹","bkcolor":"#ee7948","fcolor":"#' + '000000"},{"name":"柿色","bkcolor":"#ed6d3d","fcolor":"#000000"},{"name":"黄赤","bkcolor":"#ec6800","' + 'fcolor":"#000000"},{"name":"人参色","bkcolor":"#ec6800","fcolor":"#000000"},{"name":"橙色","bkcolor":"#' + 'ee7800","fcolor":"#000000"},{"name":"照柿","bkcolor":"#eb6238","fcolor":"#000000"},{"name":"赤橙","' +
    'bkcolor":"#ea5506","fcolor":"#000000"},{"name":"金赤","bkcolor":"#ea5506","fcolor":"#000000"},{"name":' + '"朱色","bkcolor":"#eb6101","fcolor":"#000000"},{"name":"小麦色","bkcolor":"#e49e61","fcolor":"#000000"},{' + '"name":"丹色","bkcolor":"#e45e32","fcolor":"#000000"},{"name":"黄茶","bkcolor":"#e17b34","fcolor":"#' + '000000"},{"name":"肉桂色","bkcolor":"#dd7a56","fcolor":"#000000"},{"name":"赤朽葉色","bkcolor":"#db8449","' +
    'fcolor":"#000000"},{"name":"黄櫨染","bkcolor":"#d66a35","fcolor":"#000000"},{"name":"蒲公英色","bkcolor":"#' + 'ffd900","fcolor":"#000000"},{"name":"黄色","bkcolor":"#ffd900","fcolor":"#000000"},{"name":"中黄","' + 'bkcolor":"#ffea00","fcolor":"#000000"},{"name":"菜の花色","bkcolor":"#ffec47","fcolor":"#000000"},{"' + 'name":"黄檗色","bkcolor":"#fef263","fcolor":"#000000"},{"name":"卵色","bkcolor":"#fcd575","fcolor":"#' +
    '000000"},{"name":"花葉色","bkcolor":"#fbd26b","fcolor":"#000000"},{"name":"刈安色","bkcolor":"#f5e56b","' + 'fcolor":"#000000"},{"name":"玉蜀黍色","bkcolor":"#eec362","fcolor":"#000000"},{"name":"金糸雀色","bkcolor":"' + '#ebd842","fcolor":"#000000"},{"name":"黄支子色","bkcolor":"#ffdb4f","fcolor":"#000000"},{"name":"支子色","' + 'bkcolor":"#fbca4d","fcolor":"#000000"},{"name":"向日葵色","bkcolor":"#fcc800","fcolor":"#000000"},{"' +
    'name":"山吹色","bkcolor":"#f8b500","fcolor":"#000000"},{"name":"鬱金色","bkcolor":"#fabf14","fcolor":"#' + '000000"},{"name":"藤黄","bkcolor":"#f7c114","fcolor":"#000000"},{"name":"金色","bkcolor":"#e6b422","' + 'fcolor":"#000000"},{"name":"桜色","bkcolor":"#bf242a","fcolor":"#000000"},{"name":"薄桜","bkcolor":"#' + 'fdeff2","fcolor":"#000000"},{"name":"桜鼠","bkcolor":"#e9dfe5","fcolor":"#000000"},{"name":"鸨鼠","' +
    'bkcolor":"#e4d2d8","fcolor":"#000000"},{"name":"虹色","bkcolor":"#f6bfbc","fcolor":"#000000"},{"name":' + '"珊瑚色","bkcolor":"#f5b1aa","fcolor":"#000000"},{"name":"宍色","bkcolor":"#efab93","fcolor":"#000000"},{' + '"name":"红梅色","bkcolor":"#f2a0a1","fcolor":"#000000"},{"name":"薄红","bkcolor":"#f0908d","fcolor":"#' + '000000"},{"name":"甚三红","bkcolor":"#ee827c","fcolor":"#000000"},{"name":"桃色","bkcolor":"#f09199","' +
    'fcolor":"#000000"},{"name":"鸨色","bkcolor":"#f4b3c2","fcolor":"#000000"},{"name":"撫子色","bkcolor":"#' + 'eebbcb","fcolor":"#000000"},{"name":"灰梅","bkcolor":"#e8d3c7","fcolor":"#000000"},{"name":"灰桜","' + 'bkcolor":"#e8d3d1","fcolor":"#000000"},{"name":"淡红藤","bkcolor":"#e6cde3","fcolor":"#000000"},{"' + 'name":"石竹色","bkcolor":"#e5abbe","fcolor":"#000000"},{"name":"薄红梅","bkcolor":"#e597b2","fcolor":"#' +
    '000000"},{"name":"桃花色","bkcolor":"#e198b4","fcolor":"#000000"},{"name":"水柿","bkcolor":"#e4ab9b","' + 'fcolor":"#000000"},{"name":"ときがら茶","bkcolor":"#e09e87","fcolor":"#000000"},{"name":"退红","bkcolor":"#' + 'd69090","fcolor":"#000000"},{"name":"薄柿","bkcolor":"#d4acad","fcolor":"#000000"},{"name":"长春色","' + 'bkcolor":"#c97586","fcolor":"#ffffff"},{"name":"梅鼠","bkcolor":"#c099a0","fcolor":"#ffffff"},{"name":' +
    '"鸨浅葱","bkcolor":"#b88884","fcolor":"#ffffff"},{"name":"梅染","bkcolor":"#b48a76","fcolor":"#ffffff"},{' + '"name":"苏芳香","bkcolor":"#a86965","fcolor":"#ffffff"},{"name":"浅苏芳","bkcolor":"#a25768","fcolor":"#' + 'ffffff"},{"name":"真朱","bkcolor":"#ec6d71","fcolor":"#000000"},{"name":"赤紫","bkcolor":"#eb6ea5","' + 'fcolor":"#000000"},{"name":"躑躅色","bkcolor":"#e95295","fcolor":"#000000"},{"name":"牡丹色","bkcolor":"#' +
    'e7609e","fcolor":"#000000"},{"name":"今样色","bkcolor":"#d0576b","fcolor":"#000000"},{"name":"中红","' + 'bkcolor":"#c85179","fcolor":"#ffffff"},{"name":"蔷薇色","bkcolor":"#e9546b","fcolor":"#000000"},{"' + 'name":"韩红","bkcolor":"#e95464","fcolor":"#000000"},{"name":"银朱","bkcolor":"#c85554","fcolor":"#' + 'ffffff"},{"name":"赤红","bkcolor":"#c53d43","fcolor":"#ffffff"},{"name":"红緋","bkcolor":"#e83929","' + 'fcolor":"#000000"},{"name":"赤","bkcolor":"#e60033","fcolor":"#000000"},{"name":"猩緋","bkcolor":"#' +
    'e2041b","fcolor":"#000000"},{"name":"红","bkcolor":"#d7003a","fcolor":"#000000"},{"name":"深緋","' + 'bkcolor":"#c9171e","fcolor":"#000000"},{"name":"绯色","bkcolor":"#d3381c","fcolor":"#000000"},{"name":' + '"赤丹","bkcolor":"#ce5242","fcolor":"#000000"},{"name":"红赤","bkcolor":"#d9333f","fcolor":"#000000"},{"' + 'name":"胭脂","bkcolor":"#b94047","fcolor":"#ffffff"},{"name":"朱緋","bkcolor":"#ba2636","fcolor":"#' +
    'ffffff"},{"name":"茜色","bkcolor":"#b7282e","fcolor":"#ffffff"},{"name":"深海老茶","bkcolor":"#a73836","' + 'fcolor":"#ffffff"},{"name":"苏芳","bkcolor":"#9e3d3f","fcolor":"#ffffff"},{"name":"真红","bkcolor":"#' + 'a22041","fcolor":"#ffffff"},{"name":"浓红","bkcolor":"#a22041","fcolor":"#ffffff"},{"name":"象牙色","' + 'bkcolor":"#f8f4e6","fcolor":"#000000"},{"name":"练色","bkcolor":"#ede4cd","fcolor":"#000000"},{"name":' +
    '"灰白色","bkcolor":"#e9e4d4","fcolor":"#000000"},{"name":"蒸栗色","bkcolor":"#ede1a9","fcolor":"#000000"},' + '{"name":"女郎花","bkcolor":"#f2f2b0","fcolor":"#000000"},{"name":"枯草色","bkcolor":"#e4dc8a","fcolor":"#' + '000000"},{"name":"淡黄","bkcolor":"#f8e58c","fcolor":"#000000"},{"name":"白茶","bkcolor":"#ddbb99","' + 'fcolor":"#000000"},{"name":"赤白橡","bkcolor":"#d7a98c","fcolor":"#000000"},{"name":"洗柿","bkcolor":"#' +
    'f2c9ac","fcolor":"#000000"},{"name":"鸟の子色","bkcolor":"#fff1cf","fcolor":"#000000"},{"name":"蜂蜜色","' + 'bkcolor":"#fddea5","fcolor":"#000000"},{"name":"肌色","bkcolor":"#fce2c4","fcolor":"#000000"},{"name":' + '"薄卵色","bkcolor":"#fde8d0","fcolor":"#000000"},{"name":"雄黄","bkcolor":"#f9c89b","fcolor":"#000000"},{' + '"name":"洒落柿","bkcolor":"#f7bd8f","fcolor":"#000000"},{"name":"赤香","bkcolor":"#f6b894","fcolor":"#' +
    '000000"},{"name":"砥粉色","bkcolor":"#f4dda5","fcolor":"#000000"},{"name":"肉色","bkcolor":"#f1bf99","' + 'fcolor":"#000000"},{"name":"人色","bkcolor":"#f1bf99","fcolor":"#000000"},{"name":"丁子色","bkcolor":"#' + 'efcd9a","fcolor":"#000000"},{"name":"香色","bkcolor":"#efcd9a","fcolor":"#000000"},{"name":"薄香","' + 'bkcolor":"#f0cfa0","fcolor":"#000000"},{"name":"浅黄","bkcolor":"#edd3a1","fcolor":"#000000"},{"name":' +
    '"枯色","bkcolor":"#e0c38c","fcolor":"#000000"},{"name":"淡香","bkcolor":"#f3bf88","fcolor":"#000000"},{"' + 'name":"杏色","bkcolor":"#f7b977","fcolor":"#000000"},{"name":"东云色","bkcolor":"#f19072","fcolor":"#' + '000000"},{"name":"曙色","bkcolor":"#f19072","fcolor":"#000000"},{"name":"珊瑚朱色","bkcolor":"#ee836f","' + 'fcolor":"#000000"},{"name":"深支子","bkcolor":"#eb9b6f","fcolor":"#000000"},{"name":"纁","bkcolor":"#' +
    'e0815e","fcolor":"#000000"},{"name":"浅绯","bkcolor":"#df7163","fcolor":"#000000"},{"name":"真赭","' + 'bkcolor":"#d57c6b","fcolor":"#000000"},{"name":"洗朱","bkcolor":"#d0826c","fcolor":"#000000"},{"name":' + '"遠州茶","bkcolor":"#ca8269","fcolor":"#ffffff"},{"name":"红桦色","bkcolor":"#bb5548","fcolor":"#ffffff"},' + '{"name":"赭","bkcolor":"#ab6953","fcolor":"#ffffff"}]';


var
  CNColorJson: TJSONArray;
  JPColorJson: TJSONArray;


procedure GetJson(var Json: TJSONArray; ColorJsonTxt: string);

// html颜色字符串转TAlphaColor
function HtmlToColor(WebColor: string): TAlphaColor;

// procedure BootstrapClassToColors(const AClass: string; const AAppearance: string; var AFontColor, AButtonColor: TAlphaColor; out AOutLine: Boolean);

implementation

const
  OffsetValue: array[Boolean] of Integer = (0, 1);


procedure GetJson(var Json: TJSONArray; ColorJsonTxt: string);
begin
  if not Assigned(Json) then
    Json := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(ColorJsonTxt), 0) as TJSONArray;
end;

function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
begin
  Result := Round(nNumber * nNumerator / nDenominator);
end;

// 颜色是否是一个亮色
function IsLightColor(Color: TAlphaColor): Boolean;
var
  r, g, b: Byte;
  luminance: Double;
begin
  // 获取颜色的RGB分量
  r := TAlphaColorRec(Color).r;
  g := TAlphaColorRec(Color).g;
  b := TAlphaColorRec(Color).b;
  // 计算颜色的亮度
  luminance := (0.299 * r + 0.587 * g + 0.114 * b) / 255;
  // 判断亮度是否大于等于0.5，大于等于0.5表示为亮色，否则为暗色
  Result := luminance >= 0.5;
end;

// html颜色字符串转TAlphaColor
function HtmlToColor(WebColor: string): TAlphaColor;
var
  I: Integer;
  Offset: Integer;
begin
  WebColor := WebColor.Trim;
  if (Length(WebColor) < 6) or (Length(WebColor) > 7) then
    raise Exception.Create('Invalid Color string');
  for I := 1 to Length(WebColor) do
    if not CharInSet(WebColor[I], ['#', 'a'..'f', 'A'..'F', '0'..'9']) then { do not localize }
      raise Exception.Create('Invalid Color string');
  Offset := OffsetValue[Pos('#', WebColor) = 1];
  // Result := RGB(StrToInt('$' + Copy(WebColor, 1 + Offset, 2)),                             { do not localize }
  // StrToInt('$' + Copy(WebColor, 3 + Offset, 2)), StrToInt('$' + Copy(WebColor, 5 + Offset, 2)));  { do not localize }
  Result := MakeColor(StrToInt('$' + Copy(WebColor, 1 + Offset, 2)), StrToInt('$' + Copy(WebColor, 3 + Offset, 2)), StrToInt('$' + Copy(WebColor, 5 + Offset, 2)), 255);
end;

// 颜色加亮
function LightenColor(Color: TAlphaColor; Percent: Integer): TAlphaColor;
var
  r, g, b: Byte;
begin
  // Color:=TAlphaColorRec(Color).r;
  r := TAlphaColorRec(Color).r;
  g := TAlphaColorRec(Color).g;
  b := TAlphaColorRec(Color).b;
  r := r + MulDiv(255 - r, Percent, 100); // Percent% closer to white
  g := g + MulDiv(255 - g, Percent, 100);
  b := b + MulDiv(255 - b, Percent, 100);
  Result := MakeColor(r, g, b, TAlphaColorRec(Color).A);
end;

// 颜色变暗
function DarkenColor(Color: TAlphaColor; Percent: Integer): TAlphaColor;
var
  r, g, b: Byte;
begin
  r := TAlphaColorRec(Color).r;
  g := TAlphaColorRec(Color).g;
  b := TAlphaColorRec(Color).b;
  r := r - MulDiv(r, Percent, 100); // Percent% closer to black
  g := g - MulDiv(g, Percent, 100);
  b := b - MulDiv(b, Percent, 100);
  Result := MakeColor(r, g, b, TAlphaColorRec(Color).A);
end;

// Button and Font Colors for Light Themes (DeepPurpleAndAmber)

{ TbuttonViewHelper }

procedure TTextViewHelper.SetButtonStyle(const AFamily: string; const AClass: string; const AAppearance: string);
begin
  TTextviewstyle.SetButtonStyle(self, AFamily, AClass, AAppearance);
end;

procedure TTextViewHelper.SetTextViewStyle(const AFamily, AClass: string);
begin
  TTextviewstyle.SetTextViewStyle(self, AFamily, AClass);
end;

{ Tbuttonviewstyle }

class procedure TTextviewstyle.AngularClassToDarkColors(const AClass, AAppearance: string; var AFontColor, AButtonColor: TAlphaColor);
begin
  if SameText(AClass, btn_Basic) then
  begin
    AButtonColor := HtmlToColor('#424242');
    AFontColor := HtmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_PrimaryPink) then
  begin
    AButtonColor := HtmlToColor('#E91E63');
    AFontColor := HtmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_AccentBlueGray) then
  begin
    AButtonColor := HtmlToColor('#607D8B');
    AFontColor := HtmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_PrimaryPurple) then
  begin
    AButtonColor := HtmlToColor('#9C27B0');
    AFontColor := HtmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_AccentGreen) then
  begin
    AButtonColor := HtmlToColor('#69F0AE');
    AFontColor := HtmlToColor('#0E1F17');
  end
  else if SameText(AClass, btn_Warn) then
  begin
    AButtonColor := HtmlToColor('#F44336');
    AFontColor := HtmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_Link) then
  begin
    AButtonColor := HtmlToColor('#424242');
    AFontColor := HtmlToColor('#FFFFFF');
  end;
end;

class procedure TTextviewstyle.AngularClassToLightColors(const AClass, AAppearance: string; var AFontColor, AButtonColor: TAlphaColor);
begin
  // Button and Font Colors for Light Themes (DeepPurpleAndAmber)
  if SameText(AClass, btn_Basic) then
  begin
    AButtonColor := HtmlToColor('#FFFFFF');
    AFontColor := HtmlToColor('#212121');
  end
  else if SameText(AClass, btn_PrimaryDeepPurple) then
  begin
    AButtonColor := HtmlToColor('#673AB7');
    AFontColor := HtmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_AccentAmber) then
  begin
    AButtonColor := HtmlToColor('#FFD740');
    AFontColor := HtmlToColor('#211C08');
  end
  else if SameText(AClass, btn_PrimaryIndigo) then
  begin
    AButtonColor := HtmlToColor('#3F51B5');
    AFontColor := HtmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_AccentPink) then
  begin
    AButtonColor := HtmlToColor('#FF4081');
    AFontColor := HtmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_Warn) then
  begin
    AButtonColor := HtmlToColor('#F44336');
    AFontColor := HtmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_Link) then
  begin
    AButtonColor := HtmlToColor('#FFFFFF');
    AFontColor := HtmlToColor('#212121');
  end;
end;

class procedure TTextviewstyle.BootstrapClassToColors(const AClass, AAppearance: string; var AFontColor, AButtonColor: TAlphaColor; out AOutLine: Boolean);
const
  // from bootstrap css
  bs_blue = '#0d6efd';
  bs_indigo = '#6610f2';
  bs_purple = '#6f42c1';
  bs_pink = '#d63384';
  bs_red = '#dc3545';
  bs_orange = '#fd7e14';
  bs_yellow = '#ffc107';
  bs_green = '#198754';
  bs_teal = '#20c997';
  bs_cyan = '#0dcaf0';
  bs_white = '#fff';
  bs_gray = '#6c757d';
  bs_gray_dark = '#343a40';
  bs_primary = '#0d6efd';
  bs_secondary = '#6c757d';
  bs_success = '#198754';
  bs_info = '#0dcaf0';
  bs_warning = '#ffc107';
  bs_danger = '#dc3545';
  bs_light = '#f8f9fa';
  bs_dark = '#212529';
begin
  AOutLine := SameText(AAppearance, BOOTSTRAP_OUTLINE);

  if SameText(AClass, btn_primary) then
  begin
    AButtonColor := HtmlToColor(bs_primary);
    // AButtonColor := TAlphaColors.Blue;
    AFontColor := TAlphaColors.White;
  end
  else if SameText(AClass, btn_secondary) then
  begin
    AButtonColor := HtmlToColor(bs_secondary);
    AFontColor := TAlphaColors.White;
  end
  else if SameText(AClass, btn_success) then
  begin
    AButtonColor := HtmlToColor(bs_success);
    AFontColor := TAlphaColors.White;
  end
  else if SameText(AClass, btn_danger) then
  begin
    AButtonColor := HtmlToColor(bs_danger);
    AFontColor := TAlphaColors.White;
  end
  else if SameText(AClass, btn_warning) then
  begin
    AButtonColor := HtmlToColor(bs_warning);
    AFontColor := HtmlToColor('#212529');
  end
  else if SameText(AClass, btn_info) then
  begin
    AButtonColor := HtmlToColor(bs_info);
    AFontColor := HtmlToColor('#212529');
  end
  else if SameText(AClass, btn_light) then
  begin
    AButtonColor := HtmlToColor(bs_light);
    AFontColor := HtmlToColor('#212529');
  end
  else if SameText(AClass, btn_dark) then
  begin
    AButtonColor := HtmlToColor(bs_dark);
    AFontColor := TAlphaColors.White;
  end
  else
  begin
    AOutLine := False;
    AButtonColor := $FFFAFAFA;
    AFontColor := TAlphaColors.White;
  end;
end;

class function TTextviewstyle.GetFAMILYAAppearance(AFamily: string): Tarray<string>;
begin
  if SameText(BOOTSTRAP_FAMILY, AFamily) then
  begin
    Result := ['Normal', 'Outline'];
  end
  else if SameText(ANGULAR_LIGHT_FAMILY, AFamily) or SameText(ANGULAR_DARK_FAMILY, AFamily) then
  begin
    Result := ['Flat', 'Raised', 'Basic', 'Stroked'];
  end;
end;

class function TTextviewstyle.GetFAMILYAllClass(AFamily: string): Tarray<string>;
var
  I: Integer;
begin
  if SameText(BOOTSTRAP_FAMILY, AFamily) then
  begin
    SetLength(Result, Length(BootstrapbtnKinds));
    for I := Low(BootstrapbtnKinds) to High(BootstrapbtnKinds) do
    begin
      Result[I] := BootstrapbtnKinds[I];
    end;
  end
  else if SameText(ANGULAR_LIGHT_FAMILY, AFamily) then
  begin
    SetLength(Result, Length(AngularbtnKinds));
    for I := Low(AngularbtnKinds) to High(AngularbtnKinds) do
    begin
      Result[I] := AngularbtnKinds[I];
    end;
  end
  else if SameText(ANGULAR_DARK_FAMILY, AFamily) then
  begin
    SetLength(Result, Length(AngularDarkbtnKinds));
    for I := Low(AngularDarkbtnKinds) to High(AngularDarkbtnKinds) do
    begin
      Result[I] := AngularDarkbtnKinds[I];
    end;
  end;
end;

class function TTextviewstyle.GetFAMILYClassCount(AFamily: string): Integer;
begin
  Result := Length(GetFAMILYAllClass(AFamily));
end;

class function TTextviewstyle.GetFAMILYCount: Integer;
begin
  Result := Length(GetFAMILYs);
end;

class function TTextviewstyle.GetFAMILYI(idx: Integer): string;
begin
  Result := BOOTSTRAP_FAMILY;
  case idx of
    0:
      begin
        Result := BOOTSTRAP_FAMILY;
      end;
    1:
      begin
        Result := ANGULAR_LIGHT_FAMILY;
      end;
    2:
      begin
        Result := ANGULAR_DARK_FAMILY;
      end;
  else
    begin
      Result := BOOTSTRAP_FAMILY;
    end;
  end;
end;

class function TTextviewstyle.GetAAppearance(AFamily: string; idx: Integer): string;
var
  arr: Tarray<string>;
begin
  arr := GetFAMILYAAppearance(AFamily);
  idx := EnsureRange(idx, 0, high(arr));
  Result := arr[idx];
end;

class function TTextviewstyle.GetAllClassFromFAMILYidx(AFamilyidx: Integer): Tarray<string>;
begin
  Result := GetFAMILYAllClass(GetFAMILYI(AFamilyidx));
end;

class function TTextviewstyle.GetClass(AFamily: string; idx: Integer): string;
var
  arr: Tarray<string>;
begin
  arr := GetFAMILYAllClass(AFamily);
  idx := EnsureRange(idx, 0, high(arr));
  Result := arr[idx];
end;

class function TTextviewstyle.GetFAMILYs: Tarray<string>;
begin
  Result := [BOOTSTRAP_FAMILY, ANGULAR_LIGHT_FAMILY, ANGULAR_DARK_FAMILY];
end;

class procedure TTextviewstyle.setAngularDark(button: TTextView; AClass, AAppearance: string);
var
  LFontColor, LButtonColor: TAlphaColor;
  LRaised, LStroked, LBasic: Boolean;
  LPrimaryAccentWarn: Boolean;
  Border: TViewBorder;
  C1, C2, C3: TAlphaColor;
  color20, color50: TAlphaColor;
begin
  // 通过样式名称得到按钮颜色和文字颜色
  AngularClassToDarkColors(AClass, AAppearance, LFontColor, LButtonColor);
  Border := TDrawableBorder(button.Background).Border;

  // 默认样式: Flat
  // 使用Flat Style为基础
  Border.Width := 0; // 没有外框
  button.TextSettings.Color.Default := LFontColor;
  button.TextSettings.Font.Style := [Tfontstyle.fsBold]; // 文字加粗

  LStroked := SameText(AAppearance, StrokedAttr);
  LRaised := SameText(AAppearance, RaisedAttr);
  LPrimaryAccentWarn := SameText(AClass, btn_PrimaryPink) or SameText(AClass, btn_PrimaryPurple) or SameText(AClass, btn_AccentBlueGray) or SameText(AClass, btn_AccentGreen) or SameText(AClass, btn_Warn);
  LBasic := SameText(AAppearance, BasicAttr);

  with button.Background do
  begin
    ItemDefault.Color := LButtonColor;
    ItemDefault.Kind := TViewBrushKind.Solid;
    C1 := LButtonColor;
    C2 := LFontColor;
    C3 := $FFCCCCCC;

    if LStroked then
    begin
      Border.Width := ANGULAR_STROKED_WIDTH;
      ItemDefault.Kind := TViewBrushKind.None;
      // Only for Primary, Accent and Warn, Stroked FontColor as Button Color
      C1 := TAlphaColors.Null;

      // Only for Primary, Accent and Warn, Stroked FontColor as Button Color
      if LPrimaryAccentWarn then
        C2 := LButtonColor
      else
        C2 := LFontColor;
    end
    else if LRaised then
    begin
      Border.Width := ANGULAR_RAISED_WIDTH;
      C1 := LButtonColor;
      C2 := LFontColor;
      C3 := DarkenColor(LButtonColor, 20);
    end
    else if LBasic then
    begin
      C1 := TAlphaColors.Null;
      C3 := TAlphaColors.Null;
      // Only for Primary, Accent and Warn, Stroked FontColor as Button Color
      if LPrimaryAccentWarn then
        C2 := LButtonColor
      else
        C2 := LFontColor;
    end;
    SetBtnKindColor(button, TViewState.None, C1, C2, C3);

    // btnbrush := ItemDefault;

    if LStroked or LBasic then
    begin
      // Button Hot: Button Color 50% ligthen of Font Color
      SetBtnKindColor(button, TViewState.Hovered, DarkenColor(C2, 50), C2, C3);

      // Button Pressed: Button Color 40% ligthen of Font Color
      SetBtnKindColor(button, TViewState.Pressed, DarkenColor(C2, 40), DarkenColor(C2, 50), C2);

      // Button Selected: Button Color 60% ligthen of Font Color
      SetBtnKindColor(button, TViewState.Selected, DarkenColor(C2, 60), C2, C3);

      SetBtnKindColor(button, TViewState.Focused, DarkenColor(C2, 60), C2, DarkenColor(C3, 50));
    end
    else
    begin
      // Flat and Raised Appearance
      if IsLightColor(LButtonColor) then
        color20 := DarkenColor(C1, 20)
      else
        color20 := LightenColor(C1, 20);

      if IsLightColor(LButtonColor) then
        color50 := DarkenColor(C1, 50)
      else
        color50 := LightenColor(C1, 50);

      SetBtnKindColor(button, TViewState.Hovered, color20, C2, C3);
      SetBtnKindColor(button, TViewState.Pressed, color50, C2, C1);
      SetBtnKindColor(button, TViewState.Selected, LightenColor(C1, 50), C2, C3);
      SetBtnKindColor(button, TViewState.Focused, C1, C2, C3);
    end;
  end;
end;

class procedure TTextviewstyle.setAngularlight(button: TTextView; AClass, AAppearance: string);
var
  LFontColor, LButtonColor: TAlphaColor;
  LRaised, LStroked, LBasic: Boolean;
  LPrimaryAccentWarn: Boolean;
  Border: TViewBorder;
  C1, C2, C3: TAlphaColor;
  color20, color50: TAlphaColor;
begin
  // 通过样式名称得到按钮颜色和文字颜色
  AngularClassToLightColors(AClass, AAppearance, LFontColor, LButtonColor);
  Border := TDrawableBorder(button.Background).Border;

  // 默认样式: Flat
  // 使用Flat Style为基础
  Border.Width := 0; // 没有外框
  button.TextSettings.Color.Default := LFontColor;
  button.TextSettings.Font.Style := [Tfontstyle.fsBold]; // 文字加粗

  LStroked := SameText(AAppearance, StrokedAttr);
  LRaised := SameText(AAppearance, RaisedAttr);
  LPrimaryAccentWarn := SameText(AClass, btn_PrimaryDeepPurple) or SameText(AClass, btn_PrimaryIndigo) or SameText(AClass, btn_AccentAmber) or SameText(AClass, btn_AccentPink) or SameText(AClass, btn_Warn);
  LBasic := SameText(AAppearance, BasicAttr);

  with button.Background do
  begin
    ItemDefault.Color := LButtonColor;
    ItemDefault.Kind := TViewBrushKind.Solid;
    C1 := LButtonColor;
    C2 := LFontColor;
    C3 := $FFCCCCCC;

    if LStroked then
    begin
      Border.Width := ANGULAR_STROKED_WIDTH;
      C1 := TAlphaColors.Null;

      // Only for Primary, Accent and Warn, Stroked FontColor as Button Color
      if LPrimaryAccentWarn then
        C2 := LButtonColor
      else
        C2 := LFontColor;
    end
    else if LRaised then
    begin
      Border.Width := ANGULAR_RAISED_WIDTH;
      C1 := LButtonColor;
      C2 := LFontColor;
      C3 := DarkenColor(LButtonColor, 20);
    end
    else if LBasic then
    begin
      C1 := TAlphaColors.Null;
      C3 := TAlphaColors.Null;
      // Only for Primary, Accent and Warn, Stroked FontColor as Button Color
      if LPrimaryAccentWarn then
        C2 := LButtonColor
      else
        C2 := LFontColor;
    end;
    SetBtnKindColor(button, TViewState.None, C1, C2, C3);

    // btnbrush := ItemDefault;

    if LStroked or LBasic then
    begin
      // Button Hot: Button Color 50% ligthen of Font Color
      SetBtnKindColor(button, TViewState.Hovered, LightenColor(C2, 50), C2, C3);

      // Button Pressed: Button Color 40% ligthen of Font Color
      SetBtnKindColor(button, TViewState.Pressed, LightenColor(C2, 40), LightenColor(C2, 50), C2);

      // Button Selected: Button Color 60% ligthen of Font Color
      SetBtnKindColor(button, TViewState.Selected, LightenColor(C2, 50), C2, C3);

      SetBtnKindColor(button, TViewState.Focused, LightenColor(C2, 50), C2, LightenColor(C3, 50));
    end
    else
    begin
      if IsLightColor(LButtonColor) then
        color20 := DarkenColor(C1, 20)
      else
        color20 := LightenColor(C1, 20);

      if IsLightColor(LButtonColor) then
        color50 := DarkenColor(C1, 50)
      else
        color50 := LightenColor(C1, 50);

      SetBtnKindColor(button, TViewState.Hovered, color20, C2, C3);
      SetBtnKindColor(button, TViewState.Pressed, color50, C2, C1);
      SetBtnKindColor(button, TViewState.Selected, color50, C2, C3);
      SetBtnKindColor(button, TViewState.Focused, C1, C2, C3);
    end;
  end;
end;

class procedure TTextviewstyle.setBootstrap(button: TTextView; AClass, AAppearance: string);
var
  LFontColor, LButtonColor: TAlphaColor;
  LOutLine: Boolean;
  Border: TViewBorder;
  C1, C2, C3: TAlphaColor;
  color20, color40, color50: TAlphaColor;
begin
  // 通过样式名称得到按钮颜色和文字颜色
  BootstrapClassToColors(AClass, AAppearance, LFontColor, LButtonColor, LOutLine);
  Border := TDrawableBorder(button.Background).Border; // 得到边框
  button.TextSettings.Font.Style := [Tfontstyle.fsBold]; // 默认文字加粗

  Border.Width := 3; // 默认外框设为2，默认颜色为透明
  Border.Color.Default := TAlphaColors.Null;
  Border.Kind := TBrushKind.Solid;

  // 默认状态下样式
  with button.Background do
  begin
    if LOutLine then
    begin // 外框线,内部按钮为透明色
      C1 := TAlphaColors.Null;
      C2 := LButtonColor;
      C3 := LButtonColor;
    end
    else
    begin
      C1 := LButtonColor;
      C2 := LFontColor;
      C3 := TAlphaColors.Null;

    end;
    SetBtnKindColor(button, TViewState.None, C1, C2, C3);

    // 复制默认背景色设置到其他按钮
    SetBtnKindColor(button, TViewState.Pressed, C1, C2, C3);
    SetBtnKindColor(button, TViewState.Focused, C1, C2, C3);
    SetBtnKindColor(button, TViewState.Hovered, C1, C2, C3);
    SetBtnKindColor(button, TViewState.Selected, C1, C2, C3);

    if IsLightColor(LButtonColor) then
      color20 := DarkenColor(LButtonColor, 20)
    else
      color20 := LightenColor(LButtonColor, 20);

    if IsLightColor(LButtonColor) then
      color40 := DarkenColor(LButtonColor, 40)
    else
      color40 := LightenColor(LButtonColor, 40);

    if IsLightColor(LButtonColor) then
      color50 := DarkenColor(LButtonColor, 50)
    else
      color50 := LightenColor(LButtonColor, 50);

    if LOutLine then
    begin
      SetBtnKindColor(button, TViewState.Pressed, LButtonColor, LFontColor, color50);
      SetBtnKindColor(button, TViewState.Hovered, LButtonColor, LFontColor, TAlphaColors.Null);
      SetBtnKindColor(button, TViewState.Focused, LButtonColor, LFontColor, color20);
    end
    else
    begin
      SetBtnKindColor(button, TViewState.Pressed, color20, LFontColor, color50);
      // 鼠标悬停按钮的效果和按下一致
      SetBtnKindColor(button, TViewState.Hovered, color20, LFontColor, color40);
      SetBtnKindColor(button, TViewState.Focused, color20, LFontColor, color20);
    end;
  end;
end;

class procedure TTextviewstyle.SetBtnKindColor(button: TTextView; ViewState: TViewState; btncolor, FontColor, BorderColor: TAlphaColor);
var
  Border: TViewBorder;
begin
  Border := TDrawableBorder(button.Background).Border; // 得到边框
  Border.Color.SetColor(ViewState, BorderColor);
  button.TextSettings.Color.SetColor(ViewState, FontColor);
  button.Background.SetColor(ViewState, btncolor);
end;

class procedure TTextviewstyle.SetButtonStyle(button: TTextView; const AFamily, AClass, AAppearance: Integer);
var
  AFamilystr, AClassstr, AAppearancestr: string;
begin
  AFamilystr := GetFAMILYI(AFamily);
  AClassstr := GetFAMILYAllClass(AFamilystr)[AClass];
  AAppearancestr := GetFAMILYAAppearance(AFamilystr)[AAppearance];
  SetButtonStyle(button, AFamilystr, AClassstr, AAppearancestr);
end;

class procedure TTextviewstyle.SetTextViewStyle(TextView: TTextView; const AFamily, AClass: string);
var
  I: Integer;
  Json: TJSONObject;
  fcolorstr: string;
  bkcolorstr: string;
  Border: TViewBorder;
begin
  TextView.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  TextView.TextSettings.Font.Style := [Tfontstyle.fsBold]; // 默认文字加粗
  TextView.TextSettings.Gravity := TLayoutGravity.Center;

  Border := TDrawableBorder(TextView.Background).Border; // 得到边框
  Border.Style := TViewBorderStyle.RectBorder;

  with TextView.Background do
  begin
    if SameText('CN', AFamily) then
    begin
      GetJson(CNColorJson, CNColorJsonTxt);
      for I := 0 to CNColorJson.Count - 1 do
      begin
        Json := CNColorJson.Items[I] as TJSONObject;
        if SameText(Json.GetValue<string>('name'), AClass) then
        begin
          bkcolorstr := Json.s['bkcolor'];
          fcolorstr := Json.s['fcolor'];
          ItemDefault.Color := HtmlToColor(bkcolorstr);
          TextView.TextSettings.Color.Default := HtmlToColor(fcolorstr);
          Break;
        end;
      end;
    end
    else if SameText('JP', AFamily) then
    begin
      GetJson(JPColorJson, JPColorjsonTxt);
      for I := 0 to JPColorJson.Count - 1 do
      begin
        Json := JPColorJson.Items[I] as TJSONObject;
        if SameText(Json.GetValue<string>('name'), AClass) then
        begin
          bkcolorstr := Json.s['bkcolor'];
          fcolorstr := Json.s['fcolor'];
          ItemDefault.Color := HtmlToColor(bkcolorstr);
          TextView.TextSettings.Color.Default := HtmlToColor(fcolorstr);
          Break;
        end;
      end;
    end
  end;
end;

class procedure TTextviewstyle.SetButtonStyle(button: TTextView; const AFamily, AClass, AAppearance: string);
var
  Border: TViewBorder;
begin
  // 通用样式修改
  // 圆角
  // if button is TButtonView then
  begin
    button.Background.XRadius := 5;
    button.Background.yRadius := 5;
  end;
  button.TextSettings.Gravity := TLayoutGravity.Center;

  Border := TDrawableBorder(button.Background).Border; // 得到边框
  Border.Style := TViewBorderStyle.RectBorder;

  if SameText(BOOTSTRAP_FAMILY, AFamily) then
  begin
    setBootstrap(button, AClass, AAppearance);
  end
  else if SameText(ANGULAR_LIGHT_FAMILY, AFamily) then
  begin
    setAngularlight(button, AClass, AAppearance);
  end
  else if SameText(ANGULAR_DARK_FAMILY, AFamily) then
  begin
    setAngularDark(button, AClass, AAppearance);
  end;
end;

end.

