unit CustomList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Generics.Collections, System.Threading, System.Net.URLClient,
  UI.Base, UI.Standard, UI.Frame, UI.ListView;

type
  TDataItem = record
    Title: string;
    SubTitle: string;
    Hint: string;
    ImagePath: string;
    ImageUrl: string;
    IsLarge: Boolean;
    Height: Single;
  end;

  TCustomListDataAdapter = class(TListAdapterBase)
  private
    [Weak] FList: TList<TDataItem>;
    FCancel: Boolean;
    procedure UpdateImage(const Index: Integer; ViewItem: TObject);
    procedure DoReceiveDataEvent(const Sender: TObject; AContentLength,
      AReadCount: Int64; var Abort: Boolean);
  protected
    function GetCount: Integer; override;
    function GetItem(const Index: Integer): Pointer; override;
    function IndexOf(const AItem: Pointer): Integer; override;
    function GetView(const Index: Integer; ConvertView: TViewBase;
      Parent: TViewGroup): TViewBase; override;
    function ItemDefaultHeight: Single; override;
    procedure ItemMeasureHeight(const Index: Integer; var AHeight: Single); override;
  public
    constructor Create(const AList: TList<TDataItem>);
    procedure Cancel;
  end;

  TfrmCustomList = class(TFrame)
    ListView: TListViewEx;
    procedure ListViewPullLoad(Sender: TObject);
    procedure ListViewPullRefresh(Sender: TObject);
    procedure ListViewScrollChange(Sender: TObject);
    procedure ListViewItemClick(Sender: TObject; ItemIndex: Integer;
      const ItemView: TControl);
  private
    { Private declarations }
    FAdapter: TCustomListDataAdapter;
    FList: TList<TDataItem>;
    FThreadPool: TThreadPool;
    FAdding: Boolean;
  protected
    procedure DoCreate(); override;
    procedure DoFree(); override;
    procedure DoShow(); override;
  public
    { Public declarations }
    procedure AddItems(const Count: Integer);
  end;

var
  frmCustomList: TfrmCustomList;

implementation

{$R *.fmx}

uses
  System.Net.HttpClient,
  BaseListItem, ListItem_TextImage, ListItem_Image, uFrameImageViewer,
  UI.Async;

procedure ValidateServerCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate;
  var Accepted: Boolean);
begin
  Accepted := True;
end;

{ TfrmCustomList }

////随机竖版图片展示
//https://api.uomg.com/api/rand.img2?sort=美女&format=json
////随机横版图片展示
//https://api.uomg.com/api/rand.img1?sort=美女&format=json
//https://api.uomg.com/api/image.lofter?format=json

procedure TfrmCustomList.AddItems(const Count: Integer);

  procedure DoAdd(S: string; ALarge: Boolean; AErr: string = '');
  var
    I: Integer;
    Item: TDataItem;
  begin
    I := FList.Count + 1;

    if S = '' then begin
      Item.Title := '错误';
      Item.SubTitle := AErr;
      Item.Hint := '';
      Item.ImageUrl := '';
      Item.IsLarge := ALarge;
    end
    else begin
      Item.Title := '标题' + IntToStr(I);
      Item.SubTitle := '副标题' + IntToStr(I);
      if ALarge then
        Item.Hint := '大预览'
      else
        Item.Hint := '小预览';
      Item.ImageUrl := S;
      Item.IsLarge := ALarge;
    end;

    Item.Height := -1;

    FList.Add(Item);
  end;

  function GetUrl(ALarge: Boolean): string;
  begin
    if ALarge then
      Result := 'https://api.uomg.com/api/rand.img2?sort=美女&format=text'
    else
      Result := 'https://api.uomg.com/api/rand.img1?sort=二次元&format=text';
  end;

  procedure DoHttp(ALarge: Boolean);
  var
    LStream: TMemoryStream;
    LHttp: THttpClient;
    LResponse: IHTTPResponse;
  begin
    LHttp := THttpClient.Create;
    try
      try
        LHttp.HandleRedirects := False;
        {$IFDEF MSWINDOWS}
        // ssl protocol
        // win7 32 will get error if not use TLS12
        // https://social.msdn.microsoft.com/Forums/en-US/b27a9ddd-d8f7-408c-8029-cf5f8f9ddbef/winhttp-winhttpcallbackstatusflagsecuritychannelerror-on-win7?forum=vcgeneral
        LHttp.SecureProtocols := [THTTPSecureProtocol.TLS12];
        {$ENDIF}
        // ignore ssl error
        LHttp.ValidateServerCertificateCallback := ValidateServerCertificate;
        LResponse := LHttp.Get(GetUrl(ALarge), nil);

        if not Assigned(LResponse) then
          DoAdd('', False, '没有返回值')
        else if (LResponse.StatusCode < 300) or (LResponse.StatusCode >= 400) or (LResponse.HeaderValue['location'] = '') then
          DoAdd('', False, '接口内容不合法')
        else
          DoAdd(LResponse.HeaderValue['location'], ALarge);
      except
        on E: Exception do begin
          DoAdd('', False, E.Message);
          //自己处理异常
          Exit;
        end;
      end;
    finally
      FreeAndNil(LHttp);
    end;
  end;

var
  I: Integer;
begin
  //避免重复调用
  if FAdding then
    Exit;

  FAdding := True;
  try
    for I := 0 to Count - 1 do
      DoHttp((Flist.Count + 1) mod 2 = 0);
  finally
    FAdding := False;
  end;

  TThread.Queue(nil, procedure begin
    FAdapter.NotifyDataChanged;
  end);
end;

procedure TfrmCustomList.DoCreate;
begin
  inherited;

  FList := TList<TDataItem>.Create();
  FAdapter := TCustomListDataAdapter.Create(FList);
  ListView.Adapter := FAdapter;

  FThreadPool := TThreadPool.Create;
  FThreadPool.SetMaxWorkerThreads(2);
  FThreadPool.SetMinWorkerThreads(0);
end;

procedure TfrmCustomList.DoFree;
begin
  FAdapter.Cancel;
  FThreadPool.Free;

  ListView.Adapter := nil;
  FAdapter := nil;
  FreeAndNil(FList);

  inherited;
end;

procedure TfrmCustomList.DoShow;
begin
  inherited;

  //ListView.ColumnCount := 3;
  ShowWaitDialog('加载中...', False);
  TAsync.Create()
  .SetExecute(
    procedure (Async: TAsync) begin
      AddItems(10);
    end
  )
  .SetExecuteComplete(
    procedure (Async: TAsync) begin
      HideWaitDialog;
    end
  ).Start;
end;

procedure TfrmCustomList.ListViewItemClick(Sender: TObject; ItemIndex: Integer;
  const ItemView: TControl);

  function DoAssgin(BMP: TBitmap): Integer;
  begin
      with TFrameImageViewer(StartFrame(TFrameImageViewer)) do begin
        ImageViewerEx1.Image.Assign(BMP);
        ImageViewerEx1.Zoom := Trunc(Width * 100 / BMP.Width);
      end;
  end;

var
  LView: TBaseListItemFrame;
begin
  if ItemView is TBaseListItemFrame then begin
    LView := TBaseListItemFrame(ItemView);
    if Assigned(LView.Image) then
      if not LView.Image.Image.ItemDefault.Bitmap.Bitmap.IsEmpty then
        DoAssgin(LView.Image.Image.ItemDefault.Bitmap.Bitmap)
      else begin
        Hint('重新加载中');
        FAdapter.UpdateImage(LView.ItemIndex, ItemView);
      end;
  end;
end;

procedure TfrmCustomList.ListViewPullLoad(Sender: TObject);
begin
  DelayExecute(1,
    procedure (Sender: TObject)
    begin
      AddItems(20);
      if FList.Count > 50 then
        ListView.EnablePullLoad := False;
      ListView.PullLoadComplete;
    end
  );
end;

procedure TfrmCustomList.ListViewPullRefresh(Sender: TObject);
begin
  Hint('正在加载数据');
  DelayExecute(2,
    procedure (Sender: TObject)
    begin
      FList.Clear;
      AddItems(20);
      ListView.EnablePullLoad := True;
      ListView.PullRefreshComplete;
    end
  );
end;

procedure TfrmCustomList.ListViewScrollChange(Sender: TObject);
begin
  if (ListView.ContentBounds.Height - ListView.VScrollBarValue - ListView.Height) < 100 then
    TThread.CreateAnonymousThread(procedure begin
      AddItems(4); //因为列表也是网络获取，避免太卡，这里少量增加
    end).Start;
end;

{ TCustomListDataAdapter }

procedure TCustomListDataAdapter.Cancel;
begin
  FCancel := True;
  //TTask.WaitForAll();
end;

constructor TCustomListDataAdapter.Create(const AList: TList<TDataItem>);
begin
  FList := AList;
end;

procedure TCustomListDataAdapter.DoReceiveDataEvent(const Sender: TObject;
  AContentLength, AReadCount: Int64; var Abort: Boolean);
begin
  Abort := FCancel;
end;

function TCustomListDataAdapter.GetCount: Integer;
begin
  if Assigned(FList) then
    Result := FList.Count
  else
    Result := 0;
end;

function TCustomListDataAdapter.GetItem(const Index: Integer): Pointer;
begin
  Result := nil;
end;

function TCustomListDataAdapter.GetView(const Index: Integer;
  ConvertView: TViewBase; Parent: TViewGroup): TViewBase;
var
  LItem: TDataItem;

  function DoTextImage: TViewBase;
  var
    ViewItem: TfrmListItem_TextImage;
  begin
    if FCancel then begin
      Result := nil;
      Exit;
    end;

    if (ConvertView = nil) or (not (ConvertView.ClassType = TfrmListItem_TextImage)) then begin
      ViewItem := TfrmListItem_TextImage.Create(Parent);
      ViewItem.Parent := Parent;
      ViewItem.Width := Parent.Width;
      ViewItem.CanFocus := False;
      //init
      ViewItem.ItemIndex := -1;
      // 默认大小如果小于设计大小，这里重新设定下
      ViewItem.Height := ItemDefaultHeight;
    end else
      ViewItem := TObject(ConvertView) as TfrmListItem_TextImage;

    try
      if FCancel then
        Exit;
      // same index, skip
      if (ViewItem.ItemIndex = Index) and ViewItem.ImageView1.Enabled then
        Exit;

      ViewItem.BeginUpdate;
      try
        ViewItem.ItemIndex := Index;

        ViewItem.TextView1.Text := LItem.Hint;
        ViewItem.TextView2.Text := LItem.Title;
        ViewItem.TextView3.Text := LItem.SubTitle;

        //get image
        UpdateImage(Index, ViewItem);
      finally
        ViewItem.EndUpdate;
      end;
    finally
      Result := TViewBase(ViewItem);
    end;
  end;

  function DoImage: TViewBase;
  var
    ViewItem: TfrmListItem_Image;
  begin
    if FCancel then begin
      Result := nil;
      Exit;
    end;

    if (ConvertView = nil) or (not (ConvertView.ClassType = TfrmListItem_Image)) then begin
      ViewItem := TfrmListItem_Image.Create(Parent);
      ViewItem.Parent := Parent;
      ViewItem.Width := Parent.Width;
      ViewItem.CanFocus := False;
      //init
      ViewItem.ItemIndex := -1;
      // 默认大小如果小于设计大小，这里重新设定下
      ViewItem.Height := ItemDefaultHeight;
    end else
      ViewItem := TObject(ConvertView) as TfrmListItem_Image;

    try
      if FCancel then
        Exit;
      // same index, skip
      if (ViewItem.ItemIndex = Index) and ViewItem.ImageView1.Enabled then
        Exit;

      ViewItem.BeginUpdate;
      try
        ViewItem.ItemIndex := Index;

        ViewItem.TextView1.Text := LItem.Hint;

        //get image
        UpdateImage(Index, ViewItem);
      finally
        ViewItem.EndUpdate;
      end;
    finally
      Result := TViewBase(ViewItem);
    end;
  end;

begin
  if FCancel then begin
    Result := nil;
    Exit;
  end;

  LItem := FList.Items[Index];
  if LItem.IsLarge then
    Result := DoImage
  else
    Result := DoTextImage;
end;

function TCustomListDataAdapter.IndexOf(const AItem: Pointer): Integer;
begin
  Result := -1;
end;

function TCustomListDataAdapter.ItemDefaultHeight: Single;
begin
  Result := 150;
end;

procedure TCustomListDataAdapter.ItemMeasureHeight(const Index: Integer;
  var AHeight: Single);
begin
  if FList.Items[Index].Height > 0 then
    AHeight := FList.Items[Index].Height
  else if FList.Items[Index].IsLarge then
    AHeight := 300
  else
    AHeight := 150;
end;

procedure TCustomListDataAdapter.UpdateImage(const Index: Integer; ViewItem: TObject);
var
  LItem: TDataItem;
  LView: TBaseListItemFrame;
  LStream: TMemoryStream;
  LHttp: THttpClient;
  LResponse: IHTTPResponse;
begin
  if FCancel then
    Exit;
  LView := TBaseListItemFrame(ViewItem);
  if LView.ItemIndex <> Index then
    Exit;
  LItem := FList.Items[Index];
  if LItem.ImageUrl = '' then
    Exit;
  // 已经加载过就不重复加载了
  if not LView.Image.Image.ItemDefault.Bitmap.Bitmap.IsEmpty then
    Exit;

  LView.Text.Text := '下载中';
  //display a gray page
  LView.Image.Enabled := False;
  LView.Image.Image.ItemDefault.Bitmap.Bitmap.Assign(nil);
  //用户快速滑动的时候，用TThread肯定会卡，但如果是TTask则不会，因为线程池每次执行的个数有限，所以来得及响应
  //不过Task也有一些bug，所以大家可以用QWorker，我这里只是演示，就不用QWorker了
  //Delphi 10.1 berlin TTask存在的二个问题 - http://blog.sina.com.cn/s/blog_44fa172f0102w5o0.html
  //TThread.CreateAnonymousThread(procedure begin
  TTask.Run(procedure begin
    //delay to skip quick scroll
    Sleep(100);
    //enable for test slowly network
    //Sleep(4000);
    if LView.ItemIndex <> Index then
      Exit;
    if FCancel then
      Exit;
    {$IFDEF DEBUG}
    log.d('-----------------UpdateImage:%d-IsLarge:%d-------', [Index, Ord(LItem.IsLarge)]);
    {$ENDIF}
    LStream := TMemoryStream.Create;
    try
      LHttp := THttpClient.Create;
      try
        LHttp.OnReceiveData := DoReceiveDataEvent;
        LHttp.ValidateServerCertificateCallback := ValidateServerCertificate;
        try
          LResponse := LHttp.Get(LItem.ImageUrl, LStream);
        except
          //自己处理异常
          Exit;
        end;
        if FCancel then
          Exit;
        if not Assigned(LResponse) then
          Exit;
        if (LResponse.StatusCode < 200) or (LResponse.StatusCode >= 300) then
          Exit;
        if LView.ItemIndex <> Index then
          Exit;
        if FCancel then
          Exit;
        TThread.Synchronize(nil, procedure begin
          if LView.ItemIndex <> Index then
            Exit;
          if FCancel then
            Exit;

          //如果文件很大，这个过程其实很慢。
          //但fmx的图片加载在线程下不稳定，不可靠
          //所以下一步就是把加载拆分，第一步先加载到内存，然后再同步到ui
          //不单单如此，BMP其实很大，所以如果你的图片大于实际显示大小，那么需要进一步做缩略图处理
          //总结一下：
          //1、线程加载图片到内存
          //2、线程内创建缩略图
          //3、同步更新到UI
          try
            LView.Image.Image.ItemDefault.Bitmap.Bitmap.LoadFromStream(LStream);

            if LView.ItemIndex = 0 then begin
              LItem.Height := 200;
              FList.Items[Index] := LItem;
              LView.Text.Text := '更新完成-后期更改高度demo';
              { TODO -oAdministrator -c : 增加一个后期更改尺寸的调用方法 2020-09-16 11:36:17 }
              ListView.ContentViews.Height := ListView.ContentViews.Height + 1;
            end
            else
              LView.Text.Text := '更新完成';
          except
            LView.Text.Text := '更新失败';
          end;
          LView.Image.Enabled := True;
        end);
      finally
        FreeAndNil(LHttp);
      end;
    finally
      FreeAndNil(LStream);
    end;
  end).Start;
end;

end.
