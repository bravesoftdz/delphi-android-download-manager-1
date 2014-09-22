unit Androidapi.DownloadsManager;

interface
Uses System.Classes, System.Actions, System.SysUtils,
     Generics.Collections,
     IdHttp, IdComponent;

type
  TDownloadManagers = class;

  TDownloadItems = class(TObject)
    {$REGION 'Item Class'}
    private type
      TDownloadItem = class
      private
        Http            : TIdHTTP;
        FSource,
        FTarget         : String;
        FPercent        : Integer;
        FOwner : TDownloadItems;
        function SGetWrite: String;
        function TGetWrite: String;
        procedure SSetWrite(const Value: String);
        procedure TSetWrite(const Value: String);
        procedure HttpWork (ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
      public
        constructor Create(AOwner: TDownloadItems);
        destructor  Destroy; override;
        procedure Start;
        procedure Stop;
      published
        property Source   : String       read SGetWrite write SSetWrite;
        property Target   : String       read TGetWrite write TSetWrite;
        property Percent  : Integer      read FPercent;
      end;
      {$ENDREGION}
    private
      FItems:TObjectList<TDownloadItem>;
      FSource,
      FTarget: String;
      FOwner : TDownloadManagers;
      function GetItem(const Index: Integer): TDownloadItem;
    public
      procedure StopDownload(const Index: Integer);
      constructor Create(AOwner: TDownloadManagers);
      destructor  Destroy; override;
      function Add: TDownloadItem;
      property Item[const Index: Integer]     : TDownloadItem read GetItem; default;
  end;

  TOnProgress = procedure(Sender: TObject; Items: TDownloadItems) of object;

  TDownloadManagers = class(TComponent)
  {$REGION 'Thread'}
    private type
      TDownloadThread = class(TThread)
      private
        FItems : TDownloadItems;
        FIndex : Integer;
        FOwner :TDownloadManagers;
      public
        constructor Create(TItem: TDownloadItems;TIndex: Integer);
        destructor Destroy; override;
        procedure Execute; override;
      end;
      {$ENDREGION}
    private
      Islem: TDownloadThread;
      FDownloadItems: TDownloadItems;
      function GetCount: Integer;
    protected
      FOnProgress: TOnProgress;
    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure StartAll;
      procedure ClearAll;
      procedure StopAll;
      procedure DoChange(T:TDownloadItems);

      property Count          : Integer        read GetCount;
      property Items          : TDownloadItems read FDownloadItems;
    published
      property OnProgress : TOnProgress        read FOnProgress write FOnProgress;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Android', [TDownloadManagers]);
end;

{ TDownloadManagers }

procedure TDownloadManagers.ClearAll;
begin
  if FDownloadItems.FItems.Count > 0 then
    FDownloadItems.FItems.Clear;
end;

constructor TDownloadManagers.Create(AOwner: TComponent);
begin
  inherited;
  FDownloadItems := TDownloadItems.Create(Self);
//  FOnProgress    := nil;
end;

destructor TDownloadManagers.Destroy;
begin
  FDownloadItems.Free;
  inherited;
end;

function TDownloadManagers.GetCount: Integer;
begin
  Result := FDownloadItems.FItems.Count;
end;

procedure TDownloadManagers.StartAll;
var
  I: Integer;
begin
  for I := 0 to GetCount -1  do
  begin
    Islem := TDownloadThread.Create(Items,i);
    Islem.FreeOnTerminate := True;
    Islem.Start;
  end;
end;

procedure TDownloadManagers.StopAll;
var
  I: Integer;
begin
  if FDownloadItems.FItems.Count > 0 then
    for I := 0 to GetCount -1  do
      Items[i].Stop;
//      Items.
end;

{ TDownloadItems }

function TDownloadItems.Add: TDownloadItem;
begin
  Result := TDownloadItem.Create(Self);
  FItems.Add(Result);
end;

constructor TDownloadItems.Create(AOwner: TDownloadManagers);
begin
  FItems := TObjectList<TDownloadItem>.Create;
  FOwner := AOwner;
end;

procedure TDownloadItems.StopDownload(const Index: Integer);
begin
  if FItems.Count >= Index then
    FItems[Index].Stop
  else
    raise EActionError.CreateFMT('This index not found!', ['']);
end;

destructor TDownloadItems.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TDownloadManagers.DoChange(T:TDownloadItems);
begin
//  if Assigned(TDownloadManagers(FOwner).FOnProgress) then
//    TDownloadManagers(FOwner).OnProgress(Self);
  if Assigned(FOnProgress) then FOnProgress(Self,T);
end;

function TDownloadItems.GetItem(const Index: Integer): TDownloadItem;
begin
  Result := FItems[Index];
end;

constructor TDownloadItems.TDownloadItem.Create(AOwner: TDownloadItems);
begin
  FOwner := AOwner;
end;

destructor TDownloadItems.TDownloadItem.Destroy;
begin

  inherited;
end;

procedure TDownloadItems.TDownloadItem.HttpWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
var
  Http: TIdHTTP;
  ContentLength: Int64;
begin
  Http := TIdHTTP(ASender);
  ContentLength := Http.Response.ContentLength;

  if (Pos('chunked', LowerCase(Http.Response.TransferEncoding)) = 0) and
     (ContentLength > 0) then
  begin
    FPercent := 100 * AWorkCount div ContentLength;
    if Assigned(FOwner) and Assigned(FOwner.FOwner.OnProgress) then
         FOwner.FOwner.OnProgress(Self, FOwner);


    // Tekiklenme yeri % ler hesaplanýyor.

//    if Assigned(TDownloadManagers(MasterComponent).FOnProgress) then
//      TDownloadManagers(MasterComponent).FDownloadItems.DoChange;
  end;
end;

function TDownloadItems.TDownloadItem.SGetWrite: String;
begin
  Result := FSource;
end;

procedure TDownloadItems.TDownloadItem.SSetWrite(const Value: String);
begin
  if FSource <> Value then
    FSource := Value;
end;

procedure TDownloadItems.TDownloadItem.Start;
var
  MS: TMemoryStream;
begin
  Http := TIdHTTP.Create(nil);
  With Http do
  begin
    try
      FPercent := 0;
      MS := TMemoryStream.Create;

      OnWork:= HttpWork;

      Get(FSource, MS);

      MS.SaveToFile(FTarget);
    finally
      Disconnect; Free;
      MS.Free;
    end;
  end;
end;

procedure TDownloadItems.TDownloadItem.Stop;
begin
  Http.Disconnect;
end;

function TDownloadItems.TDownloadItem.TGetWrite: String;
begin
  Result := FTarget;
end;

procedure TDownloadItems.TDownloadItem.TSetWrite(const Value: String);
begin
  if FTarget <> Value then
    FTarget := Value;
end;

{ TDownloadManagers.TDownloadThread }

constructor TDownloadManagers.TDownloadThread.Create(TItem: TDownloadItems;TIndex: Integer);
begin
  inherited Create(True);
  FItems := TItem;
  FIndex := TIndex;
end;

destructor TDownloadManagers.TDownloadThread.Destroy;
begin

  inherited;
end;

procedure TDownloadManagers.TDownloadThread.Execute;
begin
  inherited;
  FItems.FItems.Items[FIndex].Start;
end;

end.
