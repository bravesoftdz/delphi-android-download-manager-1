unit Androidapi.DownloadsManager;

interface
Uses System.Classes, System.Actions, System.SysUtils, FMX.StdCtrls,
     Generics.Collections,
     IdHttp, IdComponent,
     FMX.Layouts, FMX.Objects, FMX.Types, FMX.Forms;

type
  TDownloadManagers = class;
  TPackage = class;
  TDownloadItems = class;

  TOnProgress = procedure(Sender: TObject; Items: TDownloadItems; Package: TPackage) of object;

  TDownloadItems = class(TObject)
    {$REGION 'Item Class'}
    private type
      TDownloadItem = class
      private
        Http            : TIdHTTP;
        RList           : TList<TRectangle>;
        TTList          : TList<TText>;
        PList          : TList<TProgressBar>;
        FSource,
        FTarget         : String;
        FPercent        : Integer;
        FOwner          : TDownloadItems;
        BG              : TRectangle;
        xPercent,
        FTitle          : TText;
        FProgress       : TProgressBar;
        function SGetWrite: String;
        function TGetWrite: String;
        procedure SSetWrite(const Value: String);
        procedure TSetWrite(const Value: String);
        procedure HttpWork (ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
        procedure AddDownloadItem(AOwner:TVertScrollBox);
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
      procedure   StopDownload(const Index: Integer);
      constructor Create(AOwner: TDownloadManagers);
      destructor  Destroy; override;
      function    Add: TDownloadItem;
      property    Item[const Index: Integer]     : TDownloadItem  read GetItem; default;
  end;

  TPackageItem = class
    Const Seperators = '/';
    {$REGION 'Thread'}
    private type
     TPackageThread = class(TThread)
      private
        Http   : TIdHTTP;
        FIndex : Integer;
        FOwner :TPackage;
      public
        constructor Create(TItem: TPackage;TIndex: Integer);
        destructor Destroy; override;
        procedure Execute; override;
      end;
      {$ENDREGION}
    private
      RList       : TList<TRectangle>;
      TTList      : TList<TText>;
      PList       : TList<TProgressBar>;
      FLink       : TStringList;
      FTarget     : String;
      FIndex      : Integer;
      FOwner      : TPackage;
      FCount,
      FValue      : Integer;
      BG          : TRectangle;
      xPercent,
      FTitle      : TText;
      FProgress   : TProgressBar;
      Islem       : TPackageThread;
      procedure AddDownloadItem(AOwner:TVertScrollBox);
      function GetName(T: String): String;
      function TGetWrite: String;
      procedure TSetWrite(const Value: String);
      function GetTitle: String;
      procedure SetTitle(const Value: String);
    public
      constructor Create(AOwner: TPackage);
      destructor  Destroy; override;
      procedure   Start(Index: Integer = - 1 );
      property    Target        : String      read TGetWrite write TSetWrite;
      property    Count         : Integer     read FCount;
      property    Value         : Integer     read FValue;
      property    DownloadsLink : TStringList read FLink     write FLink;
      property    Title         : String      read GetTitle  write SetTitle;
  end;

  TPackage = class(TPersistent)
    private
      FOwner      : TDownloadManagers;
      FItems      : TObjectList<TPackageItem>;
      FName       : TList<String>;
      function GetItem(const Index: Integer): TPackageItem;
    public
      constructor Create(AOwner: TComponent);
      destructor  Destroy; override;
      procedure   Assign(Source : TPersistent); override;
      procedure   StartAll;
      function    Add: TPackageItem;
      function    Count: Integer;
      property    Item[const Index: Integer] : TPackageItem read GetItem; default;
    published
  end;

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
      FParent : TVertScrollBox;
      FPackage: TPackage;
      function GetCount: Integer;
      function GetParent: TVertScrollBox;
      procedure SetParent(const Value: TVertScrollBox);
    protected
      FOnProgress: TOnProgress;
    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure StartAll;
      procedure ClearAll;
      procedure StopAll;
      property Count           : Integer        read GetCount;
      property Items           : TDownloadItems read FDownloadItems;
      property PackageDownload : TPackage   read FPackage;
    published
      property ParentVertScroll    : TVertScrollBox read GetParent   write SetParent;
      property OnProgress : TOnProgress             read FOnProgress write FOnProgress;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Android', [TDownloadManagers]);
end;

{ TDownloadManagers }

procedure TDownloadManagers.ClearAll;
var I, J, C: Integer;
begin
  if FDownloadItems.FItems.Count > 0 then
  begin
    C := FDownloadItems.FItems.Count;
    if FParent <> nil then
    begin
      for I := 0 to C -1 do
      begin
        for J := Items.Item[I].PList.Count -1 Downto 0  do
          Items.Item[I].PList.Items[J].Free;

        for J := Items.Item[I].TTList.Count -1 Downto 0 do
          Items.Item[I].TTList.Items[J].Free;

        for J := Items.Item[I].RList.Count -1 Downto 0 do
          Items.Item[I].RList.Items[J].Free;
      end;
    end;
    FDownloadItems.FItems.Clear;
  end;
end;

constructor TDownloadManagers.Create(AOwner: TComponent);
begin
  inherited;
  FDownloadItems := TDownloadItems.Create(Self);
  FPackage       := TPackage.Create(Self);
end;

destructor TDownloadManagers.Destroy;
begin
  FDownloadItems.Free;
  FPackage.Free;
  inherited;
end;

function TDownloadManagers.GetCount: Integer;
begin
  Result := FDownloadItems.FItems.Count;
end;

function TDownloadManagers.GetParent: TVertScrollBox;
begin
  Result := FParent;
end;

procedure TDownloadManagers.SetParent(const Value: TVertScrollBox);
begin
  FParent := Value;
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
end;

destructor TDownloadItems.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TDownloadItems.GetItem(const Index: Integer): TDownloadItem;
begin
  Result := FItems[Index];
end;

procedure TDownloadItems.TDownloadItem.AddDownloadItem(AOwner: TVertScrollBox);
begin
  BG := TRectangle.Create(AOwner);
  RList.Add(BG);
  with BG do
  begin
    Parent     := AOwner;
    Fill.Color := $FFF39912;
    {$IF CompilerVersion > 26}
      Align      := TAlignLayout.Top;
    {$ELSE}
      Align      := TAlignLayout.alTop;
    {$ENDIF}
    Height     := 46;
    with Margins do
    begin
      Top   := 10;
      Left  := 5;
      Right := 5;
    end;
    Sides := [];

    FTitle := TText.Create(BG);
    TTList.Add(FTitle);
    with FTitle do
    begin
      Parent := BG;
      Height := 25;
      Color := $FF34495E;
      {$IF CompilerVersion > 26}
        Align  := TAlignLayout.Top;
        with TextSettings do
        begin
          Font.Size := 16;
          HorzAlign := TTextAlign.Leading;
        end;
      {$ELSE}
        Align         := TAlignLayout.alTop;
        Font.Size     := 16;
        HorzTextAlign := TTextAlign.taLeading;
      {$ENDIF}
      Text := 'Download Item';
    end;

    FProgress := TProgressBar.Create(BG);
    PList.Add(FProgress);
    with FProgress do
    begin
      Parent      := BG;
      with Margins do
      begin
        Left  := 5;
        Right := 5;
      end;
      {$IF CompilerVersion > 26}
        Align       := TAlignLayout.Client;
      {$ELSE}
        Align       := TAlignLayout.alClient;
      {$ENDIF}
      Min         := 0;
      Max         := 100;
    end;

    xPercent := TText.Create(BG);
    TTList.Add(xPercent);
    with xPercent do
    begin
      Parent := BG;
      Height := 15;
      {$IF CompilerVersion > 26}
        Align  := TAlignLayout.Bottom;
        with TextSettings do
          Font.Size := 12;
      {$ELSE}
        Align  := TAlignLayout.alBottom;
        Font.Size   := 12;
      {$ENDIF}
      Color := $FF34495E;
      Text := '%0';
    end;
  end;
end;

constructor TDownloadItems.TDownloadItem.Create(AOwner: TDownloadItems);
begin
  FOwner := AOwner;
  if FOwner.FOwner.FParent <> nil then
  begin
    RList  := TList<TRectangle>.Create;
    TTList := TList<TText>.Create;
    PList := TList<TProgressBar>.Create;
    AddDownloadItem(FOwner.FOwner.FParent);
  end;
end;

destructor TDownloadItems.TDownloadItem.Destroy;
begin
  RList.Clear;
  TTList.Clear;
  PList.Clear;
  RList.Free;
  TTList.Free;
  PList.Free;

  inherited;
end;

function TPackageItem.GetTitle: String;
begin
  Result := FTitle.Text;
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
    FPercent        := 100 * AWorkCount div ContentLength;
    if FOwner.FOwner.ParentVertScroll <> nil then
    begin
      FProgress.Value := FPercent;
      xPercent.Text   := '%' + FPercent.ToString;
    end;
    if Assigned(FOwner) and Assigned(FOwner.FOwner.OnProgress) then
         FOwner.FOwner.OnProgress(Self, FOwner,nil);
    // Tekiklenme yeri % ler hesaplanýyor.
  end;
end;

procedure TPackageItem.SetTitle(const Value: String);
begin
  if Assigned(FTitle) then
    if Value <> FTitle.Text then
      FTitle.Text := Value;
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

{ TPackageItem }

function TPackage.Add: TPackageItem;
begin
  Result := TPackageItem.Create(Self);
  FItems.Add(Result);
end;

procedure TPackageItem.AddDownloadItem(AOwner: TVertScrollBox);
begin
  BG := TRectangle.Create(AOwner);
  RList.Add(BG);
  with BG do
  begin
    Parent     := AOwner;
    Fill.Color := $FFF39912;
    {$IF CompilerVersion > 26}
      Align      := TAlignLayout.Top;
    {$ELSE}
      Align      := TAlignLayout.alTop;
    {$ENDIF}
    Height     := 46;
    with Margins do
    begin
      Top   := 10;
      Left  := 5;
      Right := 5;
    end;
    Sides := [];

    FTitle := TText.Create(BG);
    TTList.Add(FTitle);
    with FTitle do
    begin
      Parent := BG;
      Height := 25;
      Color := $FF34495E;
      {$IF CompilerVersion > 26}
        Align  := TAlignLayout.Top;
        with TextSettings do
        begin
          Font.Size := 16;
          HorzAlign := TTextAlign.Leading;
        end;
      {$ELSE}
        Align         := TAlignLayout.alTop;
        Font.Size     := 16;
        HorzTextAlign := TTextAlign.taLeading;
      {$ENDIF}
      Text := 'Download Item';
    end;

    FProgress := TProgressBar.Create(BG);
    PList.Add(FProgress);
    with FProgress do
    begin
      Parent      := BG;
      with Margins do
      begin
        Left  := 5;
        Right := 5;
      end;
      {$IF CompilerVersion > 26}
        Align       := TAlignLayout.Client;
      {$ELSE}
        Align       := TAlignLayout.alClient;
      {$ENDIF}
      Min         := 0;
      Max         := 100;
    end;

    xPercent := TText.Create(BG);
    TTList.Add(xPercent);
    with xPercent do
    begin
      Parent := BG;
      Height := 15;
      {$IF CompilerVersion > 26}
        Align  := TAlignLayout.Bottom;
        with TextSettings do
          Font.Size := 12;
      {$ELSE}
        Align  := TAlignLayout.alBottom;
        Font.Size   := 12;
      {$ENDIF}
      Color := $FF34495E;
      Text := '%0';
    end;
  end;
end;

procedure TPackage.Assign(Source: TPersistent);
begin
end;

function TPackage.Count: Integer;
begin
  Result := FItems.Count;
end;

constructor TPackage.Create(AOwner: TComponent);
begin
  FOwner := AOwner as TDownloadManagers;
  FItems := TObjectList<TPackageItem>.Create;
  FName  := TList<String>.Create;
end;

destructor TPackage.Destroy;
begin
  FItems.Free;
  FName.Free;
  inherited;
end;

function TPackage.GetItem(const Index: Integer): TPackageItem;
begin
  Result := FItems[Index];
end;

procedure TPackage.StartAll;
var
  I: Integer;
begin
  for I := 0 to Count -1  do
    FItems[I].Start(I);
end;

{ TPackageItem.TPackageThread }

constructor TPackageItem.TPackageThread.Create(TItem: TPackage;
  TIndex: Integer);
begin
  inherited Create(True);
  FOwner := TItem;
  FIndex := TIndex;
end;

destructor TPackageItem.TPackageThread.Destroy;
begin
  inherited;
end;

procedure TPackageItem.TPackageThread.Execute;
var
  MS: TMemoryStream;
  TmpName,
  Tmp : String;
  Key,
  xCount,
  I   : Integer;
  Temp: TPackageItem;
begin
  inherited;
  Temp := FOwner.FItems[FIndex];
  xCount := Temp.FLink.Count;
  Temp.FCount := xCount;
  Key    := 1;
  for I := 0 TO xCount - 1 do
  begin
    TmpName := Temp.GetName(Temp.FLink[i]);

    Http := TIdHTTP.Create(nil);
    With Http do
    begin
      ReadTimeout := 5000;
      try
        MS := TMemoryStream.Create;

        Get(Temp.FLink[i], MS);

        if Temp.FTarget[Temp.FTarget.Length] <> PathDelim then
          Tmp := Temp.FTarget + PathDelim
        else
          Tmp := Temp.FTarget;

        Temp.SetTitle(Copy(TmpName, 1, Pos('.',TmpName) - 1));

        MS.SaveToFile(Tmp + TmpName);

        Temp.FValue := Key;

        if Assigned(Temp.xPercent) then
          Temp.xPercent.Text := Key.ToString + '/' + xCount.ToString;

        if Assigned(FOwner.FOwner.OnProgress) then
          FOwner.FOwner.OnProgress(Self, nil,FOwner);

        Inc(Key)
      finally
        Disconnect; Free;
        MS.Free;
      end;
    end;
  end;
end;

{ TPackageItem }

constructor TPackageItem.Create(AOwner: TPackage);
begin
  FLink  := TStringList.Create;
  FOwner := AOwner;
  if Assigned(FOwner.FOwner.FParent) then
  begin
    RList  := TList<TRectangle>.Create;
    TTList := TList<TText>.Create;
    PList := TList<TProgressBar>.Create;
    AddDownloadItem(FOwner.FOwner.FParent);
  end;
end;

destructor TPackageItem.Destroy;
begin
  RList.Free;
  TTList.Free;
  PList.Free;
  FLink.Free;
  inherited;
end;

function TPackageItem.GetName(T: String): String;
begin
  if T.Trim = '' then Exit;
  while Pos(Seperators,T) > 0 do
  System.Delete(T,1,Pos(Seperators,T));
  Result := T;
end;

procedure TPackageItem.Start(Index: Integer = - 1 );
var
  Key: Integer;
begin
  if Index = -1 then
    Key := FOwner.FItems.Count - 1
  else
    Key := Index;

  Islem := TPackageThread.Create(FOwner,Key);
  Islem.FreeOnTerminate := True;
  Islem.Start;
end;

function TPackageItem.TGetWrite: String;
begin
  Result := FTarget;
end;

procedure TPackageItem.TSetWrite(const Value: String);
begin
  if Value <> FTarget then
    FTarget := Value;
end;

end.
