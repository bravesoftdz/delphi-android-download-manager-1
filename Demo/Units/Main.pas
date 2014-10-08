unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  Androidapi.DownloadsManager, FMX.Layouts, FMX.Memo, FMX.Objects,
  FMX.TabControl;

type
  TForm3 = class(TForm)
    DownloadManagers1: TDownloadManagers;
    ToolBar1: TToolBar;
    Text1: TText;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Button1: TButton;
    Button2: TButton;
    Rectangle1: TRectangle;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Rectangle2: TRectangle;
    Label7: TLabel;
    ProgressBar3: TProgressBar;
    Rectangle3: TRectangle;
    Label8: TLabel;
    ProgressBar2: TProgressBar;
    VertScrollBox1: TVertScrollBox;
    DownloadManagers2: TDownloadManagers;
    Button3: TButton;
    TabItem3: TTabItem;
    Button4: TButton;
    DownloadManagers3: TDownloadManagers;
    TabControl2: TTabControl;
    TabItem4: TTabItem;
    TabItem5: TTabItem;
    Label2: TLabel;
    VertScrollBox2: TVertScrollBox;
    DownloadManagers4: TDownloadManagers;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure DownloadManagers1Progress(Sender: TObject; Items: TDownloadItems;
      Package: TPackage);
    procedure DownloadManagers3Progress(Sender: TObject; Items: TDownloadItems;
      Package: TPackage);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;
  A: TDownloadManagers;

implementation

{$R *.fmx}

Uses System.IOUtils;

procedure TForm3.Button1Click(Sender: TObject);
begin
  A := DownloadManagers1;

  with A.Items.Add do
  begin
    Source := 'http://live.sysinternals.com/ADExplorer.exe';
    Target := System.IOUtils.TPath.GetDocumentsPath + PathDelim + 'ADExplorer1.exe';
  end;

  with A.Items.Add do
  begin
    Source := 'http://live.sysinternals.com/ADExplorer.exe';
    Target := System.IOUtils.TPath.GetDocumentsPath + PathDelim + 'ADExplorer2.exe';
  end;

  with A.Items.Add do
  begin
    Source := 'http://live.sysinternals.com/ADExplorer.exe';
    Target := System.IOUtils.TPath.GetDocumentsPath + PathDelim + 'ADExplorer3.exe';
  end;

  A.StartAll;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
//  DownloadManagers1.StopAll;
  DownloadManagers1.Items.StopDownload(5);
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  A := DownloadManagers2;

  A.ClearAll;

  with A.Items.Add do
  begin
    Source := 'http://live.sysinternals.com/ADExplorer.exe';
    Target := System.IOUtils.TPath.GetDocumentsPath + PathDelim + 'ADExplorer1.exe';
  end;

  with A.Items.Add do
  begin
    Source := 'http://live.sysinternals.com/ADExplorer.exe';
    Target := System.IOUtils.TPath.GetDocumentsPath + PathDelim + 'ADExplorer2.exe';
  end;

  with A.Items.Add do
  begin
    Source := 'http://live.sysinternals.com/ADExplorer.exe';
    Target := System.IOUtils.TPath.GetDocumentsPath + PathDelim + 'ADExplorer3.exe';
  end;

  A.StartAll;
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  case TabControl2.TabIndex of
    0:
    begin
      A := DownloadManagers3;
      with A.PackageDownload.Add do
      begin
        with DownloadsLink do
        begin
          Add('http://mangakafe.com/Mangalar/Noragami/05/0016.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0017.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0018.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0019.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0020.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0021.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0022.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0023.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0024.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0025.jpg');
        end;
        Target := System.IOUtils.TPath.GetDocumentsPath;
        Start;
      end;
    end;
    1:
    begin
      A := DownloadManagers4;
      A.ParentVertScroll := VertScrollBox2;

      with A.PackageDownload.Add do
      begin
        with DownloadsLink do
        begin
          Add('http://mangakafe.com/Mangalar/Noragami/05/0001.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0002.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0003.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0004.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0005.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0006.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0007.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0008.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0009.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0010.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0011.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0012.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0013.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0014.jpg');
          Add('http://mangakafe.com/Mangalar/Noragami/05/0015.jpg');
        end;
        Target := System.IOUtils.TPath.GetDocumentsPath;
        Start;
      end;
    end;
  end;
end;

procedure TForm3.DownloadManagers1Progress(Sender: TObject;
  Items: TDownloadItems; Package: TPackage);
var
  I: Integer;
begin
  for I := 0 to A.Count - 1 Do
  begin
    case I of
      0: begin
        Label1.Text := Format('Ýndirilen 1:  %s',[A.Items[i].Percent.ToString]);
        ProgressBar1.Value := A.Items[i].Percent;
      end;
      1: begin
        Label8.Text := Format('Ýndirilen 2:  %s',[A.Items[i].Percent.ToString]);
        ProgressBar2.Value := A.Items[i].Percent;
      end;
      2: begin
        Label7.Text := Format('Ýndirilen 3:  %s',[A.Items[i].Percent.ToString]);
        ProgressBar3.Value := A.Items[i].Percent;
      end;
    end;
  end;

end;

procedure TForm3.DownloadManagers3Progress(Sender: TObject;
  Items: TDownloadItems; Package: TPackage);
var
  I: Integer;
begin
  for I := 0 to DownloadManagers3.PackageDownload.Count - 1 Do
  begin
    case I of
      0: begin
        Label2.Text := Format('Ýndirilen 1:  %s/%s',[Package.Item[i].Value.ToString,Package.Item[i].Count.ToString]);
      end;
    end;
  end;

end;

end.
