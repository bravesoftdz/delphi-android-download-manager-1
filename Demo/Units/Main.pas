unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  Androidapi.DownloadsManager, FMX.Layouts, FMX.Memo, FMX.Objects;

type
  TForm3 = class(TForm)
    Button1: TButton;
    DownloadManagers1: TDownloadManagers;
    ToolBar1: TToolBar;
    Text1: TText;
    ProgressBar1: TProgressBar;
    Rectangle1: TRectangle;
    Label1: TLabel;
    Rectangle2: TRectangle;
    Label7: TLabel;
    ProgressBar3: TProgressBar;
    Rectangle3: TRectangle;
    Label8: TLabel;
    ProgressBar2: TProgressBar;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure DownloadManagers1Progress(Sender: TObject; Items: TDownloadItems);
    procedure Button2Click(Sender: TObject);
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

procedure TForm3.DownloadManagers1Progress(Sender: TObject;
  Items: TDownloadItems);
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

end.
