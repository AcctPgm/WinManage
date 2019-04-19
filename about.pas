unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniFiles,
  IniUnit, Options, NearTrayPos;

const
  VersionNum = '0.9.0';

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnOK: TButton;
    lblVersion: TLabel;
    StaticText1: TStaticText;

    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    InitialLeft: integer;
    InitialTop: integer;
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.FormShow(Sender: TObject);
var
  MyIni: TIniFile;
  nt: TNearTray;
begin
  lblVersion.Caption := 'Version ' + VersionNum;

  // Default position near system tray
  nt := NearTray(Width, Height);
  Left := nt.Left;
  Top := nt.Top;

  // Load the last form position
  if RememberFormPositions then
  begin
    try
      MyIni := TIniFile.Create(GetIniFileName);
      Top := MyIni.ReadInteger('Options', 'AboutFormTop', Top);
      Left := MyIni.ReadInteger('Options', 'AboutFormLeft', Left);
    finally
      MyIni.Free;
    end;
  end;

  // Save the initial position to be able to check whether the form was moved
  InitialLeft := Left;
  InitialTop := Top;
end;

procedure TfrmAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  FullIniPath: string;
  MyIni: TIniFile;
begin
  // Save the form position
  if RememberFormPositions and (Left <> InitialLeft) and (Top <> InitialTop) then
  begin
    try
      MyIni := TIniFile.Create(GetIniFileName);
      MyIni.WriteInteger('Options', 'AboutFormTop', Top);
      MyIni.WriteInteger('Options', 'AboutFormLeft', Left);
    finally
      MyIni.Free;
    end;
  end;
end;

procedure TfrmAbout.btnOKClick(Sender: TObject);
begin
  Close;
end;

end.

