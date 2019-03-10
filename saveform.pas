unit SaveForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniFiles, Windows,
  Options, TypeUnit, NearTrayPos, IniUnit;

type

  { TfrmSaveForm }

  TfrmSaveForm = class(TForm)
    btnSave: TButton;
    btnCancel: TButton;
    ediComment: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblHeight: TLabel;
    lblWidth: TLabel;
    lblTop: TLabel;
    lblLeft: TLabel;
    lblProgram: TLabel;

    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);

    function ConfirmSave(info: TWinfo; DefaultComment: string): Boolean;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    function GetComment: string;
  private
    { private declarations }
    DoSave: Boolean;
    InitialLeft: integer;
    InitialTop: integer;
  public
    { public declarations }
  end;

var
  frmSaveForm: TfrmSaveForm;

implementation

uses
  MainForm;

{$R *.lfm}

{ TfrmSaveForm }

procedure TfrmSaveForm.FormShow(Sender: TObject);
var
  MyIni: TIniFile;
//  nt: TNearTray;
begin
{  // Get position near the system tray as the default
  nt := NearTray(Width, Height);
  if nt.Left <> ntNone then
  begin
    Left := nt.Left;
    Top := nt.Top;
  end;
}
  // Position form just above, or just below, program grid on main form as the default
  if (frmMain.Top + frmMain.sgdProgs.Top - Height - 5) > 0 then
    Top := frmMain.Top + frmMain.sgdProgs.Top - Height - 5
  else
    Top := frmMain.Top + GetSystemMetrics(SM_CYCAPTION) + frmMain.sgdProgs.Top + frmMain.sgdProgs.Height + 5;
  Left := frmMain.Left + 45;

  // Load the last form position
  if RememberFormPositions then
  begin
    try
      MyIni := TIniFile.Create(GetAppFolder + '\' + IniFolderName + '\' + IniFileName);
      Top := MyIni.ReadInteger('Options', 'SaveFormTop', Top);
      Left := MyIni.ReadInteger('Options', 'SaveFormLeft', Left);
    finally
      MyIni.Free;
    end;
  end;

  // Save the initial position to be able to check whether the form was moved
  InitialLeft := Left;
  InitialTop := Top;
end;

procedure TfrmSaveForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
var
  FullIniPath: string;
  MyIni: TIniFile;
begin
  // Save the form position
  if RememberFormPositions and (Left <> InitialLeft) and (Top <> InitialTop) then
  begin
    FullIniPath := GetAppFolder + '\' + IniFolderName;
    if not DirectoryExists(FullIniPath) then
      if not CreateDir(FullIniPath) then
        Exit;

    try
      MyIni := TIniFile.Create(FullIniPath + '\' + IniFileName);
      MyIni.WriteInteger('Options', 'SaveFormTop', Top);
      MyIni.WriteInteger('Options', 'SaveFormLeft', Left);
    finally
      MyIni.Free;
    end;
  end;
end;

procedure TfrmSaveForm.btnSaveClick(Sender: TObject);
begin
  Close;
  DoSave := True;
end;

{procedure TfrmSaveForm.Button2Click(Sender: TObject);
begin
  showMessage('Titlebar height = ' + IntToStr(GetSystemMetrics(SM_CYCAPTION)));
end;
}
procedure TfrmSaveForm.btnCancelClick(Sender: TObject);
begin
  Close;
  DoSave := False;
end;

function TfrmSaveForm.ConfirmSave(info: TWinfo; DefaultComment: string): Boolean;
begin
  lblProgram.Caption := info.wProgPath;
  lblLeft.Caption := IntToStr(info.wLeft);
  lblTop.Caption := IntToStr(info.wTop);
  lblWidth.Caption := IntToStr(info.wWidth);
  lblHeight.Caption := IntToStr(info.wHeight);

  ediComment.Caption := DefaultComment;
  ActiveControl := ediComment;

  DoSave := False;

  ShowModal;

  Result := DoSave;
end;

function TfrmSaveForm.GetComment: string;
begin
  Result := ediComment.Caption;
end;

end.

