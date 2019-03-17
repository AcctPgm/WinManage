unit Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniFiles,
  IniUnit, NearTrayPos;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    cbxCloseFromForm: TCheckBox;
    cbxShowOnStartup: TCheckBox;
    cbxRememberPositions: TCheckBox;
    cbxHideOnClickAway: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
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
  frmOptions: TfrmOptions;

  CloseFromForm: Boolean;
  ShowOnStartup: Boolean;
  RememberFormPositions: Boolean;
  HideOnClickAway: Boolean;

procedure LoadOptions;
procedure SaveOptions;

implementation

{$R *.lfm}

uses
  MainForm;

procedure LoadOptions;
var
  MyIni: TIniFile;
begin
  try
    MyIni := TIniFile.Create(GetIniFileName);
    CloseFromForm := MyIni.ReadBool('Options', 'CloseFromForm', CloseFromForm);
    ShowOnStartup := MyIni.ReadBool('Options', 'ShowOnStartup', ShowOnStartup);
    RememberFormPositions := MyIni.ReadBool('Options', 'RememberFormPositions', RememberFormPositions);
    HideOnClickAway := MyIni.ReadBool('Options', 'HideOnClickAway', HideOnClickAway);
  finally
    MyIni.Free;
  end;
end;

procedure SaveOptions;
var
  FullIniPath: string;
  MyIni: TIniFile;
begin
  try
    MyIni := TIniFile.Create(GetIniFileName);
    MyIni.WriteBool('Options', 'CloseFromForm', CloseFromForm);
    MyIni.WriteBool('Options', 'ShowOnStartup', ShowOnStartup);
    MyIni.WriteBool('Options', 'RememberFormPositions', RememberFormPositions);
    MyIni.WriteBool('Options', 'HideOnClickAway', HideOnClickAway);
  finally
    MyIni.Free;
  end;
end;

{ TfrmOptions }

procedure TfrmOptions.FormShow(Sender: TObject);
var
  MyIni: TIniFile;
  nt: TNearTray;
begin
  // Set the check boxes to the current option values
  cbxCloseFromForm.Checked := CloseFromForm;
  cbxShowOnStartup.Checked := ShowOnStartup;
  cbxRememberPositions.Checked := RememberFormPositions;
  cbxHideOnClickAway.Checked := HideOnClickAway;

  // Get position near the system tray as the default
  nt := NearTray(Width, Height);
  if nt.Left <> ntNone then
  begin
    Left := nt.Left;
    Top := nt.Top;
  end;

  // Load the last form position
  if RememberFormPositions then
  begin
    try
      MyIni := TIniFile.Create(GetIniFileName);
      Top := MyIni.ReadInteger('Options', 'OptionsFormTop', Top);
      Left := MyIni.ReadInteger('Options', 'OptionsFormLeft', Left);
    finally
      MyIni.Free;
    end;
  end;

  // Save the initial position to be able to check whether the form was moved
  InitialLeft := Left;
  InitialTop := Top;
end;

procedure TfrmOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  FullIniPath: string;
  MyIni: TIniFile;
begin
  // Save the form position
  if RememberFormPositions and (Left <> InitialLeft) and (Top <> InitialTop) then
  begin
    try
      MyIni := TIniFile.Create(GetIniFileName);
      MyIni.WriteInteger('Options', 'OptionsFormTop', Top);
      MyIni.WriteInteger('Options', 'OptionsFormLeft', Left);
    finally
      MyIni.Free;
    end;
  end;
end;

procedure TfrmOptions.btnOKClick(Sender: TObject);
begin
  CloseFromForm := cbxCloseFromForm.Checked;
  ShowOnStartup := cbxShowOnStartup.Checked;
  RememberFormPositions := cbxRememberPositions.Checked;
  HideOnClickAway := cbxHideOnClickAway.Checked;
  SaveOptions;

  Close;

  frmMain.SetCloseAllowed(CloseFromForm);
end;

procedure TfrmOptions.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.

