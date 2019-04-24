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
    cbxSaveOnlyName: TCheckBox;
    ediComment: TEdit;
    ediDisplayName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblHeight: TLabel;
    lblWidth: TLabel;
    lblTop: TLabel;
    lblLeft: TLabel;
    lblProgram: TLabel;

    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbxSaveOnlyNameChange(Sender: TObject);

    function ConfirmSave(info: TWinfo; DefaultComment: string; DefaultName: string): Boolean;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    function GetComment: string;
    function GetDisplayName: string;
    function SaveWhat: TSaveWhat;
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
      MyIni := TIniFile.Create(GetIniFileName);
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
    try
      MyIni := TIniFile.Create(GetIniFileName);
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

procedure TfrmSaveForm.cbxSaveOnlyNameChange(Sender: TObject);
begin
  if cbxSaveOnlyName.State = cbChecked then
  begin
    ediComment.Enabled := False;
    lblLeft.Enabled := False;
    lblTop.Enabled := False;
    lblWidth.Enabled := False;
    lblHeight.Enabled := False;
  end
  else
  begin
    ediComment.Enabled := True;
    lblLeft.Enabled := True;
    lblTop.Enabled := True;
    lblWidth.Enabled := True;
    lblHeight.Enabled := True;
  end;
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

function TfrmSaveForm.ConfirmSave(info: TWinfo; DefaultComment: string; DefaultName: string): Boolean;
begin
  lblProgram.Caption := info.wProgPath;
  lblLeft.Caption := IntToStr(info.wLeft);
  lblTop.Caption := IntToStr(info.wTop);
  lblWidth.Caption := IntToStr(info.wWidth);
  lblHeight.Caption := IntToStr(info.wHeight);

  // Reset the Only Save Display Name checkbox
  cbxSaveOnlyName.State := cbUnchecked;

  ediComment.Caption := DefaultComment;
  ediDisplayName.Caption := DefaultName;
  ActiveControl := ediComment;

  DoSave := False;

  ShowModal;

  Result := DoSave;
end;

function TfrmSaveForm.GetComment: string;
begin
  Result := ediComment.Caption;
end;

function TfrmSaveForm.GetDisplayName: string;
begin
  Result := ediDisplayName.Caption;
end;

function TfrmSaveForm.SaveWhat: TSaveWhat;
begin
  if cbxSaveOnlyName.State = cbChecked then
  	Result := svNameOnly
  else
    Result := svAll;
end;

end.

