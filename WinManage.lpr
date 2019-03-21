program WinManage;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, Dialogs,
  MainForm, TypeUnit, WindowFuncs, IniUnit, About, Options, SaveForm,
  Windows, CheckPrevious;

{$R *.res}

var
  hdl: THandle;

begin
  RequireDerivedFormResource := True;

  // Check whether the program is already running - don't run a second instance
  hdl := GetCurrentProcess;
  if CheckPrevious.RestoreIfRunning(hdl, 1) then
  	showMessage('The program "' + ParamStr(0) + '" is already running.')
  else
  begin
    Application.Initialize;
    Application.ShowMainForm := False;
    Application.CreateForm(TfrmMain, frmMain);
    Application.CreateForm(TfrmAbout, frmAbout);
    Application.CreateForm(TfrmOptions, frmOptions);
    Application.CreateForm(TfrmSaveForm, frmSaveForm);
    Application.Run;
  end;
end.

