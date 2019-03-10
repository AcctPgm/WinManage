program WinManage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainForm, TypeUnit, WindowFuncs, IniUnit, About, Options, SaveForm,
  Windows, CheckPrevious;

{$R *.res}

var
  hdl: THandle;

begin
  RequireDerivedFormResource := True;
  hdl := GetCurrentProcess;
  if not CheckPrevious.RestoreIfRunning(hdl, 1) then
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

