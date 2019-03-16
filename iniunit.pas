unit IniUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function GetAppFolder: string;

const
  IniFileName = 'WinManage.ini';
  IniFolderName = 'WinManage';

implementation

function GetAppFolder: string;
begin
  Result := {sysutils.}GetEnvironmentVariable('LocalAppData');
  if length(Result) = 0 then
    Result := sysutils.GetEnvironmentVariable('AppData');
end;

end.

