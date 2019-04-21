unit IniUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  Dialogs;

function GetIniFileName: string;

const
  IniFileName = 'WinManage.ini';
  IniDefaultFolder = 'WinManage';

implementation

// Return the name of WinManage's ini file.
// If a path was specified on the command line, use it.
// Otherwise, the path is the user's AppData\WinManage.
// If the path doesn't exist, create it.
// The ini file name is always WinManage.ini to avoid accientally using an ini
// file belonging to another program.

function GetIniFileName: string;
begin
  // Default path is the default folder name under the AppData folder
  Result := GetEnvironmentVariable('LocalAppData') + '\' + IniDefaultFolder;
  if length(Result) = 0 then
    Result := sysutils.GetEnvironmentVariable('AppData') + '\' + IniDefaultFolder;

  // But use folder specified on the comment line, if any
  if ParamCount > 0 then
  begin
    if DirectoryExists(ParamStr(1)) then
      Result := ParamStr(1)
  	else if CreateDir(ParamStr(1)) then
      Result := ParamStr(1);
  end;

	// Ensure the path ends in a \
	if AnsiRightStr(Result, 1) <> '\' then
  	 Result := Result + '\';

  // Use standard ini file name to avoid overwriting a system file or anything
  // like that
  Result := Result + IniFileName;
end;

end.

