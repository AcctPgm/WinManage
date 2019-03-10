unit WindowFuncs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, JwaTlHelp32, JwaPsAPI, FileUtil,
  TypeUnit;

function EnumWindowsToStrList(handle:hwnd; lP:LPARAM):LongBool;stdcall;
function ProcessFileName(PID: DWORD): string;
function GetFileNameFromHandle(Handle: hwnd):string;

implementation

function EnumWindowsToStrList(handle:hwnd; lP:LPARAM):LongBool;stdcall;
var
  sl: TStringList;
  PID: DWord;
  rect: TRect;
  text: array [0..255] of char;
  tw: TWinfo;
begin
  if IsWindowVisible(handle) and
    (GetWindowLong(handle,GWL_EXSTYLE) AND WS_EX_TOOLWINDOW = 0) and  // Not a tool window
//    (GetWindow(handle,GW_OWNER) = 0) and                      //Not owned by other windows
    (GetParent(handle) = 0)          and                      //Does not have any parent
    (GetWindowText(handle, text, sizeOf(text) - 1) <> 0) then
  begin
    GetWindowRect(handle, rect);
    if (rect.Top >= 0) and ((rect.Right - rect.Left) > 0) then
    begin
      tw := TWinfo.Create;

      with tw do
      begin
        wName := GetFileNameFromHandle(handle);
        wWinTitle := text;

        wTop := rect.Top;
        wLeft := rect.Left;
        wBottom := rect.Bottom;
        wRight := rect.Right;
        wHeight := wBottom - wTop;
        wWidth := wRight - wLeft;
        wHandle := handle;

        GetWindowThreadProcessID(Handle, @PID);
        wProgPath := ProcessFileName(PID);
        if length(wProgPath) = 0 then
          wProgPath := Copy(wName, 1, length(wName));
      end;

      sl := TStringList(lp);
      sl.AddObject(tw.wName + #009 + tw.wWinTitle, tw);
    end;
  end;

  result := true;
end;

function ProcessFileName(PID: DWORD): string;
  var
    Handle: THandle;
    FullPath: BOOL;
  begin
    Result := '';
    FullPath := True;
    Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
    if Handle <> 0 then
      try
        SetLength(Result, MAX_PATH);
        if FullPath then
          if GetModuleFileNameEx(Handle, 0, PChar(Result), MAX_PATH) > 0 then
            SetLength(Result, StrLen(PChar(Result)))
          else
            Result := ''
        else
          if GetModuleBaseNameA(Handle, 0, PChar(Result), MAX_PATH) > 0 then
            SetLength(Result, StrLen(PChar(Result)))
          else
            Result := '';
      finally
        CloseHandle(Handle);
      end;
  end;

function GetFileNameFromHandle(Handle: hwnd):string;
var
	PID: DWord;
	aSnapShotHandle: THandle;
	ContinueLoop: Boolean;
	aProcessEntry32: TProcessEntry32;
begin
	GetWindowThreadProcessID(Handle, @PID);
	aSnapShotHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
	aProcessEntry32.dwSize := SizeOf(aProcessEntry32);
	ContinueLoop := Process32First(aSnapShotHandle, aProcessEntry32);
	while Integer(ContinueLoop) <> 0 do
	begin
		if aProcessEntry32.th32ProcessID = PID then
		begin
			Result := SysToUTF8(aProcessEntry32.szExeFile);
			break;
		end;
		ContinueLoop := Process32Next(aSnapShotHandle, aProcessEntry32);
	end;
	CloseHandle(aSnapShotHandle);
end;

end.

