unit NearTrayPos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  ShellAPI;

type
  TNearTray = record
    Left,
    Top : integer;
  end;

const
  ntNone = -1;

function NearTray(Width, Height: integer): TNearTray;

implementation

function NearTray(Width, Height: integer): TNearTray;
type
  TTaskBarPos = (_TOP, _BOTTOM, _LEFT, _RIGHT, _NONE);
var
  Data: TAppBarData;
  pData: ^TAppBarData;
  TBPos: TTaskBarPos;
begin
  Data.cbSize := SizeOf(TAppBarData);
  pData := @Data;

  if SHAppBarMessage(ABM_GETTASKBARPOS, pData) = 1 then
  begin
    if (Data.rc.Left = 0) and (Data.rc.Top = 0) and (Data.rc.Bottom = Screen.Height) then
      TBPos := _LEFT
    else
      if (Data.rc.Left = 0) and (Data.rc.Bottom = Screen.Height) then
        TBPos := _BOTTOM
      else
        if (Data.rc.Top = 0) and (Data.rc.Bottom = Screen.Height) then
          TBPos := _RIGHT
        else
          TBPos := _TOP;
  end
  else
    TBPos := _NONE;

  case TBPos of
    _LEFT :
      begin
        Result.Left := Data.rc.Right + 30;
        Result.Top := Screen.Height - Height - 60;
      end;
    _RIGHT :
      begin
        Result.Left := Data.rc.Left - Width - 30;
        Result.Top := Screen.Height - Height - 60;
      end;
    _TOP :
      begin
        Result.Left := Screen.Width - Width - 30;
        Result.Top := Data.rc.Bottom + 30;
      end;
    _BOTTOM :
      begin
        Result.Left := Screen.Width - Width - 30;
        Result.Top := Data.rc.Top - Height - 60;
      end;
    _NONE :
      begin
        Result.Left := ntNone;
        Result.Top := ntNone;
      end;
  end;
end;

end.

