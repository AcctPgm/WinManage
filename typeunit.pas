unit TypeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Graphics;

type

  TWinfo = class(TObject)
    wName,
    wWinTitle: String;
    wTop,
    wLeft,
    wBottom,
    wRight,
    wHeight,
    wWidth: integer;
    wHandle: hwnd;
    wProgPath: string;
    wIcon: TIcon;
  end;

  TMoveInfo = class(TObject)
    mHandle: hwnd;
    mLeft,
    mTop,
    mHeight,
    mWidth,
    mTag: integer;
  end;

var
  colIcon, colName, colTitle, colPosition, colSize : longint;

implementation

initialization
begin
	colIcon := 0;
	colName := 1;
  colTitle := 2;
  colPosition := 3;
  colSize := 4;
end;

end.

