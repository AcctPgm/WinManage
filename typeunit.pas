unit TypeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

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
  end;

  TMoveInfo = class(TObject)
    mHandle: hwnd;
    mLeft,
    mTop,
    mHeight,
    mWidth,
    mTag: integer;
  end;

implementation

end.

