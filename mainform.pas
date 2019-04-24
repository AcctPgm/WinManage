unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, Grids, IniFiles, Windows,
  TypeUnit, WindowFuncs, IniUnit, NearTrayPos, About, Options, SaveForm, types;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnReresh: TButton;
    btnClose: TButton;
    Label1: TLabel;
    mniShow: TMenuItem;
    mniOptions: TMenuItem;
    mniAbout: TMenuItem;
    mniClose: TMenuItem;
    mnuTrayMenu: TPopupMenu;
    mnuGridMenu: TPopupMenu;
    sgdProgs: TStringGrid;
    TrayIcon1: TTrayIcon;

    procedure btnCloseClick(Sender: TObject);
    procedure btnRereshClick(Sender: TObject);

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);

    procedure mniSaveClick(Sender: TObject);
    procedure mniShowClick(Sender: TObject);

    procedure sgdProgsBeforeSelection(Sender: TObject; aCol, aRow: Integer);
    procedure sgdProgsClick(Sender: TObject);
    procedure mniItemClick(Sender: TObject);
    procedure sgdProgsContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure sgdProgsDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure sgdProgsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure TrayIcon1Click(Sender: TObject);

//    procedure TrayIcon1DblClick(Sender: TObject);
    procedure mniOptionsClick(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure mniCloseClick(Sender: TObject);

    procedure SetCloseAllowed(Allowed: Boolean);
  private
    { private declarations }
    CloseAllowed: Boolean;

    InitialLeft: integer;
    InitialTop: integer;

    LastHintRow: integer;

    ExistingItem: string;
    ExistingComment: string;

    procedure GetOpenWindows;
  public
    { public declarations }
    procedure AppDeactivate(Sender: TObject);
  end;

var
  frmMain: TfrmMain;
  stlPositions: TStringList;
  stlWindows: TStringList;

implementation

{$R *.lfm}

{ TfrmMain }

{
  Form Event Handlers
}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  stlWindows := TStringList.Create;
  stlWindows.Sorted := True;;

  stlPositions := TStringList.Create;

  LoadOptions;
  CloseAllowed := CloseFromForm;

  // Labels in the title (0) row
  sgdProgs.Cells[colIcon, 0] := '';
  sgdProgs.Cells[colName,	 0] := 'Program';
  sgdProgs.Cells[colTitle, 0] := 'Title';
  sgdProgs.Cells[colPosition, 0] := 'Position';
  sgdProgs.Cells[colSize, 0] := 'Size';

  if ShowOnStartup then
    Show;

  Application.OnDeactivate:=@AppDeactivate;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  MyIni: TIniFile;
  nt: TNearTray;
begin
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
      Top := MyIni.ReadInteger('Options', 'MainFormTop', Top);
      Left := MyIni.ReadInteger('Options', 'MainFormLeft', Left);
    finally
      MyIni.Free;
    end;
  end;

  // Save the initial position to be able to check whether the form was moved
  InitialLeft := Left;
  InitialTop := Top;

  GetOpenWindows;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  // F5 key - refresh window list
  if Key = $74 then
    GetOpenWindows;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  MyIni: TIniFile;
begin
  // Determine whether to close or just hide the form
  if CloseFromForm then
    CloseAllowed := True;

  if CloseAllowed then
    CloseAction := caFree
  else
    CloseAction := caHide;

  // Save the form position
  if RememberFormPositions and ((Left <> InitialLeft) or (Top <> InitialTop)) then
  begin
    try
      MyIni := TIniFile.Create(GetIniFileName);
      MyIni.WriteInteger('Options', 'MainFormTop', Top);
      MyIni.WriteInteger('Options', 'MainFormLeft', Left);
    finally
      MyIni.Free;
    end;
  end;
end;

procedure TfrmMain.sgdProgsBeforeSelection(Sender: TObject; aCol, aRow: Integer
  );
begin
  sgdProgs.InvalidateRow(sgdProgs.Row);
end;

procedure TfrmMain.sgdProgsClick(Sender: TObject);
begin
  sgdProgs.InvalidateRow(sgdProgs.Row);
end;

procedure TfrmMain.sgdProgsDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  AGrid : TStringGrid;
begin
  AGrid:=TStringGrid(Sender);

  if gdFixed in aState then //if is fixed use the clBtnFace color
    AGrid.Canvas.Brush.Color := clBtnFace
  else
    if aRow = sgdProgs.Row then
      AGrid.Canvas.Brush.Color := $00A8FFFF
    else
      AGrid.Canvas.Brush.Color := clWindow;

  AGrid.Canvas.FillRect(aRect);
  if (aCol = colIcon) and (aRow > 0) then
    if TWinfo(sgdProgs.Objects[colName, aRow]).wIcon.Width = 16 then
	    AGrid.Canvas.Draw(aRect.Left + 1, aRect.Top + 1,
      	TWinfo(sgdProgs.Objects[colName, aRow]).wIcon)
    else
    begin
      aRect.Left := aRect.Left + 1;
      aRect.Top := aRect.Top + 1;
      aRect.Right := aRect.Left + 16;
      aRect.Bottom := aRect.Top + 16;
      AGrid.Canvas.StretchDraw(aRect, TWinfo(sgdProgs.Objects[colName, aRow]).wIcon);
    end
  else
	  AGrid.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, AGrid.Cells[aCol, aRow]);
end;

procedure TfrmMain.sgdProgsMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  R, C: Integer;
begin
  sgdProgs.MouseToCell(X, Y, C, R);
  with sgdProgs do
  begin
    if (R <> LastHintRow) then
    begin
      LastHintRow := R;
      Application.CancelHint;
      if R > 0 then
        sgdProgs.Hint := TWinfo(sgdProgs.Objects[colName, R]).wProgPath
      else
        sgdProgs.Hint := '';
    end;
  end;
end;

{
  Click handlers
}

procedure TfrmMain.btnRereshClick(Sender: TObject);
begin
  GetOpenWindows;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

{
procedure TfrmMain.TrayIcon1DblClick(Sender: TObject);
begin
//  frmMain.Show;
end;
}

procedure TfrmMain.TrayIcon1Click(Sender: TObject);
begin
  frmMain.Show;
end;

{
  Tray icon popup menu items
}

procedure TfrmMain.mniShowClick(Sender: TObject);
begin
  frmMain.Show;
end;

procedure TfrmMain.mniOptionsClick(Sender: TObject);
begin
  frmOptions.Show;
end;

procedure TfrmMain.mniAboutClick(Sender: TObject);
begin
  frmAbout.Show;
end;

procedure TfrmMain.mniCloseClick(Sender: TObject);
begin
  CloseAllowed := True;
  Close;
end;

{
  Grid context popup menu items
}

procedure TfrmMain.mniItemClick(Sender: TObject);
var
  i: Integer;
  mi: TMoveInfo;
begin
  Hide;
  with Sender as TMenuItem do
  begin
    for i := 0 to stlPositions.Count - 1 do
    begin
      mi := TMoveInfo(stlPositions.Objects[i]);
      if mi.mTag = Tag then
        MoveWindow(mi.mHandle, mi.mLeft, mi.mTop, mi.mWidth, mi.mHeight, True)
    end;
  end;
end;

// Create and display the context menu for the right-clicked window
// First item is always 'Save', and if there are ini entries for the program,
// a separator line and each of the entries
procedure TfrmMain.sgdProgsContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  C, R: integer;
  mniSave, mniItem: TMenuItem;

  FullPath: String;
  i: Integer;
  Matches: Integer;
  Moves: TMoveInfo;
  MyIni: TIniFile;
  s: String;
  Sections: TStringList;

  sLeft, sTop, sWidth, sHeight: String;
  iLeft, iTop, iWidth, iHeight: integer;
const
  defaultIkey = -9999;
begin
	// Determine which row, ie program window, was clicked on
  sgdProgs.MouseToCell(MousePos.X, MousePos.Y, C, R);
  if R <> 0 then
  begin
    sgdProgs.Row := R;

    // Clear any previous menu entries
    mnuGridMenu.Items.Clear;

    // Clear the number and comment of the entry matching the clicked window
    ExistingItem := '';
    ExistingComment := '';

    stlPositions.Clear;

    // Create the Save menu item
    mniSave := TMenuItem.Create(mnuGridMenu);
    mniSave.Caption := 'Save';
    mniSave.OnClick := @mniSaveClick;
    mniSave.Tag := R;
    mnuGridMenu.Items.Add(mniSave);

    try
      MyIni := TIniFile.Create(GetIniFileName);
      Sections := TStringList.Create;

      MyIni.ReadSections(Sections);
      Sections.Sorted := True;

      Matches := 0;
      for i := 0 to (Sections.Count - 1) do
      begin
        FullPath := TWinfo(sgdProgs.Objects[colName, sgdProgs.Row]).wProgPath;
        s := Copy(Sections.Strings[i], 1, Length(FullPath));
        if (CompareText(s, FullPath) = 0) then
        begin
          if Matches = 0 then
          begin
            mniItem := TMenuItem.Create(mnuGridMenu);
            mniItem.Caption := '-';
            mnuGridMenu.Items.Add(mniItem);
          end;
          Matches := Matches + 1;

          mniItem := TMenuItem.Create(mnuGridMenu);

          // Read position & size info, disable menu choice if any invalid value
          iLeft := MyIni.ReadInteger(Sections.Strings[i], 'Left', defaultIKey);
          if iLeft = defaultIkey then
          begin
            sLeft := '?';
            mniItem.Enabled := False;
          end
          else
            sLeft := IntToStr(iLeft);

          iTop := MyIni.ReadInteger(Sections.Strings[i], 'Top', defaultIkey);
          if iTop = defaultIkey then
          begin
            sTop := '?';
            mniItem.Enabled := False;
          end
          else
            sTop := IntToStr(iTop);

          iWidth := MyIni.ReadInteger(Sections.Strings[i], 'Width', defaultIkey);
          if iWidth = defaultIkey then
          begin
            sWidth := '?';
            mniItem.Enabled := False;
          end
          else
            sWidth := IntToStr(iWidth);

          iHeight := MyIni.ReadInteger(Sections.Strings[i], 'Height', defaultIkey);
          if iHeight = defaultIkey then
          begin
            sHeight := '?';
            mniItem.Enabled := False;
          end
          else
            sHeight := IntToStr(iHeight);

          mniItem.Caption := MyIni.ReadString(Sections.Strings[i], 'Comment', '?') +
            ' - Pos: ' +  sLeft + ', ' + sTop + ';' +
            ' Size: ' + sWidth + ' x ' + sHeight;

          mniItem.Tag := Matches;
          mniItem.OnClick := @mniItemClick;

          Moves := TMoveInfo.Create;
          Moves.mHandle := TWinfo(sgdProgs.Objects[colName, R]).wHandle;
          Moves.mLeft := iLeft;
          Moves.mTop := iTop;
          Moves.mWidth := iWidth;
          Moves.mHeight := iHeight;
          Moves.mTag := Matches;
          if (TWinfo(sgdProgs.Objects[colName, R]).wLeft = iLeft) and
              (TWinfo(sgdProgs.Objects[colName, R]).wTop = iTop) and
              (TWinfo(sgdProgs.Objects[colName, R]).wWidth = iWidth) and
              (TWinfo(sgdProgs.Objects[colName, R]).wHeight = iHeight) then
          begin
            mniItem.Checked := True;

            // Store the tag of the matching menu item to allow a save to overwrite the item
            ExistingItem := copy(Sections.Strings[i], Length(FullPath) + 2,
          	  Length(Sections.Strings[i]) - Length(FullPath));

            // Store the comment for the matching menu item
            ExistingComment := MyIni.ReadString(Sections.Strings[i], 'Comment', '?');
          end;
          stlPositions.AddObject(IntToStr(Matches), Moves);

          mnuGridMenu.Items.Add(mniItem);
        end;
      end;
    finally
      MyIni.Free;
      Sections.Free
    end;
  end;
end;

procedure TfrmMain.mniSaveClick(Sender: TObject);
var
  DefaultComment: string;
  DefaultName: string;
  i: integer;
  ItemNum: integer;
  Matches: integer;
  MaxItem: integer;
  MyIni: TIniFile;
  ProgPath: string;
  ProgWindowInfo: TWinfo;
  s: string;
  Section: string;
  Sections: TStringList;
begin
  ProgWindowInfo := TWinfo(sgdProgs.Objects[colName, sgdProgs.Row]);
  try
    MyIni := TIniFile.Create(GetIniFileName);

    // Read all section names from the ini file into a string list
    Sections := TStringList.Create;
    MyIni.ReadSections(Sections);


    Matches := 0;
    MaxItem := -1;
    for i := 0 to (Sections.Count - 1) do
    begin
      ProgPath := ProgWindowInfo.wProgPath;
      s := Copy(Sections.Strings[i], 1, Length(ProgPath));
      if (CompareText(s, ProgPath) = 0) then
      begin
        Matches := Matches + 1;
        ItemNum := StrToIntDef(Copy(Sections.Strings[i], length(s) + 2,
          length(Sections.Strings[i]) - length(s) - 1), -1);
        if ItemNum > MaxItem then
          MaxItem := ItemNum;
      end;
    end;

    // Default comment based on the number of entries for the program
    if (ExistingComment <> '') and (ExistingComment <> '?') then
    	DefaultComment := ExistingComment
    else if Matches = 0 then
      DefaultComment := 'Primary'
    else if Matches = 1 then
      DefaultComment := 'Secondary'
    else
      DefaultComment := 'Position ' + IntToStr(Matches + 1);

    // Default display name for the program
    // If there is no existing display name, use the program name (.exe)
    DefaultName := MyIni.ReadString('Display Names', ProgWindowInfo.wProgPath,
			ProgWindowInfo.wName);

    if frmSaveForm.ConfirmSave(ProgWindowInfo, DefaultComment, DefaultName) then
    begin
      // Hide the main form when user chooses to Save settings
      Close;

      // Don't save a blank display name, and only save display name if it's changed
      if frmSaveForm.GetDisplayName <> '' then
        if frmSaveForm.GetDisplayName <> DefaultName then
      	  MyIni.WriteString('Display Names', ProgWindowInfo.wProgPath, frmSaveForm.GetDisplayName);

      // Don't save Comment, position, and size if user selected only to save Dispay Name
      if frmSaveForm.SaveWhat <> svNameOnly then
      begin
        ProgPath := ProgWindowInfo.wProgPath;
        if ExistingItem = '' then
	        Section := ProgPath + ';' + IntToStr(MaxItem + 1)
        else
	        Section := ProgPath + ';' + ExistingItem;
        MyIni.WriteString(Section, 'Comment', frmSaveForm.GetComment);

        MyIni.WriteInteger(Section, 'Left', ProgWindowInfo.wLeft);
        MyIni.WriteInteger(Section, 'Top', ProgWindowInfo.wTop);
        MyIni.WriteInteger(Section, 'Width', ProgWindowInfo.wWidth);
        MyIni.WriteInteger(Section, 'Height', ProgWindowInfo.wHeight);
      end;
    end;
  finally
    MyIni.Free;
  end;
end;

{
  Other form procedures
}

procedure TfrmMain.SetCloseAllowed(Allowed: Boolean);
begin
  CloseAllowed := Allowed;
end;

procedure TfrmMain.GetOpenWindows;
var
  i: integer;
  NewRow: integer;
  MyIni: TIniFile;
  AltName: string;
begin
  // Clear the window list & grid (keep only row 1, the heading row)
  stlWindows.Clear;
  sgdProgs.RowCount := 1;

  // Get the open window information
  EnumWindows(@EnumWindowsToStrList, lparam(stlWindows));

  // Add the open window information to the grid
  try
    MyIni := TIniFile.Create(GetIniFileName);

    for i:= 0 to stlWindows.Count - 1 do
    begin
      sgdProgs.RowCount := sgdProgs.RowCount + 1;
      NewRow := sgdProgs.RowCount - 1;

      // Fill the grid with program window info
      with TWinfo(stlWindows.Objects[i]) do
      begin
      	// Read the display (alternate) name for the program from the ini file
        // Default to program.exe if there is no alternate name
        AltName := MyIni.ReadString('Display Names', wProgPath, '');
        if AltName = '' then
	        sgdProgs.Cells[colName, NewRow] := wName
        else
	        sgdProgs.Cells[colName, NewRow] := AltName;

        sgdProgs.Cells[colTitle, NewRow] := wWinTitle;
        sgdProgs.Cells[colPosition, NewRow] := IntToStr(wLeft) + ', ' + IntToStr(wTop);
        sgdProgs.Cells[colSize, NewRow] := IntToStr(wWidth) + ' x ' + IntToStr(wHeight);
      end;

      // Save the TWinfo for the program as the object of the name column
      sgdProgs.Objects[colName, NewRow] := TWinfo(stlWindows.Objects[i]);
    end;
  finally
    MyIni.free;
  end;
end;

procedure TfrmMain.AppDeactivate(Sender: TObject);
begin
  if HideOnClickAway then
    Hide;
end;

end.

