{**********************************************************************
 *
 * Unit Name: plgiface
 *
 * Purpose  : This unit contains any procedures that may be called by
 *            a plugin.
 *
 * Author   : Simon Steele
 *
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *			  agreement atwww.pnotepad.org/press/psidx.html 
 * History  : 22/07/1999 First Implementation. Very bare. Performed First
 *                       test and all appears to work. Began implementing
 *                       actual functions which may be used by developers.
 *                       NO ACTUAL TESTS PERFORMED OF FUNCTIONS. A test plugin
 *                       will need to be developed.
 *            25/07/1999 Added more procedures to the interface, and began
 *                       testing with the 'test' plugin... All seem ok so
 *                       far. Fixed a couple of problems found with the way
 *                       I was handling pChars, now ok.
 *            26/07/1999 Added: OpenFileA.
 *            14/08/1999 Added: PrintFileA, PrintA.
 *            07/10/1999 Added: GetDirectory, GetDirectorySize, GetCurrFilename,
 *                       GetCurrFilenameSize.
 *            14/12/1999 Added: RefreshSchemes, SetStatusA.
 *            19/05/2000 Added: SetResults, GetModified.
   *********************************************************************}

{$DEFINE plugin}

unit plgiface;

interface

uses sysutils, classes, Windows, menus, dialogs;

{$IFDEF plugin}
  function SaveFileA : Boolean; stdcall;
  function GetSelSizeA : Longint; stdcall;
  function GetFileCountA : Longint; stdcall;
  function GetSelA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  function SetSelTextA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  function GetSelPosA(var LI1, LI2 : Longint) : Boolean; stdcall;
  function GetHandleA(var LI1, LI2 : Longint) : Boolean; stdcall;
  function SetSelPosA(LI1, LI2 : Longint) : Boolean; stdcall;
  function AddMenuItemA(PluginName, Caption, EventProc : PChar; Menu, Position : Integer) : Boolean; stdcall;
  function AddMenuItemExA(PluginName, Caption, EventProc, ShortCut : PChar; ContainerType, Container, Position, Data : Integer) : Boolean; stdcall;
  function GetFileListSizeA : Longint; stdcall;
  function GetFileListA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  function OpenFileA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  function SetStatusA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  function PrintA : Boolean; stdcall;
  function PrintFileA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  function CloseA : Boolean; stdcall;
  function CloseAllA : Boolean; stdcall;
  function GetCurrFileNameSizeA : Longint; stdcall;
  function GetCurrFileNameA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  function GetDirectorySizeA(var Dir, Size : Longint) : Boolean; stdcall;
  function GetDirectoryA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  function RefreshSchemesA : Boolean; stdcall;
  function NewFileFromA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  function GetHTMLSchemeA : Longint; stdcall;
  function GetFormPositionA(var Top, Left : Longint) : Boolean; stdcall;
  function GetFormSizeA(var Height, Width : Longint) : Boolean; stdcall;
  function AddLogEntryA(Entry : PChar; var EntrySize : LongInt) : Boolean; stdcall;
  function SetResultsA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  function AddResultA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  function GetModifiedA : Boolean; stdcall;
  function ClearResultsA : Boolean; stdcall;
  function GetLongestLineA : LongInt; stdcall;
  function GetLineA(LineNo : Longint; Buffer : PChar; var BufferSize : Integer) : Boolean; stdcall;

{$ENDIF}

var LastDirReq : Integer;

implementation

uses main, editor, pntypes, eports;

{$ifdef plugin}

function GetSelSizeA : Longint; stdcall;
var Ed : tfrmClient;
begin
  // Return the selLength property of the current editor.
   Ed := frmMain.GetCurrentEditor;
  if Assigned(Ed) then
     Result := Ed.synMDI.SelLength
  else
     Result := -1;
end;

function GetSelA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
var Ed : tfrmClient;
    s  : string;
begin
   Ed := frmMain.GetCurrentEditor;
   if not Assigned(Ed) then
   begin
      Result := False;
      Exit;
   end;
   s := Ed.synMDI.SelText;
   if Length(s) > BufferSize - 1 then s := Copy(s, 1, BufferSize - 1);
   StrPCopy(Buffer, s);
   BufferSize := StrLen(Buffer);
   Result := True;
end;

function SetSelTextA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
var Ed : tfrmClient;
    s  : string;
begin
   Ed := frmMain.GetCurrentEditor;
   if not Assigned(Ed) then
   begin
      Result := False;
      Exit;
   end;
   s := string(Buffer);
   Ed.synMDI.SelText := s;
   Result := True;
end;

function AddMenuItemA(PluginName, Caption, EventProc : PChar; Menu, Position : Integer) : Boolean; stdcall;
var
   mi : TPluginMI;
begin
// Add a menuitem to the main form...
   mi := TPluginMI.Create(nil);
   mi.Caption := string(Caption);
   mi.Tag := 80 + frmMain.PluginMenuItems.Count;
   mi.OnClick := frmMain.MenuItemClick;
   mi.plugin := PluginName;
   mi.evname := EventProc;
   //Result := mi.Handle;
   Result := True;
   frmMain.MainMenu1.Items[Menu].Insert(Position, mi);
   frmMain.PluginMenuItems.Add(mi);
end;


function AddMenuItemExA(PluginName, Caption, EventProc, ShortCut : PChar; ContainerType, Container, Position, Data : Integer) : Boolean; stdcall;
var
   mi : TPluginMI;
begin
// Add a menuitem to the main form...
   mi := TPluginMI.Create(nil);
   mi.Caption := string(Caption);
   mi.Tag := 80 + frmMain.PluginMenuItems.Count;
   mi.ImageIndex := Data;
   mi.OnClick := frmMain.MenuItemClick;
   try
     mi.ShortCut := TextToShortCut(ShortCut);
   except
   end;
   mi.plugin := PluginName;
   mi.evname := EventProc;
   //Result := mi.Handle;
   Result := True;
   frmMain.MainMenu1.Items[Container].Insert(Position, mi);
   frmMain.PluginMenuItems.Add(mi);
end;


function GetSelPosA(var LI1, LI2 : Longint) : Boolean; stdcall;
var
   Ed : tfrmClient;
begin
   Ed := frmMain.GetCurrentEditor;
   if not Assigned(Ed) then
   begin
      Result := False;
      Exit;
   end;
   LI1 := Ed.synMDI.SelStart;
   LI2 := Ed.synMDI.SelLength;
   Result := True;
end;

function SetSelPosA(LI1, LI2 : Longint) : Boolean; stdcall;
var
   Ed : tfrmClient;
begin
   Ed := frmMain.GetCurrentEditor;
   if not Assigned(Ed) then
   begin
      Result := False;
      Exit;
   end;
   Ed.synMDI.SelStart := LI1;
   Ed.synMDI.SelLength := LI2;
   Result := True;
end;

function GetFileListSizeA : Longint; stdcall;
var n : integer;
    s : string;
begin
   s := '';
   for n := frmMain.tabs.tabs.Count - 1 downto 0 do
   begin
      if Length(s) > 0 then s := s + #13#10;
      s := s + TfrmClient(frmMain.Tabs.Tabs.Objects[n]).FileName;
   end;
   Result := StrLen(PChar(s));
end;

function GetFileListA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
var n : integer;
    s : string;
begin
   s := '';
   for n := frmMain.tabs.tabs.Count - 1 downto 0 do
   begin
      if Length(s) > 0 then s := s + #13#10;
      s := s + TfrmClient(frmMain.Tabs.Tabs.Objects[n]).FileName;
   end;
   if Length(s) > BufferSize - 1 then s := Copy(s, 1, BufferSize -1);
   StrPCopy(Buffer, s);
   Result := True;
   BufferSize := StrLen(Buffer);
end;

function OpenFileA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
var s : string;
begin
   // Open a file...
   s := string(Buffer);
   if not FileExists(s) then
   begin
     Result := False;
     Exit;
   end;
   if frmMain.fStarted then
   begin
     If Copy(s, 0, 1) <> '/' Then
        frmMain.OpenFile(s, ExtractFileExt(s));
   end else
   begin
      If not Assigned(frmMain.OpenOnStart) Then frmMain.OpenOnStart := tStringList.Create;
      frmMain.OpenOnStart.Add(s);
   end;
   Result := True;
end;

function GetHandleA(var LI1, LI2 : Longint) : Boolean; stdcall;
begin
   if not (LI1 in [0..2]) then
   begin
      Result := False;
      LI2 := 0;
      Exit;
   end;
   Result := True;
   case LI1 of
      0 : LI2 := Longint(frmMain.Handle);
      1 : begin
             if frmMain.MDIChildCount > 0 then
                LI2 := Longint(frmMain.ActiveMDIChild.Handle);
          end;
      2 : LI2 := Longint(frmMain.AppHandle);
   end;
end;

function GetFileCountA : Longint; stdcall;
begin
   // Return the number of open files...
   Result := frmMain.MDIChildCount;
end;

function SaveFileA : Boolean; stdcall;
begin
   // Open a file...
   if frmMain.MDIChildCount < 1 then
   begin
     Result := False;
     Exit;
   end;
   frmMain.actSaveExecute(nil);
   Result := True;
end;

function PrintA : Boolean; stdcall;
var
   Ed : tfrmClient;
begin
   Ed := frmMain.GetCurrentEditor;
   if not Assigned(Ed) then
   begin
      Result := False;
      Exit;
   end;
   if Ed.Mode <> emHex then
   begin
      Ed.SynMDI.Print;
   end else
   begin
      Ed.HexPrint(Ed.HexEditor.Font);
   end;
   Result := True;
end;

function PrintFileA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
var
   Ed : tfrmClient;
begin
   Ed := tfrmClient.Create(nil);
   Ed.Execute(string(Buffer), ExtractFileExt(string(Buffer)));
   if Ed.Mode <> emHex then
   begin
      Ed.SynMDI.Print;
   end else
   begin
      Ed.HexPrint(Ed.HexEditor.Font);
   end;
   Ed.synMDI.Modified := False;
   Ed.Free;
   Result := True;
end;

function CloseA : Boolean; stdcall;
var fEditor : tfrmClient;
begin
   fEditor := frmMain.GetCurrentEditor;
   if Assigned(fEditor) then
   begin
      frmMain.CloseFile;
      Result := True;
   end else Result := False;
end;

function CloseAllA : Boolean; stdcall;
var fEditor : tfrmClient;
begin
   fEditor := frmMain.GetCurrentEditor;
   if Assigned(fEditor) then
   begin
      frmMain.CloseAll;
      Result := True;
   end else Result := False;
end;

function GetCurrFileNameSizeA : Longint; stdcall;
var
   s : string;
begin
   if frmMain.tabs.tabs.count < 1 then
   begin
     result := -1;
     exit;
   end;
   s := TfrmClient(frmMain.Tabs.Tabs.Objects[frmMain.Tabs.TabIndex]).FileName;
   Result := StrLen(PChar(s));
end;

function GetCurrFileNameA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
var s : string;
begin
   if frmMain.tabs.tabs.count < 1 then
   begin
     result := False;
     exit;
   end;
   s := TfrmClient(frmMain.Tabs.Tabs.Objects[frmMain.Tabs.TabIndex]).FileName;
   if Length(s) > BufferSize - 1 then s := Copy(s, 1, BufferSize -1);
   StrPCopy(Buffer, s);
   Result := True;
   BufferSize := StrLen(Buffer);
end;

// Get Directory Directories:
// 0 : Application directory
// 1 : Scheme directory

function GetDirectoryA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
var s : string;
begin
   Case LastDirReq of
      0 : s := extractfilepath( paramstr( 0 ) );
      1 : s := frmMain.SchemePath
   end;
   if Length(s) > BufferSize - 1 then s := Copy(s, 1, BufferSize -1);
   StrPCopy(Buffer, s);
   Result := True;
   BufferSize := StrLen(Buffer);
end;

function SetStatusA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
begin
  frmMain.SetStatus(String(Buffer));
  Result := true;
end;

function GetDirectorySizeA(var Dir, Size : Longint) : Boolean; stdcall;
begin
   LastDirReq := Dir;
   Case LastDirReq of
      0 : Size := Strlen( pChar( extractfilepath( paramstr( 0 ) ) ) );
      1 : Size := Strlen( pChar( frmMain.SchemePath ) );
   end;
   Result := true;
end;

function RefreshSchemesA : Boolean; stdcall;
begin
  if MessageDlg('About to Close All Files,'+#13#10+'Continue?',
                mtWarning,
                [mbOK, mbCancel],
                0) = idOK then
  begin
    try frmMain.Parsers.Refresh except end;
    result := true;
  end else Result := False;
end;

function NewFileFromA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
begin
  Result := True;
  frmMain.NewFromTemplate(BufferSize, String(Buffer));
end;

function GetHTMLSchemeA : Longint; stdcall;
begin
  Result := frmMain.Parsers.HTML;
end;

function GetFormSizeA(var Height, Width : Longint) : Boolean; stdcall;
begin
  case Assigned(frmMain.Opt) of
    True : begin Height := frmMain.Opt.Height; Width := frmMain.Opt.Width; end;
    False : begin Height := frmMain.Height; Width := frmMain.Width; end;
  end;
  Result := True;
end;

function GetFormPositionA(var Top, Left : Longint) : Boolean; stdcall;
begin
  case Assigned(frmMain.Opt) of
    True : begin Top := frmMain.Opt.Top; Left := frmMain.Opt.Left; end;
    False : begin Top := frmMain.Top; Left := frmMain.Left; end;
  end;
  Result := True;
end;

function AddLogEntryA(Entry : PChar; var EntrySize : LongInt) : Boolean; stdcall;
begin
  frmMain.logMain.Add('PluginOutput', string(Entry));
  Result := True;
end;

function SetResultsA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
begin
  frmMain.SetResults(String(Buffer));
  Result := true;
end;

function AddResultA(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
begin
  frmMain.AddResult(string(Buffer));
  Result := True;
end;

function ClearResultsA; stdcall;
begin
  frmMain.ClearResults;
  Result := True;
end;

function GetModifiedA : Boolean; stdcall;
var FEditor : TfrmClient;
begin
  Result := True;
  FEditor := frmMain.GetCurrentEditor;
  if not Assigned(fEditor) then
  begin
    Exit;
  end;
  Result := FEditor.Modified;
end;

function GetLongestLineA : LongInt; stdcall;
var FEditor : TfrmClient;
    n : Integer;
    i : Integer;
begin
  Result := 0;
  FEditor := frmMain.GetCurrentEditor;
  if not Assigned(fEditor) then
  begin
    Exit;
  end;
  for n := 0 to FEditor.synMDI.Lines.Count - 1 do
  begin
    i := Length(FEditor.synMDI.Lines[n]);
    if i > Result then
      Result := i;
  end;
end;

function GetLineA(LineNo : Longint; Buffer : PChar; var BufferSize : Integer) : Boolean; stdcall;
var FEditor : TfrmClient;
    s       : string;
begin
  Result := False;
  FEditor := frmMain.GetCurrentEditor;
  if not Assigned(fEditor) then
  begin
    BufferSize := -1;
    Exit;
  end;
  if LineNo > (FEditor.synMDI.Lines.Count -1) then
  begin
    BufferSize := -2;
    Exit;
  end;
  s := FEditor.synMDI.Lines[LineNo];
  if Length(s) > BufferSize - 1 then
  begin
    BufferSize := Length(s) - BufferSize;
    s := Copy(s, 1, BufferSize -1);
    StrPCopy(Buffer, s);
    Result := False;
  end else
  begin
    StrPCopy(Buffer, s);
    BufferSize := StrLen(Buffer);
    Result := True;
  end;
end;

{$endif}
begin
   LastDirReq := 0;
end.
