{***************************************************************
 *
 * Unit Name: genfuncs
 * Purpose  : General Functions used mainly from Options...
 * Author   : Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *			  agreement at www.pnotepad.org/press/psidx.html.
 **************************************************************}
 
unit genfuncs;

interface

{.$define debug}

uses Classes, Forms, Windows, Registry, SysUtils, Clipbrd,
     ShlObj, ActiveX, ComObj{$ifdef debug}, Dialogs{$endif}, useful,
     ShellAPI, IniFiles;

function AlternateToLFN(alternateName: String): String;
function LFNToAlternate(LongName: String): String;
procedure RegisterWindowsExt(Ext: String);
procedure RegisterWindowsOW(Ext: String);
procedure RemoveWindowsOW(Ext: String);
procedure RestoreWindowsExt(Ext: String);
function  IsMyExt(Ext : String) : Boolean;
function  IsMyOpenWith(Ext : String) : Boolean;
procedure RegisterApp;
procedure SetCBText ( aP : PChar ; aCount : Integer );
function CreateAssociation(Ext, Description, Section : string) : string;
procedure CreateSendToLink(Handle : THandle);
procedure CreateLink(Target,Args,WorkDir,ShortCutName: String);
procedure CreateQuickLaunchLink(Handle : THandle);
procedure CreateDesktopLink(Handle : THandle);
procedure CreateShellSection(Section, Icon : string);
function  BatchFile(InstalledDir : string) : string;
function ChooseIcon(hOwner : THandle; var FileName : string; var Index : Integer) : Boolean;
function GetRegIcon(ext : string; small : boolean) : THandle;
function GetRegIconFileName(ext : string; var Index : integer) : string;
procedure RemoveRegKey(Key : string);
procedure ResetRegKey(Ext, Section : string);
procedure UninstallAssoc;

const RootKey = 'Software\Echo Software\PN';

var IsWinNT : Boolean;

implementation

procedure SysFreeString(const S: WideString); stdcall;
  external 'oleaut32.dll' name 'SysFreeString';

Function PickIconDlgA(hOwner : LongInt; szFilename : PChar;
  cchFilename : LongInt; VAR IconIndex : LongInt) : LongBool;
  stdcall; external 'shell32.dll' index 62;

Function PickIconDlgW(hOwner : LongInt; szFilename : PWideChar;
  cchFilename : LongInt; VAR IconIndex : LongInt) : LongBool;
  stdcall; external 'shell32.dll' index 62;

function GetAnyFile: String;
var
 zSysDir: array [0..MAX_PATH - 1] of Char;
 SysDir: String;
begin
  GetSystemDirectory(zSysDir,MAX_PATH);
  SysDir := StrPas(zSysDir);
  if SysDir[Length(SysDir)] <> '\' then
  begin
    SysDir := SysDir + '\';
  end;
  Result := SysDir + 'SHELL32.DLL';
end;

function ChooseIconNT(hOwner : THandle; szFilename : PChar; var cchFilename, IconIndex : Integer) : Boolean;
var szWFilename : PWideChar;
begin
  szWFilename := StringToOleStr(StrPas(szFilename));
  Result := PickIconDlgW(hOwner, szWFilename, cchFilename, IconIndex);
  StrPCopy(szFilename, WideCharToString(szWFilename));
  SysfreeString(szWFilename);
end;

function ChooseIcon(hOwner : THandle; var FileName : string; var Index : Integer) : Boolean;
var
  zFN : array[0..MAX_PATH - 1] of Char;
  zSize, zIndex : Longint;
begin
  Result := False;
  zSize := MAX_PATH;
  if Index > -1 then zIndex := Index else zIndex := 0;
  if FileName <> '' then
    StrPCopy(zFN, FileName)
  else
    StrPCopy(zFN, GetAnyFile);
  case IsWinNT of
    False : Result := PickIconDlgA(hOwner, zFN, zSize, zIndex);
    True  : Result := ChooseIconNT(hOwner, zFN, zSize, zIndex);
  end;
  FileName := StrPas(zFN);
  Index := zIndex;
end;

function AlternateToLFN(alternateName: String): String;
var
  temp: TWIN32FindData;
  searchHandle: THandle;
begin
  searchHandle := FindFirstFile(PChar(alternateName), temp);
  if searchHandle <> ERROR_INVALID_HANDLE then
    result := String(temp.cFileName)
  else
    result := '';
  Windows.FindClose(searchHandle);
end;

function LFNToAlternate(LongName: String): String;
var
  temp: TWIN32FindData;
  searchHandle: THandle;
begin
  searchHandle := FindFirstFile(PChar(LongName), temp);
  if searchHandle <> ERROR_INVALID_HANDLE then
    result := String(temp.cALternateFileName)
  else
    result := '';
  Windows.FindClose(searchHandle);
end;

procedure CreateShellSection(Section, Icon : string);
var Registry : TRegistry;
Begin
  Registry := TRegistry.Create;
  Try
     Registry.RootKey := HKEY_CLASSES_ROOT;
     Registry.OpenKey(Section + '\Shell\Open\Command',True);
     Registry.WriteString('', Application.ExeName + ' "%1"');
     Registry.CloseKey;
     Registry.OpenKey(Section + '\DefaultIcon', True);
     Registry.WriteString('', Icon);
     Registry.CloseKey;
  finally
     Registry.Free;
  end;
end;


procedure RegisterApp;
var Registry : TRegistry;
    st       : String;
    stOW     : String;
Begin
  st:='PN';
  stOW := 'PN.OpenWith';
  Registry := TRegistry.Create;
  Try
    Try
     Registry.RootKey := HKEY_CLASSES_ROOT;
     Registry.DeleteKey(st + '\Shell');
     Registry.DeleteKey(st + '\DefaultIcon');
     Registry.OpenKey(st + '\Shell\Open\Command',True);
     Registry.WriteString('', Application.ExeName + ' "%1"');
     Registry.CloseKey;
     Registry.OpenKey(st + '\DefaultIcon', True);
     Registry.WriteString('', Application.ExeName + ',1');

     Registry.CloseKey;
     Registry.OpenKey(stOW + '\Shell\PNEditWith\Command', True);
     Registry.WriteString('', Application.ExeName + ' "%1"');
     Registry.OpenKey(stOW + '\Shell\PNEditWith', True);
     Registry.WriteString('', 'Open with PN');
     Registry.CloseKey;
    Except
      // Maybe this is what is causing exceptions???
    End;
  finally
     Registry.Free;
  End;
End;

procedure RegisterWindowsExt(Ext: String);
var Registry : TRegistry;
    OldVal   : String;
Begin
  Registry := TRegistry.Create;
  Try
     Registry.RootKey := HKEY_CLASSES_ROOT;
     Registry.OpenKey(Ext,True);
     OldVal := Registry.ReadString('');
     if UpperCase(Copy(OldVal, 1, 3)) <> 'PN_' then
     begin
       Registry.WriteString('','PN');
       Registry.CloseKey;
       if OldVal <> 'PN' Then
       Begin
          Registry.RootKey := HKEY_LOCAL_MACHINE;
          Registry.OpenKey(RootKey, True);
          Registry.WriteString(Ext, OldVal);
          Registry.CloseKey;
       end;
     end;
  finally
     Registry.Free;
  End;
End;

function CreateAssociation(Ext, Description, Section : string) : string;
var Registry : TRegistry;
    OldVal   : String;
    dsc      : string;
Begin
  Registry:=TRegistry.Create;
  Try
    Registry.RootKey := HKEY_CLASSES_ROOT;
    Registry.OpenKey(Ext, True);
    OldVal := Registry.ReadString('');
    Registry.WriteString('', Section);
    Registry.CloseKey;
    dsc := Description;
    if (dsc = '') and (OldVal <> '') then
    begin
      Registry.OpenKey(OldVal, True);
      dsc := Registry.ReadString('');
      Registry.CloseKey;
    end;
    Registry.OpenKey(Section, True);
    Registry.WriteString('', dsc);
    Registry.CloseKey;
    if OldVal <> 'PN' Then
    Begin
      Registry.RootKey := HKEY_LOCAL_MACHINE;
      Registry.OpenKey(RootKey, True);
      Registry.WriteString(Ext, OldVal);
      Registry.CloseKey;
      Result := OldVal;
    end else
      Result := '';
  finally
    Registry.Free;
  end;
end;

procedure RegisterWindowsOW(Ext: String);
var Registry : tRegistry;
    subkey   : string;
begin
  Registry := TRegistry.Create;
  Try
     Registry.RootKey := HKEY_CLASSES_ROOT;
     Registry.OpenKey(Ext, True);
     SubKey := Registry.ReadString('');
     If SubKey <> '' then
     begin
        Registry.CloseKey;
        Registry.OpenKey(SubKey + '\shell\PNEditWith', True);
        Registry.WriteString('', 'Open with PN');
        Registry.CloseKey;
        Registry.OpenKey(SubKey + '\shell\PNEditWith\Command', True);
        Registry.WriteString('', Application.ExeName + ' %1');
        Registry.CloseKey;
     end else
     begin
        Registry.WriteString('', 'PN.OpenWith');
        Registry.Closekey;
     end;
   finally
      Registry.Free;
   End;
end;

procedure RemoveWindowsOW(Ext: String);
var Registry : tRegistry;
    subkey   : string;
begin
   Registry := tRegistry.Create;
   try
     Registry.RootKey := HKEY_CLASSES_ROOT;
     Registry.OpenKey(Ext, True);
     SubKey := Registry.ReadString('');
     If uppercase(SubKey) <> 'PN.EDITWITH' then
     begin
        Registry.CloseKey;
        Registry.DeleteKey(subkey + '\shell\PNEditWith\Command');
        Registry.DeleteKey(subkey + '\shell\PNEditWith');
     end else
     begin
        Registry.WriteString('', '');
        Registry.Closekey;
     end;
   finally
      Registry.free;
   end;
end;

procedure RestoreWindowsExt(Ext: String);
var Registry : TRegistry;
    OldVal   : String;
Begin
  Registry := TRegistry.Create;
  Try
     Registry.RootKey := HKEY_LOCAL_MACHINE;
     Registry.OpenKey(RootKey, True);
     OldVal := Registry.ReadString(Ext);
     Registry.DeleteValue(Ext);
     Registry.CloseKey;
     Registry.RootKey := HKEY_CLASSES_ROOT;
     Registry.OpenKey(Ext,True);
     If uppercase(Registry.ReadString('')) = 'PN' Then
        Registry.WriteString('',OldVal);
     Registry.CloseKey;
  finally
     Registry.Free;
  End;
End;

function IsMyExt(Ext : String) : Boolean;
var Registry : TRegistry;
begin
   Registry := tRegistry.Create;
   Try
      Registry.RootKey := HKEY_CLASSES_ROOT;
      Registry.OpenKey(Ext, True);
      If (uppercase(Registry.ReadString('')) = 'PN') or
         (UpperCase(Copy(Registry.ReadString(''), 1, 3)) = 'PN_') Then
         Result := True Else
         Result := False;
      Registry.CloseKey;
   finally
      Registry.Free;
   End;
end;

function IsMyOpenWith(Ext : String) : Boolean;
var Registry : tRegistry;
    s        : string;
begin
   Registry := tRegistry.Create;
   Try
      Registry.RootKey := HKEY_CLASSES_ROOT;
      Registry.OpenKey(Ext, True);
      s := Registry.ReadString('');
      Registry.CloseKey;
      If Registry.KeyExists(s + '\shell\PNEditWith') then
         Result := True Else
         Result := False;
   finally
      Registry.Free;
   end;
end;

procedure SetCBText ( aP : PChar ; aCount : Integer );
var
  Data: THandle;
  DataPtr: Pointer;
begin
  try
    Data := GlobalAlloc(GMEM_MOVEABLE, aCount);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(aP^, DataPtr^, aCount);
        ClipBoard.SetAsHandle ( CF_TEXT , Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally

  end;
end;

procedure CreateSendToLink(Handle : THandle);
var
  path           : lpstr;
  PIDL           : pItemIDList;
  s              : string;
begin
  // Get the path of the Send To Folder...
  path := StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(Handle, CSIDL_SENDTO, PIDL);
  If SHGetPathFromIDList(PIDL, Path) then
  begin
    s := path;
  end;
  StrDispose(Path);
  CreateLink(ParamStr(0), '', ExtractFilePath(ParamStr(0)), s + '\Programmers Notepad.lnk');
end;

procedure CreateQuickLaunchLink(Handle : THandle);
var
  path : lpstr;
  PIDL : pItemIDList;
  s    : string;
const
  c    = '\Microsoft\Internet Explorer\Quick Launch';
begin
  path := StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(Handle, CSIDL_APPDATA, PIDL);
  If SHGetPathFromIDList(PIDL, Path) then
  begin
    s := path;
  end;
  StrDispose(Path);

  CreateLink(ParamStr(0), '', ExtractFilePath(ParamStr(0)), s + c + '\Programmers Notepad.lnk');
end;

procedure CreateDesktopLink(Handle : THandle);
var
  path : lpstr;
  PIDL : pItemIDList;
  s    : string;
begin
  path := StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(Handle, CSIDL_DESKTOP, PIDL);
  If SHGetPathFromIDList(PIDL, Path) then
  begin
    s := path;
  end;
  StrDispose(Path);
  CreateLink(ParamStr(0), '', ExtractFilePath(ParamStr(0)), s + '\Programmers Notepad.lnk');
end;

function BatchFile(InstalledDir : string) : string;
Var
  PathStr : pChar;
  PathLen : Integer;
  Path    : String;
  Handle  : Text;
Begin
  Result := '';
  {Make Path := GetWindowsDir+'\COMMAND'}
  PathLen := GetWindowsDirectory(nil, 0);
  GetMem(PathStr, PathLen);
  GetWindowsDirectory(PathStr, PathLen);
  if DirExists(PathStr + '\command') then
    Path := PathStr + '\command'
  else
    Path := PathStr;
  FreeMem(PathStr, PathLen);
  {Construct and Write Batch File}
  AssignFile(Handle, Path + '\pn.bat');
  Try Rewrite(Handle); except Exit; end;
  Try
    Writeln(Handle, '@ECHO OFF');
    Write(Handle, '"' + InstalledDir + '\PN.EXE" %1 %2 %3 %4 %5 %6 %7 %8 %9');
  Finally
    CloseFile(Handle);
  end;
  Result := Path + '\pn.bat';
end;

{..}

// TARGET is the file that the shortcut will point to.

// ARGS is the command-line arguments you wish to pass to the Target app.

// WORKDIR is the directory that will be current when the link is launched.

// SHORTCUTNAME is the name of the shortcut link file.

// LOC is the location the file should go. Uses the WinInfo unit to get the location
//   of special system locations. Set to slNone if ShortCutName fully defines the
//   location of the shortcut.

procedure CreateLink(Target,Args,WorkDir,ShortCutName: String);
var
  IObj    : IUnknown;
  Link    : IShellLink;
  IPFile  : IPersistFile;
  TargetW : WideString;
begin
  IObj := CreateComObject(CLSID_ShellLink);
  Link := IObj as IShellLink;
  IPFile  := IObj as IPersistFile;
  with Link do
    begin
      SetPath(PChar(Target));
      SetArguments(PChar(Args));
      SetWorkingDirectory(PChar(WorkDir));
    end;
  TargetW := ShortCutName;
  IPFile.Save(PWChar(TargetW),False);
end;

function GetRegIconFileName(ext : string; var Index : integer) : string;
var Registry : TRegistry;
    Handler  : string;
    Icon     : string;
begin
  Registry := tRegistry.Create;
  Try
    Registry.RootKey := HKEY_CLASSES_ROOT;
    Registry.OpenKey(Ext, True);
    Handler := Registry.ReadString('');
    if Handler <> '' then
    begin
      Registry.CloseKey;
      Registry.OpenKey(Handler + '\DefaultIcon', True);
      Icon := Registry.ReadString('');
      if Icon <> '' then
      begin
        if Pos(',', Icon) <> 0 then
        begin
          try
            Index := StrToInt(Copy(Icon, Pos(',', Icon) + 1, Length(Icon)));
          except
            Index := -1;
          end;
          Icon := Copy(Icon, 1, Pos(',', Icon) - 1);
          Result := Icon;
        end else
          Index := 0;
      end else
        Result := '';
    end else Result := '';
  finally
    Registry.Free;
  end;
end;

function GetRegIcon(ext : string; small : boolean) : THandle;
var Registry : TRegistry;
    Handler  : string;
    Icon     : string;
    Index    : Integer;
    v, w     : HICON;
begin
   Result := 0;
   Registry := tRegistry.Create;
   Try
      Registry.RootKey := HKEY_CLASSES_ROOT;
      Registry.OpenKey(Ext, True);
      Handler := Registry.ReadString('');
      if Handler <> '' then
      begin
        Registry.CloseKey;
        Registry.OpenKey(Handler + '\DefaultIcon', True);
        Icon := Registry.ReadString('');
        if Icon <> '' then
        begin
          if Pos(',', Icon) <> 0 then
          begin
            Index := StrToInt(Copy(Icon, Pos(',', Icon) + 1, Length(Icon)));
            Icon := Copy(Icon, 1, Pos(',', Icon) - 1);
          end else
            Index := 0;
          case small of
            False : try Result := ExtractIcon(hInstance, PChar(Icon), Index); except end;
            True :
            begin
              ExtractIconEx(PChar(Icon),Index,v,w,1);
              Result := w;
            end;
          end;
        end else
          Result := 0;
      end else
        Result := 0;
      Registry.CloseKey;
   finally
      Registry.Free;
   end;
end;

procedure RemoveRegKey(Key : string);
var Registry : TRegistry;
begin
  // Remove a Registry section from HKEY_CLASSES_ROOT
  Registry := TRegistry.Create;
  Try
    Registry.RootKey := HKEY_CLASSES_ROOT;
    Registry.DeleteKey(Key);
  Finally
    Registry.Free;
  end;
end;

procedure ResetRegKey(Ext, Section : string);
var Registry : TRegistry;
Begin
  Registry:=TRegistry.Create;
  Try
    Registry.RootKey := HKEY_CLASSES_ROOT;
    Registry.OpenKey(Ext, True);
    Registry.WriteString('', Section);
    Registry.CloseKey;
  finally
    Registry.Free;
  end;
end;

procedure UninstallAssoc;
var path : string;
    myi  : tIniFile;
    i    : Integer;
    n    : Integer;
    s    : string;
    p    : string;
    sl   : TStringList;
begin
  path := ExtractFilePath(ParamStr(0));
  myi := tIniFile.Create(path + 'reguninst.ini');
  try
    i := myi.ReadInteger('Uninstall', 'Number', 0);
    for n := 1 to i do
    begin
      p := myi.ReadString('Uninstall', IntToStr(n), '');
      s := myi.ReadString(p, 'Registry', '');
      if s <> '' then
        RemoveRegKey(s);
      s := myi.ReadString(p, 'FileType', '');
      if s <> '' then
      begin
        p := myi.ReadString(p, 'Original', '');
        ResetRegKey(s, p);
      end;
    end;
  finally
    FreeAndNil(myi);
  end;
  if FileExists(path + 'files.def') then
  begin
    sl := tStringList.Create;
    Try
      sl.LoadFromFile(path + 'files.def');
      sl.Sorted := true;
      For n := 0 to sl.Count - 1 do
      begin
        if IsMyExt(sl[n]) then
        begin
          RestoreWindowsExt(sl[n]);
          if IsMyOpenWith(sl[n]) then
            RemoveWindowsOW(sl[n]);
        end else
        if IsMyOpenWith(sl[n]) then
        begin
          RemoveWindowsOW(sl[n]);
        end;
      end;
    Finally
      FreeAndNil(sl);
    end;
  end;
end;

var
  OVI    : TOsVersionInfo;
initialization
  FillChar(OVI, SizeOf(OVI), 0);
  OVI.dwOSVersionInfoSize := SizeOf(OVI);
  GetVersionEx(OVI);
  IsWinNT := OVI.dwPlatformID = VER_PLATFORM_WIN32_NT;
end.

