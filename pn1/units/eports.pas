{**********************************************************************
 *
 * Unit Name: eports
 *
 * Purpose  : This unit provides the exported functions that plugins will
 *            call.
 *
 * Author   : Simon Steele
 *
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license
 *			  agreement at www.pnotepad.org/press/psidx.html.
 *
 * History  : 24/09/2000 Transferred the exports from the Plugins DLL
 *                       to export from the actual Programmers Notepad
 *                       executable. This unit is not used by any other
 *                       unit, and simply calls functions in the plgiface
 *                       unit.
 *            19/11/2000 Added a load of functions. Converted all bool results
 *                       to longbools.
 *********************************************************************}

unit eports;

interface

implementation

uses plgiface;

const
  PluginIFVersion = 5;

function ShowAbout(PluginName, PluginCo, PluginURL, PluginEmail, PluginVersion, PluginDesc : PChar) : LongBool; stdcall;
begin
   {ShowAboutForm(PluginName, PluginCo, PluginURL, PluginEmail, PluginVersion, PluginDesc);}
   Result := False;
end;

function GetSelSize : Longint; stdcall;
begin
// This returns the length of the selection in PN.
  Result := GetSelSizeA;
end;

function GetSel(Buffer : PChar; var BufferSize : Longint) : LongBool; stdcall;
begin
  Result := GetSelA(Buffer, BufferSize);
end;

function GetSelPos(var StrPos, StrLen : Longint) : LongBool; stdcall;
begin
// This gets the two integer variables stating the position and length
// of any selection in the current editor.
  Result := GetSelPosA(StrPos, StrLen);
end;

function GetHandle(var HandleFor, Res : Longint) : LongBool; stdcall;
begin
// The first integer is which handle is requested, the second returns the Handle.
  Result := GetHandleA(HandleFor, Res);
end;

function SetSelPos(StrPos, StrLen : Longint) : LongBool; stdcall;
begin
// This sets the selection in the current editor to the passed variables.
  Result := SetSelPosA(StrPos, StrLen);
end;

function SetSelText(StrText : PChar; var StrLen : Longint) : LongBool; stdcall;
begin
// This effectively inserts the passed text at the current position,
// and selects it. Much like what the paste from file function does.
  Result := SetSelTextA(StrText, StrLen);
end;

function AddMenuItem(PluginName, Caption, EventProc : pChar; Menu, Position : Integer) : LongBool; stdcall;
begin
// This procedure adds a menuitem to the main interface...
  Result := AddMenuItemA(PluginName, Caption, EventProc, Menu, Position);
end;

function AddMenuItemEx(PluginName, Caption, EventProc, ShortCut : PChar; ContainerType, Container, Position, Data : Integer) : LongBool; stdcall;
begin
  Result := AddMenuItemExA(PluginName, Caption, EventProc, ShortCut, ContainerType, Container, Position, Data);
end;

function GetFileListSize : Longint; stdcall;
begin
  Result := GetFileListSizeA;
end;

function GetFileList(Buffer : PChar; var BufferSize : Longint) : LongBool; stdcall;
begin
  Result := GetFileListA(Buffer, BufferSize);
end;

function OpenFile(Buffer : PChar; var BufferSize : Longint) : LongBool; stdcall;
begin
  Result := OpenFileA(Buffer, BufferSize);
end;

function SaveFile : LongBool; stdcall;
begin
  Result := SaveFileA;
end;

function GetFileCount : Longint; stdcall;
begin
  Result := GetFileCountA;
end;

function Print : LongBool; stdcall;
begin
  Result := PrintA;
end;

function PrintFile(Buffer : PChar; var BufferSize : Longint) : LongBool; stdcall;
begin
  Result := PrintFileA(Buffer, BufferSize);
end;

function CloseAll : LongBool; stdcall;
begin
  Result := CloseAllA;
end;

function Close : LongBool; stdcall;
begin
  Result := CloseA;
end;

function GetCurrFileName(Buffer : PChar; var BufferSize : Longint) : LongBool; stdcall;
begin
  Result := GetCurrFileNameA(Buffer, BufferSize);
end;

function GetCurrFileNameSize : Longint; stdcall;
begin
  Result := GetCurrFileNameSizeA;
end;

function GetDirectory(Buffer : PChar; var BufferSize : Longint) : LongBool; stdcall;
begin
  Result := GetDirectoryA(Buffer, BufferSize);
end;

function GetDirectorySize(var DirectoryRef, DirectorySize : Longint) : LongBool; stdcall;
begin
  Result := GetDirectorySizeA(DirectoryRef, DirectorySize);
end;

function RefreshSchemes : LongBool; stdcall;
begin
  Result := RefreshSchemesA;
end;

function SetStatus(Buffer : PChar; var BufferSize : Longint) : LongBool; stdcall;
begin
  Result := SetStatusA(Buffer, BufferSize);
end;

function NewFileFrom(Buffer : PChar; var Scheme : Longint) : LongBool; stdcall;
begin
  Result := NewFileFromA(Buffer, Scheme);
end;

function GetHTMLScheme : Longint; stdcall;
begin
  Result := GetHTMLSchemeA;
end;

function GetFormSize(var Height, Width : Longint) : LongBool; stdcall;
begin
  Result := GetFormSizeA(Height, Width);
end;

function GetFormPosition(var Top, Left : Longint) : LongBool; stdcall;
begin
  Result := GetFormPositionA(Top, Left);
end;

function AddLogEntry(Entry : PChar; var EntrySize : Longint) : LongBool; stdcall;
begin
  Result := AddLogEntryA(Entry, EntrySize);
end;

function SetResults(Buffer : PChar; var BufferSize : Longint) : LongBool; stdcall;
begin
  Result := SetResultsA(Buffer, BufferSize);
end;

function GetModified : LongBool; stdcall;
begin
  Result := GetModifiedA;
end;

function AddResult(Buffer : PChar; var BufferSize : Longint) : LongBool; stdcall;
begin
  Result := AddResultA(Buffer, BufferSize);
end;

function ClearResults : LongBool; stdcall;
begin
  Result := ClearResultsA;
end;

function GetLongestLine : Longint; stdcall;
begin
  Result := GetLongestLineA;
end;

function GetLine(LineNo : Longint; Buffer : PChar; var BufferSize : Longint) : LongBool; stdcall;
begin
  Result := GetLineA(LineNo, Buffer, BufferSize);
end;

function IFVersion : Integer; stdcall;
begin
  Result := PluginIFVersion;
end;

exports
   ShowAbout,     { These procedures are called by the plugins, and are }
   GetSelSize,    { mostly defined calls in the eports unit.            }
   GetSel,
   GetSelPos,
   SetSelPos,
   SetSelText,
   SetStatus,
   GetFormSize,
   GetFormPosition,
   GetFileListSize,
   GetFileList,
   AddMenuItem,
   AddMenuItemEx,
   OpenFile,
   SaveFile,
   GetFileCount,
   GetHandle,
   Print,
   PrintFile,
   Close,
   CloseAll,
   GetCurrFileNameSize,
   GetCurrFileName,
   GetDirectory,
   GetDirectorySize,
   GetHTMLScheme,
   NewFileFrom,
   RefreshSchemes,
   AddLogEntry,
   SetResults,
   GetModified,
   AddResult,
   ClearResults,
   GetLongestLine,
   GetLine,
   IFVersion;

end.
