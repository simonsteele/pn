{**********************************************************************
 *
 * Unit Name: bkmrkmru
 *
 * Purpose  : Retains a list of bookmarks set for up to ten files.
 *
 * Author   : Simon Steele
 *
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *			  agreement at www.pnotepad.org/press/psidx.html.
 * 
 * History  : 15/08/1999 Work Begun.
 *            17/08/1999 Work Continues. Will use an .INI file to store
 *                       the settings, instead of silly multiple reg.
 *                       keys which would be a pain. Think it's complete.
 *                       Fixed a bug which meant that the bookmarks stayed
 *                       set once set, even after being cleared in the
 *                       editor.
 *            22/08/1999 Added the fInitialised variable to make sure that
 *                       the code can access the tStringLists etc...
 *********************************************************************}

unit bkmrkmru;

interface

uses Classes, SyntaxEd, INIFiles, SysUtils, useful;

   type tBookmarkMRU = class(TObject)
   private
      fFileNames   : TStringList;
      fFileName    : string; // Current Filename...
      fBookMarks   : array[0..9, 0..1] of Longint;
      mrufile      : string;
      fBusy        : Boolean;
      CustomDir    : string;
      Custom       : TMemoryStream;
      fInitialised : Boolean;
      procedure RefreshFilenames;
   public
      procedure SetFileName(fn : string);
      procedure Save;
      function  HasBookmarks(fn : string) : Boolean;
      procedure SetBookmarks(editor : tSyntaxMemo);
      procedure GetBookmarks(editor : tSyntaxMemo);
      procedure ClearBookmarks;
      procedure BeginSession;
      procedure EndSession;
      constructor Initialise;
      destructor Shutdown;
   published
      property Busy : Boolean read fBusy write fBusy;
   end;

implementation

{ tBookmarkMRU }

procedure tBookmarkMRU.ClearBookmarks;
var n : integer;
begin
   // Clear the current bookmarks.
   for n := 0 to 9 do
   begin
      fBookmarks[n, 0] := -1;
      fBookmarks[n, 1] := -1;
   end;
end;

constructor tBookmarkMRU.Initialise;
var MRU : tIniFile;
begin
   // Create the stringlist and
   // Load the bookmark file list up.
   if fInitialised then Exit;
   Create;
   fFileNames := TStringList.Create;
   fFilenames.Sorted := False;
   ClearBookmarks;
   fFileName := '';
   Busy := False;
   mrufile := extractfilepath(ParamStr(0)) + 'mru.ini';
   CustomDir := extractfilepath(ParamStr(0)) + 'Bookmrks\';
   if not DirExists(CustomDir) then
      CreateDir(CustomDir);
   Custom := TMemoryStream.Create;
   MRU := tIniFile.Create(mrufile);
   MRU.ReadSections(fFilenames);
   MRU.Free;
   fInitialised := True;
end;

destructor tBookmarkMRU.Shutdown;
begin
   // Free the stringlist.
   if not fInitialised then Exit;
   fInitialised := False;
   if Assigned(fFilenames) then fFileNames.Free;
   if Assigned(Custom) then Custom.Free;
end;

function tBookmarkMRU.HasBookmarks(fn: string): Boolean;
var MRU : tIniFile;
begin
   // Check if a file has bookmarks.
   Result := False;
   if not fInitialised then Exit;
   MRU := tIniFile.Create(mrufile);
   Result := MRU.SectionExists(lowercase(fn));
   MRU.Free;
end;

procedure tBookmarkMRU.Save;
var MRU : tIniFile;
    n   : integer;
    s   : string;
    fs  : string;
    i   : integer;
begin
   // Save the bookmarks.
   if not fInitialised then Exit;
   MRU := tIniFile.Create(mrufile);
   if MRU.SectionExists(lowercase(fFilename)) then
      MRU.EraseSection(lowercase(fFilename));
   for n := 0 to 9 do
   begin
      if fBookmarks[n, 0] <> -1 then
      begin
         MRU.WriteInteger(fFileName, IntToStr(n) + '_0', fBookmarks[n, 0]);
         MRU.WriteInteger(fFileName, IntToStr(n) + '_1', fBookmarks[n, 1]);
      end;
   end;
   if fFilenames.IndexOf(lowercase(fFileName)) <> -1 then
   begin
      // This file is already in the MRU, move it to the bottom.
      fFilenames.Delete(fFilenames.IndexOf(lowercase(fFileName)));
   end else
   if fFilenames.Count > 9 then
   begin
      s := lowercase(fFilenames[0]);
      try MRU.EraseSection(s); except end;
      fFilenames.Delete(0);
   end;
   // Now we deal with the custom bookmarks...
   for n := 0 to 4 do
   begin
      i := Random(25);
      Inc(i, 97);
      fs := fs + chr(i);
   end;
   fs := fs + IntToStr(Random(99) + Random(27));
   fs := fs + Copy(extractfilename(fFilename), 1, 3);
   fs := fs + '.bmk';
   if Custom.Size > 0 then
   begin
      MRU.WriteString(fFileName, 'CustomFile', fs);
      Custom.SaveToFile(CustomDir + fs);
   end;
   MRU.Free;
   RefreshFilenames;
end;

procedure tBookmarkMRU.SetBookmarks(editor: tSyntaxMemo);
var i1, i2 : integer;
    b      : Byte;
    n      : integer;
begin
   if not fInitialised then Exit;
   ClearBookmarks;
   for n := 0 to 9 do
   begin
      b := n;
      if editor.IsBookmarkSet(b, i1, i2) then
      begin
         fBookmarks[n, 0] := i1;
         fBookmarks[n, 1] := i2;
      end;
      if editor.CustomBookmarkList.Count > 0 then
      begin
         Custom.Clear;
         editor.CustomBookmarksToStream(Custom);
         Custom.Seek(0,0);
      end;
   end;
end;

procedure tBookmarkMRU.SetFileName(fn: string);
begin
   // Set the current filename.
   if not fInitialised then Exit;
   ClearBookmarks;
   Custom.Clear;
   Custom.Seek(0,0);
   fFileName := lowercase(fn);
end;

procedure tBookmarkMRU.GetBookmarks(editor: tSyntaxMemo);
var MRU    : tIniFile;
    i1, i2 : Longint;
    n      : integer;
    s      : string;
begin
   // Set the bookmarks for the current filename.
   if not fInitialised then Exit;
   MRU := tIniFile.Create(mrufile);
   for n := 0 to 9 do
   begin
      i1 := MRU.ReadInteger(lowercase(fFilename), IntToStr(n) + '_0', -1);
      i2 := MRU.ReadInteger(lowercase(fFilename), IntToStr(n) + '_1', -1);
      if i1 <> -1 then
      begin
         editor.SetBookmark(n, i1, i2);
      end;
   end;
   s := MRU.ReadString(lowercase(fFilename), 'CustomFile', '');
   if s <> '' then
   begin
      custom.Clear;
      if FileExists(CustomDir + s) then
      begin
         custom.LoadFromFile(CustomDir + s);
         custom.seek(0,0);
         editor.CustomBookmarksFromStream(custom);
      end;
      custom.Clear;
      DeleteFile(CustomDir + s);
   end;
   MRU.Free;
   ClearBookmarks;
end;

procedure tBookmarkMRU.BeginSession;
begin
   if not fInitialised then Exit;
   Busy := True;
end;

procedure tBookmarkMRU.EndSession;
begin
   if not fInitialised then Exit;
   Busy := False;
end;

procedure tBookmarkMRU.RefreshFilenames;
var MRU : tIniFile;
begin
   if not fInitialised then Exit;
   fFilenames.Clear;
   MRU := tIniFile.Create(mrufile);
   MRU.ReadSections(fFilenames);
   MRU.Free;
end;

end.
